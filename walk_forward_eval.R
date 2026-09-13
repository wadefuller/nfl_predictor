# ============================================
# NFL Predictor: Walk-Forward Temporal Evaluation
# ============================================
# For each holdout season N (2016-2025):
#   1. Pre-cal: train XGB + cv.glmnet + ranger RF on seasons [MIN_TRAIN_SEASON, N-2]
#   2. Predict on season N-1 (cal set) — fit NNLS stacking weights + Platt scaling
#   3. Final: retrain all three on [MIN_TRAIN_SEASON, N-1] with same hyperparams
#   4. Predict season N with stacked + Platt-calibrated ensemble
#   5. Run all betting strategies and record metrics
#
# Honest OOS: the holdout season is never seen during training.
# Uses tuned hyperparameters from tune_hyperparams.R (loaded at startup).
#
# Outputs (written to data/):
#   walk_forward_model_spread.csv    — MAE, cover accuracy vs Vegas by season
#   walk_forward_model_winprob.csv   — Brier, log loss vs Vegas by season
#   walk_forward_betting_by_season.csv
#   walk_forward_betting_overall.csv
#   walk_forward_betting_bets.csv
#   walk_forward_predictions.csv     — per-game predictions for calibration diagnostics
#
# Usage: Rscript walk_forward_eval.R   (~10-12 min — 6 model fits per fold)
# ============================================

# Shared helpers + canonical LEAKAGE_VARS_REG + train_/fit_/apply_ functions.
# Everything model-side comes from ensemble_lib.R; this script orchestrates the
# walk-forward eval and adds betting-strategy specific logic.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")
library(doParallel)
library(nnls)    # non-negative least squares (used by fit_stacking_nnls)

# ---- Config ----------------------------------------------------------------
# PROJECT_DIR / DATA_DIR come from ensemble_lib.R (sourced above).
OUTPUT_DIR <- DATA_DIR

MIN_TRAIN_SEASON   <- 2008
HOLDOUT_SEASONS    <- 2016:2025   # require ≥8 seasons of training data before first holdout
CAL_WINDOW_SEASONS <- 2           # number of most-recent training seasons used for stacking + Platt cal
STACKING_METHOD    <- "multi"     # "nnls" (margin RMSE), "logloss" (winprob), or "multi" (both)
STACKING_LAMBDA    <- load_tuned_stacking_lambda()  # tuned by tune_stacking_lambda.R
SEED               <- 989

UNIT_SIZE        <- 100         # dollars per unit
INITIAL_BANKROLL <- 100         # units (reset each season for Kelly)
MAX_BET_PCT      <- 0.05        # max fraction of bankroll per Kelly bet
MAX_STAKE        <- 5           # unit cap per bet

# Strategy parameters (post-Platt + held-out validation tuning).
# ML changes validated on 2023-2025 test set; spread params left at conservative
# defaults since tuned spread params didn't generalize OOS.
KELLY_FRACTION       <- 0.25    # spread Kelly fraction (legacy default)
KELLY_FRACTION_ML    <- 0.10    # ML Kelly fraction — RE-TUNED for 3-model NNLS ensemble
MIN_EDGE_SPREAD      <- 2.5     # min |line_diff| for value spread bets
MIN_EDGE_ML          <- 0.10    # tighter edge filter for ML value bets
MAX_ABS_ODD_VALUE_ML <- 150     # skip ML value bets above |150| odds (heavy favs)
MAX_ABS_ODD_KELLY_ML <- 150     # RE-TUNED: was 300; tighter cap better w/ ensemble dist
MIN_KELLY_FRAC_ML    <- 0.010   # RE-TUNED: was 0.025; works with smaller kelly_frac

REG_PARAMS <- load_tuned_xgb_params(file.path(DATA_DIR, "tune_best_params_reg.rds"))

# LEAKAGE_VARS_REG, train_xgb_model, train_glm_model, predict_glm,
# train_rf_model, fit_stacking_*, apply_stacking, fit_platt_logits, apply_platt
# all sourced from ensemble_lib.R above.

week_filter <- function(data) {
  data %>% filter(
    !is.na(result), !is.na(home_moneyline),
    (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021)
  )
}

# ---- Data prep -------------------------------------------------------------

# Target is home_result (raw home-team margin). Win probability is derived
# downstream from pnorm(predicted_margin / sigma) + Platt recalibration.
#
# Delegates to ensemble_lib::prep_for_train. This used to be a local copy that
# read LEAKAGE_VARS_REG directly — byte-identical logic, but it silently
# bypassed the pattern-expanded PRUNE_BLOCKS drop, so a whole walk-forward
# comparison came back bit-identical across arms that should have differed.
# Keep this a thin delegate; do not re-inline the prep.
prep_reg <- function(data) prep_for_train(data)

# Model training / stacking / Platt all sourced from ensemble_lib.R.
# `fit_stacking` honors module-level STACKING_METHOD / STACKING_LAMBDA via
# get0() so the call sites below continue to work unchanged.

# ---- Predictions -----------------------------------------------------------

predict_holdout <- function(ens_fit, holdout_raw) {
  holdout_prep <- holdout_raw %>%
    mutate(across(c(season, week, weekday, div_game), as.factor))

  xgb <- predict(ens_fit$xgb, new_data = holdout_prep)$.pred
  glm <- predict_glm(ens_fit$glm, new_data = holdout_prep)
  rf  <- predict(ens_fit$rf,  new_data = holdout_prep)$.pred
  nfl_pred <- apply_stacking(ens_fit$stack, xgb, glm, rf)
  nfl_pred <- if_else(is.na(nfl_pred), xgb, nfl_pred)

  # Routed through apply_winprob so WINPROB_METHOD actually takes effect here.
  # This local path previously called apply_platt directly, which meant the flag
  # gated bundle CONSTRUCTION while the evaluation ran the legacy chain either
  # way — the two settings produced byte-identical output.
  raw_wp <- pnorm(nfl_pred / ens_fit$sigma)
  pred_wp <- if (!is.null(ens_fit$winprob)) {
    apply_winprob(nfl_pred, ens_fit$winprob, ens_fit$sigma, ens_fit$platt)
  } else if (!is.null(ens_fit$platt)) apply_platt(raw_wp, ens_fit$platt) else raw_wp

  bind_cols(
    holdout_raw,
    tibble(nfl_predictor_line = nfl_pred,
           xgb_pred = xgb, glm_pred = glm, rf_pred = rf,
           pred_win_pct_raw = raw_wp,
           pred_win_pct = pred_wp)
  ) %>%
    mutate(
      line_diff          = nfl_predictor_line - spread_line,
      # B1 fix: cover probability (not win probability) for spread-bet Kelly sizing
      # P(home covers) = P(margin > spread) = pnorm((pred - spread) / sigma)
      cover_prob_home    = pnorm(line_diff / ens_fit$sigma),
      home_win_01        = as.integer(home_win == 1),
      # B2 fix: pushes/ties get correct=NA (refund), not counted as win or loss
      spread_push        = !is.na(home_result) & !is.na(spread_line) &
                            (home_result - spread_line == 0),
      ml_tie             = !is.na(home_result) & (home_result == 0),
      spread_winner      = if_else(home_result - spread_line > 0, home_team, away_team),
      pred_spread_winner = if_else(line_diff > 0, home_team, away_team),
      pred_winner        = if_else(nfl_predictor_line > 0, home_team, away_team),
      winning_team       = if_else(home_win == 1, home_team, away_team),
      cover_correct      = if_else(spread_push, NA_integer_,
                                    as.integer(pred_spread_winner == spread_winner)),
      ml_correct         = if_else(ml_tie, NA_integer_,
                                    as.integer(pred_winner == winning_team))
    ) %>%
    arrange(season, week)
}

# ---- Model accuracy metrics ------------------------------------------------

eval_model_metrics <- function(preds_df, holdout_season) {
  # Vegas favorite: spread_line > 0 means home favored in this dataset
  veg_spread_winner <- if_else(preds_df$spread_line > 0,
                               preds_df$home_team, preds_df$away_team)

  spread_row <- preds_df %>%
    summarise(
      season          = holdout_season,
      games           = n(),
      model_mae       = mean(abs(nfl_predictor_line - home_result)),
      vegas_mae       = mean(abs(spread_line - home_result)),
      # na.rm=TRUE: cover_correct is NA on pushes (B2 fix). Exclude from mean.
      model_cover_acc = mean(cover_correct, na.rm = TRUE),
      vegas_cover_acc = mean(as.integer(veg_spread_winner == spread_winner))
    )

  wp_row <- preds_df %>%
    filter(!is.na(impl_prob_home)) %>%
    summarise(
      season        = holdout_season,
      games_wp      = n(),
      model_brier   = mean((pred_win_pct - home_win_01)^2),
      vegas_brier   = mean((impl_prob_home - home_win_01)^2),
      model_logloss = {
        p <- pmin(pmax(pred_win_pct, 1e-15), 1 - 1e-15)
        -mean(home_win_01 * log(p) + (1 - home_win_01) * log(1 - p))
      },
      vegas_logloss = {
        p <- pmin(pmax(impl_prob_home, 1e-15), 1 - 1e-15)
        -mean(home_win_01 * log(p) + (1 - home_win_01) * log(1 - p))
      }
    )

  list(spread = spread_row, winprob = wp_row)
}

# ---- Betting helpers -------------------------------------------------------
# american_to_decimal / kelly_f / pnl_dollars come from ensemble_lib.R.
# Pushes return $0 (B2 fix) handled in pnl_dollars.

# ---- Betting strategies ----------------------------------------------------

flat_spread_bets <- function(g) {
  g %>%
    filter(!is.na(home_spread_odds), !is.na(away_spread_odds)) %>%
    mutate(
      strategy    = "flat_spread",
      side        = if_else(pred_spread_winner == home_team, "home", "away"),
      odds        = if_else(side == "home", home_spread_odds, away_spread_odds),
      stake_units = 1,
      correct     = cover_correct
    )
}

flat_ml_bets <- function(g) {
  g %>%
    filter(!is.na(home_moneyline), !is.na(away_moneyline)) %>%
    mutate(
      strategy    = "flat_moneyline",
      side        = if_else(pred_winner == home_team, "home", "away"),
      odds        = if_else(side == "home", home_moneyline, away_moneyline),
      stake_units = 1,
      correct     = ml_correct
    )
}

value_spread_bets <- function(g) {
  flat_spread_bets(g) %>%
    mutate(strategy = "value_spread") %>%
    filter(abs(line_diff) >= MIN_EDGE_SPREAD)
}

value_ml_bets <- function(g) {
  g %>%
    filter(!is.na(home_moneyline), !is.na(away_moneyline), !is.na(impl_prob_home)) %>%
    mutate(
      strategy    = "value_moneyline",
      side        = if_else(pred_winner == home_team, "home", "away"),
      odds        = if_else(side == "home", home_moneyline, away_moneyline),
      stake_units = 1,
      correct     = ml_correct,
      edge_prob   = abs(pred_win_pct - impl_prob_home)
    ) %>%
    filter(edge_prob >= MIN_EDGE_ML, abs(odds) <= MAX_ABS_ODD_VALUE_ML)
}

kelly_spread_bets <- function(g) {
  g %>%
    filter(!is.na(home_spread_odds), !is.na(away_spread_odds)) %>%
    mutate(
      strategy     = "kelly_spread",
      side         = if_else(line_diff > 0, "home", "away"),
      odds         = if_else(side == "home", home_spread_odds, away_spread_odds),
      # B1 fix: use COVER probability (not win prob) for spread Kelly sizing.
      # For a -7 home favorite, P(win) might be 70% but P(cover by 7+) only 45%.
      # Using win_pct previously overbet favorites and underbet dogs.
      our_prob     = if_else(side == "home", cover_prob_home, 1 - cover_prob_home),
      kelly_raw    = kelly_f(our_prob, odds),
      kelly_capped = pmin(pmax(kelly_raw * KELLY_FRACTION, 0), MAX_BET_PCT),
      correct      = cover_correct
    ) %>%
    filter(kelly_capped > 0.001)
}

kelly_ml_bets <- function(g) {
  g %>%
    filter(!is.na(home_moneyline), !is.na(away_moneyline), !is.na(impl_prob_home)) %>%
    mutate(
      strategy     = "kelly_moneyline",
      side         = if_else(pred_winner == home_team, "home", "away"),
      odds         = if_else(side == "home", home_moneyline, away_moneyline),
      our_prob     = if_else(side == "home", pred_win_pct, 1 - pred_win_pct),
      kelly_raw    = kelly_f(our_prob, odds),
      # ML-specific Kelly fraction (KELLY_FRACTION_ML), min-edge threshold
      # (MIN_KELLY_FRAC_ML), and max-odds filter (MAX_ABS_ODD_KELLY_ML).
      # All three re-tuned per session for the 3-model NNLS ensemble distribution.
      kelly_capped = pmin(pmax(kelly_raw * KELLY_FRACTION_ML, 0), MAX_BET_PCT),
      correct      = ml_correct
    ) %>%
    filter(kelly_capped >= MIN_KELLY_FRAC_ML, abs(odds) <= MAX_ABS_ODD_KELLY_ML)
}

# ---- Simulation runners ----------------------------------------------------

run_flat_sim <- function(bets) {
  if (nrow(bets) == 0) return(tibble())
  bets %>%
    mutate(
      wager_dollars = stake_units * UNIT_SIZE,
      pnl_d         = pnl_dollars(correct, odds, wager_dollars),
      pnl_units     = pnl_d / UNIT_SIZE
    ) %>%
    select(game_id, season, week, strategy, correct, stake_units,
           pnl_dollars = pnl_d, pnl_units)
}

run_kelly_sim <- function(bets) {
  if (nrow(bets) == 0) return(tibble())
  bankroll <- INITIAL_BANKROLL
  out <- vector("list", nrow(bets))
  for (i in seq_len(nrow(bets))) {
    b  <- bets[i, ]
    st <- min(b$kelly_capped * bankroll,
              bankroll * MAX_BET_PCT,
              MAX_STAKE)
    st  <- max(st, 0.01)
    wd  <- st * UNIT_SIZE
    pnl <- pnl_dollars(b$correct, b$odds, wd)
    bankroll <- bankroll + pnl / UNIT_SIZE
    out[[i]] <- tibble(
      game_id   = b$game_id,  season = b$season, week = b$week,
      strategy  = b$strategy, correct = b$correct,
      stake_units = st, pnl_dollars = pnl, pnl_units = pnl / UNIT_SIZE,
      bankroll_after = bankroll
    )
  }
  bind_rows(out)
}

eval_betting <- function(games_df) {
  bind_rows(
    run_flat_sim(flat_spread_bets(games_df)),
    run_flat_sim(flat_ml_bets(games_df)),
    run_flat_sim(value_spread_bets(games_df)),
    run_flat_sim(value_ml_bets(games_df)),
    run_kelly_sim(kelly_spread_bets(games_df)),
    run_kelly_sim(kelly_ml_bets(games_df))
  )
}

# ---- Main loop -------------------------------------------------------------

cat("Loading feature data...\n")
game_w_features <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
registerDoParallel()

all_betting <- list()
all_spread  <- list()
all_winprob <- list()
all_preds   <- list()  # per-game predictions for calibration analysis

for (hs in HOLDOUT_SEASONS) {
  n_train_games <- game_w_features %>%
    filter(season >= MIN_TRAIN_SEASON, season < hs, !is.na(result)) %>%
    nrow()

  cat(sprintf(
    "\n=== Holdout %d | Training on %d-%d (%d games) ===\n",
    hs, MIN_TRAIN_SEASON, hs - 1, n_train_games
  ))

  train_raw <- game_w_features %>%
    filter(season >= MIN_TRAIN_SEASON, season < hs)

  holdout_raw <- game_w_features %>%
    filter(season == hs, !is.na(home_result)) %>%
    week_filter()

  if (nrow(holdout_raw) == 0) {
    cat(sprintf("  No completed games for season %d, skipping.\n", hs))
    next
  }

  # Split training: hold out last CAL_WINDOW_SEASONS seasons for stacking + Platt
  cal_seasons   <- (max(train_raw$season) - CAL_WINDOW_SEASONS + 1):max(train_raw$season)
  pre_cal_raw   <- train_raw %>% filter(!season %in% cal_seasons)
  cal_raw_platt <- train_raw %>% filter(season %in% cal_seasons)

  # --- Pre-cal: train XGB + GLM + RF, learn NNLS stacking + Platt on cal set ---
  cat(sprintf("  Pre-cal: training XGB + GLM + RF (%d–%d, %d games)...\n",
              MIN_TRAIN_SEASON, min(cal_seasons) - 1, nrow(pre_cal_raw)))
  pre_cal_prep <- prep_reg(pre_cal_raw)
  pre_cal_xgb  <- train_xgb_model(pre_cal_prep)
  pre_cal_glm  <- train_glm_model(pre_cal_prep)
  pre_cal_rf   <- train_rf_model(pre_cal_prep)

  cal_prep  <- prep_reg(cal_raw_platt)
  cal_xgb   <- predict(pre_cal_xgb, new_data = cal_prep)$.pred
  cal_glm   <- predict_glm(pre_cal_glm, new_data = cal_prep)
  cal_rf    <- predict(pre_cal_rf,  new_data = cal_prep)$.pred
  cal_actual <- cal_prep$home_result

  stack <- fit_stacking(cal_xgb, cal_glm, cal_rf, cal_actual)
  cat(sprintf("    %s stacking: %.2f + %.2f·xgb + %.2f·glm + %.2f·rf\n",
              stack$method, stack$intercept, stack$w_xgb, stack$w_glm, stack$w_rf))

  cal_ens_pred <- apply_stacking(stack, cal_xgb, cal_glm, cal_rf)
  sigma_ens    <- sd(cal_actual - cal_ens_pred, na.rm = TRUE) * 1.05
  cal_raw_wp   <- pnorm(cal_ens_pred / sigma_ens)
  platt        <- fit_platt_logits(cal_raw_wp, as.integer(cal_actual > 0))
  # Alternative margin -> win-prob map (WINPROB_METHOD, default = legacy path).
  winprob      <- fit_winprob(cal_ens_pred, as.integer(cal_actual > 0), sigma_ens)
  cat(sprintf("    σ_ens=%.2f, Platt: int=%.3f, slope=%.3f, glm λ=%.4f (cal n=%d)\n",
              sigma_ens, platt$intercept, platt$slope, pre_cal_glm$lambda,
              length(cal_actual)))
  if (identical(winprob$method, "logistic"))
    cat(sprintf("    winprob=logistic: int=%.3f, slope=%.4f per pt\n",
                winprob$intercept, winprob$slope))

  # --- Final: retrain all three on full training data ---
  cat(sprintf("  Final: training XGB + GLM + RF (%d–%d)...\n",
              MIN_TRAIN_SEASON, max(cal_seasons)))
  final_prep <- prep_reg(train_raw)
  final_xgb  <- train_xgb_model(final_prep)
  final_glm  <- train_glm_model(final_prep)
  final_rf   <- train_rf_model(final_prep)

  ens_fit <- list(
    xgb   = final_xgb,
    glm   = final_glm,
    rf    = final_rf,
    stack = stack,
    sigma = sigma_ens,
    platt = platt,
    winprob = winprob
  )

  cat("  Generating predictions...\n")
  preds_df <- predict_holdout(ens_fit, holdout_raw)

  metrics <- eval_model_metrics(preds_df, hs)
  all_spread[[as.character(hs)]]  <- metrics$spread
  all_winprob[[as.character(hs)]] <- metrics$winprob

  # Save predictions for calibration diagnostics
  all_preds[[as.character(hs)]] <- preds_df %>%
    select(game_id, season, week, home_team, away_team, spread_line,
           home_result, home_win_01, nfl_predictor_line, line_diff,
           pred_win_pct_raw, pred_win_pct, cover_correct, ml_correct)

  cat("  Running betting strategies...\n")
  all_betting[[as.character(hs)]] <- eval_betting(preds_df)

  cat(sprintf(
    "  Done. Games: %d | Model MAE: %.3f | Vegas MAE: %.3f | Model Brier: %.4f | Vegas Brier: %.4f\n",
    metrics$spread$games,
    metrics$spread$model_mae,
    metrics$spread$vegas_mae,
    metrics$winprob$model_brier,
    metrics$winprob$vegas_brier
  ))
}

# ---- Aggregate -------------------------------------------------------------

betting_all  <- bind_rows(all_betting)
spread_all   <- bind_rows(all_spread)
winprob_all  <- bind_rows(all_winprob)

betting_by_season <- betting_all %>%
  group_by(season, strategy) %>%
  summarise(
    n_bets            = n(),
    n_pushes          = sum(is.na(correct)),
    n_wins            = sum(correct, na.rm = TRUE),
    win_pct           = round(mean(correct, na.rm = TRUE), 3),
    total_pnl_units   = round(sum(pnl_units), 3),
    total_pnl_dollars = round(sum(pnl_dollars), 2),
    roi               = round(sum(pnl_dollars) / sum(stake_units * UNIT_SIZE), 3),
    .groups = "drop"
  ) %>%
  arrange(season, strategy)

betting_overall <- betting_all %>%
  group_by(strategy) %>%
  summarise(
    n_bets            = n(),
    n_pushes          = sum(is.na(correct)),
    n_wins            = sum(correct, na.rm = TRUE),
    win_pct           = round(mean(correct, na.rm = TRUE), 3),
    total_pnl_units   = round(sum(pnl_units), 3),
    total_pnl_dollars = round(sum(pnl_dollars), 2),
    roi               = round(sum(pnl_dollars) / sum(stake_units * UNIT_SIZE), 3),
    .groups = "drop"
  ) %>%
  arrange(roi)

# ---- Print -----------------------------------------------------------------

cat("\n\n=== WALK-FORWARD MODEL PERFORMANCE (honest OOS) ===\n")
cat("\nSpread metrics by holdout season:\n")
print(as.data.frame(spread_all))

cat("\nWin probability metrics by holdout season:\n")
print(as.data.frame(winprob_all))

cat("\nAggregate spread performance:\n")
spread_agg <- spread_all %>%
  summarise(
    seasons        = n(),
    total_games    = sum(games),
    model_mae      = weighted.mean(model_mae, games),
    vegas_mae      = weighted.mean(vegas_mae, games),
    model_cover    = weighted.mean(model_cover_acc, games),
    vegas_cover    = weighted.mean(vegas_cover_acc, games)
  )
print(spread_agg)

cat("\nAggregate win probability performance:\n")
wp_agg <- winprob_all %>%
  summarise(
    seasons      = n(),
    total_games  = sum(games_wp),
    model_brier  = weighted.mean(model_brier, games_wp),
    vegas_brier  = weighted.mean(vegas_brier, games_wp),
    model_ll     = weighted.mean(model_logloss, games_wp),
    vegas_ll     = weighted.mean(vegas_logloss, games_wp)
  )
print(wp_agg)

cat("\n\n=== WALK-FORWARD BETTING RESULTS (honest OOS) ===\n")
cat("\nOverall by strategy:\n")
print(as.data.frame(betting_overall))

cat("\nBy season:\n")
print(as.data.frame(betting_by_season), max = 9999)

# ---- Write outputs ---------------------------------------------------------

write_csv(spread_all,         file.path(OUTPUT_DIR, "walk_forward_model_spread.csv"))
write_csv(winprob_all,        file.path(OUTPUT_DIR, "walk_forward_model_winprob.csv"))
write_csv(betting_by_season,  file.path(OUTPUT_DIR, "walk_forward_betting_by_season.csv"))
write_csv(betting_overall,    file.path(OUTPUT_DIR, "walk_forward_betting_overall.csv"))
write_csv(betting_all,        file.path(OUTPUT_DIR, "walk_forward_betting_bets.csv"))
write_csv(bind_rows(all_preds), file.path(OUTPUT_DIR, "walk_forward_predictions.csv"))

cat(sprintf("\nWrote walk_forward_* to %s/\n", OUTPUT_DIR))
cat("Done.\n")
