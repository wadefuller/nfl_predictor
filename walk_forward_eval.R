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
# Usage:
#   Rscript walk_forward_eval.R                         # full confirmation
#   ATS_PROFILE=probe Rscript walk_forward_eval.R       # one-season triage
#   ATS_PROFILE=screen ATS_WORKERS=2 Rscript walk_forward_eval.R
#
# The evaluator is also the execution engine for ATS experiments.  It supports
# progressive profiles, fold-level caches, and bounded season parallelism while
# leaving the honest rolling-origin split unchanged.  Production is GLM-only,
# so that mode fits only the learner whose stacking weight is non-zero.
# ============================================

# Shared helpers + canonical LEAKAGE_VARS_REG + train_/fit_/apply_ functions.
# Everything model-side comes from ensemble_lib.R; this script orchestrates the
# walk-forward eval and adds betting-strategy specific logic.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")
library(nnls)    # non-negative least squares (used by fit_stacking_nnls)
library(digest)

# ---- Config ----------------------------------------------------------------
# PROJECT_DIR / DATA_DIR come from ensemble_lib.R (sourced above).
ATS_PROFILE <- match.arg(Sys.getenv("ATS_PROFILE", "confirm"),
                         c("probe", "screen", "confirm"))
ATS_PROFILE_SEASONS <- list(
  probe   = 2025L,
  # Deliberately spans distinct NFL/market regimes rather than using four
  # adjacent seasons. A screen is triage, never promotion evidence.
  screen  = c(2016L, 2019L, 2022L, 2025L),
  confirm = 2016:2025
)

parse_seasons <- function(x) {
  if (!nzchar(x)) return(NULL)
  bits <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  out <- unlist(lapply(bits, function(bit) {
    if (grepl("^[0-9]{4}[:-][0-9]{4}$", bit)) {
      z <- as.integer(strsplit(bit, "[:-]")[[1]])
      seq.int(z[1], z[2])
    } else as.integer(bit)
  }))
  if (anyNA(out)) stop("ATS_HOLDOUT_SEASONS must be comma-separated years or ranges")
  sort(unique(out))
}

HOLDOUT_OVERRIDE <- parse_seasons(Sys.getenv("ATS_HOLDOUT_SEASONS", ""))
HOLDOUT_SEASONS <- HOLDOUT_OVERRIDE %||% ATS_PROFILE_SEASONS[[ATS_PROFILE]]

OUTPUT_DIR <- Sys.getenv(
  "ATS_OUTPUT_DIR",
  if (ATS_PROFILE == "confirm") DATA_DIR
  else file.path(DATA_DIR, "ats_experiments", paste0("latest-", ATS_PROFILE))
)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

ATS_CACHE_MODE <- match.arg(Sys.getenv("ATS_CACHE_MODE", "use"),
                            c("use", "refresh", "off"))
ATS_CACHE_DIR <- Sys.getenv("ATS_CACHE_DIR",
                            file.path(DATA_DIR, "ats_experiment_cache", "folds"))
dir.create(ATS_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

default_workers <- if (ATS_PROFILE == "probe") 1L else 2L
ATS_WORKERS <- suppressWarnings(as.integer(Sys.getenv("ATS_WORKERS", default_workers)))
if (!is.finite(ATS_WORKERS) || ATS_WORKERS < 1L) ATS_WORKERS <- 1L

resolve_stack_mode <- function() {
  mode <- Sys.getenv("STACK_MODE", "")
  if (!nzchar(mode)) mode <- if (Sys.getenv("STACK_FULL", "0") == "1") "full" else "glm_only"
  match.arg(mode, c("glm_only", "xgb_only", "full"))
}
STACK_MODE_ACTIVE <- resolve_stack_mode()
ATS_FORCE_UNUSED_LEARNERS <- Sys.getenv("ATS_FORCE_UNUSED_LEARNERS", "0") == "1"

MIN_TRAIN_SEASON   <- 2008
CAL_WINDOW_SEASONS <- 2           # number of most-recent training seasons used for stacking + Platt cal
STACKING_METHOD    <- "multi"     # "nnls" (margin RMSE), "logloss" (winprob), or "multi" (both)
STACKING_LAMBDA    <- load_tuned_stacking_lambda()  # tuned by tune_stacking_lambda.R
SEED               <- 989
GLM_ALPHA_ACTIVE <- if (nzchar(Sys.getenv("ATS_GLM_ALPHA", "")))
  suppressWarnings(as.numeric(Sys.getenv("ATS_GLM_ALPHA"))) else load_tuned_glm_alpha()
if (length(GLM_ALPHA_ACTIVE) != 1L || !is.finite(GLM_ALPHA_ACTIVE) ||
    GLM_ALPHA_ACTIVE < 0 || GLM_ALPHA_ACTIVE > 1)
  stop("ATS_GLM_ALPHA must be a number between 0 and 1")
GLM_LAMBDA_RULE     <- match.arg(Sys.getenv("GLM_LAMBDA_RULE", "min"),
                                 c("min", "1se", "repeat_median", "temporal",
                                   "fixed_prior"))
GLM_CV_REPEATS      <- suppressWarnings(as.integer(Sys.getenv("GLM_CV_REPEATS", "5")))
GLM_TEMPORAL_FOLDS  <- suppressWarnings(as.integer(Sys.getenv("GLM_TEMPORAL_FOLDS", "4")))
GLM_PRIOR_FOLDS     <- suppressWarnings(as.integer(Sys.getenv("GLM_PRIOR_FOLDS", "5")))

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

  n <- nrow(holdout_prep)
  xgb <- if (!is.null(ens_fit$xgb))
    predict(ens_fit$xgb, new_data = holdout_prep)$.pred else rep(0, n)
  glm <- if (!is.null(ens_fit$glm))
    predict_glm(ens_fit$glm, new_data = holdout_prep) else rep(0, n)
  rf <- if (!is.null(ens_fit$rf))
    predict(ens_fit$rf, new_data = holdout_prep)$.pred else rep(0, n)
  nfl_pred <- apply_stacking(ens_fit$stack, xgb, glm, rf)
  fallback <- if (!is.null(ens_fit$glm)) glm else xgb
  nfl_pred <- if_else(is.na(nfl_pred), fallback, nfl_pred)

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

train_active_learners <- function(train_prep, train_raw = NULL) {
  arm <- Sys.getenv("ATS_TRAINING_ARM", "expanding")
  stopifnot(arm %in% c("expanding", "window8", "window12", "half4", "half8", "half12", "hybrid"))
  weights <- NULL
  if (arm != "expanding") {
    if (STACK_MODE_ACTIVE != "glm_only") stop("Training-window research requires GLM-only")
    raw <- week_filter(train_raw)
    age <- max(raw$season) - raw$season
    keep <- if (grepl("window", arm)) age < as.integer(sub("window", "", arm)) else rep(TRUE, nrow(raw))
    raw <- raw[keep, ]; age <- age[keep]
    train_prep <- prep_reg(raw)
    train_raw <- raw
    if (!grepl("window", arm)) {
      half_life <- if (arm == "hybrid") 4 else as.numeric(sub("half", "", arm))
      weights <- 2^(-age / half_life)
      if (arm == "hybrid") weights <- pmax(.25, weights)
    }
  }
  list(
    xgb = if (ATS_FORCE_UNUSED_LEARNERS || STACK_MODE_ACTIVE %in% c("xgb_only", "full"))
      train_xgb_model(train_prep) else NULL,
    glm = if (ATS_FORCE_UNUSED_LEARNERS || STACK_MODE_ACTIVE %in% c("glm_only", "full"))
      train_glm_model(train_prep, alpha = GLM_ALPHA_ACTIVE, raw_data = train_raw,
                      observation_weights = weights,
                      lambda_rule = GLM_LAMBDA_RULE,
                      cv_repeats = GLM_CV_REPEATS,
                      temporal_folds = GLM_TEMPORAL_FOLDS,
                      prior_folds = GLM_PRIOR_FOLDS) else NULL,
    rf = if (ATS_FORCE_UNUSED_LEARNERS || STACK_MODE_ACTIVE == "full")
      train_rf_model(train_prep) else NULL
  )
}

predict_active_learners <- function(fits, new_prep) {
  n <- nrow(new_prep)
  list(
    xgb = if (!is.null(fits$xgb)) predict(fits$xgb, new_data = new_prep)$.pred else rep(0, n),
    glm = if (!is.null(fits$glm)) predict_glm(fits$glm, new_data = new_prep) else rep(0, n),
    rf  = if (!is.null(fits$rf)) predict(fits$rf, new_data = new_prep)$.pred else rep(0, n)
  )
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

# ---- Cached fold runner ----------------------------------------------------

file_fingerprint <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(path, algo = "sha256", file = TRUE)
}

TRAINING_CACHE_VERSION <- 2L
FEATURES_PATH <- Sys.getenv("ATS_FEATURES_PATH", file.path(DATA_DIR, "game_w_features.csv"))
TRAINING_FINGERPRINT <- list(
  version = TRAINING_CACHE_VERSION,
  panel = file_fingerprint(FEATURES_PATH),
  learner_source = file_fingerprint(file.path(PROJECT_DIR, "ensemble_lib.R")),
  glm_tune = file_fingerprint(file.path(DATA_DIR, "tune_best_glm.rds")),
  glm_alpha = GLM_ALPHA_ACTIVE,
  xgb_tune = file_fingerprint(file.path(DATA_DIR, "tune_best_params_reg.rds")),
  rf_tune = file_fingerprint(file.path(DATA_DIR, "tune_best_rf.rds")),
  stack_mode = STACK_MODE_ACTIVE,
  stacking_method = STACKING_METHOD,
  stacking_lambda = STACKING_LAMBDA,
  winprob_method = WINPROB_METHOD,
  min_train_season = MIN_TRAIN_SEASON,
  cal_window_seasons = CAL_WINDOW_SEASONS,
  seed = SEED,
  prune_dead = Sys.getenv("PRUNE_DEAD", "1"),
  prune_blocks = Sys.getenv("PRUNE_BLOCKS", ""),
  xgb_direct = Sys.getenv("XGB_DIRECT", "0"),
  force_unused_learners = ATS_FORCE_UNUSED_LEARNERS,
  glm_lambda_rule = GLM_LAMBDA_RULE,
  training_arm = Sys.getenv("ATS_TRAINING_ARM", "expanding"),
  glm_cv_repeats = GLM_CV_REPEATS,
  glm_temporal_folds = GLM_TEMPORAL_FOLDS,
  glm_prior_folds = GLM_PRIOR_FOLDS
)

fold_hash <- function(holdout_season) {
  substr(digest::digest(c(TRAINING_FINGERPRINT,
                          list(holdout_season = as.integer(holdout_season))),
                        algo = "sha256"), 1, 20)
}

fold_cache_path <- function(holdout_season) {
  file.path(ATS_CACHE_DIR, sprintf("season-%d_%s.rds", holdout_season,
                                   fold_hash(holdout_season)))
}

atomic_save_rds <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(".", basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(value, tmp)
  if (!file.rename(tmp, path)) stop("Could not atomically write fold cache: ", path)
  invisible(path)
}

run_fold <- function(hs, game_w_features) {
  cache_path <- fold_cache_path(hs)
  if (ATS_CACHE_MODE == "use" && file.exists(cache_path)) {
    cache_started <- Sys.time()
    artifact <- readRDS(cache_path)
    artifact$cache_hit <- TRUE
    artifact$cache_read_seconds <- as.numeric(difftime(Sys.time(), cache_started,
                                                       units = "secs"))
    return(artifact)
  }

  started <- Sys.time()
  n_train_games <- game_w_features %>%
    filter(season >= MIN_TRAIN_SEASON, season < hs, !is.na(result)) %>%
    nrow()

  train_raw <- game_w_features %>%
    filter(season >= MIN_TRAIN_SEASON, season < hs)

  holdout_raw <- game_w_features %>%
    filter(season == hs, !is.na(home_result)) %>%
    week_filter()

  if (nrow(holdout_raw) == 0) {
    return(list(season = hs, skipped = TRUE, reason = "no completed games",
                cache_hit = FALSE, cache_hash = fold_hash(hs)))
  }

  # Split training: hold out last CAL_WINDOW_SEASONS seasons for stacking + Platt
  cal_seasons   <- (max(train_raw$season) - CAL_WINDOW_SEASONS + 1):max(train_raw$season)
  pre_cal_raw   <- train_raw %>% filter(!season %in% cal_seasons)
  cal_raw_platt <- train_raw %>% filter(season %in% cal_seasons)

  # --- Pre-cal: train only learners used by the selected architecture. ---
  pre_cal_prep <- prep_reg(pre_cal_raw)
  pre_cal_fit <- train_active_learners(pre_cal_prep, pre_cal_raw)

  cal_prep  <- prep_reg(cal_raw_platt)
  cal_scale_raw <- cal_raw_platt %>% week_filter()
  cal_pred <- predict_active_learners(pre_cal_fit, cal_prep)
  cal_xgb <- cal_pred$xgb
  cal_glm <- cal_pred$glm
  cal_rf <- cal_pred$rf
  cal_actual <- cal_prep$home_result
  if (nrow(cal_scale_raw) != length(cal_actual)) {
    stop(sprintf("Calibration-row alignment failed for holdout %d: raw=%d prepared=%d",
                 hs, nrow(cal_scale_raw), length(cal_actual)))
  }

  stack <- fit_stacking(cal_xgb, cal_glm, cal_rf, cal_actual)

  cal_ens_pred <- apply_stacking(stack, cal_xgb, cal_glm, cal_rf)
  calibration_frame <- cal_scale_raw %>%
    transmute(
      holdout_season = hs,
      game_id, calibration_season = season, week,
      spread_line, total_line, home_result,
      temp, wind, is_dome, high_wind,
      home_team_rest, away_team_rest,
      new_starter_home, new_starter_away,
      starter_games_team_season_home, starter_games_team_season_away,
      inj_qb_starter_out_home, inj_qb_starter_out_away,
      predicted_margin = cal_ens_pred,
      residual = home_result - predicted_margin
    )
  sigma_ens    <- sd(cal_actual - cal_ens_pred, na.rm = TRUE) * 1.05
  cal_raw_wp   <- pnorm(cal_ens_pred / sigma_ens)
  platt        <- fit_platt_logits(cal_raw_wp, as.integer(cal_actual > 0))
  # Alternative margin -> win-prob map (WINPROB_METHOD, default = legacy path).
  winprob      <- fit_winprob(cal_ens_pred, as.integer(cal_actual > 0), sigma_ens)

  # --- Final: retrain the active learners on the full training data. ---
  final_prep <- prep_reg(train_raw)
  final_fit <- train_active_learners(final_prep, train_raw)

  ens_fit <- list(
    xgb   = final_fit$xgb,
    glm   = final_fit$glm,
    rf    = final_fit$rf,
    stack = stack,
    sigma = sigma_ens,
    platt = platt,
    winprob = winprob
  )

  preds_df <- predict_holdout(ens_fit, holdout_raw)
  artifact <- list(
    season = hs,
    skipped = FALSE,
    cache_hit = FALSE,
    cache_hash = fold_hash(hs),
    predictions = preds_df,
    calibration_frame = calibration_frame,
    provenance = list(
      profile = ATS_PROFILE,
      architecture = STACK_MODE_ACTIVE,
      training_fingerprint = TRAINING_FINGERPRINT,
      n_train_games = n_train_games,
      cal_seasons = cal_seasons,
      glm_diagnostics = list(
        pre_cal = pre_cal_fit$glm$diagnostics %||% NULL,
        final = final_fit$glm$diagnostics %||% NULL
      ),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
  )
  if (ATS_CACHE_MODE != "off") atomic_save_rds(artifact, cache_path)
  artifact
}

# ---- Main loop -------------------------------------------------------------

cat(sprintf("Loading feature data... profile=%s seasons=%s architecture=%s workers=%d cache=%s\n",
            ATS_PROFILE, paste(HOLDOUT_SEASONS, collapse = ","),
            STACK_MODE_ACTIVE, ATS_WORKERS, ATS_CACHE_MODE))
game_w_features <- read.csv(FEATURES_PATH)

if (STACK_MODE_ACTIVE == "full" && Sys.getenv("ATS_WORKERS", "") == "") {
  ATS_WORKERS <- 1L
  cat("  Full stack selected: defaulting to one fold worker to avoid nested learner oversubscription.\n")
}

fold_runner <- function(hs) run_fold(hs, game_w_features)
if (ATS_WORKERS > 1L && length(HOLDOUT_SEASONS) > 1L && .Platform$OS.type != "windows") {
  folds <- parallel::mclapply(HOLDOUT_SEASONS, fold_runner,
                              mc.cores = min(ATS_WORKERS, length(HOLDOUT_SEASONS)),
                              mc.preschedule = FALSE, mc.set.seed = TRUE)
} else {
  folds <- lapply(HOLDOUT_SEASONS, fold_runner)
}

failed <- vapply(folds, inherits, logical(1), "try-error")
if (any(failed)) stop("ATS fold failure(s): ", paste(HOLDOUT_SEASONS[failed], collapse = ", "))
folds <- Filter(function(x) !isTRUE(x$skipped), folds)
if (!length(folds)) stop("No completed holdout folds for selected profile")

all_betting <- lapply(folds, function(x) eval_betting(x$predictions))
all_metrics <- lapply(folds, function(x) eval_model_metrics(x$predictions, x$season))
all_spread <- lapply(all_metrics, `[[`, "spread")
all_winprob <- lapply(all_metrics, `[[`, "winprob")
all_preds <- lapply(folds, function(x) x$predictions %>%
  select(game_id, season, week, home_team, away_team, spread_line,
         home_result, home_win_01, nfl_predictor_line, line_diff,
         pred_win_pct_raw, pred_win_pct, cover_correct, ml_correct))
all_scale_holdout <- lapply(folds, function(x) x$predictions %>%
  transmute(
    holdout_season = season,
    game_id, week, spread_line, total_line, home_result,
    temp, wind, is_dome, high_wind,
    home_team_rest, away_team_rest,
    new_starter_home, new_starter_away,
    starter_games_team_season_home, starter_games_team_season_away,
    inj_qb_starter_out_home, inj_qb_starter_out_away,
    predicted_margin = nfl_predictor_line,
    global_cover_probability = cover_prob_home,
    spread_push,
    home_cover = if_else(spread_push, NA_integer_,
                         as.integer(home_result > spread_line)),
    residual = home_result - predicted_margin
  ))

for (i in seq_along(folds)) {
  f <- folds[[i]]
  m <- all_metrics[[i]]
  elapsed <- if (isTRUE(f$cache_hit)) f$cache_read_seconds
             else f$provenance$elapsed_seconds
  cat(sprintf("  %d %s | %.1fs | n=%d MAE %.3f vs %.3f | Brier %.4f vs %.4f\n",
              f$season, if (isTRUE(f$cache_hit)) "cache" else "fit",
              elapsed, m$spread$games,
              m$spread$model_mae, m$spread$vegas_mae,
              m$winprob$model_brier, m$winprob$vegas_brier))
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
write_csv(bind_rows(lapply(folds, `[[`, "calibration_frame")),
          file.path(OUTPUT_DIR, "scale_calibration_rows.csv"))
write_csv(bind_rows(all_scale_holdout),
          file.path(OUTPUT_DIR, "scale_holdout_rows.csv"))

glm_diagnostics <- bind_rows(lapply(folds, function(f) {
  stages <- f$provenance$glm_diagnostics %||% list()
  bind_rows(lapply(names(stages), function(stage) {
    d <- stages[[stage]]
    if (is.null(d)) return(NULL)
    tibble(
      season = f$season,
      stage = stage,
      lambda_rule = d$lambda_rule,
      selected_lambda = d$selected_lambda,
      lambda_sd = d$lambda_sd,
      nonzero_count = d$nonzero_count,
      nonzero_features = paste(d$nonzero_features, collapse = "|"),
      selection_lambdas = paste(signif(d$selected_lambdas, 10), collapse = "|"),
      temporal_validation_seasons = paste(d$temporal_validation_seasons, collapse = "|"),
      temporal_validation_rows = d$temporal_validation_rows,
      temporal_mae = d$temporal_mae,
      prior_holdout_seasons = paste(d$prior_holdout_seasons, collapse = "|")
    )
  }))
}))
if (nrow(glm_diagnostics)) {
  write_csv(glm_diagnostics, file.path(OUTPUT_DIR, "glm_diagnostics.csv"))
  coefficient_stability <- glm_diagnostics %>%
    filter(stage == "final") %>%
    select(season, nonzero_features) %>%
    separate_rows(nonzero_features, sep = "\\|") %>%
    filter(nzchar(nonzero_features)) %>%
    count(nonzero_features, name = "selected_folds") %>%
    mutate(total_folds = n_distinct(glm_diagnostics$season),
           selection_rate = selected_folds / total_folds) %>%
    arrange(desc(selection_rate), nonzero_features)
  write_csv(coefficient_stability,
            file.path(OUTPUT_DIR, "glm_coefficient_stability.csv"))

  final_glm <- glm_diagnostics %>% filter(stage == "final")
  selected_sets <- strsplit(final_glm$nonzero_features, "\\|")
  pairwise_jaccard_distance <- if (length(selected_sets) < 2L) {
    NA_real_
  } else {
    pairs <- combn(seq_along(selected_sets), 2L)
    mean(apply(pairs, 2L, function(ix) {
      a <- selected_sets[[ix[[1]]]]
      b <- selected_sets[[ix[[2]]]]
      1 - length(intersect(a, b)) / length(union(a, b))
    }))
  }
  stability_summary <- final_glm %>% summarise(
    folds = n(),
    lambda_mean = mean(selected_lambda),
    lambda_sd_across_folds = sd(selected_lambda),
    nonzero_count_mean = mean(nonzero_count),
    nonzero_count_sd = sd(nonzero_count)
  ) %>% mutate(
    mean_pairwise_coefficient_jaccard_distance = pairwise_jaccard_distance
  )
  write_csv(stability_summary,
            file.path(OUTPUT_DIR, "glm_stability_summary.csv"))
}

run_metadata <- list(
  profile = ATS_PROFILE,
  holdout_seasons = as.integer(vapply(folds, `[[`, integer(1), "season")),
  promotion_authority = identical(ATS_PROFILE, "confirm") && is.null(HOLDOUT_OVERRIDE),
  architecture = STACK_MODE_ACTIVE,
  glm_lambda_rule = GLM_LAMBDA_RULE,
  workers = ATS_WORKERS,
  cache_mode = ATS_CACHE_MODE,
  cache_hits = sum(vapply(folds, function(x) isTRUE(x$cache_hit), logical(1))),
  cache_misses = sum(!vapply(folds, function(x) isTRUE(x$cache_hit), logical(1))),
  training_fingerprint = TRAINING_FINGERPRINT,
  fold_hashes = stats::setNames(vapply(folds, `[[`, character(1), "cache_hash"),
                                vapply(folds, `[[`, integer(1), "season")),
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)
jsonlite::write_json(run_metadata, file.path(OUTPUT_DIR, "run_metadata.json"),
                     pretty = TRUE, auto_unbox = TRUE, null = "null")

cat(sprintf("\nWrote walk_forward_* to %s/\n", OUTPUT_DIR))
cat("Done.\n")
