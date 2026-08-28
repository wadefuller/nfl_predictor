# ============================================
# NFL Predictor: Strategy Param Held-Out Validation
# ============================================
# Splits OOS predictions into:
#   - TUNE set: 2016-2022 (7 years)
#   - TEST set: 2023-2025 (3 years, untouched during sweep)
# Sweeps params on TUNE, picks best (with stability filter), evaluates on TEST.
# Degradation TUNE → TEST estimates how optimistic the in-sample sweep was.
#
# Pick rule: highest TUNE ROI subject to n_winning_yrs >= 4 (out of 7 tune yrs).
# Avoids "lucky 1-year" wins.
#
# Usage: Rscript strategy_holdout_validation.R
# ============================================

# Helpers + DATA_DIR + PROJECT_DIR all from ensemble_lib.R.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

UNIT_SIZE       <- 100
MAX_STAKE_UNITS <- 5
TUNE_LAST       <- 2022
MIN_N           <- 50    # min bets for inclusion
MIN_WIN_YRS_T   <- 4     # require winning in 4+ tune years for robustness

preds <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"), show_col_types = FALSE)
g     <- read_csv(file.path(DATA_DIR, "game_w_features.csv"), show_col_types = FALSE)

p_full <- preds %>%
  left_join(g %>% select(game_id, home_moneyline, away_moneyline,
                         home_spread_odds, away_spread_odds, impl_prob_home),
            by = "game_id") %>%
  mutate(
    pred_winner        = if_else(nfl_predictor_line > 0, home_team, away_team),
    pred_spread_winner = if_else(line_diff > 0, home_team, away_team)
  ) %>%
  arrange(season, week)

p_tune <- p_full %>% filter(season <= TUNE_LAST)
p_test <- p_full %>% filter(season >  TUNE_LAST)

cat(sprintf("TUNE: %d games across %d seasons (%d-%d)\n",
            nrow(p_tune), n_distinct(p_tune$season), min(p_tune$season), max(p_tune$season)))
cat(sprintf("TEST: %d games across %d seasons (%d-%d)\n\n",
            nrow(p_test), n_distinct(p_test$season), min(p_test$season), max(p_test$season)))

simulate_bets <- function(bets) {
  if (nrow(bets) == 0) {
    return(tibble(n = 0, win_pct = NA_real_, roi_overall = NA_real_,
                  roi_sd_yr = NA_real_, n_winning_yrs = 0, n_total_yrs = 0))
  }
  bets <- bets %>% mutate(pnl = pnl_dollars(correct, odds, stake))
  by_season <- bets %>% group_by(season) %>% summarise(
    season_pnl   = sum(pnl),
    season_stake = sum(stake),
    season_roi   = if_else(season_stake > 0, season_pnl / season_stake, NA_real_),
    .groups = "drop"
  )
  tibble(
    n             = nrow(bets),
    win_pct       = round(mean(bets$correct), 3),
    roi_overall   = round(sum(bets$pnl) / sum(bets$stake), 4),
    roi_sd_yr     = round(sd(by_season$season_roi, na.rm = TRUE), 4),
    n_winning_yrs = sum(by_season$season_roi > 0, na.rm = TRUE),
    n_total_yrs   = nrow(by_season)
  )
}

# ---- Bet derivation per strategy with params ----

derive_value_spread <- function(data, min_edge_pts) {
  data %>%
    filter(!is.na(home_spread_odds), !is.na(away_spread_odds),
           abs(line_diff) >= min_edge_pts) %>%
    mutate(
      side    = if_else(pred_spread_winner == home_team, "home", "away"),
      odds    = if_else(side == "home", home_spread_odds, away_spread_odds),
      stake   = UNIT_SIZE,
      correct = cover_correct
    )
}

derive_value_ml <- function(data, min_edge_prob, max_abs_odd) {
  data %>%
    filter(!is.na(home_moneyline), !is.na(away_moneyline), !is.na(impl_prob_home)) %>%
    mutate(
      side      = if_else(pred_winner == home_team, "home", "away"),
      odds      = if_else(side == "home", home_moneyline, away_moneyline),
      stake     = UNIT_SIZE,
      correct   = ml_correct,
      edge_prob = abs(pred_win_pct - impl_prob_home)
    ) %>%
    filter(edge_prob >= min_edge_prob, abs(odds) <= max_abs_odd)
}

derive_kelly_spread <- function(data, kelly_frac, min_kelly) {
  data %>%
    filter(!is.na(home_spread_odds), !is.na(away_spread_odds)) %>%
    mutate(
      side       = if_else(line_diff > 0, "home", "away"),
      odds       = if_else(side == "home", home_spread_odds, away_spread_odds),
      our_prob   = if_else(side == "home", pred_win_pct, 1 - pred_win_pct),
      kelly_raw  = pmax(kelly_f(our_prob, odds), 0),
      stake_unit = pmin(kelly_raw * kelly_frac, MAX_STAKE_UNITS),
      stake      = stake_unit * UNIT_SIZE,
      correct    = cover_correct
    ) %>%
    filter(kelly_raw * kelly_frac >= min_kelly)
}

derive_kelly_ml <- function(data, kelly_frac, min_kelly, max_abs_odd) {
  data %>%
    filter(!is.na(home_moneyline), !is.na(away_moneyline), !is.na(impl_prob_home)) %>%
    mutate(
      side       = if_else(pred_winner == home_team, "home", "away"),
      odds       = if_else(side == "home", home_moneyline, away_moneyline),
      our_prob   = if_else(side == "home", pred_win_pct, 1 - pred_win_pct),
      kelly_raw  = pmax(kelly_f(our_prob, odds), 0),
      stake_unit = pmin(kelly_raw * kelly_frac, MAX_STAKE_UNITS),
      stake      = stake_unit * UNIT_SIZE,
      correct    = ml_correct
    ) %>%
    filter(kelly_raw * kelly_frac >= min_kelly, abs(odds) <= max_abs_odd)
}

# ---- Sweeps on TUNE set ----

sweep_vs <- function(data) {
  map_dfr(c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0), function(e) {
    bind_cols(tibble(strategy = "value_spread", min_edge_pts = e),
              simulate_bets(derive_value_spread(data, e)))
  })
}

sweep_vm <- function(data) {
  expand_grid(e = c(0.02, 0.03, 0.05, 0.07, 0.10, 0.15),
              mo = c(Inf, 300, 200, 150)) %>%
    pmap_dfr(function(e, mo) {
      bind_cols(tibble(strategy = "value_ml", min_edge_prob = e, max_abs_odd = mo),
                simulate_bets(derive_value_ml(data, e, mo)))
    })
}

sweep_ks <- function(data) {
  expand_grid(kf = c(0.1, 0.2, 0.25, 0.5, 1.0),
              mk = c(0.001, 0.01, 0.025, 0.05)) %>%
    pmap_dfr(function(kf, mk) {
      bind_cols(tibble(strategy = "kelly_spread", kelly_frac = kf, min_kelly = mk),
                simulate_bets(derive_kelly_spread(data, kf, mk)))
    })
}

sweep_km <- function(data) {
  expand_grid(kf = c(0.1, 0.2, 0.25, 0.5, 1.0),
              mk = c(0.001, 0.01, 0.025, 0.05),
              mo = c(Inf, 300, 200, 150)) %>%
    pmap_dfr(function(kf, mk, mo) {
      bind_cols(tibble(strategy = "kelly_ml", kelly_frac = kf, min_kelly = mk, max_abs_odd = mo),
                simulate_bets(derive_kelly_ml(data, kf, mk, mo)))
    })
}

cat("Sweeping params on TUNE set (2016-2022)...\n")
tune_vs <- sweep_vs(p_tune)
tune_vm <- sweep_vm(p_tune)
tune_ks <- sweep_ks(p_tune)
tune_km <- sweep_km(p_tune)

# Pick rule: highest ROI subject to n >= MIN_N AND n_winning_yrs >= MIN_WIN_YRS_T
pick_best <- function(sweep_df, min_n = MIN_N, min_win_yrs = MIN_WIN_YRS_T) {
  sweep_df %>%
    filter(n >= min_n, n_winning_yrs >= min_win_yrs) %>%
    arrange(desc(roi_overall)) %>%
    slice(1)
}

pick_vs <- pick_best(tune_vs)
pick_vm <- pick_best(tune_vm)
pick_ks <- pick_best(tune_ks)
pick_km <- pick_best(tune_km)

cat("\n=== TUNE-SET PICKS (highest ROI with n≥", MIN_N, ", ≥", MIN_WIN_YRS_T, " winning yrs) ===\n", sep="")
print(bind_rows(pick_vs, pick_vm, pick_ks, pick_km) %>%
        select(strategy, min_edge_pts, min_edge_prob, max_abs_odd, kelly_frac, min_kelly,
               n, win_pct, roi_overall, n_winning_yrs) %>% as.data.frame())

# ---- Evaluate picks on TEST set (2023-2025) ----

eval_on_test <- function(picks) {
  map_dfr(seq_len(nrow(picks)), function(i) {
    pk <- picks[i, ]
    bets <- switch(pk$strategy,
      "value_spread" = derive_value_spread(p_test, pk$min_edge_pts),
      "value_ml"     = derive_value_ml(p_test, pk$min_edge_prob, pk$max_abs_odd),
      "kelly_spread" = derive_kelly_spread(p_test, pk$kelly_frac, pk$min_kelly),
      "kelly_ml"     = derive_kelly_ml(p_test, pk$kelly_frac, pk$min_kelly, pk$max_abs_odd)
    )
    bind_cols(tibble(strategy = pk$strategy), simulate_bets(bets))
  })
}

test_results <- eval_on_test(bind_rows(pick_vs, pick_vm, pick_ks, pick_km))

cat("\n=== TEST-SET RESULTS (2023-2025) — same params from tune picks ===\n")
print(test_results %>% as.data.frame())

# ---- Comparison ----
cat("\n=== TUNE vs TEST DEGRADATION ===\n")
cmp <- tibble(
  strategy    = c(pick_vs$strategy, pick_vm$strategy, pick_ks$strategy, pick_km$strategy),
  tune_n      = c(pick_vs$n, pick_vm$n, pick_ks$n, pick_km$n),
  tune_roi    = c(pick_vs$roi_overall, pick_vm$roi_overall, pick_ks$roi_overall, pick_km$roi_overall),
  tune_yrs    = c(pick_vs$n_winning_yrs, pick_vm$n_winning_yrs, pick_ks$n_winning_yrs, pick_km$n_winning_yrs),
  test_n      = test_results$n,
  test_roi    = test_results$roi_overall,
  test_yrs    = test_results$n_winning_yrs
) %>%
  mutate(
    roi_drop = test_roi - tune_roi,
    test_yrs = paste0(test_yrs, "/", test_results$n_total_yrs)
  )
print(cmp %>% as.data.frame())

cat("\n=== INTERPRETATION ===\n")
cat("If test_roi is close to tune_roi → params generalize\n")
cat("If test_roi << tune_roi → tune-set overfitting; back off the param choice\n")
cat("If test_yrs == 0 → strategy lost every test year; do not deploy\n")

# Also baseline: how does CURRENT production params do on test set?
cat("\n=== CURRENT PRODUCTION PARAMS on TEST set ===\n")
cur_results <- tibble(
  strategy = c("value_spread", "value_ml", "kelly_spread", "kelly_ml"),
  bets = list(
    derive_value_spread(p_test, 2.5),
    derive_value_ml(p_test, 0.05, Inf),
    derive_kelly_spread(p_test, 0.25, 0.001),
    derive_kelly_ml(p_test, 0.25, 0.001, Inf)
  )
) %>%
  mutate(metrics = map(bets, simulate_bets)) %>%
  unnest(metrics) %>%
  select(strategy, n, win_pct, roi_overall, n_winning_yrs)
print(cur_results %>% as.data.frame())

cat("\n=== TUNED vs CURRENT on TEST set ===\n")
diff_test <- cur_results %>%
  rename(cur_n = n, cur_roi = roi_overall, cur_yrs = n_winning_yrs) %>%
  select(strategy, cur_n, cur_roi, cur_yrs) %>%
  left_join(test_results %>% select(strategy, tuned_n = n, tuned_roi = roi_overall, tuned_yrs = n_winning_yrs),
            by = "strategy") %>%
  mutate(roi_gain = tuned_roi - cur_roi)
print(diff_test %>% as.data.frame())
