# ============================================
# NFL Predictor: Betting Strategy Parameter Sweep
# ============================================
# Sweeps edge thresholds, Kelly fractions, and max-odds filters across the
# 4 selective strategies (value_spread, value_ml, kelly_spread, kelly_ml)
# using the saved walk-forward OOS predictions.
#
# Reports:
#   - Overall ROI per combo
#   - SD of per-season ROI (stability — overfit configs have high SD)
#   - # winning years out of 10 (robustness)
#
# NOTE: This sweeps params on the same OOS predictions we use to evaluate
# results, so the "best" combo is in-sample-optimized for the sweep set.
# To mitigate, prefer combos with high # winning years and low SD.
#
# Usage: Rscript strategy_sweep.R
# Runtime: < 1 min (no model fitting — just bet re-derivation)
# ============================================

# Helpers + DATA_DIR + PROJECT_DIR all from ensemble_lib.R.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

UNIT_SIZE       <- 100
MAX_STAKE_UNITS <- 5   # per-bet cap

# ---- Load ----
preds <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"), show_col_types = FALSE)
g     <- read_csv(file.path(DATA_DIR, "game_w_features.csv"), show_col_types = FALSE)

p <- preds %>%
  left_join(g %>% select(game_id, home_moneyline, away_moneyline,
                         home_spread_odds, away_spread_odds, impl_prob_home),
            by = "game_id") %>%
  mutate(
    pred_winner        = if_else(nfl_predictor_line > 0, home_team, away_team),
    pred_spread_winner = if_else(line_diff > 0, home_team, away_team)
  ) %>%
  arrange(season, week)

cat(sprintf("Loaded %d predictions across %d seasons (%d–%d)\n\n",
            nrow(p), n_distinct(p$season), min(p$season), max(p$season)))

# Simulate: overall ROI + per-season stability metrics
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

# ---- Sweeps ----

sweep_value_spread <- function() {
  edges <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0)
  map_dfr(edges, function(e) {
    bets <- p %>%
      filter(!is.na(home_spread_odds), !is.na(away_spread_odds),
             abs(line_diff) >= e) %>%
      mutate(
        side    = if_else(pred_spread_winner == home_team, "home", "away"),
        odds    = if_else(side == "home", home_spread_odds, away_spread_odds),
        stake   = UNIT_SIZE,
        correct = cover_correct
      )
    bind_cols(
      tibble(strategy = "value_spread", min_edge_pts = e,
             max_abs_odd = NA_real_, kelly_frac = NA_real_, min_kelly = NA_real_),
      simulate_bets(bets)
    )
  })
}

sweep_value_ml <- function() {
  edges    <- c(0.02, 0.03, 0.05, 0.07, 0.10, 0.15)
  max_odds <- c(Inf, 300, 200, 150)
  expand_grid(min_edge_prob = edges, max_abs_odd = max_odds) %>%
    pmap_dfr(function(min_edge_prob, max_abs_odd) {
      bets <- p %>%
        filter(!is.na(home_moneyline), !is.na(away_moneyline), !is.na(impl_prob_home)) %>%
        mutate(
          side      = if_else(pred_winner == home_team, "home", "away"),
          odds      = if_else(side == "home", home_moneyline, away_moneyline),
          stake     = UNIT_SIZE,
          correct   = ml_correct,
          edge_prob = abs(pred_win_pct - impl_prob_home)
        ) %>%
        filter(edge_prob >= min_edge_prob, abs(odds) <= max_abs_odd)
      bind_cols(
        tibble(strategy = "value_ml", min_edge_pts = NA_real_,
               max_abs_odd = max_abs_odd, min_edge_prob = min_edge_prob,
               kelly_frac = NA_real_, min_kelly = NA_real_),
        simulate_bets(bets)
      )
    })
}

sweep_kelly_spread <- function() {
  fracs   <- c(0.1, 0.2, 0.25, 0.5, 1.0)
  min_ks  <- c(0.001, 0.01, 0.025, 0.05)
  expand_grid(kelly_frac = fracs, min_kelly = min_ks) %>%
    pmap_dfr(function(kelly_frac, min_kelly) {
      bets <- p %>%
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
      bind_cols(
        tibble(strategy = "kelly_spread", min_edge_pts = NA_real_,
               max_abs_odd = NA_real_, kelly_frac = kelly_frac, min_kelly = min_kelly),
        simulate_bets(bets)
      )
    })
}

sweep_kelly_ml <- function() {
  fracs    <- c(0.1, 0.2, 0.25, 0.5, 1.0)
  min_ks   <- c(0.001, 0.01, 0.025, 0.05)
  max_odds <- c(Inf, 300, 200, 150)
  expand_grid(kelly_frac = fracs, min_kelly = min_ks, max_abs_odd = max_odds) %>%
    pmap_dfr(function(kelly_frac, min_kelly, max_abs_odd) {
      bets <- p %>%
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
      bind_cols(
        tibble(strategy = "kelly_ml", min_edge_pts = NA_real_,
               max_abs_odd = max_abs_odd, kelly_frac = kelly_frac, min_kelly = min_kelly),
        simulate_bets(bets)
      )
    })
}

# ---- Run ----
cat("Running sweeps...\n")
vs <- sweep_value_spread()
vm <- sweep_value_ml()
ks <- sweep_kelly_spread()
km <- sweep_kelly_ml()

# Threshold for trustworthy sample
MIN_N <- 50

format_table <- function(x, cols) {
  x %>% select(any_of(cols)) %>% as.data.frame()
}

cat("\n=== TOP COMBOS BY ROI (min n = ", MIN_N, "; sorted by stability-adjusted ROI) ===\n", sep = "")

cat("\n-- value_spread --\n")
print(format_table(
  vs %>% filter(n >= MIN_N) %>% arrange(desc(roi_overall)) %>% head(8),
  c("min_edge_pts", "n", "win_pct", "roi_overall", "roi_sd_yr", "n_winning_yrs", "n_total_yrs")
))

cat("\n-- value_ml --\n")
print(format_table(
  vm %>% filter(n >= MIN_N) %>% arrange(desc(roi_overall)) %>% head(10),
  c("min_edge_prob", "max_abs_odd", "n", "win_pct", "roi_overall", "roi_sd_yr", "n_winning_yrs")
))

cat("\n-- kelly_spread --\n")
print(format_table(
  ks %>% filter(n >= MIN_N) %>% arrange(desc(roi_overall)) %>% head(10),
  c("kelly_frac", "min_kelly", "n", "win_pct", "roi_overall", "roi_sd_yr", "n_winning_yrs")
))

cat("\n-- kelly_ml --\n")
print(format_table(
  km %>% filter(n >= MIN_N) %>% arrange(desc(roi_overall)) %>% head(10),
  c("kelly_frac", "min_kelly", "max_abs_odd", "n", "win_pct", "roi_overall", "roi_sd_yr", "n_winning_yrs")
))

cat("\n=== CURRENT PRODUCTION PARAMS BENCHMARK ===\n")
# Replicate exactly what walk_forward_eval.R currently does
current_vs <- vs %>% filter(min_edge_pts == 2.5)
current_vm <- vm %>% filter(min_edge_prob == 0.05, is.infinite(max_abs_odd))
current_ks <- ks %>% filter(kelly_frac == 0.25, min_kelly == 0.001)
current_km <- km %>% filter(kelly_frac == 0.25, min_kelly == 0.001, is.infinite(max_abs_odd))
print(bind_rows(current_vs, current_vm, current_ks, current_km) %>%
        select(strategy, min_edge_pts, min_edge_prob, max_abs_odd, kelly_frac, min_kelly,
               n, win_pct, roi_overall, n_winning_yrs) %>% as.data.frame())

# Save full results
all_results <- bind_rows(vs, vm, ks, km)
write_csv(all_results, file.path(DATA_DIR, "strategy_sweep_results.csv"))
cat(sprintf("\nWrote %d combos to strategy_sweep_results.csv\n", nrow(all_results)))
