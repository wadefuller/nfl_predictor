# ============================================
# NFL Predictor: Portfolio Parameter Sweep
# ============================================
# Sweeps (MIN_EV, KELLY_MULTIPLIER) on the portfolio sim to find Pareto-
# efficient settings. Reports CAGR, max drawdown, Sharpe per combo so we
# can pick by risk tolerance.
#
# Usage: Rscript portfolio_sweep.R
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")  # helpers + EV_CALIBRATION_DEFAULT + DATA_DIR

# ---- Fixed parameters ----
INITIAL_BANKROLL <- 10000
MAX_BET_PCT      <- 0.05
MAX_ABS_ODDS     <- 300
ROUND_TO         <- 5
MIN_STAKE        <- 5

# ---- Sweep grid (CALIBRATED EV thresholds; raw≈calibrated/0.36) ----
MIN_EV_GRID    <- c(0.02, 0.04, 0.06, 0.08, 0.10, 0.12)
KELLY_MULT_GRID <- c(0.05, 0.10, 0.15, 0.25, 0.50)

# Canonical engine: default to the deployed bundle's baked EV calibration.
EV_CAL <- production_ev_calibration()

preds <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"),
                  show_col_types = FALSE)
g     <- read_csv(file.path(DATA_DIR, "game_w_features.csv"),
                  show_col_types = FALSE)
p <- preds %>%
  left_join(g %>% select(game_id, home_moneyline, away_moneyline,
                         home_spread_odds, away_spread_odds, impl_prob_home),
            by = "game_id") %>%
  arrange(season, week, game_id)
if (!"cover_prob_home" %in% names(p)) {
  p <- p %>% mutate(cover_prob_home = pnorm(line_diff / 12.91))
}
p <- p %>% filter(!is.na(home_result))

# Helpers all sourced from ensemble_lib.R above.

build_candidates <- function(row) {
  out <- list()
  if (!is.na(row$home_spread_odds) && !is.na(row$away_spread_odds) &&
      !is.na(row$cover_prob_home)) {
    if (row$line_diff > 0) {
      side <- "home_spread"; odds <- row$home_spread_odds; prob <- row$cover_prob_home
    } else {
      side <- "away_spread"; odds <- row$away_spread_odds; prob <- 1 - row$cover_prob_home
    }
    out[[length(out) + 1]] <- tibble(bet_type = side, odds = odds,
                                       our_prob = prob, correct = row$cover_correct)
  }
  if (!is.na(row$home_moneyline) && !is.na(row$away_moneyline) &&
      !is.na(row$pred_win_pct)) {
    if (row$pred_win_pct > 0.5) {
      side <- "home_ml"; odds <- row$home_moneyline; prob <- row$pred_win_pct
    } else {
      side <- "away_ml"; odds <- row$away_moneyline; prob <- 1 - row$pred_win_pct
    }
    out[[length(out) + 1]] <- tibble(bet_type = side, odds = odds,
                                       our_prob = prob, correct = row$ml_correct)
  }
  if (length(out) == 0) tibble() else bind_rows(out)
}

simulate <- function(predictions, min_ev, kelly_mult) {
  bankroll <- INITIAL_BANKROLL
  history  <- vector("list", nrow(predictions) * 2)
  hist_i   <- 0
  for (i in seq_len(nrow(predictions))) {
    row   <- predictions[i, ]
    cands <- build_candidates(row)
    if (nrow(cands) == 0) next
    cands <- cands %>%
      mutate(
        ev_raw      = ev_per_dollar(our_prob, odds),
        ev          = calibrate_ev(ev_raw, EV_CAL),
        kelly_frac  = kelly_fraction_nonneg(our_prob, odds),
        stake_target= pmin(kelly_frac * kelly_mult * bankroll, bankroll * MAX_BET_PCT)
      ) %>%
      filter(ev >= min_ev, abs(odds) <= MAX_ABS_ODDS, stake_target >= MIN_STAKE)
    if (nrow(cands) == 0) next
    for (j in seq_len(nrow(cands))) {
      bet <- cands[j, ]
      stake <- max(MIN_STAKE, round(bet$stake_target / ROUND_TO) * ROUND_TO)
      if (stake > bankroll * MAX_BET_PCT)
        stake <- floor(bankroll * MAX_BET_PCT / ROUND_TO) * ROUND_TO
      if (stake < MIN_STAKE) next
      pnl <- case_when(
        is.na(bet$correct) ~ 0,
        bet$correct == 1   ~ pnl_for_win(stake, bet$odds),
        TRUE               ~ -stake
      )
      bankroll <- bankroll + pnl
      hist_i <- hist_i + 1
      history[[hist_i]] <- tibble(
        season = row$season, week = row$week, stake = stake,
        correct = bet$correct, pnl = pnl, bankroll_after = bankroll
      )
    }
  }
  bind_rows(history[seq_len(hist_i)])
}

summarize_run <- function(trades, n_seasons) {
  if (nrow(trades) == 0) return(NULL)
  final_bankroll <- INITIAL_BANKROLL + sum(trades$pnl)
  trades <- trades %>%
    mutate(peak = cummax(bankroll_after),
           drawdown = (peak - bankroll_after) / peak)
  weekly <- trades %>%
    group_by(season, week) %>%
    summarise(week_pnl = sum(pnl),
              bankroll_end = last(bankroll_after), .groups = "drop") %>%
    arrange(season, week) %>%
    mutate(bankroll_start = lag(bankroll_end, default = INITIAL_BANKROLL),
           week_return    = week_pnl / bankroll_start)
  tibble(
    n_bets      = nrow(trades),
    win_pct     = round(mean(trades$correct, na.rm = TRUE), 3),
    total_pnl   = round(sum(trades$pnl), 0),
    roi         = round(sum(trades$pnl) / sum(trades$stake), 4),
    cagr        = round((final_bankroll/INITIAL_BANKROLL)^(1/n_seasons) - 1, 4),
    max_dd      = round(max(trades$drawdown, na.rm = TRUE), 3),
    sharpe_yr   = round(mean(weekly$week_return) / sd(weekly$week_return) * sqrt(18), 2),
    end_bankroll= round(final_bankroll, 0)
  )
}

# ---- Sweep ----
n_seasons <- n_distinct(p$season)
cat(sprintf("Sweeping %d EV × %d Kelly = %d combos over %d games...\n",
            length(MIN_EV_GRID), length(KELLY_MULT_GRID),
            length(MIN_EV_GRID) * length(KELLY_MULT_GRID), nrow(p)))

results <- expand_grid(min_ev = MIN_EV_GRID, kelly_mult = KELLY_MULT_GRID) %>%
  pmap_dfr(function(min_ev, kelly_mult) {
    trades <- simulate(p, min_ev, kelly_mult)
    res <- summarize_run(trades, n_seasons)
    if (is.null(res)) return(NULL)
    bind_cols(tibble(min_ev = min_ev, kelly_mult = kelly_mult), res)
  })

cat("\n=== SWEEP RESULTS (sorted by Sharpe) ===\n")
print(results %>% arrange(desc(sharpe_yr)) %>% head(15) %>% as.data.frame())

cat("\n=== TOP BY CAGR ===\n")
print(results %>% arrange(desc(cagr)) %>% head(10) %>% as.data.frame())

cat("\n=== TOP BY ROI (with min n_bets to be meaningful) ===\n")
print(results %>% filter(n_bets >= 200) %>% arrange(desc(roi)) %>% head(10) %>% as.data.frame())

write_csv(results, file.path(DATA_DIR, "portfolio_sweep_results.csv"))
cat("\nWrote portfolio_sweep_results.csv\n")
