# ============================================
# NFL Predictor: Combined-Portfolio Simulation
# ============================================
# Single bankroll across all candidate bets per game, sized by EV + Kelly.
# Replaces the per-strategy independent-bankroll backtest with a realistic
# "one wallet" simulation. Outputs total return, drawdown, Sharpe, and
# per-bet-type breakdowns.
#
# Bet candidates per game:
#   - 1 spread bet (side with positive line_diff vs Vegas) using cover_prob_home
#   - 1 ML bet     (side our pred_win_pct favors)         using pred_win_pct
#
# Filter (must all pass):
#   - EV per $1 >= MIN_EV (e.g., 3%)
#   - |odds| <= MAX_ABS_ODDS
#   - Kelly stake target >= MIN_STAKE
#
# Sizing:
#   - Quarter Kelly × bankroll, capped at MAX_BET_PCT × bankroll, rounded
#
# Usage: Rscript portfolio_sim.R (uses walk_forward_predictions.csv)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")  # american_to_decimal,
# kelly_fraction_nonneg, ev_per_dollar, pnl_for_win, calibrate_ev, DATA_DIR.

# ---- Parameters (balanced config + EV calibration) ----
INITIAL_BANKROLL <- 10000    # dollars
KELLY_MULTIPLIER <- 0.15     # tuned for Sharpe / drawdown balance
MAX_BET_PCT      <- 0.05     # max 5% of bankroll per bet
MIN_EV           <- 0.05     # CALIBRATED EV threshold (≈ raw 0.128 ≈ 5% real edge)
MAX_ABS_ODDS     <- 300      # skip bets with |odds| > 300 (heavy fav/dog)
ROUND_TO         <- 5        # round stakes to nearest $5
MIN_STAKE        <- 5        # min stake $5

# EV calibration — defaults to the deployed bundle's baked cal (the canonical
# engine); env vars override it for sweeps where we refit per variant.
.prod_cal <- production_ev_calibration()
EV_CAL <- list(
  intercept = as.numeric(Sys.getenv("EV_CAL_INTERCEPT_OVERRIDE", .prod_cal$intercept)),
  slope     = as.numeric(Sys.getenv("EV_CAL_SLOPE_OVERRIDE", .prod_cal$slope))
)
message(sprintf("[portfolio_sim] EV calibration: intercept=%.4f, slope=%.4f",
                EV_CAL$intercept, EV_CAL$slope))

# ---- Load ----
preds <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"),
                  show_col_types = FALSE)
g     <- read_csv(file.path(DATA_DIR, "game_w_features.csv"),
                  show_col_types = FALSE)

p <- preds %>%
  left_join(g %>% select(game_id, home_moneyline, away_moneyline,
                         home_spread_odds, away_spread_odds, impl_prob_home),
            by = "game_id") %>%
  arrange(season, week, game_id)

# Re-derive cover_prob_home using sigma from training (we don't have it in predictions
# csv; use a typical σ if missing). Walk_forward saves cover_prob_home indirectly via
# line_diff + sigma; if not present, approximate from line_diff and σ=12.91 (current).
if (!"cover_prob_home" %in% names(p)) {
  cat("[note] cover_prob_home not in predictions; approximating with σ=12.91\n")
  p <- p %>% mutate(cover_prob_home = pnorm(line_diff / 12.91))
}

# Also need cover_correct + ml_correct
# (these ARE saved per fold to walk_forward_predictions.csv)
stopifnot(all(c("cover_correct", "ml_correct", "spread_line", "home_result") %in% names(p)))

cat(sprintf("Loaded %d games across %d seasons\n",
            nrow(p), n_distinct(p$season)))

# Helpers (american_to_decimal, kelly_fraction_nonneg, ev_per_dollar,
# pnl_for_win, calibrate_ev) all come from ensemble_lib.R.

# ---- Per-game candidates ----
# Returns 0-2 candidate bets per game, each with EV + Kelly + correctness.
build_candidates <- function(row) {
  out <- list()

  # Spread bet: side with positive cover edge (line_diff > 0 → home, else away)
  if (!is.na(row$home_spread_odds) && !is.na(row$away_spread_odds) &&
      !is.na(row$cover_prob_home)) {
    if (row$line_diff > 0) {
      side  <- "home_spread"
      odds  <- row$home_spread_odds
      prob  <- row$cover_prob_home
    } else {
      side  <- "away_spread"
      odds  <- row$away_spread_odds
      prob  <- 1 - row$cover_prob_home
    }
    out[[length(out) + 1]] <- tibble(
      bet_type = side, odds = odds, our_prob = prob,
      correct  = row$cover_correct
    )
  }

  # ML bet: side our pred_win_pct favors
  if (!is.na(row$home_moneyline) && !is.na(row$away_moneyline) &&
      !is.na(row$pred_win_pct)) {
    if (row$pred_win_pct > 0.5) {
      side  <- "home_ml"
      odds  <- row$home_moneyline
      prob  <- row$pred_win_pct
    } else {
      side  <- "away_ml"
      odds  <- row$away_moneyline
      prob  <- 1 - row$pred_win_pct
    }
    out[[length(out) + 1]] <- tibble(
      bet_type = side, odds = odds, our_prob = prob,
      correct  = row$ml_correct
    )
  }

  if (length(out) == 0) tibble() else bind_rows(out)
}

# ---- Simulator ----
# Sequential through (season, week, game) order. Bankroll updates per bet.
simulate_portfolio <- function(predictions) {
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
        ev          = calibrate_ev(ev_raw, EV_CAL),  # calibrated EV
        kelly_frac  = kelly_fraction_nonneg(our_prob, odds),
        stake_target= pmin(kelly_frac * KELLY_MULTIPLIER * bankroll,
                            bankroll * MAX_BET_PCT)
      ) %>%
      filter(ev >= MIN_EV,
             abs(odds) <= MAX_ABS_ODDS,
             stake_target >= MIN_STAKE)

    if (nrow(cands) == 0) next

    for (j in seq_len(nrow(cands))) {
      bet   <- cands[j, ]
      stake <- max(MIN_STAKE, round(bet$stake_target / ROUND_TO) * ROUND_TO)
      if (stake > bankroll * MAX_BET_PCT) {
        stake <- floor(bankroll * MAX_BET_PCT / ROUND_TO) * ROUND_TO
      }
      if (stake < MIN_STAKE) next

      pnl <- case_when(
        is.na(bet$correct) ~ 0,                            # push refund
        bet$correct == 1   ~ pnl_for_win(stake, bet$odds), # win
        TRUE               ~ -stake                         # loss
      )
      bankroll <- bankroll + pnl
      hist_i <- hist_i + 1
      history[[hist_i]] <- tibble(
        season = row$season, week = row$week, game_id = row$game_id,
        bet_type = bet$bet_type, odds = bet$odds,
        our_prob = bet$our_prob, ev = bet$ev, kelly_frac = bet$kelly_frac,
        stake = stake, correct = bet$correct, pnl = pnl,
        bankroll_after = bankroll
      )
    }
  }
  bind_rows(history[seq_len(hist_i)])
}

# ---- Run ----
cat("\nSimulating portfolio...\n")
trades <- simulate_portfolio(p %>% filter(!is.na(home_result)))

if (nrow(trades) == 0) {
  stop("No bets placed. Check EV / odds filters.")
}

# ---- Headline metrics ----
total_stake     <- sum(trades$stake)
total_pnl       <- sum(trades$pnl)
total_roi       <- total_pnl / total_stake
final_bankroll  <- INITIAL_BANKROLL + total_pnl
n_seasons       <- n_distinct(trades$season)
ann_pnl         <- total_pnl / n_seasons
cagr            <- (final_bankroll / INITIAL_BANKROLL) ^ (1 / n_seasons) - 1

# Drawdown
trades <- trades %>%
  mutate(peak = cummax(bankroll_after),
         drawdown = (peak - bankroll_after) / peak)
max_dd <- max(trades$drawdown, na.rm = TRUE)

# Weekly returns + Sharpe
weekly <- trades %>%
  group_by(season, week) %>%
  summarise(
    week_pnl = sum(pnl),
    bankroll_end = last(bankroll_after),
    .groups = "drop"
  ) %>%
  arrange(season, week) %>%
  mutate(bankroll_start = lag(bankroll_end, default = INITIAL_BANKROLL),
         week_return    = week_pnl / bankroll_start)

mean_wk     <- mean(weekly$week_return)
sd_wk       <- sd(weekly$week_return)
sharpe_yr   <- (mean_wk / sd_wk) * sqrt(18)  # ~18 wks per season

cat(sprintf("\n=== PORTFOLIO PERFORMANCE ===\n"))
cat(sprintf("Bets placed:           %d\n", nrow(trades)))
cat(sprintf("Pushes:                %d\n", sum(is.na(trades$correct))))
cat(sprintf("Win rate (decided):    %.3f\n", mean(trades$correct, na.rm = TRUE)))
cat(sprintf("Total stake:           $%s\n", format(round(total_stake), big.mark=",")))
cat(sprintf("Total PnL:             $%s\n", format(round(total_pnl), big.mark=",")))
cat(sprintf("ROI on staked $:       %.2f%%\n", 100 * total_roi))
cat(sprintf("\nStarting bankroll:     $%s\n", format(INITIAL_BANKROLL, big.mark=",")))
cat(sprintf("Final bankroll:        $%s\n", format(round(final_bankroll), big.mark=",")))
cat(sprintf("Avg $/yr:              $%s\n", format(round(ann_pnl), big.mark=",")))
cat(sprintf("CAGR:                  %.2f%%\n", 100 * cagr))
cat(sprintf("Max drawdown:          %.1f%%\n", 100 * max_dd))
cat(sprintf("Weekly Sharpe (ann.):  %.2f\n", sharpe_yr))

cat(sprintf("\n=== BY BET TYPE ===\n"))
bt_summary <- trades %>%
  group_by(bet_type) %>%
  summarise(
    n_bets   = n(),
    n_wins   = sum(correct, na.rm = TRUE),
    win_pct  = round(mean(correct, na.rm = TRUE), 3),
    avg_stake= round(mean(stake), 0),
    avg_ev   = round(mean(ev), 3),
    avg_kelly= round(mean(kelly_frac), 3),
    total_pnl= round(sum(pnl), 0),
    roi      = round(sum(pnl) / sum(stake), 4)
  ) %>%
  arrange(desc(roi))
print(as.data.frame(bt_summary))

cat(sprintf("\n=== BY SEASON ===\n"))
season_summary <- trades %>%
  group_by(season) %>%
  summarise(
    n_bets       = n(),
    total_stake  = round(sum(stake), 0),
    total_pnl    = round(sum(pnl), 0),
    roi          = round(sum(pnl) / sum(stake), 4),
    end_bankroll = round(last(bankroll_after), 0),
    .groups = "drop"
  )
print(as.data.frame(season_summary))

# ---- Save ----
write_csv(trades,         file.path(DATA_DIR, "portfolio_trades.csv"))
write_csv(weekly,         file.path(DATA_DIR, "portfolio_weekly.csv"))
write_csv(bt_summary,     file.path(DATA_DIR, "portfolio_bet_type_summary.csv"))
write_csv(season_summary, file.path(DATA_DIR, "portfolio_season_summary.csv"))
cat("\nWrote portfolio_trades.csv + 3 summary tables\n")
