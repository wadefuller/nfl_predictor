# ============================================
# NFL Predictor: Portfolio Performance Deep Dive
# ============================================
# Reads portfolio_trades.csv and looks for patterns:
#   - Performance by week-of-season (when does the model win/lose?)
#   - Performance by bet type within weeks
#   - Per-season variance
#   - Calibration: predicted EV vs realized ROI per bucket
#   - Drawdown periods (when does the model bleed?)
#   - Best/worst weeks
#
# Run after portfolio_sim.R has produced portfolio_trades.csv.
# Usage: Rscript portfolio_analysis.R
# ============================================

# DATA_DIR + tidyverse from ensemble_lib.R.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

trades <- read_csv(file.path(DATA_DIR, "portfolio_trades.csv"), show_col_types = FALSE)
weekly <- read_csv(file.path(DATA_DIR, "portfolio_weekly.csv"), show_col_types = FALSE)

cat(sprintf("Loaded %d trades over %d weeks, %d seasons\n",
            nrow(trades), n_distinct(paste(trades$season, trades$week)),
            n_distinct(trades$season)))
cat(sprintf("Total: %d bets, %d wins, %d pushes, total PnL $%.0f\n\n",
            nrow(trades),
            sum(trades$correct == 1, na.rm = TRUE),
            sum(is.na(trades$correct)),
            sum(trades$pnl)))

# ---- Performance by week of season ----
cat("=== BY WEEK-OF-SEASON ===\n")
by_week <- trades %>%
  group_by(week) %>%
  summarise(
    seasons     = n_distinct(season),
    n_bets      = n(),
    bets_per_szn= round(n() / n_distinct(season), 1),
    win_pct     = round(mean(correct == 1, na.rm = TRUE), 3),
    avg_ev      = round(mean(ev), 3),
    avg_stake   = round(mean(stake), 0),
    total_pnl   = round(sum(pnl), 0),
    roi         = round(sum(pnl) / sum(stake), 4),
    .groups = "drop"
  ) %>%
  arrange(week)
print(as.data.frame(by_week))

# ---- Performance by season ----
cat("\n=== BY SEASON ===\n")
by_season <- trades %>%
  group_by(season) %>%
  summarise(
    n_bets    = n(),
    win_pct   = round(mean(correct == 1, na.rm = TRUE), 3),
    total_pnl = round(sum(pnl), 0),
    roi       = round(sum(pnl) / sum(stake), 4),
    end_bankroll = round(last(bankroll_after), 0),
    .groups = "drop"
  )
print(as.data.frame(by_season))

# ---- Performance by bet type × week ----
cat("\n=== BY BET TYPE (overall) ===\n")
by_bet_type <- trades %>%
  group_by(bet_type) %>%
  summarise(
    n_bets    = n(),
    win_pct   = round(mean(correct == 1, na.rm = TRUE), 3),
    avg_ev    = round(mean(ev), 3),
    total_pnl = round(sum(pnl), 0),
    roi       = round(sum(pnl) / sum(stake), 4),
    .groups = "drop"
  ) %>% arrange(desc(roi))
print(as.data.frame(by_bet_type))

# ---- EV calibration: do high-EV bets actually deliver? ----
cat("\n=== EV CALIBRATION (does predicted EV match realized ROI?) ===\n")
ev_buckets <- trades %>%
  mutate(ev_bucket = cut(ev, breaks = c(0.15, 0.20, 0.25, 0.30, 0.40, Inf),
                          labels = c("0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.40", "0.40+"),
                          include.lowest = TRUE)) %>%
  group_by(ev_bucket) %>%
  summarise(
    n_bets   = n(),
    avg_ev_pred = round(mean(ev), 3),
    win_pct  = round(mean(correct == 1, na.rm = TRUE), 3),
    realized_roi = round(sum(pnl) / sum(stake), 3),
    avg_stake = round(mean(stake), 0),
    .groups = "drop"
  )
cat("(If model is well-calibrated, realized_roi ≈ avg_ev_pred per bucket)\n")
print(as.data.frame(ev_buckets))

# ---- Odds buckets: how do we do at different odds levels? ----
cat("\n=== BY ODDS BUCKET ===\n")
odds_buckets <- trades %>%
  mutate(
    odds_bucket = cut(odds,
      breaks = c(-Inf, -250, -150, -110, 100, 150, 250, Inf),
      labels = c("<= -250 (heavy fav)", "-249..-150", "-149..-110", "-109..100 (juice)",
                  "101..150", "151..250", ">= 251 (heavy dog)"))) %>%
  group_by(odds_bucket) %>%
  summarise(
    n_bets   = n(),
    win_pct  = round(mean(correct == 1, na.rm = TRUE), 3),
    avg_ev   = round(mean(ev), 3),
    realized_roi = round(sum(pnl) / sum(stake), 3),
    .groups = "drop"
  )
print(as.data.frame(odds_buckets))

# ---- Drawdown analysis: identify worst stretches ----
cat("\n=== DRAWDOWN PERIODS ===\n")
trades_dd <- trades %>%
  mutate(peak = cummax(bankroll_after),
         drawdown = (peak - bankroll_after) / peak)
worst_dd <- trades_dd %>%
  arrange(desc(drawdown)) %>%
  slice(1) %>%
  select(season, week, bankroll_after, peak, drawdown)
cat(sprintf("Max drawdown: %.1f%% (peak $%s → trough $%s in %d wk %d)\n",
            100*worst_dd$drawdown,
            format(round(worst_dd$peak), big.mark=","),
            format(round(worst_dd$bankroll_after), big.mark=","),
            worst_dd$season, worst_dd$week))

# ---- Best / worst weeks ----
cat("\n=== BEST WEEKS ===\n")
print(weekly %>% arrange(desc(week_return)) %>% head(5) %>%
        select(season, week, week_pnl, week_return, bankroll_end) %>%
        as.data.frame())

cat("\n=== WORST WEEKS ===\n")
print(weekly %>% arrange(week_return) %>% head(5) %>%
        select(season, week, week_pnl, week_return, bankroll_end) %>%
        as.data.frame())

# ---- Weekly return distribution ----
cat("\n=== WEEKLY RETURN DISTRIBUTION ===\n")
cat(sprintf("Weeks with bets: %d\n", nrow(weekly)))
cat(sprintf("Positive weeks: %d (%.1f%%)\n",
            sum(weekly$week_return > 0), 100 * mean(weekly$week_return > 0)))
cat(sprintf("Median weekly return: %.2f%%\n", 100 * median(weekly$week_return)))
cat(sprintf("Mean weekly return:   %.2f%%\n", 100 * mean(weekly$week_return)))
cat(sprintf("SD weekly return:     %.2f%%\n", 100 * sd(weekly$week_return)))

# ---- Stake-weighted vs bet-count concentration ----
cat("\n=== CONCENTRATION ANALYSIS ===\n")
top_10pct_n <- ceiling(0.10 * nrow(trades))
top_bets <- trades %>% arrange(desc(stake)) %>% head(top_10pct_n)
cat(sprintf("Top 10%% of bets by stake: %d bets, %.1f%% of total $ staked, %.1f%% of PnL\n",
            top_10pct_n,
            100 * sum(top_bets$stake) / sum(trades$stake),
            100 * sum(top_bets$pnl)   / sum(trades$pnl)))
