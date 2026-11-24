# ============================================
# NFL Predictor: EV Calibration Refitter
# ============================================
# Reads walk_forward_predictions.csv + game_w_features.csv, builds ML and
# spread bet candidates with raw EV, then fits a linear regression of
# realized PnL/$ on raw EV to get fresh (intercept, slope) constants.
#
# Output:
#   - Prints fitted constants
#   - Writes data/ev_calibration.json (machine-readable)
#
# Usage: Rscript scripts/recalibrate_ev.R
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")  # american_to_decimal, ev_per_dollar, DATA_DIR
suppressPackageStartupMessages(library(jsonlite))

preds <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"),
                  show_col_types = FALSE)
g <- read_csv(file.path(DATA_DIR, "game_w_features.csv"),
              show_col_types = FALSE)

p <- preds %>%
  left_join(g %>% select(game_id, home_moneyline, away_moneyline,
                          home_spread_odds, away_spread_odds), by = "game_id") %>%
  filter(!is.na(home_result))
if (!"cover_prob_home" %in% names(p)) {
  p <- p %>% mutate(cover_prob_home = pnorm(line_diff / 12.91))
}

# Build ML candidates (one per game: pick side our pred favors)
# `correct` must describe the side STAKED. The bet goes on the higher win
# PROBABILITY (that is the probability the EV is priced from), while ml_correct
# grades the MARGIN's pick. They disagree on coin-flip games, so the calibration
# was being fitted against outcomes for bets it had not placed — on exactly the
# thin-edge rows that set the slope.
ml_bets <- p %>%
  mutate(
    odds = if_else(pred_win_pct > 0.5, home_moneyline, away_moneyline),
    prob = if_else(pred_win_pct > 0.5, pred_win_pct, 1 - pred_win_pct),
    bet_team = if_else(pred_win_pct > 0.5, home_team, away_team),
    correct  = if_else(home_result == 0, NA_integer_,
                       as.integer(bet_team == if_else(home_result > 0, home_team, away_team)))
  ) %>%
  filter(!is.na(odds), !is.na(prob), !is.na(correct)) %>%
  mutate(
    ev_raw   = ev_per_dollar(prob, odds),
    pnl_unit = if_else(correct == 1, if_else(odds > 0, odds/100, 100/abs(odds)), -1),
    bet_kind = "ml"
  )

# Build spread candidates (one per game: side with positive cover edge)
spread_bets <- p %>%
  mutate(
    odds = if_else(line_diff > 0, home_spread_odds, away_spread_odds),
    prob = if_else(line_diff > 0, cover_prob_home, 1 - cover_prob_home),
    correct = cover_correct
  ) %>%
  filter(!is.na(odds), !is.na(prob), !is.na(correct)) %>%
  mutate(
    ev_raw   = ev_per_dollar(prob, odds),
    pnl_unit = if_else(correct == 1, if_else(odds > 0, odds/100, 100/abs(odds)), -1),
    bet_kind = "spread"
  )

all_bets <- bind_rows(ml_bets, spread_bets)
cat(sprintf("Calibration sample: %d ML + %d spread = %d bets\n",
            nrow(ml_bets), nrow(spread_bets), nrow(all_bets)))

# Fit linear: pnl_unit ~ ev_raw
fit <- lm(pnl_unit ~ ev_raw, data = all_bets)
intercept <- unname(coef(fit)[1])
slope     <- unname(coef(fit)[2])

cat(sprintf("\nFresh EV calibration:\n  intercept = %.4f\n  slope     = %.4f\n",
            intercept, slope))
cat(sprintf("  (previous defaults: intercept=0.0039, slope=0.3596)\n"))

# Bucket diagnostic
all_bets %>%
  mutate(bucket = cut(ev_raw, c(-Inf, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.3, Inf))) %>%
  group_by(bucket) %>%
  summarise(n = n(), mean_ev_raw = round(mean(ev_raw), 3),
            realized_pnl = round(mean(pnl_unit), 3),
            .groups = "drop") %>%
  as.data.frame() %>% print()

# Write JSON
out <- list(intercept = intercept, slope = slope,
            fit_n = nrow(all_bets),
            fit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
out_path <- file.path(DATA_DIR, "ev_calibration.json")
write_json(out, out_path, auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nWrote %s\n", out_path))
