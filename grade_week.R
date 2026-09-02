# ============================================
# Post-week grader — nfl_predictor game model + bet card
# ============================================
# After a week's games are final, score the model against actuals and replay the
# bet card it would have placed:
#   * per-game: predicted vs actual margin (MAE), ATS cover correctness, ML
#     correctness, win-prob Brier.
#   * bet card: rebuild candidates with outcomes, size with the bundle's EV
#     calibration, settle P&L, summarize record + ROI.
# Writes data/graded_<season>_wkNN.csv and the website model_scorecard.json.
#
# Usage: Rscript grade_week.R                 # auto: latest fully-played week
#        Rscript grade_week.R 2024 10         # explicit
# ============================================

suppressPackageStartupMessages({ library(tidyverse) })

NFL_DIR <- Sys.getenv("NFL_PROJECT_DIR", "~/Projects/R/nfl_predictor")
source(file.path(NFL_DIR, "ensemble_lib.R"))
source("/Users/wadefuller/Projects/R/weekly_run/lib/export_web.R")

args <- commandArgs(trailingOnly = TRUE)
arg_season <- if (length(args) >= 1) as.integer(args[1]) else NA
arg_week   <- if (length(args) >= 2) as.integer(args[2]) else NA

gwf <- read.csv(file.path(DATA_DIR, "game_w_features.csv")) %>% as_tibble()
season <- if (!is.na(arg_season)) arg_season else max(gwf$season)

# Latest fully-completed week of the season (all results present).
resolve_done <- function(d, season) {
  d %>% filter(season == !!season) %>% group_by(week) %>%
    summarise(done = all(!is.na(home_result)), .groups = "drop") %>%
    filter(done) %>% summarise(w = max(week)) %>% pull(w)
}
week <- if (!is.na(arg_week)) arg_week else resolve_done(gwf, season)
if (!is.finite(week)) stop(sprintf("No fully-completed week found in season %d.", season))
cat(sprintf("Grading nfl_predictor: season %d, week %d\n", season, week))

games <- gwf %>% filter(season == !!season, week == !!week, !is.na(home_result))
if (nrow(games) == 0) stop("No completed games for that week.")

bundle <- readRDS(file.path(MODELS_DIR, "ensemble_latest.rds"))

# ---- in-sample guard --------------------------------------------------------
# The deployed bundle is retrained on ALL completed seasons, so grading a season
# it trained on is IN-SAMPLE (optimistic) — and silently overwrites the honest
# out-of-sample scorecard written by weekly_run/backfill_model_oos.R. Mixing the
# two inside one season corrupts the aggregate that export_season_betting.R
# builds. Refuse by default; ALLOW_IN_SAMPLE=1 to override deliberately.
train_rng <- bundle$metadata$training_seasons
in_sample <- !is.null(train_rng) && season >= min(train_rng) && season <= max(train_rng)
if (in_sample) {
  msg <- sprintf(paste0(
    "season %d is INSIDE the bundle's training range (%d-%d) — grading it here is in-sample.\n",
    "  For a historical week use the out-of-sample path instead:\n",
    "    Rscript weekly_run/backfill_model_oos.R %d\n",
    "  Set ALLOW_IN_SAMPLE=1 to override (this overwrites the OOS scorecard)."),
    season, min(train_rng), max(train_rng), season)
  if (Sys.getenv("ALLOW_IN_SAMPLE", "0") != "1") stop(msg)
  warning(msg)
}
# Stamped into the JSON so a mixed-provenance season is detectable downstream.
grade_source <- if (in_sample) "bundle_in_sample" else "bundle_oos"
preds <- predict_with_ensemble(bundle, games) %>%
  left_join(games %>% select(game_id, home_result, home_win,
                             home_spread_odds, away_spread_odds,
                             impl_prob_home, impl_prob_away), by = "game_id")

# ---- per-game correctness ---------------------------------------------------
scored <- preds %>%
  mutate(
    actual_margin      = home_result,
    abs_err            = abs(nfl_predictor_line - actual_margin),
    spread_winner      = if_else(home_result - spread_line > 0, home_team, away_team),
    pred_spread_winner = if_else(line_diff > 0, home_team, away_team),
    pred_winner        = if_else(nfl_predictor_line > 0, home_team, away_team),
    winning_team       = if_else(home_win == 1, home_team, away_team),
    spread_push        = (home_result - spread_line == 0),
    ml_tie             = (home_result == 0),
    cover_correct      = if_else(spread_push, NA_integer_, as.integer(pred_spread_winner == spread_winner)),
    ml_correct         = if_else(ml_tie, NA_integer_, as.integer(pred_winner == winning_team)),
    brier              = (pred_win_pct - home_win)^2
  )

# Both win probabilities are stated for the HOME team (the second name in `game`),
# and the market's pair is de-vigged so it sums to 1 and is comparable to the
# model's. edge_pct = how much more confident the model is than the market.
game_summary <- scored %>%
  mutate(vig = impl_prob_home + impl_prob_away,
         vegas_win_pct = if_else(!is.na(vig) & vig > 0, impl_prob_home / vig, NA_real_)) %>%
  transmute(game = sprintf("%s @ %s", away_team, home_team),
            spread_line, total_line,
            pred_margin = round(nfl_predictor_line, 1), actual_margin,
            abs_err = round(abs_err, 1),
            pred_win_pct = round(pred_win_pct, 3),
            vegas_win_pct = round(vegas_win_pct, 3),
            edge_pct = round(pred_win_pct - vegas_win_pct, 3),
            cover_correct, ml_correct)

model_metrics <- scored %>% summarise(
  games = n(),
  mae   = round(mean(abs_err), 3),
  cover_acc = round(mean(cover_correct, na.rm = TRUE), 3),
  ml_acc    = round(mean(ml_correct, na.rm = TRUE), 3),
  brier     = round(mean(brier), 4)
)

# ---- bet card replay (settled) ----------------------------------------------
# Canonical engine: PORTFOLIO_DEFAULTS (min_ev = 0.05, calibrated) + the bundle's
# baked EV calibration — identical policy to the pre-week card, so what we grade
# is exactly the set of bets the card showed.
BANKROLL <- as.numeric(Sys.getenv("BANKROLL", "1000"))  # 1 unit = 1% = $10
PARAMS   <- PORTFOLIO_DEFAULTS
ev_cal <- ev_calibration_from_bundle(bundle)

cands <- build_bet_candidates(scored)
bets <- filter_and_size_bets(cands, BANKROLL, PARAMS, ev_cal)
if (nrow(bets) > 0) {
  bets <- bets %>% mutate(pnl = pnl_dollars(correct, odds, stake))
}
bet_summary <- if (nrow(bets) == 0) {
  tibble(n_bets = 0, staked = 0, pnl = 0, roi = NA_real_, wins = 0, losses = 0, pushes = 0)
} else {
  bets %>% summarise(
    n_bets = n(), staked = round(sum(stake)), pnl = round(sum(pnl)),
    roi = round(sum(pnl) / sum(stake), 3),
    wins = sum(correct == 1, na.rm = TRUE),
    losses = sum(correct == 0, na.rm = TRUE),
    pushes = sum(is.na(correct)))
}

bets_out <- if (nrow(bets) == 0) tibble() else bets %>%
  transmute(game = sprintf("%s @ %s", away_team, home_team),
            bet = sprintf("%s %s", bet_team, str_remove(bet_type, "^(home|away)_")),
            odds = round(odds), ev = round(ev, 3), stake = round(stake),
            units = round(stake / (BANKROLL / 100), 1),
            result = case_when(correct == 1 ~ "WIN", correct == 0 ~ "LOSS", TRUE ~ "PUSH"),
            pnl = round(pnl))

# ---- write CSV + web JSON ---------------------------------------------------
csv_path <- file.path(DATA_DIR, sprintf("graded_%d_wk%02d.csv", season, week))
readr::write_csv(scored %>% select(game_id, home_team, away_team, spread_line,
  total_line, nfl_predictor_line, actual_margin, abs_err, pred_win_pct,
  cover_correct, ml_correct, brier), csv_path)
cat(sprintf("  wrote %s\n", basename(csv_path)))

write_web_json(season, week, "model_scorecard",
  list(season = season, week = week, source = grade_source,
       metrics = model_metrics,
       bet_summary = bet_summary, games = game_summary, bets = bets_out))
update_web_index(season, week, "post")

cat("\nModel metrics:\n"); print(as.data.frame(model_metrics), row.names = FALSE)
cat("\nBet card:\n"); print(as.data.frame(bet_summary), row.names = FALSE)
