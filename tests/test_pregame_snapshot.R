suppressPackageStartupMessages({ library(dplyr); library(readr) })
source("R/lib/pregame_snapshot.R")
ev_calibration_from_bundle <- function(bundle) list(intercept = 0.01, slope = 0.4)

root <- tempfile("pregame-test-")
dir.create(root)
game <- tibble(
  game_id = "2026_05_TB_DAL", gameday = "2026-10-08", gametime = "20:15",
  home_spread_odds = -110, away_spread_odds = -110,
  impl_prob_home = .75, impl_prob_away = .25
)
pred <- tibble(
  game_id = game$game_id, season = 2026L, week = 5L,
  home_team = "DAL", away_team = "TB", spread_line = 8.5, total_line = 47.5,
  home_moneyline = -300, away_moneyline = 250,
  nfl_predictor_line = 8.1, line_diff = -.4, pred_win_pct = .78,
  cover_prob_home = .49, availability_ok = TRUE,
  availability_reason = NA_character_, availability_checked_at = "2026-10-07T20:00:00Z"
)
first <- as.POSIXct("2026-10-08 00:00:00", tz = "UTC")
stopifnot(record_pregame_snapshot(pred, game, list(), root, first) == 1L)
stopifnot(record_pregame_snapshot(pred, game, list(), root, first) == 0L)
pred$nfl_predictor_line <- 7.5
stopifnot(record_pregame_snapshot(pred, game, list(), root, first + 3600) == 1L)
stopifnot(record_pregame_snapshot(pred, game, list(), root, first + 90000) == 0L)
frozen <- read_frozen_pregame(root, 2026, 5, game$game_id)
stopifnot(nrow(frozen) == 1L, frozen$nfl_predictor_line == 7.5,
          frozen$snapshot_epoch < frozen$kickoff_epoch)
missing <- try(read_frozen_pregame(root, 2026, 5, "other-game"), silent = TRUE)
stopifnot(inherits(missing, "try-error"))
cat("pregame snapshot contract passed\n")
