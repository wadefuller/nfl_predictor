suppressPackageStartupMessages({ library(dplyr); library(readr) })
source("R/lib/pregame_snapshot.R")
source("R/lib/historical_export.R")

root <- tempfile("historical-export-")
dir.create(root)
dir.create(file.path(root, "pregame_snapshots"))
games <- tibble(game_id = c("old", "frozen", "excluded", "missing", "future", "started"),
  season = c(2025L, rep(2026L, 5)), week = 5L,
  gameday = c(rep("2026-10-01", 4), "2026-10-20", "2026-10-01"),
  gametime = "13:00", home_result = c(3, -2, 4, 7, NA, NA),
  spread_line = 99, total_line = 45, home_moneyline = -110,
  away_moneyline = -110, home_spread_odds = -110, away_spread_odds = -110,
  impl_prob_home = .5, impl_prob_away = .5)
write_csv(tibble(game_id = "old", nfl_predictor_line = 2, pred_win_pct = .6,
                 spread_line = 1), file.path(root, "walk_forward_predictions.csv"))
ledger <- tibble(game_id = c("frozen", "frozen", "frozen", "excluded"),
  snapshot_epoch = c(10, 20, 40, 20), kickoff_epoch = 30,
  nfl_predictor_line = c(-1, -3, 90, 4), pred_win_pct = c(.4, .3, .99, .7),
  spread_line = c(-2, -2.5, 90, 1), home_moneyline = -150,
  provenance = c(NA_character_, NA_character_, NA_character_, "excluded"))
write_csv(ledger, file.path(root, "pregame_snapshots", "2026-wk05.csv"))
now <- as.POSIXct("2026-10-13 00:00:00", tz = "UTC")
live <- function(value) function(rows) {
  stopifnot(identical(rows$game_id, "future"))
  tibble(game_id = rows$game_id, nfl_predictor_line = value, pred_win_pct = .8,
         availability_ok = TRUE, qb_features_ready = TRUE)
}
a <- export_forecasts(games, 2026L, root, live(10), now)
b <- export_forecasts(games, 2026L, root, live(-10), now)
stopifnot(identical(a[1:4, ], b[1:4, ]),
  a$nfl_predictor_line[1] == 2, a$spread_line[1] == 1,
  a$nfl_predictor_line[2] == -3, a$pred_win_pct[2] == .3,
  a$spread_line[2] == -2.5, a$home_moneyline[2] == -150,
  identical(a$forecast_graded, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
  all(is.na(a$nfl_predictor_line[c(3, 4, 6)])),
  a$forecast_source[1] == "walk_forward", a$forecast_source[2] == "frozen_pregame",
  a$nfl_predictor_line[5] == 10, b$nfl_predictor_line[5] == -10,
  a$availability_ok[5], !a$availability_ok[1])
bad <- games
bad$game_id[1] <- "missing-historical"
stopifnot(inherits(try(export_forecasts(bad, 2026L, root, live(1), now),
                      silent = TRUE), "try-error"))
past <- export_forecasts(games[1:4, ], 2026L, root,
                        function(rows) stop("Must not call live model for completed games"), now)
stopifnot(identical(past$nfl_predictor_line, a$nfl_predictor_line[1:4]))
late_games <- games[2:3, ]
late_games$game_id <- c("approved-backfill", "unapproved-late")
late_games$week <- 4L
late_ledger <- tibble(game_id = late_games$game_id, snapshot_epoch = 40,
  kickoff_epoch = 30, nfl_predictor_line = c(1, 90), pred_win_pct = c(.6, .99),
  provenance = c("backfill_preview_archive", NA_character_))
write_csv(late_ledger, file.path(root, "pregame_snapshots", "2026-wk04.csv"))
late <- export_forecasts(late_games, 2026L, root,
                         function(rows) stop("No live prediction for settled backfills"), now)
stopifnot(late$forecast_source[1] == "pregame_backfill_archive",
  late$forecast_graded[1], late$nfl_predictor_line[1] == 1,
  late$forecast_source[2] == "uncovered", !late$forecast_graded[2],
  is.na(late$nfl_predictor_line[2]))
stopifnot(inherits(try(export_forecasts(games, 2026L, root,
  function(rows) tibble(game_id = character()), now), silent = TRUE), "try-error"))
cat("PASS: archived grading stable across live model changes; future-only inference; missing/excluded forecasts safe\n")
