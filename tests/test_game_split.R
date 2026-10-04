suppressPackageStartupMessages({ library(dplyr); library(zoo) })
source("R/infra/game_split.R")
source("R/features/_module_base.R")
source("R/features/trending.R")

games <- tibble::tibble(
  game_id = c("2026_01_A_B", "2026_02_B_C"),
  gameday = c("2026-09-10", "2026-09-17"), season = 2026L,
  week = c(1L, 2L), weekday = "Thursday", gametime = "20:15",
  spread_line = c(3, -2), total_line = c(44, 45),
  home_spread_odds = -110, away_spread_odds = -110,
  home_moneyline = c(-150, 120), away_moneyline = c(130, -140),
  div_game = FALSE, result = c(-4, 7),
  home_team = c("B", "C"), away_team = c("A", "B"),
  home_score = c(17, 24), away_score = c(21, 17),
  total = c(38, 41), home_rest = c(7, 6), away_rest = c(9, 8)
)
long <- split_home_away(games)
g1 <- long %>% filter(game_id == "2026_01_A_B")
home <- g1 %>% filter(team == "B")
away <- g1 %>% filter(team == "A")
stopifnot(
  home$result == -4, away$result == 4,
  home$win == 0L, away$win == 1L,
  home$spread_line == -3, away$spread_line == 3,
  home$spread_adjusted_result == -7, away$spread_adjusted_result == 7,
  home$spread_cover == 0L, away$spread_cover == 1L,
  home$team_rest == 7, away$team_rest == 9,
  home$opponent_rest == 9, away$opponent_rest == 7,
  home$rest_diff == -2, away$rest_diff == 2,
  home$points_for == 17, away$points_for == 21
)
# Team B lost at home in Week 1 and must retain that loss when it is away
# in Week 2. This catches the original bug even when the current game pivots
# correctly back to the home team.
trend <- generate_trending_features(long)
prior_b <- trend %>% filter(game_id == "2026_02_B_C", team == "B")
stopifnot(prior_b$win_season_avg == 0,
          prior_b$result_season_avg == -4,
          prior_b$spread_cover_season_avg == 0)
cat("PASS: home/away results, covers, rest, and lagged team form use team perspective\n")
