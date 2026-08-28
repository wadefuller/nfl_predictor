# Split per-game rows from `games` into two per-team rows (home + away).
# This is the canonical "long-format" frame on top of which all per-team
# pre-pivot feature modules layer their rolling stats. Wave-shaped: in =
# 1 game-row → out = 2 team-rows.

COMMON_VARS <- c(
  "game_id", "gameday", "season", "week", "weekday",
  "gametime", "spread_line", "total_line",
  "home_spread_odds", "away_spread_odds",
  "home_moneyline", "away_moneyline", "div_game", "result"
)

# Per-team outcomes the trending module computes rolling means over.
METRICS <- c(
  "win", "result", "points_for", "spread_cover",
  "spread_adjusted_result", "impl_win_prob", "game_info_gain",
  "win_prob_captured"
)

split_home_away <- function(games) {
  home_games <- games %>%
    transmute(
      !!!syms(COMMON_VARS),
      team               = home_team,
      opposing_team      = away_team,
      points_for         = home_score,
      points_against     = away_score,
      total_points_scored = total,
      pct_points_scored  = points_for / total_points_scored,
      team_rest          = home_rest,
      opponent_rest      = away_rest,
      spread_line        = spread_line * -1,
      impl_prob_home     = if_else(sign(home_moneyline) == -1,
                                   (-1 * home_moneyline / ((-1 * home_moneyline) + 100)),
                                   (100 / (home_moneyline + 100))),
      impl_prob_away     = if_else(sign(away_moneyline) == -1,
                                   (-1 * away_moneyline / ((-1 * away_moneyline) + 100)),
                                   (100 / (away_moneyline + 100))),
      home               = 1
    )

  away_games <- games %>%
    transmute(
      !!!syms(COMMON_VARS),
      team               = away_team,
      opposing_team      = home_team,
      points_for         = away_score,
      points_against     = home_score,
      total_points_scored = total,
      pct_points_scored  = points_for / total_points_scored,
      team_rest          = home_rest,
      opponent_rest      = away_rest,
      spread_line        = spread_line * -1,
      impl_prob_home     = if_else(sign(home_moneyline) == -1,
                                   (-1 * home_moneyline / ((-1 * home_moneyline) + 100)),
                                   (100 / (home_moneyline + 100))),
      impl_prob_away     = if_else(sign(away_moneyline) == -1,
                                   (-1 * away_moneyline / ((-1 * away_moneyline) + 100)),
                                   (100 / (away_moneyline + 100))),
      home               = 0
    )

  bind_rows(home_games, away_games) %>%
    mutate(
      home                  = as.factor(home),
      win                   = as.integer(result > 0),
      spread_adjusted_result = spread_line + result,
      spread_cover          = as.integer(spread_adjusted_result > 0),
      impl_win_prob         = if_else(home == 1, impl_prob_home, impl_prob_away),
      rest_diff             = team_rest - opponent_rest,
      game_info_gain        = if_else(win == 1,
                                      pmax(1 - 2 * impl_win_prob^3, 0, na.rm = TRUE),
                                      pmax(1 - 2 * impl_win_prob^3 - 1, -1, na.rm = TRUE)),
      win_prob_captured     = if_else(win == 1, impl_win_prob, -impl_win_prob)
    )
}
