# Pythagorean win expectation per team. Computed from cumulative points
# for/against (excluding the current game via lag).

generate_pythag_features <- function(f_game_split) {
  f_game_split %>%
    group_by(season, team) %>%
    arrange(season, team, week) %>%
    mutate(
      games_played       = row_number() - 1,
      cumsum_pts_for     = lag(cumsum(points_for)),
      cumsum_pts_against = lag(cumsum(points_against)),
      pythag_win_pct     = (cumsum_pts_for^2.37) /
                           ((cumsum_pts_for^2.37) + (cumsum_pts_against^2.37)),
      pythag_wins        = pythag_win_pct * games_played
    ) %>%
    ungroup() %>%
    select(game_id, season, week, team,
           games_played, pythag_win_pct, pythag_wins)
}

PYTHAG_MODULE <- feature_module(
  name        = "pythag",
  description = "Cumulative Pythagorean win expectation (Bill James exponent 2.37)",
  level       = "team_game_pre_pivot",
  needs       = c("game_split"),
  build       = function(seasons, deps) generate_pythag_features(deps$game_split)
)
