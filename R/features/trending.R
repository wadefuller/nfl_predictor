# Per-team rolling outcomes: 16-game season averages + 4-game trends of
# win/result/points/cover/etc. Computed from the long-format game_split.

generate_trending_features <- function(game_split) {
  game_split %>%
    arrange(season, team, week) %>%
    group_by(season, team) %>%
    mutate(
      across(
        all_of(METRICS),
        ~zoo::rollapply(., list(-(16:1)), mean, align = "right",
                         fill = NA, partial = TRUE),
        .names = "{.col}_season_avg"
      ),
      across(
        all_of(METRICS),
        ~zoo::rollapply(., list(-(4:1)), mean, align = "right",
                         fill = NA, partial = TRUE),
        .names = "{.col}_4gm_trend"
      )
    ) %>%
    ungroup() %>%
    select(game_id, season, week, team,
           ends_with("_season_avg"), ends_with("_4gm_trend"))
}

TRENDING_MODULE <- feature_module(
  name        = "trending",
  description = "16-game season averages + 4-game trends of per-team outcomes",
  level       = "team_game_pre_pivot",
  needs       = c("game_split"),
  build       = function(seasons, deps) generate_trending_features(deps$game_split)
)
