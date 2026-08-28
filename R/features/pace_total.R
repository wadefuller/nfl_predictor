# Pace + total environment per (game_id, team) + rolling r4/r12/r16.
#
# Goal: give the ensemble explicit signal about LOW-TOTAL / SLOW-PACE
# contexts so it can dampen magnitude predictions in close-game-prone
# environments. Targets the structural close-game overshoot (+0.225 MAE
# on 0-3 actual margin) documented in docs/EXPERIMENTS_RULED_OUT.md.
#
# What's new (vs trending module which already has points_for):
#   - team_points_against:  defensive scoring rate (rolling) — NOT in trending
#   - team_total_env:       combined for+against per game (rolling) — expected
#                           total environment; correlated with game pace
#   - team_off_plays:       offensive plays per game (pace proxy from PBP)
#
# All rolling versions are leakage-safe (zoo::rollapply offset list(-(k:1))
# excludes the current game). The current-game cols are flagged via
# leakage_vars and dropped at recipe stage.

generate_pace_total <- function(game_split, plys) {
  # Per-game per-team scoring environment (from game_split).
  env <- game_split %>%
    transmute(game_id, season, week, team,
              team_points_against = points_against,
              team_total_env      = points_for + points_against)

  # Per-game offensive plays (pace proxy). PBP is pre-filtered to
  # pass/run/penalty (see R/infra/data_loaders.R) — exactly the standard
  # pace denominator.
  plays <- plys %>%
    filter(!is.na(posteam)) %>%
    group_by(game_id, season, week, posteam) %>%
    summarise(team_off_plays = dplyr::n(), .groups = "drop") %>%
    rename(team = posteam)

  env %>% left_join(plays, by = c("game_id", "season", "week", "team"))
}

PACE_TOTAL_MODULE <- feature_module(
  name        = "pace_total",
  description = "Pace + total environment (rolling): team_points_against, team_total_env, team_off_plays",
  level       = "team_game_pre_pivot",
  needs       = c("game_split", "pbp"),
  build       = function(seasons, deps) {
    raw <- generate_pace_total(deps$game_split, deps$pbp)
    roll_epa(raw, c("team_points_against", "team_total_env", "team_off_plays"))
  },
  leakage_vars = c("team_points_against", "team_total_env", "team_off_plays")
)
