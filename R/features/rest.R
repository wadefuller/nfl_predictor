# Rest-pattern features. v1 design (simple binaries + rest_advantage + b2b
# flags). The v2 attempt 2026-05-25 added explicit interactions + prior-game
# state + cumulative counts — REJECTED: walk-forward MAE regressed
# 9.823→9.848, the targeted home-short-rest failure widened from +0.265 to
# +0.346, and only prior_margin (1 of 8 new features) survived XGB selection.
# Kept the simpler v1 since the rest signal is genuinely thin at this scale.
# If revisiting, focus on travel distance / timezone change instead.
#
# Per-team-week features:
#   rest_short_week   — rest < 7   (Thu game after Sun)
#   rest_extra_short  — rest < 5   (MNF → Thu pattern)
#   rest_off_bye      — rest >= 13 (post-bye-week boost)
#   rest_advantage    — own_rest - opp_rest, clamped ±7
#   rest_b2b_road     — prior game also away (consecutive road trip)
#   rest_b2b_home     — prior game also home

extract_rest_features <- function(games) {
  long <- bind_rows(
    games %>% transmute(season, week, gameday, team = home_team,
                         team_rest = home_rest, opp_rest = away_rest,
                         was_home  = 1L),
    games %>% transmute(season, week, gameday, team = away_team,
                         team_rest = away_rest, opp_rest = home_rest,
                         was_home  = 0L)
  ) %>% filter(!is.na(team))

  long <- long %>%
    arrange(team, gameday, season, week) %>%
    group_by(team) %>%
    mutate(prior_was_home = lag(was_home)) %>%
    ungroup()

  long %>%
    transmute(
      season, week, team,
      rest_short_week  = as.integer(!is.na(team_rest) & team_rest < 7),
      rest_extra_short = as.integer(!is.na(team_rest) & team_rest < 5),
      rest_off_bye     = as.integer(!is.na(team_rest) & team_rest >= 13),
      rest_advantage   = pmin(pmax(coalesce(team_rest - opp_rest, 0L), -7L), 7L),
      rest_b2b_road    = as.integer(!is.na(prior_was_home) & prior_was_home == 0L &
                                     was_home == 0L &
                                     !is.na(team_rest) & team_rest <= 14),
      rest_b2b_home    = as.integer(!is.na(prior_was_home) & prior_was_home == 1L &
                                     was_home == 1L &
                                     !is.na(team_rest) & team_rest <= 14)
    )
}

REST_MODULE <- feature_module(
  name        = "rest",
  description = "Rest patterns: short/extra-short weeks, off-bye, back-to-back home/road, rest advantage delta",
  level       = "team_week_post_pivot",
  needs       = c("games"),
  build       = function(seasons, deps) extract_rest_features(deps$games)
)
