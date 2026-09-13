# Next Gen Stats per-team features. Captures advanced tracking-derived
# signals beyond EPA: CPOE/time-to-throw/aggressiveness (passing), rush
# yards over expected (rushing), separation/YAC over expected (receiving).
#
# DISABLED by default (2026-05-22 ablation: 3.5% gain but +0.0008 Brier
# regression — model is feature-saturated). Enable via NGS_ENABLED=1.

extract_ngs_features <- function(seasons) {
  ngs_max     <- min(max(seasons), nflreadr::most_recent_season())
  ngs_seasons <- intersect(seasons, 2016:ngs_max)
  if (length(ngs_seasons) == 0) return(tibble())

  ngs_cols <- c("season", "week", "season_type", "team_abbr")
  ngs_pass <- expect_frame(nflreadr::load_nextgen_stats(seasons = ngs_seasons, stat_type = "passing"),
                           cols = c(ngs_cols, "attempts"), label = "load_nextgen_stats(passing)") %>%
    filter(season_type == "REG", week > 0, !is.na(team_abbr)) %>%
    mutate(team_abbr = stringr::str_replace_all(team_abbr, TEAM_REPLACEMENTS)) %>%
    group_by(season, week, team_abbr) %>%
    arrange(desc(attempts)) %>% slice(1) %>% ungroup() %>%
    transmute(season, week, team = team_abbr,
              ngs_qb_cpoe          = completion_percentage_above_expectation,
              ngs_qb_time_to_throw = avg_time_to_throw,
              ngs_qb_aggressiveness = aggressiveness,
              ngs_qb_air_yards     = avg_intended_air_yards)

  ngs_rush <- expect_frame(nflreadr::load_nextgen_stats(seasons = ngs_seasons, stat_type = "rushing"),
                           cols = c(ngs_cols, "rush_attempts"), label = "load_nextgen_stats(rushing)") %>%
    filter(season_type == "REG", week > 0, !is.na(team_abbr)) %>%
    mutate(team_abbr = stringr::str_replace_all(team_abbr, TEAM_REPLACEMENTS)) %>%
    group_by(season, week, team_abbr) %>%
    arrange(desc(rush_attempts)) %>% slice(1) %>% ungroup() %>%
    transmute(season, week, team = team_abbr,
              ngs_rb_ryoe_per_att = rush_yards_over_expected_per_att,
              ngs_rb_efficiency   = efficiency)

  ngs_rec <- expect_frame(nflreadr::load_nextgen_stats(seasons = ngs_seasons, stat_type = "receiving"),
                          cols = c(ngs_cols, "targets"), label = "load_nextgen_stats(receiving)") %>%
    filter(season_type == "REG", week > 0, !is.na(team_abbr)) %>%
    mutate(team_abbr = stringr::str_replace_all(team_abbr, TEAM_REPLACEMENTS)) %>%
    group_by(season, week, team_abbr) %>%
    arrange(desc(targets)) %>% slice(1) %>% ungroup() %>%
    transmute(season, week, team = team_abbr,
              ngs_wr_separation = avg_separation,
              ngs_wr_yac_oe     = avg_yac_above_expectation)

  ngs_pass %>%
    full_join(ngs_rush, by = c("season", "week", "team")) %>%
    full_join(ngs_rec,  by = c("season", "week", "team")) %>%
    arrange(season, team, week) %>%
    group_by(season, team) %>%
    mutate(across(starts_with("ngs_"), ~lag(.))) %>%
    ungroup() %>%
    rename_with(~ paste0(.x, "_lastwk"), starts_with("ngs_"))
}

NGS_MODULE <- feature_module(
  name        = "ngs",
  description = "Next Gen Stats: CPOE, time-to-throw, aggressiveness, RYOE, separation, YAC-OE (prior-game)",
  level       = "team_week_post_pivot",
  enabled     = function() Sys.getenv("NGS_ENABLED", "0") == "1",
  needs       = character(0),
  build       = function(seasons, deps) extract_ngs_features(seasons)
)
