# Upstream data loaders — games (habitatring), PBP, contracts/rosters.
# These are shared by multiple feature modules; build_features.R loads
# them once and passes via `deps` to the registry.
#
# Requires:
#   - team_normalize.R (normalize_team_names + TEAM_REPLACEMENTS)
#   - SEASONS module-level vector (build_features.R defines it)

suppressPackageStartupMessages(library(nflreadr))

load_game_data <- function(seasons = SEASONS) {
  message("Loading game data from habitatring.com...")
  readRDS(url("http://www.habitatring.com/games.rds")) %>%
    filter(season >= min(seasons)) %>%
    normalize_team_names()
}

load_pbp_data <- function(seasons = SEASONS) {
  pbp_max     <- min(max(seasons), nflreadr::most_recent_season())
  pbp_seasons <- min(seasons):pbp_max
  message(sprintf("Loading PBP for seasons %d-%d (this can take a few minutes)...",
                  min(pbp_seasons), max(pbp_seasons)))
  pbp <- nflreadr::load_pbp(seasons = pbp_seasons)
  message(sprintf("  Loaded %s plays.", format(nrow(pbp), big.mark = ",")))

  pbp %>%
    filter(
      !is.na(epa), !is.na(ep), !is.na(posteam), !is.na(down),
      play_type %in% c("pass", "run") | penalty == 1,
      qb_kneel != 1
    ) %>%
    normalize_team_names()
}

load_player_cap_by_season <- function(seasons) {
  con <- nflreadr::load_contracts() %>%
    filter(!is.na(gsis_id), !is.na(year_signed), !is.na(years))

  map_dfr(seasons, function(s) {
    con %>%
      filter(year_signed <= s, year_signed + years > s) %>%
      group_by(gsis_id) %>%
      slice_max(year_signed, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(gsis_id, season = s, apy, apy_cap_pct)
  })
}

build_team_cap_aggregates <- function(seasons, player_cap) {
  ros_seasons <- intersect(seasons, 2002:min(max(seasons), nflreadr::most_recent_season()))
  ros <- nflreadr::load_rosters(ros_seasons) %>%
    distinct(season, gsis_id, team)

  player_cap %>%
    inner_join(ros, by = c("season", "gsis_id")) %>%
    group_by(season, team) %>%
    summarise(
      team_total_apy     = sum(apy, na.rm = TRUE),
      top5_apy_threshold = ifelse(n() >= 5,
                                  sort(apy, decreasing = TRUE)[5],
                                  min(apy, na.rm = TRUE)),
      .groups = "drop"
    )
}
