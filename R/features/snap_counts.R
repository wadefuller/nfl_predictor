# Per-team snap-count features from the most recent prior game. Captures
# depth-chart truth Vegas may not have priced — e.g. WR1 only got 38%
# snaps last week (returning from injury sooner than expected) or OL
# continuity broke mid-game.
# Lag-shifted one game per team. Pre-2012 returns empty tibble.

lag_snap_features <- function(per_game, games) {
  # Join to the schedule *before* shifting. Otherwise an upcoming game has no
  # snap-count row of its own and can never receive the previous game's snaps.
  schedule <- dplyr::bind_rows(
    games %>% transmute(season, week, team = home_team),
    games %>% transmute(season, week, team = away_team)
  ) %>% distinct(season, week, team)
  schedule %>%
    left_join(per_game, by = c("season", "week", "team")) %>%
    arrange(season, team, week) %>%
    group_by(season, team) %>%
    mutate(across(starts_with("snap_"), ~lag(.))) %>%
    ungroup() %>%
    rename_with(~ paste0(.x, "_lastwk"), starts_with("snap_"))
}

extract_snap_features <- function(seasons, games) {
  sc_max     <- min(max(seasons), nflreadr::most_recent_season())
  sc_seasons <- intersect(seasons, 2012:sc_max)
  if (length(sc_seasons) == 0) return(tibble())

  ol_pos  <- c("T","G","C","OL","LT","RT","LG","RG","OT","OG")
  sec_pos <- c("CB","S","FS","SS","DB")

  sc <- expect_frame(nflreadr::load_snap_counts(sc_seasons),
                     cols = c("game_type", "team", "week", "season", "position",
                             "offense_pct", "defense_pct"),
                     label = sprintf("load_snap_counts(%d-%d)", min(sc_seasons), max(sc_seasons))) %>%
    filter(game_type == "REG", !is.na(team), !is.na(week)) %>%
    normalize_team_names() %>%
    transmute(season, week, team, position,
              offense_pct = coalesce(offense_pct, 0),
              defense_pct = coalesce(defense_pct, 0))

  per_game <- sc %>%
    group_by(season, week, team) %>%
    summarise(
      snap_off_top11_avg_pct = mean(head(sort(offense_pct, decreasing = TRUE), 11), na.rm = TRUE),
      snap_def_top11_avg_pct = mean(head(sort(defense_pct, decreasing = TRUE), 11), na.rm = TRUE),
      snap_qb_pct  = max(c(0, offense_pct[position == "QB"]), na.rm = TRUE),
      snap_rb1_pct = max(c(0, offense_pct[position == "RB"]), na.rm = TRUE),
      snap_wr1_pct = max(c(0, offense_pct[position == "WR"]), na.rm = TRUE),
      snap_te1_pct = max(c(0, offense_pct[position == "TE"]), na.rm = TRUE),
      snap_ol_starters_high  = sum(offense_pct >= 0.85 & position %in% ol_pos,  na.rm = TRUE),
      snap_sec_starters_high = sum(defense_pct >= 0.80 & position %in% sec_pos, na.rm = TRUE),
      .groups = "drop"
    )

  lag_snap_features(per_game, games %>% filter(season %in% sc_seasons))
}

SNAP_COUNTS_MODULE <- feature_module(
  name        = "snap_counts",
  description = "Prior-game snap-count features: top-11 avg snap%, skill-position workload, OL/secondary depth-chart anchors",
  level       = "team_week_post_pivot",
  needs       = c("games"),
  enabled     = function() TRUE,
  build       = function(seasons, deps) extract_snap_features(seasons, deps$games)
)
