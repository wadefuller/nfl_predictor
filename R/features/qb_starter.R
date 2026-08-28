# QB starter features: identify the starting QB per team-game (most
# dropbacks), then compute player-level rolling EPA + team-level stability.

identify_starters <- function(plys) {
  plys %>%
    filter(qb_dropback == 1, !is.na(passer_player_id)) %>%
    group_by(game_id, season, week, posteam, passer_player_id) %>%
    summarise(
      dropbacks         = n(),
      starter_qb_epa    = mean(qb_epa, na.rm = TRUE),
      starter_pass_epa  = mean(epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(game_id, season, week, posteam) %>%
    slice_max(dropbacks, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(team = posteam, starter_id = passer_player_id)
}

# Player-level rolling QB EPA: follows the QB across teams. Excludes
# current game via lag-1 alignment.
roll_qb_player <- function(starters) {
  starters %>%
    group_by(starter_id) %>%
    arrange(starter_id, season, week) %>%
    mutate(
      starter_qb_epa_r4   = zoo::rollapply(starter_qb_epa,   list(-(4:1)), mean, align = "right", fill = NA, partial = TRUE),
      starter_qb_epa_r8   = zoo::rollapply(starter_qb_epa,   list(-(8:1)), mean, align = "right", fill = NA, partial = TRUE),
      starter_pass_epa_r4 = zoo::rollapply(starter_pass_epa, list(-(4:1)), mean, align = "right", fill = NA, partial = TRUE),
      starter_pass_epa_r8 = zoo::rollapply(starter_pass_epa, list(-(8:1)), mean, align = "right", fill = NA, partial = TRUE),
      starter_career_games = row_number() - 1L
    ) %>%
    ungroup() %>%
    select(-starter_qb_epa, -starter_pass_epa, -dropbacks)
}

# Team-level QB stability: did the starter change vs last game?
qb_team_stability <- function(starters_w_rolling) {
  starters_w_rolling %>%
    group_by(team, season, starter_id) %>%
    arrange(team, season, starter_id, week) %>%
    mutate(starter_games_team_season = row_number() - 1L) %>%
    ungroup() %>%
    group_by(team) %>%
    arrange(team, season, week) %>%
    mutate(
      prior_starter = lag(starter_id),
      new_starter   = if_else(is.na(prior_starter), 0L,
                              as.integer(starter_id != prior_starter))
    ) %>%
    ungroup() %>%
    select(-prior_starter)
}

QB_STARTER_MODULE <- feature_module(
  name        = "qb_starter",
  description = "QB starter rolling features: player-level EPA r4/r8, career games, team-season games, new-starter flag",
  level       = "team_game_pre_pivot",
  needs       = c("pbp"),
  build       = function(seasons, deps) {
    deps$pbp %>%
      identify_starters() %>%
      roll_qb_player() %>%
      qb_team_stability() %>%
      select(-starter_id)
  }
)
