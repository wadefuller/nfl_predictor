# QB history uses the primary passer (most dropbacks) after each completed
# game. A game's own primary passer is NOT a valid pregame feature: a surprise
# scratch or in-game replacement cannot be learned from that game's PBP.

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

# Historical pregame proxy: the last observed primary passer for this team.
# There is no archived pregame depth chart for old seasons. In particular,
# never join the current game's postgame primary passer to its own predictors.
historical_pregame_qb_ids <- function(game_split, actual) {
  game_split %>%
    distinct(game_id, season, week, gameday, team) %>%
    left_join(actual %>% select(game_id, team, actual_id = starter_id),
              by = c("game_id", "team")) %>%
    arrange(team, season, week, gameday, game_id) %>%
    group_by(team) %>%
    tidyr::fill(actual_id, .direction = "down") %>%
    mutate(expected_id = lag(actual_id)) %>%
    ungroup() %>%
    select(game_id, season, week, team, expected_id)
}

historical_pregame_qb_features <- function(game_split, plys) {
  actual <- identify_starters(plys) %>% arrange(season, week, game_id)
  schedule <- historical_pregame_qb_ids(game_split, actual)

  by_player <- split(actual, actual$starter_id)
  by_team <- split(actual, actual$team)
  feature_names <- c("starter_qb_epa_r4", "starter_qb_epa_r8",
                     "starter_pass_epa_r4", "starter_pass_epa_r8",
                     "starter_career_games", "starter_games_team_season",
                     "new_starter")
  values <- t(vapply(seq_len(nrow(schedule)), function(i) {
    row <- schedule[i, ]
    id <- row$expected_id
    if (is.na(id) || !nzchar(id))
      return(stats::setNames(rep(NA_real_, length(feature_names)), feature_names))
    prior <- function(x) {
      if (is.null(x)) return(actual[0, ])
      x[x$season < row$season | (x$season == row$season & x$week < row$week), , drop = FALSE]
    }
    p <- prior(by_player[[id]])
    t <- prior(by_team[[row$team]])
    avg <- function(col, n) if (!nrow(p)) NA_real_ else mean(tail(p[[col]], n))
    c(starter_qb_epa_r4 = avg("starter_qb_epa", 4),
      starter_qb_epa_r8 = avg("starter_qb_epa", 8),
      starter_pass_epa_r4 = avg("starter_pass_epa", 4),
      starter_pass_epa_r8 = avg("starter_pass_epa", 8),
      starter_career_games = nrow(p),
      starter_games_team_season = sum(p$team == row$team & p$season == row$season),
      new_starter = if (!nrow(t)) 0 else as.integer(tail(t$starter_id, 1) != id))
  }, stats::setNames(numeric(length(feature_names)), feature_names)))
  bind_cols(schedule %>% select(game_id, season, week, team),
            tibble::as_tibble(values))
}

QB_STARTER_MODULE <- feature_module(
  name        = "qb_starter",
  description = "Pregame QB proxy from the team's last observed primary passer and that player's earlier starts",
  level       = "team_game_pre_pivot",
  needs       = c("pbp", "game_split"),
  build       = function(seasons, deps)
    historical_pregame_qb_features(deps$game_split, deps$pbp)
)
