# Per-team coaching-change features derived from schedule's home_coach /
# away_coach. Captures markets misprice early-season transitions
# (first-year HCs adjust to new systems) and in-season interim hires.

extract_coaching_features <- function(games) {
  hc_long <- bind_rows(
    games %>% transmute(season, week, gameday, team = home_team, coach = home_coach),
    games %>% transmute(season, week, gameday, team = away_team, coach = away_coach)
  ) %>%
    filter(!is.na(coach), !is.na(team)) %>%
    arrange(team, gameday, season, week)

  season_first <- hc_long %>%
    group_by(season, team) %>% arrange(week) %>% slice(1) %>% ungroup() %>%
    select(season, team, season_first_coach = coach)

  season_last <- hc_long %>%
    group_by(season, team) %>% arrange(desc(week)) %>% slice(1) %>% ungroup() %>%
    select(season, team, season_last_coach = coach)

  new_hc <- season_first %>%
    left_join(season_last %>% mutate(season = season + 1) %>%
                rename(prior_last_coach = season_last_coach),
              by = c("team", "season")) %>%
    mutate(coach_new_hc = as.integer(
      !is.na(prior_last_coach) & season_first_coach != prior_last_coach
    )) %>%
    select(season, team, season_first_coach, coach_new_hc)

  feats <- hc_long %>%
    left_join(new_hc, by = c("season", "team")) %>%
    mutate(coach_is_interim = as.integer(!is.na(season_first_coach) &
                                          coach != season_first_coach))

  team_coach_runs <- feats %>%
    distinct(team, coach, season) %>%
    arrange(team, coach, season) %>%
    group_by(team, coach) %>%
    mutate(coach_team_seasons = row_number()) %>%
    ungroup()

  feats <- feats %>%
    left_join(team_coach_runs, by = c("team", "coach", "season")) %>%
    arrange(team, coach, gameday, season, week) %>%
    group_by(team, coach) %>%
    mutate(coach_team_games = row_number()) %>%
    ungroup()

  feats %>%
    transmute(
      season, week, team,
      coach_new_hc       = coalesce(coach_new_hc, 0L),
      coach_is_interim   = coalesce(coach_is_interim, 0L),
      coach_team_games   = coach_team_games,
      coach_team_seasons = coach_team_seasons
    )
}

COACHING_MODULE <- feature_module(
  name        = "coaching",
  description = "Coaching change indicators: new HC flag, interim flag, team-coach tenure (games + seasons)",
  level       = "team_week_post_pivot",
  needs       = c("games"),
  build       = function(seasons, deps) extract_coaching_features(deps$games)
)
