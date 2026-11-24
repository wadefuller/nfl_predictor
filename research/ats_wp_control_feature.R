# Build a leak-safe, team-level candidate feature from live play-by-play WP.
#
# For each completed regular-season game, integrate the team's live WP over
# regulation game-clock seconds and report the share of those seconds with
# WP > 50%. Then expose each team's mean share across its trailing four prior
# games (available history may be shorter early in the data).
#
# This is research-only: the output lives under data/ats_experiments and does
# not modify the production feature panel or model bundle.

suppressPackageStartupMessages({ library(tidyverse) })
source("../shared/pbp.R")

TRAILING_N <- as.integer(Sys.getenv("ATS_WP_CONTROL_TRAILING_N", "4"))
if (is.na(TRAILING_N) || TRAILING_N < 1L) stop("trailing N must be a positive integer")

base <- readr::read_csv("data/game_w_features.csv", show_col_types = FALSE)
seasons <- sort(unique(as.integer(base$season)))
inventory <- pbp_inventory()
pbp_seasons <- intersect(seasons, inventory$season)

# nflfastR's regulation game_seconds_remaining runs continuously from 3600 to
# 0. We intentionally use qtr <= 4: OT clock representations do not share the
# same continuous regulation scale, so this feature is explicitly regulation
# game-clock control, not elapsed wall-clock time.
pbp <- read_pbp(
  pbp_seasons,
  cols = c("season_type", "play_id", "qtr", "game_seconds_remaining",
           "home_team", "away_team", "home_wp", "away_wp")
) %>%
  filter(season_type == "REG", qtr <= 4,
         between(game_seconds_remaining, 0, 3600),
         is.finite(home_wp), is.finite(away_wp)) %>%
  # Multiple events can share a game-clock second. Retain the last recorded
  # WP state at that second, then assign it to the interval until the next
  # distinct remaining-clock value.
  arrange(game_id, desc(game_seconds_remaining), play_id) %>%
  group_by(game_id, game_seconds_remaining) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(game_id, desc(game_seconds_remaining), play_id) %>%
  group_by(game_id) %>%
  mutate(
    next_game_seconds_remaining = lead(game_seconds_remaining),
    interval_seconds = pmax(0, game_seconds_remaining -
                              coalesce(next_game_seconds_remaining,
                                       game_seconds_remaining))
  ) %>%
  ungroup()

game_control <- pbp %>%
  group_by(game_id) %>%
  summarise(
    regulation_seconds_observed = sum(interval_seconds, na.rm = TRUE),
    home_control_share_game = if_else(
      regulation_seconds_observed > 0,
      sum(interval_seconds * (home_wp > 0.5), na.rm = TRUE) /
        regulation_seconds_observed,
      NA_real_
    ),
    away_control_share_game = if_else(
      regulation_seconds_observed > 0,
      sum(interval_seconds * (away_wp > 0.5), na.rm = TRUE) /
        regulation_seconds_observed,
      NA_real_
    ),
    home_team = first(home_team),
    away_team = first(away_team),
    .groups = "drop"
  )

team_game_control <- bind_rows(
  game_control %>% transmute(game_id, team = home_team,
                             control_share_game = home_control_share_game),
  game_control %>% transmute(game_id, team = away_team,
                             control_share_game = away_control_share_game)
) %>%
  left_join(base %>% distinct(game_id, season, week, gameday), by = "game_id") %>%
  arrange(team, season, week, gameday, game_id) %>%
  group_by(team) %>%
  mutate(
    control_share_prior_n = vapply(seq_len(n()), function(i) {
      if (i == 1L) return(NA_real_)
      prior <- control_share_game[max(1L, i - TRAILING_N):(i - 1L)]
      if (all(!is.finite(prior))) NA_real_ else mean(prior, na.rm = TRUE)
    }, numeric(1)),
    control_games_prior_n = vapply(seq_len(n()), function(i) {
      if (i == 1L) return(0L)
      prior <- control_share_game[max(1L, i - TRAILING_N):(i - 1L)]
      as.integer(sum(is.finite(prior)))
    }, integer(1))
  ) %>%
  ungroup()

home_features <- base %>% distinct(game_id, home_team, away_team) %>%
  left_join(team_game_control %>%
              select(game_id, team, control_share_prior_n, control_games_prior_n),
            by = c("game_id", "home_team" = "team")) %>%
  rename(control_share_prior_n_home = control_share_prior_n,
         control_games_prior_n_home = control_games_prior_n)
away_features <- base %>% distinct(game_id, home_team, away_team) %>%
  left_join(team_game_control %>%
              select(game_id, team, control_share_prior_n, control_games_prior_n),
            by = c("game_id", "away_team" = "team")) %>%
  rename(control_share_prior_n_away = control_share_prior_n,
         control_games_prior_n_away = control_games_prior_n)

features <- home_features %>%
  left_join(away_features %>% select(game_id, control_share_prior_n_away,
                                     control_games_prior_n_away), by = "game_id") %>%
  transmute(
    game_id,
    control_share_prior_n_home,
    control_share_prior_n_away,
    control_share_prior_n_diff = control_share_prior_n_home -
      control_share_prior_n_away,
    control_games_prior_n_home,
    control_games_prior_n_away
  )

out <- base %>% left_join(features, by = "game_id")
out_dir <- file.path("data", "ats_experiments", "feature-candidates")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, paste0("wp-control-prior", TRAILING_N, ".csv"))
readr::write_csv(out, out_path)

coverage <- game_control %>% summarise(
  games = n(),
  median_regulation_seconds = median(regulation_seconds_observed, na.rm = TRUE),
  pct_games_with_full_3600_seconds = mean(regulation_seconds_observed >= 3599),
  pct_games_with_both_wp_shares = mean(is.finite(home_control_share_game) &
                                         is.finite(away_control_share_game))
)
cat(sprintf(
  paste0("trailing_n=%d games=%d full_clock=%.1f%% wp_coverage=%.1f%% ",
         "rows=%d feature_rows=%d output=%s\n"),
  TRAILING_N, coverage$games, 100 * coverage$pct_games_with_full_3600_seconds,
  100 * coverage$pct_games_with_both_wp_shares, nrow(out),
  sum(is.finite(out$control_share_prior_n_home) &
        is.finite(out$control_share_prior_n_away)), normalizePath(out_path)
))
print(coverage)
