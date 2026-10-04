# Build leak-safe, team-level candidate features for ATS walk-forward tests.
#
#   Rscript ats_feature_candidates.R [favored|model_error|both]
#
# favored: trailing-four share of prior games where the team's pregame live WP
#          was > 50% at the first play (home_wp / away_wp).
# model_error: trailing-four average of prior OOF point-margin residuals,
#              actual team margin minus the production model's predicted margin.
#
# Output is research-only under data/ats_experiments/feature-candidates/; no
# production feature file or model bundle is changed.

suppressPackageStartupMessages({ library(tidyverse) })
source("../shared/pbp.R")

args <- commandArgs(trailingOnly = TRUE)
arm <- if (length(args)) args[[1]] else "both"
if (!arm %in% c("favored", "model_error", "both"))
  stop("arm must be one of: favored, model_error, both")

base <- readr::read_csv("data/game_w_features.csv", show_col_types = FALSE)
oos <- readr::read_csv("data/walk_forward_predictions.csv", show_col_types = FALSE) %>%
  select(game_id, home_result, nfl_predictor_line)
inventory <- pbp_inventory()
pbp_seasons <- intersect(sort(unique(as.integer(base$season))), inventory$season)
pbp <- read_pbp(pbp_seasons, cols = c("season_type", "play_id", "game_seconds_remaining",
                                      "home_team", "away_team", "home_wp", "away_wp")) %>%
  filter(season_type == "REG", is.finite(home_wp), is.finite(away_wp)) %>%
  arrange(game_id, desc(game_seconds_remaining), play_id) %>%
  group_by(game_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(game_id, home_wp_open = home_wp, away_wp_open = away_wp)

games <- base %>%
  left_join(pbp, by = "game_id") %>%
  left_join(oos, by = "game_id", suffix = c("", "_oos")) %>%
  mutate(
    actual_margin = coalesce(home_result, home_result_oos),
    margin_resid = actual_margin - nfl_predictor_line
  )

team_games <- bind_rows(
  games %>% transmute(game_id, season, week, team = home_team,
                      pregame_favored = if_else(is.finite(home_wp_open), home_wp_open > 0.5, NA),
                      model_margin_resid = margin_resid),
  games %>% transmute(game_id, season, week, team = away_team,
                      pregame_favored = if_else(is.finite(away_wp_open), away_wp_open > 0.5, NA),
                      model_margin_resid = -margin_resid)
) %>%
  arrange(team, season, week) %>%
  group_by(team) %>%
  mutate(
    favored_share_prior4 = vapply(seq_len(n()), function(i) {
      if (i == 1) return(NA_real_)
      x <- pregame_favored[max(1L, i - 4L):(i - 1L)]
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    }, numeric(1)),
    model_error_prior4 = vapply(seq_len(n()), function(i) {
      if (i == 1) return(NA_real_)
      x <- model_margin_resid[max(1L, i - 4L):(i - 1L)]
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    }, numeric(1))
  ) %>%
  ungroup()

home <- games %>% select(game_id, home_team, away_team) %>%
  left_join(team_games %>% select(game_id, team, favored_share_prior4, model_error_prior4),
            by = c("game_id", "home_team" = "team")) %>%
  rename(favored_share_prior4_home = favored_share_prior4,
         model_error_prior4_home = model_error_prior4)
away <- games %>% select(game_id, home_team, away_team) %>%
  left_join(team_games %>% select(game_id, team, favored_share_prior4, model_error_prior4),
            by = c("game_id", "away_team" = "team")) %>%
  rename(favored_share_prior4_away = favored_share_prior4,
         model_error_prior4_away = model_error_prior4)

features <- home %>%
  left_join(away %>% select(game_id, favored_share_prior4_away, model_error_prior4_away),
            by = "game_id") %>%
  transmute(
    game_id,
    favored_share_prior4_home,
    favored_share_prior4_away,
    favored_share_prior4_diff = favored_share_prior4_home - favored_share_prior4_away,
    model_error_prior4_home,
    model_error_prior4_away,
    model_error_prior4_diff = model_error_prior4_home - model_error_prior4_away
  )

if (arm == "favored") features <- features %>% select(game_id, starts_with("favored_"))
if (arm == "model_error") features <- features %>% select(game_id, starts_with("model_error_"))

out <- base %>% left_join(features, by = "game_id")
out_dir <- file.path("data", "ats_experiments", "feature-candidates")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, paste0(arm, ".csv"))
readr::write_csv(out, out_path)

cat(sprintf("arm=%s rows=%d games_with_open_wp=%d games_with_oos_error=%d output=%s\n",
            arm, nrow(out), sum(is.finite(games$home_wp_open) & is.finite(games$away_wp_open)),
            sum(is.finite(games$margin_resid)), normalizePath(out_path)))
