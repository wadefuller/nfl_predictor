# Referee-tendency features. Some refs are well-known for calling more
# penalties (affects clock + drives), favoring home teams (slight home-cooking
# bias is statistically observable), or producing higher/lower totals.
#
# Approach: for each game, look up the head referee's career stats from
# their PRIOR games only (lag-safe — no peeking at the current game). XGB's
# native NA handling absorbs pre-2015 games (no officials data).
#
# Features per (game_id):
#   ref_career_n_games        — referee tenure (count of prior officiated games)
#   ref_career_home_win_pct   — home team win rate in ref's prior games
#   ref_career_avg_total      — avg total points in ref's prior games
#   ref_career_avg_margin     — avg abs(home_result) in ref's prior games

extract_officials_features <- function(games) {
  # load_officials available 2015+; cap at most_recent_season for live games
  off_max     <- nflreadr::most_recent_season()
  off_seasons <- intersect(unique(games$season), 2015:off_max)
  if (length(off_seasons) == 0) {
    # Return one row per game with all-NA officials features so the left-join
    # still produces the expected columns.
    return(games %>% transmute(game_id,
                                ref_career_n_games      = NA_integer_,
                                ref_career_home_win_pct = NA_real_,
                                ref_career_avg_total    = NA_real_,
                                ref_career_avg_margin   = NA_real_))
  }

  # load_officials uses the legacy YYYYMMDDXX game_id (matches games$old_game_id).
  # We join on that, then re-key to the modern game_id for downstream use.
  refs <- nflreadr::load_officials(off_seasons) %>%
    filter(position == "Referee") %>%
    group_by(game_id) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(old_game_id = game_id, referee = official_name)

  ref_games <- games %>%
    transmute(game_id, old_game_id, season, week, gameday,
              home_score, away_score,
              home_win   = as.integer(home_score > away_score),
              total      = home_score + away_score,
              margin_abs = abs(home_score - away_score)) %>%
    inner_join(refs, by = "old_game_id") %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    arrange(gameday, game_id)

  # Per referee chronologically: cumulative-mean-of-PRIOR-games (lag by 1)
  ref_features <- ref_games %>%
    group_by(referee) %>%
    arrange(referee, gameday, game_id) %>%
    mutate(
      ref_career_n_games      = row_number() - 1L,
      ref_career_home_win_pct = lag(cummean(home_win)),
      ref_career_avg_total    = lag(cummean(total)),
      ref_career_avg_margin   = lag(cummean(margin_abs))
    ) %>%
    ungroup() %>%
    select(game_id, ref_career_n_games, ref_career_home_win_pct,
           ref_career_avg_total, ref_career_avg_margin)

  # Left-join back to ALL games (including pre-2015 with NA, and future
  # scheduled games where the head ref may not yet be assigned)
  games %>%
    select(game_id) %>%
    left_join(ref_features, by = "game_id")
}

OFFICIALS_MODULE <- feature_module(
  name        = "officials",
  description = "Head-referee career tendencies (tenure, home win%, avg total, avg margin) from prior officiated games",
  level       = "game",
  needs       = c("games"),
  build       = function(seasons, deps) extract_officials_features(deps$games)
)
