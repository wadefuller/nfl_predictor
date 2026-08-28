# Per-team join (combine_features) + home/away pivot (combine_home_away_differentials).
# Runs after all pre-pivot per-team modules complete + carryover decay applied.
#
# DIFFERENTIAL_COLS is the list of per-team columns that get pivoted into
# explicit home/away/diff versions. When you add a new pre-pivot feature
# module that wants a _diff column, add its output cols here.

DIFFERENTIAL_COLS <- c(
  "win_season_avg", "result_season_avg",
  "points_for_season_avg", "spread_cover_season_avg",
  "spread_adjusted_result_season_avg",
  "impl_win_prob_season_avg", "game_info_gain_season_avg",
  "win_prob_captured_season_avg",
  "win_4gm_trend", "result_4gm_trend",
  "points_for_4gm_trend", "spread_cover_4gm_trend",
  "spread_adjusted_result_4gm_trend",
  "impl_win_prob_4gm_trend",
  "game_info_gain_4gm_trend", "win_prob_captured_4gm_trend",
  # Offensive EPA rolling
  "off_epa_r4", "off_epa_r16",
  "off_pass_epa_r4", "off_pass_epa_r16",
  "off_rush_epa_r4", "off_rush_epa_r16",
  "success_rate_r4", "success_rate_r16",
  "qb_epa_r4", "qb_epa_r16",
  # Defensive EPA rolling
  "def_epa_r4", "def_epa_r16",
  "def_pass_epa_r4", "def_pass_epa_r16",
  "def_rush_epa_r4", "def_rush_epa_r16",
  "def_success_rate_r4", "def_success_rate_r16",
  # Defensive splits rolling (pass rush / run / turnover / situational)
  "def_sack_rate_r4", "def_sack_rate_r16",
  "def_qb_hit_rate_r4", "def_qb_hit_rate_r16",
  "def_stuff_rate_r4", "def_stuff_rate_r16",
  "def_int_rate_r4", "def_int_rate_r16",
  "def_third_dn_rate_r4", "def_third_dn_rate_r16",
  # Pythagorean
  "pythag_win_pct", "pythag_wins",
  # Pace + total environment rolling
  "team_points_against_r4", "team_points_against_r16",
  "team_total_env_r4", "team_total_env_r16",
  "team_off_plays_r4", "team_off_plays_r16",
  # QB starter features (player-specific rolling + team stability)
  "starter_qb_epa_r4", "starter_qb_epa_r8",
  "starter_pass_epa_r4", "starter_pass_epa_r8",
  "starter_career_games", "starter_games_team_season",
  "new_starter"
)

# Reduce-join all per-team module outputs onto game_split.
# Each module output should be keyed by (game_id, season, week, team).
combine_per_team_features <- function(game_split, module_outputs) {
  out <- game_split
  for (df in module_outputs) {
    out <- out %>%
      left_join(df, by = c("game_id", "season", "week", "team"), suffix = c("", ""))
  }
  out %>% select(-ends_with(".x"), -ends_with(".y"))
}

combine_home_away_differentials <- function(game_w_features) {
  home <- game_w_features %>%
    filter(home == 1) %>%
    rename_with(~ paste0(.x, "_home"), all_of(DIFFERENTIAL_COLS))

  away <- game_w_features %>%
    filter(home == 0) %>%
    rename_with(~ paste0(.x, "_away"), all_of(DIFFERENTIAL_COLS))

  joined <- home %>%
    inner_join(away, by = "game_id", suffix = c("_h", "_a")) %>%
    mutate(
      home_team               = team_h,
      away_team               = opposing_team_h,
      home_points             = points_for_h,
      away_points             = points_against_h,
      home_result             = home_points - away_points,
      home_win                = as.integer(home_result > 0),
      home_pct_points_scored  = pct_points_scored_h,
      away_pct_points_scored  = 1 - pct_points_scored_h,
      home_team_rest          = team_rest_h,
      away_team_rest          = opponent_rest_h,
      gameday                 = gameday_h,
      season                  = season_h,
      week                    = week_h,
      weekday                 = weekday_h,
      gametime                = gametime_h,
      total_line              = total_line_h,
      div_game                = div_game_h,
      spread_line             = spread_line_h * -1,
      home_moneyline          = home_moneyline_h,
      away_moneyline          = away_moneyline_h,
      home_spread_odds        = home_spread_odds_h,
      away_spread_odds        = away_spread_odds_h,
      team                    = team_h,
      opposing_team           = opposing_team_h,
      points_for              = points_for_h,
      points_against          = points_against_h,
      total_points_scored     = total_points_scored_h,
      pct_points_scored       = pct_points_scored_h,
      team_rest               = team_rest_h,
      opponent_rest           = opponent_rest_h,
      impl_prob_home          = impl_prob_home_h,
      impl_prob_away          = impl_prob_away_h,
      home                    = home_h,
      win                     = win_h,
      result                  = result_h,
      spread_adjusted_result  = spread_adjusted_result_h,
      spread_cover            = spread_cover_h,
      rest_diff               = rest_diff_h,
      game_info_gain          = game_info_gain_h,
      win_prob_captured       = win_prob_captured_h,
      # Current-game EPA (LEAKAGE — dropped at recipe stage)
      off_epa                 = off_epa_h,
      off_pass_epa            = off_pass_epa_h,
      off_rush_epa            = off_rush_epa_h,
      success_rate            = success_rate_h,
      qb_epa                  = qb_epa_h,
      # Defensive current-game (LEAKAGE — dropped at recipe stage)
      def_epa                 = def_epa_h,
      def_pass_epa            = def_pass_epa_h,
      def_rush_epa            = def_rush_epa_h,
      def_success_rate        = def_success_rate_h,
      # r12 versions (not in DIFFERENTIAL_COLS, kept as home perspective)
      off_epa_r12             = off_epa_r12_h,
      off_pass_epa_r12        = off_pass_epa_r12_h,
      off_rush_epa_r12        = off_rush_epa_r12_h,
      success_rate_r12        = success_rate_r12_h,
      qb_epa_r12              = qb_epa_r12_h,
      def_epa_r12             = def_epa_r12_h,
      def_pass_epa_r12        = def_pass_epa_r12_h,
      def_rush_epa_r12        = def_rush_epa_r12_h,
      def_success_rate_r12    = def_success_rate_r12_h,
      games_played            = games_played_h
    ) %>%
    mutate(across(
      ends_with("_home"),
      ~. - get(sub("_home$", "_away", cur_column())),
      .names = "{.col}_diff"
    )) %>%
    select(
      # Identifiers & lines
      game_id, gameday, season, week, weekday, gametime,
      spread_line, total_line, home_spread_odds, away_spread_odds,
      home_moneyline, away_moneyline, div_game,
      # Per-team (from home record)
      result, team, opposing_team, points_for, points_against,
      total_points_scored, pct_points_scored, team_rest, opponent_rest,
      impl_prob_home, impl_prob_away, home, win,
      spread_adjusted_result, spread_cover, rest_diff,
      game_info_gain, win_prob_captured,
      # Differential _home and _away
      ends_with("_home"), ends_with("_away"),
      # Current-game EPA (leakage-flagged)
      off_epa, off_pass_epa, off_rush_epa, success_rate, qb_epa,
      def_epa, def_pass_epa, def_rush_epa, def_success_rate,
      # r12 versions
      off_epa_r12, off_pass_epa_r12, off_rush_epa_r12,
      success_rate_r12, qb_epa_r12,
      def_epa_r12, def_pass_epa_r12, def_rush_epa_r12, def_success_rate_r12,
      games_played,
      # Derived home/away
      home_team, home_result, away_team, home_points, away_points,
      home_pct_points_scored, away_pct_points_scored,
      home_team_rest, away_team_rest, home_win,
      ends_with("_home_diff")
    )
}
