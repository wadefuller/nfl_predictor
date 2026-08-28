# Generic helper to attach per-(season, week, team) features to the wide
# game frame. Produces home_/away_/_diff cols for each numeric feature.
# Used by all post-pivot modules (injuries, snap_counts, coaching, ngs).

attach_per_team_features <- function(f_df, per_team_df) {
  feat_cols <- setdiff(names(per_team_df), c("season", "week", "team"))
  if (length(feat_cols) == 0) return(f_df)

  home <- per_team_df %>%
    rename(home_team = team) %>%
    rename_with(~ paste0(.x, "_home"), all_of(feat_cols))
  away <- per_team_df %>%
    rename(away_team = team) %>%
    rename_with(~ paste0(.x, "_away"), all_of(feat_cols))

  f_df <- f_df %>%
    left_join(home, by = c("season", "week", "home_team")) %>%
    left_join(away, by = c("season", "week", "away_team"))

  for (col in feat_cols) {
    f_df[[paste0(col, "_diff")]] <-
      f_df[[paste0(col, "_home")]] - f_df[[paste0(col, "_away")]]
  }
  f_df
}
