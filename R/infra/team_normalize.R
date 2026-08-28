# Team-name normalization. Source of truth for old-franchise → current
# franchise mapping (used by all upstream loaders).

TEAM_REPLACEMENTS <- c(
  "JAC" = "JAX",
  "SD"  = "LAC",
  "OAK" = "LV",
  "STL" = "LA"
)

normalize_team_names <- function(df) {
  df %>% mutate(across(
    .cols = contains("team"),
    .fns  = ~stringr::str_replace_all(., TEAM_REPLACEMENTS)
  ))
}
