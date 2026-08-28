# Standard module spec. Each R/features/*.R file should define exactly one
# module variable using `feature_module()` and either:
#   - return a tibble keyed by (game_id, season, week, team) for level =
#     "team_game_pre_pivot" (joined into combined per-team frame, gets
#     carryover decay, then pivoted to wide via DIFFERENTIAL_COLS),
#   - return a tibble keyed by (season, week, team) for level =
#     "team_week_post_pivot" (attached post-pivot via attach_per_team_features,
#     producing home_/away_/_diff cols), or
#   - return a tibble keyed by (game_id) for level = "game" (left-joined
#     to the wide game frame).
#
# Fields:
#   name        — short ID; also the registry key
#   description — one-line; used by feature-catalog generator
#   level       — see above
#   enabled     — zero-arg function returning TRUE/FALSE
#   needs       — character vector of deps the build function expects
#                 (e.g. "games", "pbp", "player_cap", "team_cap")
#   build       — function(seasons, deps) → tibble
#   leakage_vars — character vector of cols that should NOT be model features
#                  (current-game stats etc., dropped in ensemble_lib::LEAKAGE_VARS_REG)

feature_module <- function(name,
                            description,
                            level    = c("team_game_pre_pivot",
                                          "team_week_post_pivot",
                                          "game"),
                            enabled  = function() TRUE,
                            needs    = character(0),
                            build,
                            leakage_vars = character(0)) {
  level <- match.arg(level)
  structure(list(
    name         = name,
    description  = description,
    level        = level,
    enabled      = enabled,
    needs        = needs,
    build        = build,
    leakage_vars = leakage_vars
  ), class = "feature_module")
}

# Helpers used by modules below

# Filter a list of deps to only the names the module declared in `needs`,
# raising if any required dep is missing.
select_deps <- function(deps, mod) {
  missing <- setdiff(mod$needs, names(deps))
  if (length(missing) > 0) {
    stop(sprintf("Module '%s' needs deps %s but only got %s",
                 mod$name,
                 paste(mod$needs, collapse = ","),
                 paste(names(deps), collapse = ",")))
  }
  deps[mod$needs]
}
