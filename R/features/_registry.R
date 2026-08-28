# Single source of truth for which feature modules exist. To add a new
# feature group:
#   1. Write R/features/your_feature.R defining YOUR_FEATURE_MODULE via
#      `feature_module(...)`.
#   2. Add it to the source() list + MODULES list below.
#   3. If it produces pre-pivot features that need a _diff column, add the
#      column names to DIFFERENTIAL_COLS in R/infra/combine_pivot.R.
#   4. If it produces current-game stats that shouldn't be model features,
#      list them in `leakage_vars = ...` in the module spec.
#
# That's it — build_features.R picks it up automatically.

source(file.path(PROJECT_DIR, "R/features/_module_base.R"))

# Pre-pivot per-team modules (joined onto game_split, then carryover decay,
# then pivoted to home/away/diff via DIFFERENTIAL_COLS).
source(file.path(PROJECT_DIR, "R/features/trending.R"))
source(file.path(PROJECT_DIR, "R/features/pythag.R"))
source(file.path(PROJECT_DIR, "R/features/off_epa.R"))
source(file.path(PROJECT_DIR, "R/features/def_epa.R"))
source(file.path(PROJECT_DIR, "R/features/qb_starter.R"))
source(file.path(PROJECT_DIR, "R/features/pace_total.R"))

# Post-pivot per-team modules (attached via attach_per_team_features after
# pivot — produces home_/away_/_diff cols).
source(file.path(PROJECT_DIR, "R/features/injuries.R"))
source(file.path(PROJECT_DIR, "R/features/snap_counts.R"))
source(file.path(PROJECT_DIR, "R/features/coaching.R"))
source(file.path(PROJECT_DIR, "R/features/rest.R"))
source(file.path(PROJECT_DIR, "R/features/ngs.R"))

# Game-keyed modules (left-joined on game_id).
source(file.path(PROJECT_DIR, "R/features/weather_venue.R"))
source(file.path(PROJECT_DIR, "R/features/officials.R"))
source(file.path(PROJECT_DIR, "R/features/elo.R"))

MODULES <- list(
  # pre-pivot
  trending     = TRENDING_MODULE,
  pythag       = PYTHAG_MODULE,
  off_epa      = OFF_EPA_MODULE,
  def_epa      = DEF_EPA_MODULE,
  qb_starter   = QB_STARTER_MODULE,
  pace_total   = PACE_TOTAL_MODULE,
  # post-pivot
  injuries     = INJURIES_MODULE,
  snap_counts  = SNAP_COUNTS_MODULE,
  coaching     = COACHING_MODULE,
  rest         = REST_MODULE,
  ngs          = NGS_MODULE,
  # game-keyed
  weather_venue = WEATHER_VENUE_MODULE,
  officials    = OFFICIALS_MODULE,
  elo          = ELO_MODULE
)

# Convenience: list of enabled modules at a given moment (each module's
# `enabled()` function may read env vars).
enabled_modules <- function() {
  Filter(function(m) m$enabled(), MODULES)
}

# Convenience: collect all declared leakage vars across enabled modules.
# Used by the leakage check in tests/test_features.R + serves as a
# cross-reference for ensemble_lib::LEAKAGE_VARS_REG.
all_declared_leakage_vars <- function() {
  unique(unlist(lapply(MODULES, function(m) m$leakage_vars)))
}
