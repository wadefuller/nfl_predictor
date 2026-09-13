# ============================================
# NFL Predictor: Feature Engineering Orchestrator
# ============================================
# Thin driver around the feature module registry (R/features/_registry.R).
# Loads upstream data once, then walks the registry to build each enabled
# module's contribution. Modules are categorized by `level`:
#   - team_game_pre_pivot — joined into combined per-team frame, gets
#     carryover decay, then pivoted to wide via DIFFERENTIAL_COLS
#   - team_week_post_pivot — attached post-pivot via attach_per_team_features
#     (produces home_/away_/_diff cols)
#   - game — left-joined on game_id
#
# To ADD A NEW FEATURE GROUP:
#   1. Write R/features/your_feature.R defining a module via feature_module()
#   2. Add it to R/features/_registry.R (source + MODULES list)
#   3. Optional: add output cols to DIFFERENTIAL_COLS in
#      R/infra/combine_pivot.R if you want home/away/diff pivot
#   4. Optional: declare current-game leakage cols in your module spec
#
# Usage: Rscript build_features.R     (or ./run_pipeline.sh features)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")  # PROJECT_DIR, DATA_DIR
suppressPackageStartupMessages({
  library(nflreadr)
  library(zoo)
  library(jsonlite)
})

SEASONS <- 2000:2026

# ---- why this build is NOT incremental --------------------------------------
# Tempting: rebuild only the last 2-3 seasons and splice onto the cache, so a
# twice-daily job doesn't re-derive 2000-2026 each time. It does not work, and
# the reason is worth recording so nobody tries it again.
#
# Measured: rebuilding 2024-2026 and diffing season 2025 against a full build
# leaves 138 of 374 columns different. They are all UNBOUNDED-HISTORY counters —
#   ref_career_n_games, ref_career_avg_total, ref_career_home_win_pct,
#   starter_career_games, coach_team_games, coach_team_seasons
# — which accumulate from 2000, not over a trailing window. A referee's career
# game count in 2025 depends on every game they have ever worked, so no window
# reproduces it. Carryover (infinite half-life) compounds this.
#
# The daily job therefore does NOT rebuild features at all; it runs with
# SKIP_FEATURES=1. Nothing in the historical matrix changes until games are
# played, and what does move day to day — lines, injuries, the upcoming slate —
# reaches the models through export_upcoming.R and the game-env join instead.
# The full rebuild runs weekly, on Tuesday, after the week completes.

# ---- Load infra (shared helpers) ----
source("/Users/wadefuller/Projects/R/shared/contracts.R")
source(file.path(PROJECT_DIR, "R/infra/team_normalize.R"))
source(file.path(PROJECT_DIR, "R/infra/data_loaders.R"))
source(file.path(PROJECT_DIR, "R/infra/rolling.R"))
source(file.path(PROJECT_DIR, "R/infra/game_split.R"))
source(file.path(PROJECT_DIR, "R/infra/attach.R"))
source(file.path(PROJECT_DIR, "R/infra/carryover.R"))
source(file.path(PROJECT_DIR, "R/infra/combine_pivot.R"))

# ---- Load feature registry ----
source(file.path(PROJECT_DIR, "R/features/_registry.R"))

# ---- Pipeline -----------------------------------------------------------------

generate_all_features <- function() {
  # Upstream data — loaded once, threaded through modules via `deps`.
  games      <- load_game_data(SEASONS)
  game_split <- split_home_away(games)
  pbp        <- load_pbp_data(SEASONS)
  player_cap <- load_player_cap_by_season(SEASONS)
  team_cap   <- build_team_cap_aggregates(SEASONS, player_cap)

  deps <- list(
    games      = games,
    game_split = game_split,
    pbp        = pbp,
    player_cap = player_cap,
    team_cap   = team_cap
  )

  modules <- enabled_modules()
  cat(sprintf("Building %d enabled modules: %s\n",
              length(modules),
              paste(vapply(modules, `[[`, character(1), "name"), collapse = ", ")))

  # Group module outputs by level for the join phases.
  pre_pivot_outputs  <- list()
  post_pivot_outputs <- list()
  game_outputs       <- list()

  for (mod in modules) {
    message(sprintf("[%s] building...", mod$name))
    out <- mod$build(SEASONS, deps)
    if (is.null(out) || nrow(out) == 0) {
      message(sprintf("  (empty — skipped)"))
      next
    }
    if      (mod$level == "team_game_pre_pivot")  pre_pivot_outputs[[mod$name]]  <- out
    else if (mod$level == "team_week_post_pivot") post_pivot_outputs[[mod$name]] <- out
    else                                          game_outputs[[mod$name]]       <- out
  }

  # ---- Phase 1: combine all pre-pivot modules onto game_split -----------------
  message("Combining per-team pre-pivot features...")
  per_team <- combine_per_team_features(game_split, pre_pivot_outputs)

  # ---- Phase 2: carryover decay ----------------------------------------------
  message("Applying team-state carryover (per-class exponential decay)...")
  per_team <- apply_team_carryover_decay(per_team)

  # ---- Phase 3: pivot to wide (home/away/diff) -------------------------------
  message("Building home/away differentials...")
  f_df <- combine_home_away_differentials(per_team)

  # ---- Phase 4: attach game-keyed features (weather, elo) --------------------
  for (g in game_outputs) {
    f_df <- f_df %>% left_join(g, by = "game_id")
  }

  # ---- Phase 5: attach post-pivot per-team features --------------------------
  # (injuries, snap_counts, coaching, ngs — produces home_/away_/_diff cols)
  for (p in post_pivot_outputs) {
    f_df <- attach_per_team_features(f_df, p)
  }

  # Drop scheduled-but-empty future-season padding rows.
  n_before <- nrow(f_df)
  f_df <- f_df %>%
    filter(!(is.na(spread_line) & is.na(total_line) & is.na(home_moneyline)))
  message(sprintf("Dropped %d empty padding rows", n_before - nrow(f_df)))

  out_path <- file.path(DATA_DIR, "game_w_features.csv")

  message(sprintf("Writing %s rows, %s cols to %s",
                  format(nrow(f_df), big.mark = ","),
                  format(ncol(f_df), big.mark = ","),
                  out_path))
  write_csv(f_df, out_path)
  invisible(f_df)
}

# Only run when invoked as a script. Sourcing this file loads the function
# defs without triggering a 10-minute build.
if (sys.nframe() == 0L) {
  generate_all_features()
  cat("Done.\n")
}
