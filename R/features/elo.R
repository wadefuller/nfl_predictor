# QB-adjusted Elo features. Computed by build_elo.R as a separate step
# (chronological walk over all games); this module just reads the cached
# CSV and exposes the pre-game ratings as game-keyed features.
#
# DISABLED by default (2026-05-21 ablation showed team-only Elo barely
# moves Brier and hurts MAE without carryover). Enable via ELO_DISABLED=0.

ELO_MODULE <- feature_module(
  name        = "elo",
  description = "Pre-game team Elo + QB-adjusted Elo + implied win prob (538-style)",
  level       = "game",
  enabled     = function() Sys.getenv("ELO_DISABLED", "1") != "1",
  needs       = character(0),
  build       = function(seasons, deps) {
    elo_path <- file.path(DATA_DIR, "elo_per_game.csv")
    # Regenerate if missing OR if hyperparams overridden (sweep mode).
    if (!file.exists(elo_path) || nzchar(Sys.getenv("ELO_PARAMS_JSON", ""))) {
      message("  Rebuilding elo_per_game.csv")
      system2("Rscript", file.path(PROJECT_DIR, "scripts/build_elo.R"))
    }
    read_csv(elo_path, show_col_types = FALSE) %>%
      transmute(
        game_id,
        home_team_elo_pre, away_team_elo_pre,
        home_qb_elo_pre,   away_qb_elo_pre,
        home_elo_adj,      away_elo_adj,
        elo_diff,
        elo_prob_home,
        team_elo_diff      = home_team_elo_pre - away_team_elo_pre,
        qb_elo_diff        = home_qb_elo_pre   - away_qb_elo_pre,
        elo_prob_home_diff = elo_prob_home - 0.5
      )
  }
)
