# ============================================
# NFL Predictor: Elo Hyperparameter Sweep
# ============================================
# Tests whether QB-adjusted Elo features improve metrics, with auto-refit
# EV calibration per config (the prior hardcoded constants were fit on a
# no-Elo model and don't generalize across configs).
#
# Per config:
#   1. build_features (with ELO_DISABLED=1 or ELO_PARAMS_JSON)
#   2. walk_forward_eval
#   3. recalibrate_ev → ev_calibration.json
#   4. portfolio_sim (with EV_CAL_*_OVERRIDE env vars from JSON)
#
# Records each row to data/elo_sweep_results.csv.
#
# Usage: Rscript elo_sweep.R
# Runtime: ~7 min/config × 6 configs = ~42 min
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")    # PROJECT_DIR, DATA_DIR
source("~/Projects/R/nfl_predictor/sweep_harness.R")   # run_sweep, print_leaderboard

setwd(PROJECT_DIR)

RESULTS_CSV <- file.path(DATA_DIR, "elo_sweep_results.csv")
CACHE_DIR   <- file.path(DATA_DIR, "elo_sweep_cache")

# ---- Config definitions ---------------------------------------------------

CONFIGS <- list(
  list(label = "baseline_no_elo",
       elo_disabled = TRUE,  elo_params = NULL),
  list(label = "elo_default",
       elo_disabled = FALSE,
       elo_params = list(K_TEAM = 20, K_QB = 20, HOME_ADV = 55,
                         REGRESSION_TEAM = 0.75, REGRESSION_QB = 0.85,
                         QB_SHARE = 0.30, QB_WEIGHT = 1.00)),
  list(label = "elo_team_only",
       elo_disabled = FALSE,
       elo_params = list(K_TEAM = 20, K_QB = 20, HOME_ADV = 55,
                         REGRESSION_TEAM = 0.75, REGRESSION_QB = 0.85,
                         QB_SHARE = 0.00, QB_WEIGHT = 0.00)),
  list(label = "elo_qb_heavy",
       elo_disabled = FALSE,
       elo_params = list(K_TEAM = 20, K_QB = 20, HOME_ADV = 55,
                         REGRESSION_TEAM = 0.75, REGRESSION_QB = 0.85,
                         QB_SHARE = 0.70, QB_WEIGHT = 1.50)),
  list(label = "elo_low_ha",
       elo_disabled = FALSE,
       elo_params = list(K_TEAM = 20, K_QB = 20, HOME_ADV = 40,
                         REGRESSION_TEAM = 0.75, REGRESSION_QB = 0.85,
                         QB_SHARE = 0.30, QB_WEIGHT = 1.00)),
  list(label = "elo_slow_k",
       elo_disabled = FALSE,
       elo_params = list(K_TEAM = 12, K_QB = 12, HOME_ADV = 55,
                         REGRESSION_TEAM = 0.80, REGRESSION_QB = 0.90,
                         QB_SHARE = 0.30, QB_WEIGHT = 1.00))
)

# ---- Per-config env-var builder + extras -----------------------------------

env_builder_elo <- function(cfg) {
  if (cfg$elo_disabled) {
    c(ELO_DISABLED = "1")
  } else {
    c(ELO_DISABLED   = "0",
      ELO_PARAMS_JSON = as.character(jsonlite::toJSON(cfg$elo_params,
                                                       auto_unbox = TRUE)))
  }
}

row_extras_elo <- function(cfg) {
  list(
    label        = cfg$label,
    elo_disabled = cfg$elo_disabled,
    config_json  = if (is.null(cfg$elo_params)) ""
                   else as.character(jsonlite::toJSON(cfg$elo_params, auto_unbox = TRUE))
  )
}

# ---- Run -----------------------------------------------------------------

if (sys.nframe() == 0L) {
  cat(sprintf(">>> Elo sweep: %d configs\n", length(CONFIGS)))
  run_sweep(CONFIGS,
            env_builder   = env_builder_elo,
            cache_dir     = CACHE_DIR,
            results_csv   = RESULTS_CSV,
            refit_ev      = TRUE,
            phase_label   = "elo",
            label_fn      = function(cfg) cfg$label,
            row_extras_fn = row_extras_elo)
  print_leaderboard(RESULTS_CSV, extra_cols = c("label"))
  cat("\nDone.\n")
}
