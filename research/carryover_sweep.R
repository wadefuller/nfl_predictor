# ============================================
# NFL Predictor: Carryover Decay Sweep
# ============================================
# Sweeps the per-feature-class CARRYOVER_HL_JSON env var that build_features.R
# consumes, to tune how long prior-season state persists into the new season.
# For each setting: rebuild features → walk-forward eval → portfolio sim.
#
# Phases:
#   A (default): uniform half-life across all 5 classes
#   B: coordinate descent around Phase A winner (per-class)
#
# Usage:
#   Rscript research/carryover_sweep.R              # run Phase A
#   Rscript research/carryover_sweep.R --phase A    # explicit
#   Rscript research/carryover_sweep.R --phase B    # refine winner found by Phase A
#   Rscript research/carryover_sweep.R --phase AB   # both
#
# Results in data/carryover_sweep_results.csv. Builds cached by config hash.
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")    # PROJECT_DIR, DATA_DIR
source("~/Projects/R/nfl_predictor/research/sweep_harness.R")   # run_sweep, print_leaderboard

setwd(PROJECT_DIR)

RESULTS_CSV <- file.path(DATA_DIR, "carryover_sweep_results.csv")
CACHE_DIR   <- file.path(DATA_DIR, "carryover_cache")

CLASSES <- c("team_epa", "season_agg", "pythag", "qb_starter", "games_played")

# ---- Config helpers --------------------------------------------------------

make_config <- function(team_epa = Inf, season_agg = Inf, pythag = Inf,
                        qb_starter = Inf, games_played = Inf) {
  list(team_epa = team_epa, season_agg = season_agg, pythag = pythag,
       qb_starter = qb_starter, games_played = games_played)
}

# jsonlite serializes Inf as null; encode as the string "Inf" so the
# build-time parser can round-trip.
config_to_json <- function(cfg) {
  cfg_str <- lapply(cfg, function(v) if (is.infinite(v)) "Inf" else v)
  as.character(jsonlite::toJSON(cfg_str, auto_unbox = TRUE))
}

config_label <- function(cfg) {
  paste(vapply(CLASSES, function(c) {
    v <- cfg[[c]]; sprintf("%s=%s", c, if (is.infinite(v)) "Inf" else v)
  }, character(1)), collapse = ",")
}

env_builder_carryover <- function(cfg) {
  c(CARRYOVER_HL_JSON = config_to_json(cfg))
}

row_extras_carryover <- function(cfg) {
  list(config_label = config_label(cfg), config_json = config_to_json(cfg))
}

# Hash matches pre-refactor format exactly: digest the jsonlite json object
# (with its class attribute) BEFORE coercing to character. Preserves
# compatibility with the existing data/carryover_sweep_results.csv cache.
hash_carryover <- function(cfg) {
  cfg_str <- lapply(cfg, function(v) if (is.infinite(v)) "Inf" else v)
  substr(digest::digest(jsonlite::toJSON(cfg_str, auto_unbox = TRUE),
                         algo = "md5"), 1, 10)
}

# ---- Phase A: global half-life sweep --------------------------------------

run_phase_A <- function() {
  grid <- c(Inf, 12, 8, 6, 4)
  cat(sprintf("\n>>> Phase A: %d uniform half-life settings\n", length(grid)))
  configs <- lapply(grid, function(hl) {
    make_config(team_epa = hl, season_agg = hl, pythag = hl,
                qb_starter = hl, games_played = hl)
  })
  run_sweep(configs,
            env_builder   = env_builder_carryover,
            cache_dir     = CACHE_DIR,
            results_csv   = RESULTS_CSV,
            phase_label   = "A",
            label_fn      = config_label,
            row_extras_fn = row_extras_carryover,
            hash_fn       = hash_carryover)
  print_leaderboard(RESULTS_CSV, phase_filter = "A",
                    extra_cols = c("config_label"))
}

# ---- Phase B: coordinate descent around Phase A winner --------------------

run_phase_B <- function() {
  if (!file.exists(RESULTS_CSV))
    stop("Phase A must run first (no results CSV).")

  df <- read_csv(RESULTS_CSV, show_col_types = FALSE,
                 col_types = cols(timestamp = col_character(),
                                  config_hash = col_character())) %>%
        filter(phase == "A") %>% arrange(brier)
  if (nrow(df) == 0) stop("No Phase A rows in results.")

  winner_cfg <- jsonlite::fromJSON(df$config_json[[1]], simplifyVector = TRUE)
  winner_cfg <- lapply(winner_cfg, function(x)
                       if (is.character(x) && x == "Inf") Inf else as.numeric(x))
  cat(sprintf(">>> Phase B anchor: %s\n", config_label(winner_cfg)))

  order <- c("team_epa", "season_agg", "qb_starter", "pythag", "games_played")
  configs <- list()
  for (cls in order) {
    base_hl <- winner_cfg[[cls]]
    if (is.infinite(base_hl)) base_hl <- 8  # jumping-off if anchor is Inf
    candidates <- unique(c(max(1, base_hl / 2), base_hl, base_hl * 2))
    for (hl in candidates) {
      cfg <- winner_cfg
      cfg[[cls]] <- hl
      configs[[length(configs) + 1]] <- cfg
    }
  }
  run_sweep(configs,
            env_builder   = env_builder_carryover,
            cache_dir     = CACHE_DIR,
            results_csv   = RESULTS_CSV,
            phase_label   = "B",
            label_fn      = config_label,
            row_extras_fn = row_extras_carryover,
            hash_fn       = hash_carryover)
  print_leaderboard(RESULTS_CSV, phase_filter = c("A", "B"),
                    extra_cols = c("config_label"))
}

# ---- Arg parsing + dispatch ----------------------------------------------

parse_args <- function(argv) {
  args <- list(phase = "A")
  i <- 1
  while (i <= length(argv)) {
    a <- argv[[i]]
    if (a == "--phase") { args$phase <- argv[[i + 1]]; i <- i + 2; next }
    stop("Unknown arg: ", a)
  }
  args
}

if (sys.nframe() == 0L) {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  phases <- strsplit(args$phase, "")[[1]]
  if ("A" %in% phases) run_phase_A()
  if ("B" %in% phases) run_phase_B()
  cat("\nDone.\n")
}
