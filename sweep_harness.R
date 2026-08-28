# ============================================
# NFL Predictor: Shared Sweep Harness
# ============================================
# Common infrastructure used by carryover_sweep.R and elo_sweep.R (and any
# future hyperparam sweep). Handles:
#   - Hash-based caching (skip a config if we've already evaluated it)
#   - Subprocess orchestration: build_features → walk_forward_eval → portfolio_sim
#   - Optional recalibrate_ev step between walk_forward and portfolio_sim
#   - Metric collection from the standard walk_forward / portfolio_trades CSVs
#   - Append-only results CSV with consistent column types
#   - Leaderboard printing
#
# Callers provide:
#   - configs (list of opaque cfg objects — only used by env_builder + hash)
#   - env_builder(cfg) → named character vector of env vars set for the
#     build_features / walk_forward subprocesses
#   - cache_dir + results_csv paths
#   - refit_ev (TRUE → insert recalibrate_ev.R between walk_forward and
#     portfolio_sim; pass fresh constants as EV_CAL_*_OVERRIDE env vars)
#   - Optional: extra columns to merge into each results row (e.g. label,
#     config_json) via the `row_extras` argument to run_sweep
#
# DATA_DIR, PROJECT_DIR come from ensemble_lib.R — caller must source it.
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(digest)
})

# ---- Hashing -------------------------------------------------------------

config_hash <- function(cfg) substr(digest::digest(cfg, algo = "md5"), 1, 10)

# ---- Subprocess driver --------------------------------------------------

# Run one config end-to-end. Returns NULL if cached (caller can use cached
# row from the results CSV directly), otherwise returns the newly-built
# metrics tibble row.
run_one <- function(cfg,
                     env_builder,
                     cache_dir,
                     results_csv,
                     refit_ev    = FALSE,
                     phase_label = NA_character_,
                     label       = NULL,
                     row_extras  = list(),
                     hash_fn     = config_hash) {
  hash <- hash_fn(cfg)
  label <- if (is.null(label)) hash else label
  cat(sprintf("\n=== [%s] %s (hash=%s) ===\n", phase_label, label, hash))

  existing <- read_results(results_csv)
  if (!is.null(existing) && hash %in% existing$config_hash) {
    cat("  (cached — skipping rebuild)\n")
    return(invisible(existing %>% filter(config_hash == hash) %>% slice_tail(n = 1)))
  }

  env_vec  <- env_builder(cfg)
  env_args <- paste0(names(env_vec), "=", shQuote(env_vec))

  t0 <- Sys.time()

  shell_step("build_features.R",     env_args, cache_dir, hash, "build")
  shell_step("walk_forward_eval.R",  env_args, cache_dir, hash, "wfe")

  port_env_args <- if (refit_ev) {
    shell_step("recalibrate_ev.R", character(), cache_dir, hash, "recal")
    ev_cal_path <- file.path(DATA_DIR, "ev_calibration.json")
    if (!file.exists(ev_cal_path)) stop("recalibrate_ev.R did not write ev_calibration.json")
    ev_cal <- jsonlite::fromJSON(ev_cal_path)
    cat(sprintf("  EV cal refit: intercept=%.4f slope=%.4f\n",
                ev_cal$intercept, ev_cal$slope))
    paste0(c("EV_CAL_INTERCEPT_OVERRIDE", "EV_CAL_SLOPE_OVERRIDE"), "=",
           shQuote(c(as.character(ev_cal$intercept), as.character(ev_cal$slope))))
  } else {
    env_args
  }
  shell_step("portfolio_sim.R", port_env_args, cache_dir, hash, "port")

  metrics <- collect_metrics()
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("  done in %.1f min: brier=%.5f cover_acc=%.4f cagr=%.4f sharpe=%.2f maxdd=%.3f n_bets=%d\n",
              elapsed, metrics$brier, metrics$cover_acc, metrics$cagr,
              metrics$sharpe, metrics$max_dd, metrics$n_bets))

  # Snapshot the prediction CSV by hash for later paired-fold analysis.
  file.copy(file.path(DATA_DIR, "walk_forward_predictions.csv"),
            file.path(cache_dir, sprintf("preds_%s.csv", hash)),
            overwrite = TRUE)

  row <- tibble(
    phase        = phase_label,
    config_hash  = hash,
    !!!row_extras,
    elapsed_min  = round(elapsed, 2),
    !!!metrics,
    timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  append_result(row, results_csv)
  invisible(row)
}

shell_step <- function(script, env_args, cache_dir, hash, tag) {
  cat(sprintf("  %s ...\n", script))
  log_path <- file.path(cache_dir, sprintf("%s_%s.log", tag, hash))
  rc <- system2("Rscript", script, env = env_args,
                stdout = log_path, stderr = log_path)
  if (rc != 0) stop(sprintf("%s failed (rc=%d) — see %s", script, rc, log_path))
}

# Top-level: iterate configs, run each, return invisible(NULL).
# `configs` is a list of cfg objects; `label_fn(cfg)` optionally produces a
# pretty label per cfg; `row_extras_fn(cfg)` optionally produces a named list
# of additional columns to merge into each results row.
run_sweep <- function(configs,
                      env_builder,
                      cache_dir,
                      results_csv,
                      refit_ev      = FALSE,
                      phase_label   = NA_character_,
                      label_fn      = function(cfg) NULL,
                      row_extras_fn = function(cfg) list(),
                      hash_fn       = config_hash) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  for (cfg in configs) {
    run_one(cfg,
            env_builder  = env_builder,
            cache_dir    = cache_dir,
            results_csv  = results_csv,
            refit_ev     = refit_ev,
            phase_label  = phase_label,
            label        = label_fn(cfg),
            row_extras   = row_extras_fn(cfg),
            hash_fn      = hash_fn)
  }
  invisible(NULL)
}

# ---- Results CSV I/O -----------------------------------------------------

# Read the results CSV with consistent col_types so bind_rows on append
# doesn't fail with character↔datetime mismatches.
read_results <- function(results_csv) {
  if (!file.exists(results_csv)) return(NULL)
  read_csv(results_csv, show_col_types = FALSE,
           col_types = cols(timestamp = col_character(),
                            config_hash = col_character(),
                            phase = col_character()))
}

append_result <- function(row, results_csv) {
  existing <- read_results(results_csv)
  combined <- if (is.null(existing)) row else bind_rows(existing, row)
  write_csv(combined, results_csv)
}

# ---- Metrics aggregation -------------------------------------------------

collect_metrics <- function() {
  wp     <- read_csv(file.path(DATA_DIR, "walk_forward_model_winprob.csv"),
                     show_col_types = FALSE)
  sp     <- read_csv(file.path(DATA_DIR, "walk_forward_model_spread.csv"),
                     show_col_types = FALSE)
  preds  <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"),
                     show_col_types = FALSE)
  trades <- read_csv(file.path(DATA_DIR, "portfolio_trades.csv"),
                     show_col_types = FALSE)

  agg_wt <- function(df, n_col, val_col) {
    n <- df[[n_col]]; v <- df[[val_col]]
    sum(n * v, na.rm = TRUE) / sum(n, na.rm = TRUE)
  }

  brier     <- agg_wt(wp, "games_wp", "model_brier")
  logloss   <- agg_wt(wp, "games_wp", "model_logloss")
  mae       <- agg_wt(sp, "games",    "model_mae")
  cover_acc <- if ("cover_correct" %in% names(preds))
                  mean(preds$cover_correct, na.rm = TRUE) else NA_real_

  initial_bankroll <- 10000
  n_seasons <- dplyr::n_distinct(trades$season)
  final_bk  <- initial_bankroll + sum(trades$pnl)
  cagr      <- (final_bk / initial_bankroll) ^ (1 / n_seasons) - 1

  trades_dd <- trades %>%
    arrange(season, week) %>%
    mutate(peak = cummax(bankroll_after),
           drawdown = (peak - bankroll_after) / peak)
  max_dd <- max(trades_dd$drawdown, na.rm = TRUE)

  weekly <- trades %>%
    group_by(season, week) %>%
    summarise(week_pnl = sum(pnl),
              bk_end   = dplyr::last(bankroll_after),
              .groups  = "drop") %>%
    arrange(season, week) %>%
    mutate(bk_start = lag(bk_end, default = initial_bankroll),
           wk_ret   = week_pnl / bk_start)
  sharpe <- (mean(weekly$wk_ret) / sd(weekly$wk_ret)) * sqrt(18)

  list(
    n_bets    = nrow(trades),
    brier     = brier,   logloss = logloss, mae = mae,
    cover_acc = cover_acc,
    total_pnl = sum(trades$pnl),
    cagr      = cagr,    sharpe = sharpe,   max_dd = max_dd
  )
}

# ---- Leaderboard ---------------------------------------------------------

# Sort by core model metric (Brier) per the May 2026 evaluation directive —
# portfolio metrics are EV-calibration brittle; rank by accuracy first.
print_leaderboard <- function(results_csv,
                               phase_filter = NULL,
                               extra_cols   = character()) {
  df <- read_results(results_csv)
  if (is.null(df)) { cat("No results yet.\n"); return(invisible()) }
  if (!is.null(phase_filter)) df <- df %>% filter(phase %in% phase_filter)
  if (nrow(df) == 0) {
    cat(sprintf("No rows for phase=%s.\n", paste(phase_filter, collapse = ",")))
    return(invisible())
  }
  df <- df %>% arrange(brier)
  cat(sprintf("\n=== LEADERBOARD (phase=%s, sorted by Brier) ===\n",
              paste(phase_filter %||% "ALL", collapse = ",")))
  cols <- intersect(c("phase", extra_cols, "brier", "mae", "logloss",
                       "cover_acc", "cagr", "sharpe", "max_dd", "n_bets"),
                     names(df))
  print(df %>% select(all_of(cols)) %>%
        mutate(across(any_of(c("brier", "logloss")), ~round(., 5)),
               across(any_of(c("mae", "cover_acc", "cagr", "max_dd")), ~round(., 4)),
               across(any_of(c("sharpe")), ~round(., 2))) %>%
        as.data.frame())
}

# ---- Helpers shared by callers -------------------------------------------

# Used by both sweeps in their main() blocks
`%||%` <- function(a, b) if (is.null(a)) b else a
