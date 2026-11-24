# ============================================
# ATS experiment comparison + diagnostics
# ============================================
# Cheap evaluation layer for two completed walk-forward runs. No model fitting.
# It enforces paired game populations, computes game/season deltas, block-
# bootstrap uncertainty, and emits three compact diagnostic charts.
#
# Usage:
#   Rscript research/ats_compare.R \
#     --incumbent data/ats_experiments/incumbent-screen \
#     --candidate data/ats_experiments/candidate-screen \
#     --out data/ats_experiments/comparisons/my-feature
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

parse_args <- function(argv) {
  out <- list(bootstrap = 2000L)
  i <- 1L
  while (i <= length(argv)) {
    key <- argv[[i]]
    if (!key %in% c("--incumbent", "--candidate", "--out", "--bootstrap"))
      stop("Unknown argument: ", key)
    if (i == length(argv)) stop("Missing value for ", key)
    val <- argv[[i + 1L]]
    nm <- sub("^--", "", key)
    out[[nm]] <- if (nm == "bootstrap") as.integer(val) else val
    i <- i + 2L
  }
  missing <- setdiff(c("incumbent", "candidate", "out"), names(out))
  if (length(missing)) stop("Missing arguments: --", paste(missing, collapse = ", --"))
  out
}

read_run <- function(path, label) {
  pred_path <- file.path(path, "walk_forward_predictions.csv")
  meta_path <- file.path(path, "run_metadata.json")
  if (!file.exists(pred_path)) stop(label, " predictions not found: ", pred_path)
  list(
    predictions = read_csv(pred_path, show_col_types = FALSE),
    metadata = if (file.exists(meta_path)) fromJSON(meta_path, simplifyVector = TRUE)
               else list(profile = "unknown", promotion_authority = FALSE)
  )
}

score_frame <- function(x) {
  x %>% mutate(
    signed_error = nfl_predictor_line - home_result,
    abs_error = abs(signed_error),
    brier = (pred_win_pct - home_win_01)^2,
    logloss = -(home_win_01 * log(pmin(pmax(pred_win_pct, 1e-15), 1 - 1e-15)) +
      (1 - home_win_01) * log(1 - pmin(pmax(pred_win_pct, 1e-15), 1 - 1e-15)))
  )
}

metric_row <- function(x, prefix) {
  tibble(
    !!paste0(prefix, "_mae") := mean(x$abs_error),
    !!paste0(prefix, "_brier") := mean(x$brier),
    !!paste0(prefix, "_logloss") := mean(x$logloss),
    !!paste0(prefix, "_cover") := mean(x$cover_correct, na.rm = TRUE)
  )
}

paired_metrics <- function(x) {
  bind_cols(
    metric_row(transmute(x, abs_error = abs_error_inc,
                         brier = brier_inc, logloss = logloss_inc,
                         cover_correct = cover_correct_inc), "incumbent"),
    metric_row(transmute(x, abs_error = abs_error_cand,
                         brier = brier_cand, logloss = logloss_cand,
                         cover_correct = cover_correct_cand), "candidate")
  ) %>% mutate(
    delta_mae = candidate_mae - incumbent_mae,
    delta_brier = candidate_brier - incumbent_brier,
    delta_logloss = candidate_logloss - incumbent_logloss,
    delta_cover = candidate_cover - incumbent_cover
  )
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
inc <- read_run(args$incumbent, "Incumbent")
cand <- read_run(args$candidate, "Candidate")

i <- score_frame(inc$predictions)
c <- score_frame(cand$predictions)
needed <- c("game_id", "season", "week", "home_result", "spread_line",
            "signed_error", "abs_error", "brier", "logloss", "cover_correct")
if (!all(needed %in% names(i)) || !all(needed %in% names(c)))
  stop("Prediction files do not contain the required evaluation columns")

paired <- i %>% select(all_of(needed)) %>%
  inner_join(c %>% select(all_of(needed)), by = "game_id", suffix = c("_inc", "_cand"))
if (nrow(paired) != nrow(i) || nrow(paired) != nrow(c))
  stop("Incumbent and candidate populations differ; comparison fails closed")
if (!isTRUE(all.equal(paired$home_result_inc, paired$home_result_cand, tolerance = 0)) ||
    !isTRUE(all.equal(paired$spread_line_inc, paired$spread_line_cand, tolerance = 0)))
  stop("Outcomes or market lines differ between runs; comparison fails closed")

paired <- paired %>% mutate(season = season_inc, week = week_inc)
overall <- paired_metrics(paired) %>% mutate(
  games = nrow(paired), seasons = n_distinct(paired$season), .before = 1
)

by_season <- paired %>% group_by(season) %>%
  group_modify(~paired_metrics(.x)) %>% ungroup() %>%
  mutate(mae_better = delta_mae < 0, brier_better = delta_brier < 0)

# Confirmation uses season blocks. Probe/screen uses week blocks so its interval
# is at least descriptive, but only a full confirm run has promotion authority.
block_var <- if (n_distinct(paired$season) >= 6L) "season" else "season_week"
paired <- paired %>% mutate(season_week = interaction(season, week, drop = TRUE))
blocks <- split(seq_len(nrow(paired)), paired[[block_var]])
set.seed(989)
boots <- map_dfr(seq_len(args$bootstrap), function(iter) {
  draw <- sample(names(blocks), length(blocks), replace = TRUE)
  idx <- unlist(blocks[draw], use.names = FALSE)
  paired_metrics(paired[idx, ]) %>% select(starts_with("delta_"))
})
bootstrap_summary <- boots %>% pivot_longer(everything(), names_to = "metric") %>%
  group_by(metric) %>% summarise(
    estimate = overall[[first(metric)]],
    lo90 = quantile(value, 0.05, na.rm = TRUE),
    hi90 = quantile(value, 0.95, na.rm = TRUE),
    probability_improvement = if_else(first(metric) == "delta_cover",
                                      mean(value > 0, na.rm = TRUE),
                                      mean(value < 0, na.rm = TRUE)),
    .groups = "drop"
  )

profile <- cand$metadata$profile %||% "unknown"
promotion_authority <- isTRUE(cand$metadata$promotion_authority) &&
  isTRUE(inc$metadata$promotion_authority)
clear_regression <- overall$delta_mae > 0.03 && overall$delta_brier > 0.0005
promising <- (overall$delta_mae <= -0.01 && overall$delta_brier <= 0) ||
  (overall$delta_brier <= -0.0005 && overall$delta_mae <= 0)
decision <- if (!promotion_authority) {
  if (clear_regression) "stop_for_futility"
  else if (promising) "advance_to_next_profile"
  else "hold_or_refine"
} else {
  mae_hi <- bootstrap_summary$hi90[bootstrap_summary$metric == "delta_mae"]
  stable <- sum(by_season$mae_better) >= 6L &&
    overall$delta_brier <= 0 && overall$delta_logloss <= 0
  if (overall$delta_mae <= -0.01 && mae_hi < 0 && stable) "promotion_candidate"
  else "do_not_promote"
}

dir.create(args$out, recursive = TRUE, showWarnings = FALSE)
write_csv(overall, file.path(args$out, "overall.csv"))
write_csv(by_season, file.path(args$out, "by_season.csv"))
write_csv(bootstrap_summary, file.path(args$out, "bootstrap.csv"))
write_json(list(profile = profile, promotion_authority = promotion_authority,
                decision = decision, block_unit = block_var,
                bootstrap_replicates = args$bootstrap),
           file.path(args$out, "decision.json"), pretty = TRUE, auto_unbox = TRUE)

dist <- bind_rows(
  transmute(paired, model = "Incumbent", signed_error = signed_error_inc,
            abs_error = abs_error_inc),
  transmute(paired, model = "Candidate", signed_error = signed_error_cand,
            abs_error = abs_error_cand)
)
theme_ats <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

ggsave(file.path(args$out, "signed_error_distribution.png"),
       ggplot(dist, aes(signed_error, colour = model, fill = model)) +
         geom_density(alpha = 0.12, linewidth = 0.8) + geom_vline(xintercept = 0, linetype = 2) +
         labs(title = "ATS margin error distribution", x = "Predicted margin − actual margin", y = "Density") +
         theme_ats, width = 8, height = 4.5, dpi = 160)
ggsave(file.path(args$out, "absolute_error_cdf.png"),
       ggplot(dist, aes(abs_error, colour = model)) + stat_ecdf(linewidth = 0.9) +
         labs(title = "Absolute margin error", x = "Absolute error (points)", y = "Share of games at or below") +
         theme_ats, width = 8, height = 4.5, dpi = 160)
ggsave(file.path(args$out, "paired_delta_by_season.png"),
       ggplot(by_season, aes(factor(season), -delta_mae, fill = -delta_mae > 0)) +
         geom_col(show.legend = FALSE) + geom_hline(yintercept = 0, colour = "grey45") +
         scale_fill_manual(values = c("#d95f59", "#39a96b")) +
         labs(title = "Candidate MAE gain by season", x = NULL,
              y = "MAE improvement (points; positive is better)") + theme_ats,
       width = 8, height = 4.5, dpi = 160)

cat(sprintf("%s: n=%d | MAE %+.4f | Brier %+.5f | LogLoss %+.5f | Cover %+.2fpp\n",
            decision, nrow(paired), overall$delta_mae, overall$delta_brier,
            overall$delta_logloss, 100 * overall$delta_cover))
cat("Wrote comparison to ", normalizePath(args$out), "\n", sep = "")
