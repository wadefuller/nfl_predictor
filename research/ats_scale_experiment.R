# ATS R1.3 — conditional residual-scale evaluation on frozen margins.
# Usage: Rscript research/ats_scale_experiment.R --run <walk-forward-dir> --out <result-dir>

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
clamp_probability <- function(p) pmin(pmax(p, 1e-6), 1 - 1e-6)

parse_args <- function(argv) {
  out <- list(bootstrap = 2000L)
  i <- 1L
  while (i <= length(argv)) {
    key <- argv[[i]]
    if (!key %in% c("--run", "--out", "--bootstrap")) stop("Unknown argument: ", key)
    if (i == length(argv)) stop("Missing value for ", key)
    value <- argv[[i + 1L]]
    out[[sub("^--", "", key)]] <- if (key == "--bootstrap") as.integer(value) else value
    i <- i + 2L
  }
  missing <- setdiff(c("run", "out"), names(out))
  if (length(missing)) stop("Missing arguments: --", paste(missing, collapse = ", --"))
  out
}

add_scale_features <- function(x) {
  x %>% mutate(
    abs_spread = abs(spread_line),
    spread_bucket = cut(abs_spread, c(-Inf, 2.5, 6.5, Inf),
                        labels = c("close", "medium", "large"), right = TRUE),
    total_bucket = cut(total_line, c(-Inf, 43.5, 48.5, Inf),
                       labels = c("low", "medium", "high"), right = TRUE),
    qb_uncertainty = coalesce(new_starter_home, 0) + coalesce(new_starter_away, 0) +
      as.integer(coalesce(starter_games_team_season_home, 99) < 3) +
      as.integer(coalesce(starter_games_team_season_away, 99) < 3) +
      coalesce(inj_qb_starter_out_home, 0) + coalesce(inj_qb_starter_out_away, 0),
    qb_regime = if_else(qb_uncertainty > 0, "uncertain", "stable"),
    rest_asymmetry = abs(home_team_rest - away_team_rest),
    outdoor = 1 - coalesce(is_dome, 0),
    temp_extreme = outdoor * abs(temp - 60),
    wind_effect = outdoor * wind,
    late_season = pmax(week - 12, 0),
    bucket_key = interaction(spread_bucket, total_bucket, drop = TRUE),
    conformal_key = interaction(spread_bucket, qb_regime, drop = TRUE)
  )
}

normal_predictions <- function(holdout, sigma, arm) {
  q50 <- qnorm(0.75); q80 <- qnorm(0.90); q90 <- qnorm(0.95)
  holdout %>% mutate(
    arm = arm, conditional_sigma = sigma, student_df = NA_real_,
    cover_probability = pnorm((predicted_margin - spread_line) / sigma),
    lo50 = predicted_margin - q50 * sigma, hi50 = predicted_margin + q50 * sigma,
    lo80 = predicted_margin - q80 * sigma, hi80 = predicted_margin + q80 * sigma,
    lo90 = predicted_margin - q90 * sigma, hi90 = predicted_margin + q90 * sigma
  )
}

bucket_sigmas <- function(calibration, holdout, global_sigma, prior_n = 75) {
  cell <- calibration %>% group_by(bucket_key) %>% summarise(
    n = n(), second_moment = mean(residual^2), .groups = "drop"
  )
  holdout %>% left_join(cell, by = "bucket_key") %>% transmute(
    sigma = sqrt((coalesce(n, 0) * coalesce(second_moment, global_sigma^2) +
                    prior_n * global_sigma^2) / (coalesce(n, 0) + prior_n))
  ) %>% pull(sigma)
}

variance_glm_sigmas <- function(calibration, holdout, global_sigma) {
  vars <- c("abs_spread", "total_line", "qb_uncertainty", "rest_asymmetry",
            "outdoor", "temp_extreme", "wind_effect", "late_season")
  cal <- calibration; out <- holdout
  for (v in vars) {
    med <- median(cal[[v]], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    cal[[v]][!is.finite(cal[[v]])] <- med
    out[[v]][!is.finite(out[[v]])] <- med
    center <- mean(cal[[v]]); scale <- sd(cal[[v]])
    if (!is.finite(scale) || scale == 0) scale <- 1
    cal[[paste0("z_", v)]] <- (cal[[v]] - center) / scale
    out[[paste0("z_", v)]] <- (out[[v]] - center) / scale
  }
  cal$variance_target <- pmax(cal$residual^2, 0.25)
  fit <- suppressWarnings(glm(reformulate(paste0("z_", vars), "variance_target"),
                              data = cal, family = Gamma(link = "log")))
  raw_cal <- sqrt(pmax(predict(fit, newdata = cal, type = "response"), 0.25))
  raw_out <- sqrt(pmax(predict(fit, newdata = out, type = "response"), 0.25))
  normalizer <- global_sigma / sqrt(mean(raw_cal^2))
  pmin(pmax(raw_out * normalizer, 0.60 * global_sigma), 1.70 * global_sigma)
}

fit_student_t <- function(residual) {
  objective <- function(theta) {
    scale <- exp(theta[[1]]); df <- 2 + exp(theta[[2]])
    -sum(dt(residual / scale, df = df, log = TRUE) - log(scale))
  }
  fit <- optim(c(log(sd(residual) * 1.05), log(6)), objective,
               method = "L-BFGS-B", lower = c(log(1), log(0.1)),
               upper = c(log(40), log(48)))
  list(scale = exp(fit$par[[1]]), df = 2 + exp(fit$par[[2]]),
       convergence = fit$convergence)
}

student_predictions <- function(holdout, fit) {
  scale <- fit$scale; df <- fit$df
  q50 <- qt(0.75, df); q80 <- qt(0.90, df); q90 <- qt(0.95, df)
  holdout %>% mutate(
    arm = "student_t", conditional_sigma = scale, student_df = df,
    cover_probability = pt((predicted_margin - spread_line) / scale, df = df),
    lo50 = predicted_margin - q50 * scale, hi50 = predicted_margin + q50 * scale,
    lo80 = predicted_margin - q80 * scale, hi80 = predicted_margin + q80 * scale,
    lo90 = predicted_margin - q90 * scale, hi90 = predicted_margin + q90 * scale
  )
}

conformal_predictions <- function(calibration, holdout, prior_n = 75) {
  global_residual <- calibration$residual
  groups <- split(calibration$residual, calibration$conformal_key)
  probability_one <- function(threshold, key) {
    group <- groups[[as.character(key)]] %||% numeric(0)
    global_p <- (sum(global_residual > threshold) + 0.5) / (length(global_residual) + 1)
    if (!length(group)) return(global_p)
    group_p <- (sum(group > threshold) + 0.5) / (length(group) + 1)
    (length(group) * group_p + prior_n * global_p) / (length(group) + prior_n)
  }
  quantile_one <- function(probability, key) {
    group <- groups[[as.character(key)]] %||% numeric(0)
    global_q <- unname(quantile(global_residual, probability, type = 8))
    if (!length(group)) return(global_q)
    group_q <- unname(quantile(group, probability, type = 8))
    (length(group) * group_q + prior_n * global_q) / (length(group) + prior_n)
  }
  threshold <- holdout$spread_line - holdout$predicted_margin
  keys <- holdout$conformal_key
  q <- function(p) map2_dbl(rep(p, length(keys)), keys, quantile_one)
  holdout %>% mutate(
    arm = "conformal_regime", conditional_sigma = NA_real_, student_df = NA_real_,
    cover_probability = map2_dbl(threshold, keys, probability_one),
    lo50 = predicted_margin + q(0.25), hi50 = predicted_margin + q(0.75),
    lo80 = predicted_margin + q(0.10), hi80 = predicted_margin + q(0.90),
    lo90 = predicted_margin + q(0.05), hi90 = predicted_margin + q(0.95)
  )
}

score_probability <- function(x) {
  scored <- x %>% filter(!is.na(home_cover)) %>% mutate(
    p = clamp_probability(cover_probability),
    brier_row = (p - home_cover)^2,
    logloss_row = -(home_cover * log(p) + (1 - home_cover) * log(1 - p)),
    probability_band = cut(p, seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE)
  )
  slope_fit <- suppressWarnings(tryCatch(
    glm(home_cover ~ qlogis(p), data = scored, family = binomial()),
    error = function(e) NULL
  ))
  reliability <- scored %>% group_by(probability_band) %>% summarise(
    games = n(), mean_probability = mean(p), observed = mean(home_cover), .groups = "drop"
  )
  tibble(
    games = nrow(scored), brier = mean(scored$brier_row),
    logloss = mean(scored$logloss_row),
    ece = weighted.mean(abs(reliability$observed - reliability$mean_probability),
                        reliability$games),
    calibration_intercept = if (is.null(slope_fit)) NA_real_ else unname(coef(slope_fit)[[1]]),
    calibration_slope = if (is.null(slope_fit)) NA_real_ else unname(coef(slope_fit)[[2]])
  )
}

reliability_rows <- function(x) {
  x %>% filter(!is.na(home_cover)) %>% mutate(
    p = clamp_probability(cover_probability),
    probability_band = cut(p, seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE)
  ) %>% group_by(arm, probability_band) %>% summarise(
    games = n(), mean_probability = mean(p), observed_cover_rate = mean(home_cover),
    .groups = "drop"
  )
}

interval_rows <- function(x) {
  map_dfr(c(50, 80, 90), function(level) {
    lo <- x[[paste0("lo", level)]]; hi <- x[[paste0("hi", level)]]
    tibble(nominal_coverage = level / 100,
           empirical_coverage = mean(x$home_result >= lo & x$home_result <= hi),
           mean_width = mean(hi - lo))
  })
}

fit_fold <- function(calibration, holdout) {
  cal <- add_scale_features(calibration); out <- add_scale_features(holdout)
  global_sigma <- sd(cal$residual) * 1.05
  incumbent <- normal_predictions(out, global_sigma, "global_normal")
  if (max(abs(incumbent$cover_probability - out$global_cover_probability)) > 1e-10) {
    stop("Incumbent cover probability was not reproduced")
  }
  bucket_sigma <- bucket_sigmas(cal, out, global_sigma)
  variance_sigma <- variance_glm_sigmas(cal, out, global_sigma)
  student_fit <- fit_student_t(cal$residual)
  list(
    predictions = bind_rows(
      incumbent,
      normal_predictions(out, bucket_sigma, "bucket_shrinkage"),
      normal_predictions(out, variance_sigma, "variance_glm"),
      student_predictions(out, student_fit),
      conformal_predictions(cal, out)
    ),
    diagnostics = tibble(
      holdout_season = unique(out$holdout_season), calibration_games = nrow(cal),
      global_sigma = global_sigma, student_scale = student_fit$scale,
      student_df = student_fit$df, student_convergence = student_fit$convergence,
      bucket_sigma_min = min(bucket_sigma), bucket_sigma_max = max(bucket_sigma),
      variance_sigma_min = min(variance_sigma), variance_sigma_max = max(variance_sigma)
    )
  )
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
calibration_path <- file.path(args$run, "scale_calibration_rows.csv")
holdout_path <- file.path(args$run, "scale_holdout_rows.csv")
metadata_path <- file.path(args$run, "run_metadata.json")
if (!file.exists(calibration_path) || !file.exists(holdout_path)) {
  stop("Run directory is missing scale calibration/holdout rows: ", args$run)
}
calibration <- read_csv(calibration_path, show_col_types = FALSE)
holdout <- read_csv(holdout_path, show_col_types = FALSE)
metadata <- if (file.exists(metadata_path)) fromJSON(metadata_path) else list(profile = "unknown")
seasons <- sort(intersect(unique(calibration$holdout_season), unique(holdout$holdout_season)))
fits <- map(seasons, function(season) fit_fold(
  calibration %>% filter(holdout_season == season),
  holdout %>% filter(holdout_season == season)
))
predictions <- bind_rows(map(fits, "predictions"))
diagnostics <- bind_rows(map(fits, "diagnostics"))

margin_check <- predictions %>% group_by(game_id) %>%
  summarise(n_margin = n_distinct(predicted_margin), .groups = "drop")
if (any(margin_check$n_margin != 1L)) stop("A scale arm changed the frozen margin")

overall <- predictions %>% group_by(arm) %>% group_modify(~score_probability(.x)) %>% ungroup()
by_season <- predictions %>% group_by(arm, holdout_season) %>%
  group_modify(~score_probability(.x)) %>% ungroup()
reliability <- reliability_rows(predictions)
intervals <- predictions %>% group_by(arm) %>% group_modify(~interval_rows(.x)) %>% ungroup()

incumbent <- predictions %>% filter(arm == "global_normal", !is.na(home_cover)) %>%
  select(game_id, holdout_season, week, home_cover,
         incumbent_probability = cover_probability)
paired <- predictions %>% filter(arm != "global_normal") %>%
  select(game_id, arm, candidate_probability = cover_probability) %>%
  inner_join(incumbent, by = "game_id") %>% mutate(
    incumbent_probability = clamp_probability(incumbent_probability),
    candidate_probability = clamp_probability(candidate_probability),
    delta_brier = (candidate_probability - home_cover)^2 -
      (incumbent_probability - home_cover)^2,
    delta_logloss = -(home_cover * log(candidate_probability) +
      (1 - home_cover) * log(1 - candidate_probability)) +
      home_cover * log(incumbent_probability) +
      (1 - home_cover) * log(1 - incumbent_probability),
    season_week = interaction(holdout_season, week, drop = TRUE)
  )
block_variable <- if (length(seasons) >= 6L) "holdout_season" else "season_week"
set.seed(989)
bootstrap <- paired %>% group_split(arm) %>% map_dfr(function(x) {
  arm_name <- first(x$arm); blocks <- split(seq_len(nrow(x)), x[[block_variable]])
  draws <- map_dfr(seq_len(args$bootstrap), function(iteration) {
    picked <- sample(names(blocks), length(blocks), replace = TRUE)
    z <- x[unlist(blocks[picked], use.names = FALSE), ]
    tibble(iteration = iteration, delta_brier = mean(z$delta_brier),
           delta_logloss = mean(z$delta_logloss))
  })
  draws %>% pivot_longer(starts_with("delta_"), names_to = "metric") %>%
    group_by(metric) %>% summarise(
      arm = arm_name, estimate = mean(value), lo90 = quantile(value, 0.05),
      hi90 = quantile(value, 0.95), probability_improvement = mean(value < 0),
      .groups = "drop"
    )
})

inc_overall <- overall %>% filter(arm == "global_normal")
decisions <- overall %>% filter(arm != "global_normal") %>% rowwise() %>% mutate(
  delta_brier = brier - inc_overall$brier,
  delta_logloss = logloss - inc_overall$logloss,
  delta_ece = ece - inc_overall$ece,
  brier_seasons_better = sum(
    by_season$brier[by_season$arm == arm] <
      by_season$brier[by_season$arm == "global_normal"]
  ),
  slope_distance_change = abs(calibration_slope - 1) -
    abs(inc_overall$calibration_slope - 1),
  decision = case_when(
    metadata$profile == "probe" & delta_brier > 0.001 & delta_logloss > 0 ~ "stop_for_futility",
    metadata$profile == "probe" ~ "advance_to_screen",
    metadata$profile == "screen" & delta_brier < 0 & delta_logloss <= 0 &
      brier_seasons_better >= 3 & delta_ece <= 0.005 & slope_distance_change <= 0 ~
      "advance_to_confirm",
    metadata$profile == "screen" ~ "stop_after_screen",
    metadata$profile == "confirm" & delta_brier < 0 & delta_logloss <= 0 &
      brier_seasons_better >= 6 &
      bootstrap$hi90[bootstrap$arm == arm & bootstrap$metric == "delta_brier"] < 0 ~
      "promotion_candidate",
    metadata$profile == "confirm" ~ "do_not_promote",
    TRUE ~ "diagnostic_only"
  )
) %>% ungroup()

dir.create(args$out, recursive = TRUE, showWarnings = FALSE)
write_csv(predictions, file.path(args$out, "scale_predictions.csv"))
write_csv(diagnostics, file.path(args$out, "fold_diagnostics.csv"))
write_csv(overall, file.path(args$out, "overall.csv"))
write_csv(by_season, file.path(args$out, "by_season.csv"))
write_csv(reliability, file.path(args$out, "reliability.csv"))
write_csv(intervals, file.path(args$out, "interval_coverage.csv"))
write_csv(bootstrap, file.path(args$out, "bootstrap.csv"))
write_csv(decisions, file.path(args$out, "decisions.csv"))
write_json(list(profile = metadata$profile %||% "unknown", seasons = seasons,
                block_unit = block_variable, margins_frozen = TRUE,
                bootstrap_replicates = args$bootstrap),
           file.path(args$out, "metadata.json"), pretty = TRUE, auto_unbox = TRUE)

theme_ats <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
ggsave(file.path(args$out, "reliability.png"),
       ggplot(reliability, aes(mean_probability, observed_cover_rate)) +
         geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey55") +
         geom_line(linewidth = 0.7, colour = "#2f7ed8") +
         geom_point(aes(size = games), colour = "#2f7ed8") +
         facet_wrap(~arm) + guides(size = "none") +
         coord_equal(xlim = c(0.2, 0.8), ylim = c(0.2, 0.8)) +
         labs(title = "Cover-probability reliability", x = "Predicted probability",
              y = "Observed home-cover rate") + theme_ats,
       width = 8.5, height = 5.2, dpi = 160)
delta_plot <- by_season %>% select(arm, holdout_season, brier) %>%
  left_join(by_season %>% filter(arm == "global_normal") %>%
              select(holdout_season, incumbent_brier = brier), by = "holdout_season") %>%
  filter(arm != "global_normal") %>% mutate(improvement = incumbent_brier - brier)
ggsave(file.path(args$out, "brier_gain_by_season.png"),
       ggplot(delta_plot, aes(factor(holdout_season), improvement,
                              fill = improvement > 0)) +
         geom_col(show.legend = FALSE) + facet_wrap(~arm, scales = "free_y") +
         geom_hline(yintercept = 0, colour = "grey50") +
         scale_fill_manual(values = c("#d95f59", "#39a96b")) +
         labs(title = "Conditional-scale Brier gain by season", x = NULL,
              y = "Brier improvement (positive is better)") + theme_ats,
       width = 10, height = 6.2, dpi = 160)

print(decisions %>% select(arm, delta_brier, delta_logloss, delta_ece,
                           brier_seasons_better, decision))
cat("Wrote ATS R1.3 results to ", normalizePath(args$out), "\n", sep = "")
