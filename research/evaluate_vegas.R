# ============================================
# NFL Predictor: Evaluate Model vs Vegas
# ============================================
# Compares the nfl_predictor model to Vegas on spread (margin MAE, cover accuracy)
# and win probability (Brier score, log loss). Outputs summary tables and
# time-series CSV for plotting performance over time.
#
# Usage: Rscript research/evaluate_vegas.R
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)
})

source("~/Projects/R/nfl_predictor/ensemble_lib.R")

# ---- Configuration ----
# PROJECT_DIR / DATA_DIR / MODELS_DIR come from ensemble_lib.R.
OUTPUT_DIR  <- DATA_DIR
BUNDLE_PATH <- file.path(MODELS_DIR, "ensemble_latest.rds")

# Run ensemble + merge predictions back to raw frame
make_predictions <- function(data, bundle) {
  preds <- predict_with_ensemble(bundle, data) %>%
    select(game_id, nfl_predictor_line, line_diff, pred_win_pct)
  data %>% left_join(preds, by = "game_id")
}

# Brier score: mean((pred - actual)^2). Actual = 0/1.
brier_score <- function(pred, actual) {
  mean((pred - actual)^2, na.rm = TRUE)
}

# Log loss: -mean(actual * log(p) + (1-actual) * log(1-p)). Clamp p to avoid log(0).
log_loss_binary <- function(pred, actual, eps = 1e-15) {
  p <- pmin(pmax(pred, eps), 1 - eps)
  -mean(actual * log(p) + (1 - actual) * log(1 - p), na.rm = TRUE)
}

# ---- Load data and bundle ----
cat("Loading data and ensemble bundle...\n")
game_w_features <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
bundle <- readRDS(BUNDLE_PATH)
cat(sprintf("  Bundle trained %s on %d games (seasons %d-%d)\n",
            bundle$metadata$trained_date,
            bundle$metadata$n_training_games,
            bundle$metadata$training_seasons[1],
            bundle$metadata$training_seasons[2]))

# ---- Filter to completed games (2008+ matches min training season) ----
completed <- game_w_features %>%
  filter(!is.na(home_result), season >= 2008)
completed_with_impl <- completed %>%
  filter(!is.na(impl_prob_home))

cat(sprintf("Completed games (all): %d\n", nrow(completed)))
cat(sprintf("Completed games with impl_prob_home: %d\n", nrow(completed_with_impl)))

if (nrow(completed) == 0) stop("No completed games found. Exiting.")

# ---- Generate predictions ----
# NOTE: evaluation is partially in-sample because the bundle was trained on
# the same data we're evaluating against. For honest OOS metrics use the
# walk_forward_eval.R outputs in walk_forward_model_*.csv.
cat("Generating ensemble predictions...\n")
preds_all <- make_predictions(completed, bundle)

# Join impl_prob_home for games that have it (same row order as completed)
preds_all <- preds_all %>%
  mutate(
    # Spread: actual margin = home_result
    margin_mae_model = abs(nfl_predictor_line - home_result),
    margin_mae_vegas = abs(spread_line - home_result),
    line_diff = nfl_predictor_line - spread_line,
    spread_winner = if_else(home_result - spread_line > 0, home_team, away_team),
    pred_spread_adj_winner = if_else(line_diff > 0, home_team, away_team),
    # Vegas favorite: spread_line > 0 means home is favored in this dataset
    # (verified empirically — home favorites cover ~49%, dogs ~47%, both near 50%)
    veg_pred_winner = if_else(spread_line > 0, home_team, away_team),
    cover_correct_model = as.integer(pred_spread_adj_winner == spread_winner),
    cover_correct_vegas = as.integer(veg_pred_winner == spread_winner),
    # Win prob: home_win 0/1 for Brier/log loss
    home_win_01 = as.integer(home_win == 1)
  )

# Win-prob metrics only on games with impl_prob_home
preds_impl <- preds_all %>%
  filter(!is.na(impl_prob_home)) %>%
  mutate(
    brier_model = (pred_win_pct - home_win_01)^2,
    brier_vegas = (impl_prob_home - home_win_01)^2,
    logloss_model = -(
      home_win_01 * log(pmin(pmax(pred_win_pct, 1e-15), 1 - 1e-15)) +
        (1 - home_win_01) * log(1 - pmin(pmax(pred_win_pct, 1e-15), 1 - 1e-15))
    ),
    logloss_vegas = -(
      home_win_01 * log(pmin(pmax(impl_prob_home, 1e-15), 1 - 1e-15)) +
        (1 - home_win_01) * log(1 - pmin(pmax(impl_prob_home, 1e-15), 1 - 1e-15))
    )
  )

# ---- Summary: overall ----
overall_spread <- preds_all %>%
  summarise(
    games = n(),
    model_margin_mae = mean(margin_mae_model),
    vegas_margin_mae = mean(margin_mae_vegas),
    model_cover_accuracy = mean(cover_correct_model),
    vegas_cover_accuracy = mean(cover_correct_vegas),
    .groups = "drop"
  )

overall_winprob <- preds_impl %>%
  summarise(
    games = n(),
    model_brier = brier_score(pred_win_pct, home_win_01),
    vegas_brier = brier_score(impl_prob_home, home_win_01),
    model_logloss = log_loss_binary(pred_win_pct, home_win_01),
    vegas_logloss = log_loss_binary(impl_prob_home, home_win_01),
    .groups = "drop"
  )

cat("\n=== Overall (all completed games) ===\n")
cat("Spread:\n")
print(overall_spread)
cat("\nWin probability (games with impl_prob_home):\n")
print(overall_winprob)

# ---- Summary: by season ----
by_season_spread <- preds_all %>%
  group_by(season) %>%
  summarise(
    games = n(),
    model_margin_mae = mean(margin_mae_model),
    vegas_margin_mae = mean(margin_mae_vegas),
    model_cover_accuracy = mean(cover_correct_model),
    vegas_cover_accuracy = mean(cover_correct_vegas),
    .groups = "drop"
  )

by_season_winprob <- preds_impl %>%
  group_by(season) %>%
  summarise(
    games = n(),
    model_brier = brier_score(pred_win_pct, home_win_01),
    vegas_brier = brier_score(impl_prob_home, home_win_01),
    model_logloss = log_loss_binary(pred_win_pct, home_win_01),
    vegas_logloss = log_loss_binary(impl_prob_home, home_win_01),
    .groups = "drop"
  )

cat("\n=== By season (spread) ===\n")
print(by_season_spread)
cat("\n=== By season (win probability) ===\n")
print(by_season_winprob)

# ---- Weekly and cumulative (for time series) ----
# Spread: by season-week
weekly_spread <- preds_all %>%
  group_by(season, week) %>%
  arrange(season, week) %>%
  summarise(
    games = n(),
    wk_model_margin_mae = mean(margin_mae_model),
    wk_vegas_margin_mae = mean(margin_mae_vegas),
    wk_model_cover_accuracy = mean(cover_correct_model),
    wk_vegas_cover_accuracy = mean(cover_correct_vegas),
    .groups = "drop"
  ) %>%
  mutate(
    cml_gms = cumsum(games),
    cml_model_margin_mae = cumsum(wk_model_margin_mae * games) / cml_gms,
    cml_vegas_margin_mae = cumsum(wk_vegas_margin_mae * games) / cml_gms,
    cml_model_cover_correct = cumsum(wk_model_cover_accuracy * games),
    cml_vegas_cover_correct = cumsum(wk_vegas_cover_accuracy * games),
    cml_model_cover_accuracy = cml_model_cover_correct / cml_gms,
    cml_vegas_cover_accuracy = cml_vegas_cover_correct / cml_gms
  )

# Win prob: by season-week (only games with impl_prob_home)
weekly_winprob <- preds_impl %>%
  group_by(season, week) %>%
  arrange(season, week) %>%
  summarise(
    games = n(),
    wk_model_brier = mean(brier_model),
    wk_vegas_brier = mean(brier_vegas),
    wk_model_logloss = mean(logloss_model),
    wk_vegas_logloss = mean(logloss_vegas),
    .groups = "drop"
  ) %>%
  mutate(
    cml_gms = cumsum(games),
    cml_model_brier = cumsum(wk_model_brier * games) / cml_gms,
    cml_vegas_brier = cumsum(wk_vegas_brier * games) / cml_gms,
    cml_model_logloss = cumsum(wk_model_logloss * games) / cml_gms,
    cml_vegas_logloss = cumsum(wk_vegas_logloss * games) / cml_gms
  )

# ---- Time-series long format for plotting ----
# Spread: margin_mae and cover_accuracy, weekly and cumulative
ts_spread_weekly <- weekly_spread %>%
  select(season, week, games,
         wk_model_margin_mae, wk_vegas_margin_mae,
         wk_model_cover_accuracy, wk_vegas_cover_accuracy) %>%
  pivot_longer(
    cols = -c(season, week, games),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    period = "weekly",
    metric = str_remove(metric, "^wk_"),
    source = if_else(str_detect(metric, "model"), "model", "vegas"),
    metric = str_remove(metric, "^(model|vegas)_")
  )

ts_spread_cml <- weekly_spread %>%
  select(season, week, games,
         cml_model_margin_mae, cml_vegas_margin_mae,
         cml_model_cover_accuracy, cml_vegas_cover_accuracy) %>%
  pivot_longer(
    cols = -c(season, week, games),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    period = "cumulative",
    metric = str_remove(metric, "^cml_"),
    source = if_else(str_detect(metric, "model"), "model", "vegas"),
    metric = str_remove(metric, "^(model|vegas)_")
  )

# Win prob: brier and logloss, weekly and cumulative
ts_winprob_weekly <- weekly_winprob %>%
  select(season, week, games,
         wk_model_brier, wk_vegas_brier,
         wk_model_logloss, wk_vegas_logloss) %>%
  pivot_longer(
    cols = -c(season, week, games),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    period = "weekly",
    metric = str_remove(metric, "^wk_"),
    source = if_else(str_detect(metric, "model"), "model", "vegas"),
    metric = str_remove(metric, "^(model|vegas)_")
  )

ts_winprob_cml <- weekly_winprob %>%
  select(season, week, games,
         cml_model_brier, cml_vegas_brier,
         cml_model_logloss, cml_vegas_logloss) %>%
  pivot_longer(
    cols = -c(season, week, games),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    period = "cumulative",
    metric = str_remove(metric, "^cml_"),
    source = if_else(str_detect(metric, "model"), "model", "vegas"),
    metric = str_remove(metric, "^(model|vegas)_")
  )

# Single time-series table: season, week, period, metric, source, value, games
vegas_evaluation_timeseries <- bind_rows(
  ts_spread_weekly %>% mutate(metric_type = "spread"),
  ts_spread_cml %>% mutate(metric_type = "spread"),
  ts_winprob_weekly %>% mutate(metric_type = "win_prob"),
  ts_winprob_cml %>% mutate(metric_type = "win_prob")
) %>%
  select(season, week, period, metric_type, metric, source, value, games)

# ---- Write outputs ----
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

summary_overall_file <- file.path(OUTPUT_DIR, "vegas_evaluation_summary_overall.csv")
summary_by_season_file <- file.path(OUTPUT_DIR, "vegas_evaluation_summary_by_season.csv")
timeseries_file <- file.path(OUTPUT_DIR, "vegas_evaluation_timeseries.csv")

# Overall: one row for spread, one for winprob (wide)
overall_export <- bind_rows(
  overall_spread %>% mutate(metric_type = "spread"),
  overall_winprob %>% mutate(metric_type = "win_prob")
)
write_csv(overall_export, summary_overall_file)
cat(sprintf("\nWrote %s\n", summary_overall_file))

# By season: spread and winprob in one CSV
by_season_spread %>%
  mutate(metric_type = "spread") %>%
  bind_rows(by_season_winprob %>% mutate(metric_type = "win_prob")) %>%
  write_csv(summary_by_season_file)
cat(sprintf("Wrote %s\n", summary_by_season_file))

write_csv(vegas_evaluation_timeseries, timeseries_file)
cat(sprintf("Wrote %s\n", timeseries_file))

cat("\nDone. Summary tables and vegas_evaluation_timeseries.csv written to", OUTPUT_DIR, "\n")
