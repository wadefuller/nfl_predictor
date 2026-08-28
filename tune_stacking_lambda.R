# ============================================
# NFL Predictor: Stacking λ Sweep (multi-objective mix)
# ============================================
# Currently STACKING_LAMBDA = 0.5 (50/50 RMSE vs LogLoss in the multi-objective
# NNLS fit). Now that all 3 base learners are individually tuned, see if a
# different λ produces a better cal-set Brier.
#
# Approach (cheap): one train/val split mirroring train_and_save_ensemble's
# pre-cal/cal structure. Train all 3 base learners ONCE on 2008-2023, predict
# on 2024-2025, then refit stacking with each λ ∈ {0, 0.25, 0.5, 0.75, 1.0}
# and score by Brier on val.
#
# Output: data/tune_best_lambda.rds with $lambda + sweep_results.
# Usage: Rscript tune_stacking_lambda.R   (~3 min)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")

LAMBDA_GRID <- c(0.0, 0.25, 0.5, 0.75, 1.0)
TRAIN_LAST  <- 2023
VAL_START   <- 2024

cat("Loading features...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
completed <- g %>% filter(!is.na(home_result), !is.na(home_moneyline),
                           (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
                           season >= 2008)
train_raw <- completed %>% filter(season <= TRAIN_LAST)
val_raw   <- completed %>% filter(season >= VAL_START)
cat(sprintf("Train: %d rows  Val: %d rows\n", nrow(train_raw), nrow(val_raw)))

# Load tuned params (matches train_and_save_ensemble)
REG_PARAMS <- load_tuned_xgb_params(file.path(DATA_DIR, "tune_best_params_reg.rds"))
GLM_ALPHA  <- load_tuned_glm_alpha()
RF_PARAMS  <- load_tuned_rf_params()

train_prep <- prep_for_train(train_raw)
val_prep   <- prep_for_train(val_raw)

cat("\nTraining base learners on 2008-2023...\n")
t0 <- Sys.time()
xgb_fit <- train_xgb_model(train_prep, params = REG_PARAMS)
cat(sprintf("  XGB done in %.1fs\n", as.numeric(Sys.time() - t0)))
t0 <- Sys.time()
glm_fit <- train_glm_model(train_prep, alpha = GLM_ALPHA)
cat(sprintf("  GLM done in %.1fs\n", as.numeric(Sys.time() - t0)))
t0 <- Sys.time()
rf_fit  <- train_rf_model(train_prep, rf_params = RF_PARAMS)
cat(sprintf("  RF done in %.1fs\n", as.numeric(Sys.time() - t0)))

cat("\nPredicting on 2024-2025 val set...\n")
xgb_pred <- predict(xgb_fit, new_data = val_prep)$.pred
glm_pred <- predict_glm(glm_fit, val_prep)
rf_pred  <- predict(rf_fit,  new_data = val_prep)$.pred
actual   <- val_prep$home_result

cat(sprintf("\n=== Stacking λ sweep on %d val games ===\n", length(actual)))
results <- map_dfr(LAMBDA_GRID, function(lam) {
  stack <- fit_stacking_multi(xgb_pred, glm_pred, rf_pred, actual, lambda = lam)
  pred  <- apply_stacking(stack, xgb_pred, glm_pred, rf_pred)
  sigma <- sd(actual - pred)
  wp    <- pnorm(pred / sigma)
  win   <- as.integer(actual > 0)
  mae   <- mean(abs(pred - actual))
  brier <- mean((wp - win)^2)
  cat(sprintf("  λ=%.2f  weights: %.2f·xgb + %.2f·glm + %.2f·rf  MAE=%.3f  Brier=%.4f\n",
              lam, stack$w_xgb, stack$w_glm, stack$w_rf, mae, brier))
  tibble(lambda = lam,
         w_xgb = stack$w_xgb, w_glm = stack$w_glm, w_rf = stack$w_rf,
         intercept = stack$intercept,
         mae = mae, brier = brier)
})

# Default objective is Brier (per "rank by core metrics" philosophy). Override
# with STACKING_TUNE_OBJECTIVE=mae if you specifically want to minimize MAE
# (e.g. for spread-prediction-focused use cases — the trade-off is slightly
# worse probability calibration).
objective <- Sys.getenv("STACKING_TUNE_OBJECTIVE", "brier")
stopifnot(objective %in% c("brier", "mae", "logloss"))

best_by <- function(metric) results %>% arrange(.data[[metric]]) %>% slice(1)
winner_brier <- best_by("brier")
winner_mae   <- best_by("mae")

cat(sprintf("\nBest λ by Brier: %.2f  (Brier=%.4f, MAE=%.3f)\n",
            winner_brier$lambda, winner_brier$brier, winner_brier$mae))
cat(sprintf("Best λ by MAE  : %.2f  (Brier=%.4f, MAE=%.3f)\n",
            winner_mae$lambda, winner_mae$brier, winner_mae$mae))
winner <- switch(objective,
                 brier   = winner_brier,
                 mae     = winner_mae,
                 logloss = best_by("logloss"))

cat(sprintf("\nSaving winner by %s → λ=%.2f\n", objective, winner$lambda))

out <- list(lambda = winner$lambda, sweep_results = results,
            fit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
saveRDS(out, file.path(DATA_DIR, "tune_best_lambda.rds"))
cat(sprintf("\nWrote %s\n", file.path(DATA_DIR, "tune_best_lambda.rds")))
