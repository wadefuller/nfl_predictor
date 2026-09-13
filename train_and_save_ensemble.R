# ============================================
# NFL Predictor: Train + Save Production Ensemble
# ============================================
# Trains the production ensemble (XGB + cv.glmnet + ranger RF, NNLS-stacked,
# Platt-recalibrated) on the full training window and saves a self-contained
# bundle for use in predict_week.rmd and export_json.R.
#
# Two-stage training (mirrors walk_forward_eval.R per-fold logic):
#   1. Pre-cal: train all three on [MIN_TRAIN, CAL_SEASON-1], predict on CAL_SEASON
#      to learn NNLS stacking weights + Platt scaling
#   2. Final:   retrain all three on [MIN_TRAIN, CAL_SEASON] with same hyperparams
#
# Saves: models/ensemble_<DATE>.rds  AND  models/ensemble_latest.rds (symlink)
#
# Usage: Rscript train_and_save_ensemble.R
# Runtime: ~3 min (single set of pre-cal + final fits)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
suppressPackageStartupMessages(library(jsonlite))   # read ev_calibration.json

# PROJECT_DIR / DATA_DIR / MODELS_DIR come from ensemble_lib.R.
TUNED_PARAMS <- file.path(DATA_DIR, "tune_best_params_reg.rds")

MIN_TRAIN_SEASON   <- 2008
CAL_WINDOW_SEASONS <- 2        # most-recent training seasons used for stacking + Platt
STACKING_METHOD    <- "multi"  # "nnls" (margin RMSE), "logloss" (winprob), or "multi" (both)
STACKING_LAMBDA    <- load_tuned_stacking_lambda()  # tuned by tune_stacking_lambda.R
SEED               <- 989

# Tuned XGB params loaded via shared helper in ensemble_lib.R
REG_PARAMS <- load_tuned_xgb_params(TUNED_PARAMS)

# ---- Load features and split ---------------------------------------------

cat("Loading feature data...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))

# Train on the latest available complete data. Use all completed games up to
# the most recent fully-played season. CAL_SEASON is the most recent one,
# pre-cal trains on everything before it.
completed <- g %>% filter(!is.na(home_result))
max_season  <- max(completed$season)
cat(sprintf("Most recent completed season: %d\n", max_season))

train_raw   <- g %>% filter(season >= MIN_TRAIN_SEASON,
                            season <= max_season,
                            !is.na(home_result))
cal_seasons <- (max(train_raw$season) - CAL_WINDOW_SEASONS + 1):max(train_raw$season)
pre_cal_raw <- train_raw %>% filter(!season %in% cal_seasons)
cal_raw     <- train_raw %>% filter( season %in% cal_seasons)

cat(sprintf("Training window: %d–%d (%d games)\n",
            MIN_TRAIN_SEASON, max(cal_seasons), nrow(train_raw)))
cat(sprintf("Pre-cal window:  %d–%d (%d games)\n",
            MIN_TRAIN_SEASON, min(cal_seasons) - 1, nrow(pre_cal_raw)))
cat(sprintf("Cal seasons:     %s (%d games)\n",
            paste(cal_seasons, collapse = ", "), nrow(cal_raw)))

# ---- Stage 1: pre-cal training + stacking + Platt -----------------------

cat("\n[Stage 1] Pre-cal training XGB...\n"); t0 <- Sys.time()
pre_cal_prep <- prep_for_train(pre_cal_raw)
pre_cal_xgb  <- train_xgb_model(pre_cal_prep, REG_PARAMS, SEED)
cat(sprintf("  done in %.1fs\n", as.numeric(Sys.time() - t0)))

cat("[Stage 1] Pre-cal training GLM...\n"); t0 <- Sys.time()
pre_cal_glm <- train_glm_model(pre_cal_prep, SEED)
cat(sprintf("  done in %.1fs (lambda=%.4f)\n",
            as.numeric(Sys.time() - t0), pre_cal_glm$lambda))

cat("[Stage 1] Pre-cal training RF...\n"); t0 <- Sys.time()
pre_cal_rf <- train_rf_model(pre_cal_prep, SEED)
cat(sprintf("  done in %.1fs\n", as.numeric(Sys.time() - t0)))

cat("\n[Stage 1] Predicting on cal season + fitting stacking + Platt...\n")
cal_prep   <- prep_for_train(cal_raw)
cal_xgb    <- predict(pre_cal_xgb, new_data = cal_prep)$.pred
cal_glm    <- predict_glm(pre_cal_glm, cal_prep)
cal_rf     <- predict(pre_cal_rf,  new_data = cal_prep)$.pred
cal_actual <- cal_prep$home_result

stack <- fit_stacking(cal_xgb, cal_glm, cal_rf, cal_actual,
                       method = STACKING_METHOD, lambda = STACKING_LAMBDA)
cat(sprintf("  Stacking (%s): %.2f + %.2f·xgb + %.2f·glm + %.2f·rf\n",
            stack$method, stack$intercept, stack$w_xgb, stack$w_glm, stack$w_rf))

cal_ens_pred <- apply_stacking(stack, cal_xgb, cal_glm, cal_rf)
# NOTE: tried a post-stacking margin calibration layer (isotonic + linear).
# Both rejected:
#   - Isotonic: overfit cal set (cal MAE 9.69→9.41), walk-fwd MAE regressed
#     9.84→9.99, close-game gap WIDENED to +0.425. ~100 step changes on
#     544 cal points fit fold-specific noise rather than systematic bias.
#   - Linear: OLS slope ≈ 1.0, intercept ≈ 0. No-op. Confirms ensemble
#     output has no systematic global shrinkage to recover.
# Close-game overshoot is structural — fixing it needs features that flag
# close-game scenarios, not a post-hoc calibration. Path-not-taken kept in
# ensemble_lib::fit_margin_calibrator for future experimentation.
sigma        <- sd(cal_actual - cal_ens_pred, na.rm = TRUE) * 1.05
cal_raw_wp   <- pnorm(cal_ens_pred / sigma)
platt        <- fit_platt(cal_raw_wp, as.integer(cal_actual > 0))
winprob      <- fit_winprob(cal_ens_pred, as.integer(cal_actual > 0), sigma)
cat(sprintf("  σ=%.2f, Platt: int=%.3f, slope=%.3f\n",
            sigma, platt$intercept, platt$slope))
cat(sprintf("  winprob method: %s%s\n", winprob$method,
            if (identical(winprob$method, "logistic"))
              sprintf(" (int=%.3f, slope=%.4f per pt)", winprob$intercept, winprob$slope) else ""))

# ---- Cal-set OOS metrics (proxy for production model error) ----
# cal predictions are OOS for the pre-cal models (the final model trained on
# the full window has slightly more data so true OOS error should be similar).
cal_pred_wp    <- apply_platt(cal_raw_wp, platt)
cal_actual_win <- as.integer(cal_actual > 0)
clamp_p        <- pmin(pmax(cal_pred_wp, 1e-15), 1 - 1e-15)
cal_metrics <- list(
  mae       = mean(abs(cal_ens_pred - cal_actual)),
  rmse      = sqrt(mean((cal_ens_pred - cal_actual)^2)),
  brier     = mean((cal_pred_wp - cal_actual_win)^2),
  logloss   = -mean(cal_actual_win * log(clamp_p) + (1 - cal_actual_win) * log(1 - clamp_p)),
  cover_acc = mean(sign(cal_ens_pred - 0) == sign(cal_actual))
)
cat(sprintf("  Cal-set OOS metrics: MAE=%.2f  Brier=%.4f  LogLoss=%.4f\n",
            cal_metrics$mae, cal_metrics$brier, cal_metrics$logloss))

# ---- Stage 2: final training on full window -------------------------------

cat("\n[Stage 2] Final training on full window (all 3 models)...\n"); t0 <- Sys.time()
final_prep <- prep_for_train(train_raw)
final_xgb  <- train_xgb_model(final_prep, REG_PARAMS, SEED)
final_glm  <- train_glm_model(final_prep, SEED)
final_rf   <- train_rf_model(final_prep, SEED)
cat(sprintf("  done in %.1fs\n", as.numeric(Sys.time() - t0)))

# ---- Build bundle --------------------------------------------------------

bundle <- list(
  xgb              = final_xgb,
  glm              = final_glm,
  rf               = final_rf,
  stack            = stack,
  sigma            = sigma,
  platt            = platt,
  winprob          = winprob,
  # EFFECTIVE drop list: leakage vars plus any PRUNE_BLOCKS expanded against
  # the real column names. Storing the expanded form means the bundle predicts
  # identically whether or not PRUNE_BLOCKS is set in the serving environment.
  leakage_vars     = unique(c(ENSEMBLE_LEAKAGE_VARS,
                              expand_prune_blocks(names(train_raw)))),
  metadata = list(
    trained_date     = Sys.Date(),
    training_seasons = c(MIN_TRAIN_SEASON, max(cal_seasons)),
    cal_seasons      = cal_seasons,
    n_training_games = nrow(train_raw),
    n_cal_games      = nrow(cal_raw),
    stacking_method  = STACKING_METHOD,
    glm_lambda       = pre_cal_glm$lambda,
    reg_params       = REG_PARAMS,
    cal_set_metrics  = cal_metrics
  )
)

# ---- Save ----------------------------------------------------------------

dir.create(MODELS_DIR, showWarnings = FALSE, recursive = TRUE)
dated_path  <- file.path(MODELS_DIR, sprintf("ensemble_%s.rds", Sys.Date()))
latest_path <- file.path(MODELS_DIR, "ensemble_latest.rds")
saveRDS(bundle, dated_path)
saveRDS(bundle, latest_path)

cat(sprintf("\n✓ Saved bundle:\n  %s\n  %s\n", dated_path, latest_path))
cat(sprintf("  Size: %.1f MB\n", file.info(dated_path)$size / 1024^2))

# ---- Sanity check: predict on holdout ------------------------------------

cat("\nSanity check — predicting on cal season:\n")
preds <- predict_with_ensemble(bundle, cal_raw)
mae   <- mean(abs(preds$nfl_predictor_line - cal_raw$home_result), na.rm = TRUE)
cover <- mean(sign(preds$line_diff) ==
              sign(cal_raw$home_result - cal_raw$spread_line), na.rm = TRUE)
cat(sprintf("  MAE: %.2f, cover acc: %.3f\n", mae, cover))

# ---- Bake fresh EV calibration into bundle metadata ----------------------
# walk_forward_eval.R must have run already (we need walk_forward_predictions.csv
# to fit fresh EV cal). If not, skip — bundle uses default constants.
wf_preds_path <- file.path(DATA_DIR, "walk_forward_predictions.csv")
if (file.exists(wf_preds_path)) {
  cat("\nRecalibrating EV from walk_forward_predictions.csv...\n")
  rc <- system2("Rscript", "recalibrate_ev.R", stdout = TRUE, stderr = TRUE)
  ev_cal_path <- file.path(DATA_DIR, "ev_calibration.json")
  if (file.exists(ev_cal_path)) {
    ev_cal <- jsonlite::fromJSON(ev_cal_path)
    bundle$metadata$ev_calibration <- list(
      intercept = ev_cal$intercept, slope = ev_cal$slope,
      fit_n = ev_cal$fit_n, fit_timestamp = ev_cal$fit_timestamp
    )
    # Re-save bundle with EV cal baked in
    saveRDS(bundle, dated_path)
    saveRDS(bundle, latest_path)
    cat(sprintf("  EV cal baked into bundle: intercept=%.4f slope=%.4f (n=%d)\n",
                ev_cal$intercept, ev_cal$slope, ev_cal$fit_n))
  }
} else {
  cat("\n[skipped EV recal — no walk_forward_predictions.csv]\n")
}

cat("Done.\n")
