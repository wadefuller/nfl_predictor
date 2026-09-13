# ============================================
# NFL Predictor: XGB-only Challenge
# ============================================
# Question: with more features, task-specific tuning, and feature selection,
# can an XGB-ONLY model beat the GLM-only production model?
#
# Baseline to beat (walk-forward 2016-2025):
#   MAE 9.7783 | Brier 0.21033 | LogLoss 0.60871 | Cover 0.5336
#
# Prior attempts used the tidymodels wrapper with params tuned on CV-RMSE
# over a disjoint window. That is not a fair shot. This script gives XGB
# everything the earlier diagnostics said it was missing:
#   * base_margin = spread_line  -> the market as a free offset, so trees
#     never have to reconstruct the linear anchor with a staircase
#   * monotone constraint on spread_line in the standard-target regime
#   * colsample_bytree            -> never swept before
#   * heavy-regularization corner -> tiny eta, shallow trees, strong L1/L2,
#     which is where low-SNR problems live
#   * feature selection by gain   -> the census showed 94 dead columns and
#     72 more with Gain < 0.001
#
# Protocol: rolling-origin CV inside 2008-2023 (validate on 2020/21/22/23).
# 2024-2025 is NEVER touched here — it stays clean for the final check.
#
# !!! KNOWN FLAW — READ BEFORE TRUSTING ANY NUMBER THIS PRINTS !!!
# eval_config() early-stops on the SAME fold it then scores, so the stopping
# round is selected using validation data. Measured cost of that leak in
# tune_hyperparams.R on comparable folds: ~0.069 MAE. Every MAE below is
# optimistic by roughly that much.
#
# Concretely: this script's headline "best XGB 9.7745 beats GLM 9.8373 on
# 2020-23 folds" does not survive the correction — 9.7745 + ~0.069 is ~9.84,
# i.e. WORSE than the GLM. The full walk-forward (which has no such leak)
# independently confirmed XGB-only loses: 9.8228 vs GLM 9.7783.
#
# tune_hyperparams.R::eval_config has the correct nested implementation
# (train < vy-1, early-stop on vy-1, refit, score vy). Port it here before
# using this script for anything decision-relevant. Kept as-is because its
# STAGE 1 regime comparison (monotone vs offset vs plain) is internally
# consistent — all arms share the same bias — and that comparison is what
# surfaced the monotone-constraint finding, which does hold up.
#
# Usage: Rscript xgb_challenge.R    (~8 min)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
suppressPackageStartupMessages({library(xgboost); library(recipes)})

SEED       <- 989
VAL_YEARS  <- 2020:2023          # rolling-origin validation seasons
MIN_TRAIN  <- 2008

set.seed(SEED)

# ---------------------------------------------------------------------
# Build the design matrix once
# ---------------------------------------------------------------------
cat("Building design matrix...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv")) %>%
  filter(!is.na(home_result), !is.na(home_moneyline),
         (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
         season >= MIN_TRAIN, season <= 2023)

seasons <- g$season
prep    <- prep_for_train(g)

rec <- recipe(home_result ~ ., data = prep) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)
baked <- bake(prep(rec, training = prep), new_data = prep)

y   <- baked$home_result
X   <- as.matrix(baked %>% select(-home_result))
mkt <- prep$spread_line
cat(sprintf("  %d rows x %d cols, seasons %d-%d\n\n", nrow(X), ncol(X), min(seasons), max(seasons)))

spread_col <- which(colnames(X) == "spread_line")
stopifnot(length(spread_col) == 1)

# ---------------------------------------------------------------------
# Fold-wise evaluation harness
# ---------------------------------------------------------------------
# regime: "standard"  -> predict home_result directly
#         "offset"    -> base_margin = spread_line, model the deviation
eval_config <- function(params, regime = "offset", nrounds = 5000,
                        feat_idx = NULL, monotone = FALSE, verbose = FALSE) {
  cols <- if (is.null(feat_idx)) seq_len(ncol(X)) else feat_idx
  Xc   <- X[, cols, drop = FALSE]

  if (monotone && regime == "standard") {
    mc <- rep(0L, ncol(Xc))
    sc <- which(colnames(Xc) == "spread_line")
    if (length(sc) == 1) mc[sc] <- 1L
    params$monotone_constraints <- paste0("(", paste(mc, collapse = ","), ")")
  }

  maes <- c(); briers <- c(); iters <- c()
  for (vy in VAL_YEARS) {
    tr <- which(seasons <  vy)
    va <- which(seasons == vy)
    if (length(va) == 0) next

    dtr <- xgb.DMatrix(Xc[tr, , drop = FALSE], label = y[tr])
    dva <- xgb.DMatrix(Xc[va, , drop = FALSE], label = y[va])
    if (regime == "offset") {
      setinfo(dtr, "base_margin", mkt[tr])
      setinfo(dva, "base_margin", mkt[va])
    }

    fit <- xgb.train(params = params, data = dtr, nrounds = nrounds,
                     watchlist = list(val = dva), early_stopping_rounds = 100,
                     verbose = 0)
    pred <- predict(fit, dva)

    sig <- sd(y[tr] - predict(fit, dtr))
    wp  <- pnorm(pred / sig)
    win <- as.integer(y[va] > 0)

    maes   <- c(maes,   mean(abs(pred - y[va])))
    briers <- c(briers, mean((wp - win)^2))
    iters  <- c(iters,  fit$best_iteration)
  }
  list(mae = mean(maes), brier = mean(briers), iters = round(mean(iters)))
}

base_params <- function(eta, depth, min_child, subsample, colsample, alpha, lambda) {
  list(objective = "reg:squarederror", eval_metric = "mae",
       eta = eta, max_depth = depth, min_child_weight = min_child,
       subsample = subsample, colsample_bytree = colsample,
       alpha = alpha, lambda = lambda, nthread = 4)
}

# ---------------------------------------------------------------------
# References on the SAME folds — this is the only fair comparison
# ---------------------------------------------------------------------
mkt_mae <- mean(sapply(VAL_YEARS, function(vy) {
  va <- which(seasons == vy); mean(abs(mkt[va] - y[va]))
}))
cat(sprintf("Reference — market alone:  MAE %.4f\n", mkt_mae))

# GLM (production learner) evaluated fold-for-fold against the same splits
cat("Reference — fitting GLM per fold...\n")
glm_maes <- c(); glm_briers <- c()
for (vy in VAL_YEARS) {
  tr_raw <- g %>% filter(season <  vy)
  va_raw <- g %>% filter(season == vy)
  if (nrow(va_raw) == 0) next
  gf <- train_glm_model(prep_for_train(tr_raw))
  vp <- prep_for_train(va_raw)
  pr <- predict_glm(gf, vp)
  sig <- sd(prep_for_train(tr_raw)$home_result - predict_glm(gf, prep_for_train(tr_raw)))
  glm_maes   <- c(glm_maes,   mean(abs(pr - vp$home_result)))
  glm_briers <- c(glm_briers, mean((pnorm(pr/sig) - as.integer(vp$home_result > 0))^2))
}
glm_mae <- mean(glm_maes); glm_brier <- mean(glm_briers)
cat(sprintf("Reference — GLM (to beat): MAE %.4f  Brier %.5f\n\n", glm_mae, glm_brier))

# ---------------------------------------------------------------------
# STAGE 1 — which regime? (standard vs market-offset vs monotone)
# ---------------------------------------------------------------------
cat("=====================================================================\n")
cat("STAGE 1: regime test\n")
cat("=====================================================================\n")
archetypes <- list(
  "current-ish (eta .02, d5)" = base_params(0.020, 5, 18, 0.85, 0.60, 0.04, 0.05),
  "shallow+slow (eta .005,d3)"= base_params(0.005, 3, 30, 0.70, 0.40, 0.50, 2.00),
  "stumps (eta .01, d2)"      = base_params(0.010, 2, 40, 0.70, 0.30, 1.00, 5.00)
)
for (nm in names(archetypes)) {
  for (rg in c("standard", "offset")) {
    r <- eval_config(archetypes[[nm]], regime = rg)
    cat(sprintf("  %-28s %-9s MAE=%.4f  Brier=%.5f  (%d rounds)\n",
                nm, rg, r$mae, r$brier, r$iters))
  }
}
cat("\n  monotone-constrained standard target:\n")
for (nm in names(archetypes)) {
  r <- eval_config(archetypes[[nm]], regime = "standard", monotone = TRUE)
  cat(sprintf("  %-28s %-9s MAE=%.4f  Brier=%.5f  (%d rounds)\n",
              nm, "mono", r$mae, r$brier, r$iters))
}

# ---------------------------------------------------------------------
# STAGE 2 — hyperparameter sweep in the offset regime
# ---------------------------------------------------------------------
cat("\n=====================================================================\n")
cat("STAGE 2: hyperparameter sweep (STANDARD target + monotone constraint)\n")
cat("Stage 1 showed this regime beats market-offset. Biasing the grid toward\n")
cat("the shallow/slow corner where stage 1's winner lived.\n")
cat("=====================================================================\n")
set.seed(SEED)
N_CONFIG <- 30
grid <- data.frame(
  eta       = 10^runif(N_CONFIG, -2.8, -1.6),
  depth     = sample(c(2, 2, 2, 3, 3, 4, 5), N_CONFIG, replace = TRUE),
  min_child = sample(c(10, 20, 30, 50, 80), N_CONFIG, replace = TRUE),
  subsample = runif(N_CONFIG, 0.5, 0.95),
  colsample = runif(N_CONFIG, 0.2, 0.8),
  alpha     = 10^runif(N_CONFIG, -2, 1),
  lambda    = 10^runif(N_CONFIG, -1, 1.5)
)
res <- data.frame()
for (i in seq_len(N_CONFIG)) {
  p <- base_params(grid$eta[i], grid$depth[i], grid$min_child[i],
                   grid$subsample[i], grid$colsample[i], grid$alpha[i], grid$lambda[i])
  r <- eval_config(p, regime = "standard", monotone = TRUE)
  res <- rbind(res, cbind(grid[i, ], mae = r$mae, brier = r$brier, iters = r$iters))
  cat(sprintf("  [%2d/%d] eta=%.4f d=%d mc=%2d sub=%.2f col=%.2f a=%.3f l=%5.2f -> MAE=%.4f Brier=%.5f %s\n",
              i, N_CONFIG, grid$eta[i], grid$depth[i], grid$min_child[i],
              grid$subsample[i], grid$colsample[i], grid$alpha[i], grid$lambda[i],
              r$mae, r$brier, ifelse(r$mae < glm_mae, "<-- beats GLM", "")))
}
res <- res[order(res$mae), ]
cat("\n  Top 5 by MAE:\n")
print(head(res, 5), row.names = FALSE, digits = 4)
best <- res[1, ]
cat(sprintf("\n  Best XGB: MAE %.4f   vs GLM %.4f   vs market %.4f\n",
            best$mae, glm_mae, mkt_mae))

# ---------------------------------------------------------------------
# STAGE 3 — feature selection with the winning hyperparams
# ---------------------------------------------------------------------
cat("\n=====================================================================\n")
cat("STAGE 3: feature selection (winning hyperparams, standard+monotone)\n")
cat("=====================================================================\n")
best_p <- base_params(best$eta, best$depth, best$min_child,
                      best$subsample, best$colsample, best$alpha, best$lambda)

# pilot fit on all pre-2024 data to rank features by gain
dall  <- xgb.DMatrix(X, label = y)
pilot <- xgb.train(params = best_p, data = dall, nrounds = max(50, best$iters), verbose = 0)
imp   <- xgb.importance(model = pilot)
cat(sprintf("\n  pilot fit used %d of %d columns\n", nrow(imp), ncol(X)))
cat(sprintf("  targets to beat:  GLM %.4f   market %.4f\n\n", glm_mae, mkt_mae))

r_all <- eval_config(best_p, regime = "standard", monotone = TRUE)
cat(sprintf("  %-22s MAE=%.4f  Brier=%.5f %s\n", sprintf("all %d", ncol(X)),
            r_all$mae, r_all$brier, ifelse(r_all$mae < glm_mae, "<-- beats GLM", "")))
for (k in c(150, 100, 60, 40, 25, 15)) {
  if (k > nrow(imp)) next
  idx <- match(imp$Feature[1:k], colnames(X)); idx <- idx[!is.na(idx)]
  r <- eval_config(best_p, regime = "standard", monotone = TRUE, feat_idx = idx)
  cat(sprintf("  %-22s MAE=%.4f  Brier=%.5f %s\n", sprintf("top %d by gain", k),
              r$mae, r$brier, ifelse(r$mae < glm_mae, "<-- beats GLM", "")))
}
# GLM's own 19 features, as a control
glm_feats <- c("spread_line","impl_prob_home","impl_prob_home_diff","impl_prob_away",
  "starter_games_team_season_home_diff","away_moneyline","impl_win_prob_season_avg_home_diff",
  "home_moneyline","impl_win_prob_4gm_trend_home_diff","success_rate_r16_home_diff",
  "team_points_against_r4_home","game_info_gain_4gm_trend_home","coach_team_games_diff",
  "starter_pass_epa_r8_home_diff","team_points_against_r16_home",
  "starter_games_team_season_home","off_rush_epa_r16_home_diff","new_starter_home_diff",
  "game_info_gain_season_avg_home_diff")
idx <- match(glm_feats, colnames(X)); idx <- idx[!is.na(idx)]
r <- eval_config(best_p, regime = "standard", monotone = TRUE, feat_idx = idx)
cat(sprintf("  %-22s MAE=%.4f  Brier=%.5f %s\n", sprintf("GLM's %d features", length(idx)),
            r$mae, r$brier, ifelse(r$mae < glm_mae, "<-- beats GLM", "")))

cat("\n=====================================================================\n")
cat("VERDICT (rolling-origin folds 2020-2023)\n")
cat("=====================================================================\n")
cat(sprintf("  market alone      MAE %.4f\n", mkt_mae))
cat(sprintf("  GLM (production)  MAE %.4f  Brier %.5f\n", glm_mae, glm_brier))
cat(sprintf("  best XGB          MAE %.4f  Brier %.5f\n", best$mae, best$brier))
cat(sprintf("\n  XGB - GLM = %+.4f MAE  (negative means XGB wins)\n", best$mae - glm_mae))

saveRDS(list(best = best, top_features = imp$Feature, grid = res,
             glm_mae = glm_mae, glm_brier = glm_brier, mkt_mae = mkt_mae),
        file.path(DATA_DIR, "xgb_challenge_results.rds"))
cat(sprintf("\nSaved -> %s\n", file.path(DATA_DIR, "xgb_challenge_results.rds")))
cat("\nDone.\n")
