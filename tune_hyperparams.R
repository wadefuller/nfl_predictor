# ============================================
# NFL Predictor: XGBoost Hyperparameter Tuning
# ============================================
# Tunes the direct-xgboost learner (`ensemble_lib::train_xgb_direct`).
#
# REWRITTEN 2026-06-07. The previous version was silently producing params
# that LOSE TO THE MARKET: on rolling-origin folds 2020-23 they scored MAE
# 9.8667 against a market baseline of 9.8134. Four causes, all fixed here:
#
#   1. Wrong objective. It selected on CV *RMSE*. RMSE rewards hedging toward
#      the mean on a target that is 81% irreducible noise. We select on MAE
#      (the headline spread metric) and report Brier alongside.
#   2. No monotone constraint. The target is ~linear in spread_line
#      (R^2 0.19, slope 1.05). Unconstrained trees approximate that with a
#      staircase that collapses toward the training mean at the tails — at
#      spread > 10 the market said 13.3 and XGB said 12.0 while actuals
#      averaged 18.0. Constraining spread_line monotone-increasing is worth
#      ~0.01 MAE on its own and much more in combination.
#   3. No colsample_bytree. Never swept. Winners sit near 0.35, far from
#      the implicit 1.0.
#   4. Grid never reached the good corner. Winners are depth-2 stumps at
#      eta ~0.008 with lambda ~13. The old grid had tree_depth >= 3 and
#      reg_lambda <= 10.
#
# Tunes on 2008-2020 with 6-fold rolling origin (validate 2015-2020), leaving
# 2021-2025 untouched for walk-forward — same window policy as before.
#
# Output: data/tune_best_xgb_direct.rds  (consumed by train_xgb_direct)
#         data/tune_results_xgb_direct.csv
#
# NOTE: data/tune_best_params_reg.rds (the old tidymodels-format file) is no
# longer written. The tidymodels path in train_xgb_model() is legacy; it
# cannot express a monotone constraint, which is why it underperforms.
#
# Runtime: ~15-20 min (32 configs x 6 folds, early stopping)
# Usage: Rscript tune_hyperparams.R
#        TUNE_OBJECTIVE=brier Rscript tune_hyperparams.R   # select on Brier
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
suppressPackageStartupMessages({library(xgboost); library(recipes)})

SEED         <- 989
TUNE_SEASONS <- 2008:2020
VAL_YEARS    <- 2015:2020      # 6 rolling-origin folds
GRID_SIZE    <- 32
MAX_ROUNDS   <- 3000
OBJECTIVE    <- Sys.getenv("TUNE_OBJECTIVE", "mae")
stopifnot(OBJECTIVE %in% c("mae", "brier"))

set.seed(SEED)

# ---- Load + prep -----------------------------------------------------------
cat("Loading data...\n")
g_raw <- read.csv(file.path(DATA_DIR, "game_w_features.csv")) %>%
  filter(season %in% TUNE_SEASONS,
         !is.na(result), !is.na(home_moneyline),
         (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021)) %>%
  arrange(as.integer(season), week)

seasons <- g_raw$season
prep    <- prep_for_train(g_raw)

# Same recipe as train_xgb_direct, so tuned params transfer exactly.
rec <- prep(recipe(home_result ~ ., data = prep) %>%
              step_nzv(all_numeric_predictors()) %>%
              step_novel(all_nominal_predictors()) %>%
              step_dummy(all_nominal_predictors(), one_hot = TRUE),
            training = prep)
baked <- bake(rec, new_data = prep)
X <- as.matrix(baked %>% select(-home_result))
y <- baked$home_result
mkt <- prep$spread_line

cat(sprintf("  %d rows x %d cols, seasons %d-%d\n", nrow(X), ncol(X),
            min(seasons), max(seasons)))
cat(sprintf("  folds: validate %s\n\n", paste(VAL_YEARS, collapse = ", ")))

# Monotone constraint: spread_line increasing, everything else free.
mono <- rep(0L, ncol(X))
sc   <- which(colnames(X) == "spread_line")
if (length(sc) == 1) {
  mono[sc] <- 1L
} else {
  warning("spread_line not found — tuning WITHOUT the monotone constraint")
}
MONO_STR <- paste0("(", paste(mono, collapse = ","), ")")

# ---- Fold evaluation -------------------------------------------------------
# NESTED early stopping. The obvious implementation — early-stop on the same
# fold you then score — leaks: the stopping round is chosen using validation
# data, which flatters every config. Measured cost of that leak on these
# folds was ~0.04 MAE (reported 9.8738 vs honest 9.9146), enough to turn
# "beats the market" into "loses to the market".
#
# So for validation season vy we train on seasons < vy-1, early-stop against
# season vy-1, and score on vy. Nothing about vy touches the fit.
eval_config <- function(p) {
  maes <- c(); briers <- c(); iters <- c()
  for (vy in VAL_YEARS) {
    va <- which(seasons == vy)
    es <- which(seasons == vy - 1)          # inner early-stopping season
    tr <- which(seasons <  vy - 1)
    if (length(va) == 0 || length(es) == 0 || length(tr) < 200) next

    dtr <- xgb.DMatrix(X[tr, , drop = FALSE], label = y[tr])
    des <- xgb.DMatrix(X[es, , drop = FALSE], label = y[es])
    fit <- xgb.train(params = p, data = dtr, nrounds = MAX_ROUNDS,
                     watchlist = list(es = des),
                     early_stopping_rounds = 100, verbose = 0)

    # Refit on train + early-stopping season at the chosen round, so the
    # final model uses all data available before vy (matches production,
    # where train_xgb_direct sees everything prior to the target week).
    nb  <- fit$best_iteration
    trf <- c(tr, es)
    fit2 <- xgb.train(params = p,
                      data = xgb.DMatrix(X[trf, , drop = FALSE], label = y[trf]),
                      nrounds = nb, verbose = 0)

    pr  <- predict(fit2, X[va, , drop = FALSE])
    sig <- sd(y[trf] - predict(fit2, X[trf, , drop = FALSE]))
    maes   <- c(maes,   mean(abs(pr - y[va])))
    briers <- c(briers, mean((pnorm(pr / sig) - as.integer(y[va] > 0))^2))
    iters  <- c(iters,  nb)
  }
  list(mae = mean(maes), brier = mean(briers), iters = round(mean(iters)))
}

# Market reference on the identical folds — the bar any config must clear.
mkt_mae <- mean(sapply(VAL_YEARS, function(vy) {
  va <- which(seasons == vy); if (!length(va)) return(NA); mean(abs(mkt[va] - y[va]))
}), na.rm = TRUE)
cat(sprintf("Market baseline on these folds: MAE %.4f\n\n", mkt_mae))

# ---- Grid ------------------------------------------------------------------
# Ranges chosen to COVER the corner the old grid missed: depth 2 is weighted,
# lambda reaches 30, colsample is swept, eta goes down to 1e-3.
set.seed(SEED)
grid <- data.frame(
  eta       = 10^runif(GRID_SIZE, -3.0, -1.5),
  depth     = sample(c(2, 2, 2, 3, 3, 4, 5, 6), GRID_SIZE, replace = TRUE),
  min_child = sample(c(10, 20, 30, 50, 80), GRID_SIZE, replace = TRUE),
  subsample = runif(GRID_SIZE, 0.5, 0.95),
  colsample = runif(GRID_SIZE, 0.2, 0.8),
  alpha     = 10^runif(GRID_SIZE, -2, 1),
  lambda    = 10^runif(GRID_SIZE, -1, 1.5)
)

cat(sprintf("=== Tuning %d configs x %d folds (selecting on %s) ===\n",
            GRID_SIZE, length(VAL_YEARS), toupper(OBJECTIVE)))
res <- data.frame()
t0 <- Sys.time()
for (i in seq_len(GRID_SIZE)) {
  p <- list(objective = "reg:squarederror", eval_metric = "mae",
            eta = grid$eta[i], max_depth = grid$depth[i],
            min_child_weight = grid$min_child[i], subsample = grid$subsample[i],
            colsample_bytree = grid$colsample[i], alpha = grid$alpha[i],
            lambda = grid$lambda[i], nthread = 4,
            monotone_constraints = MONO_STR)
  r <- eval_config(p)
  res <- rbind(res, cbind(grid[i, ], mae = r$mae, brier = r$brier, iters = r$iters))
  cat(sprintf("  [%2d/%d] eta=%.4f d=%d mc=%2d sub=%.2f col=%.2f a=%.3f l=%5.2f -> MAE=%.4f Brier=%.5f %s\n",
              i, GRID_SIZE, grid$eta[i], grid$depth[i], grid$min_child[i],
              grid$subsample[i], grid$colsample[i], grid$alpha[i], grid$lambda[i],
              r$mae, r$brier, ifelse(r$mae < mkt_mae, "<- beats market", "")))
}
cat(sprintf("\nTuning took %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

res <- res[order(res[[OBJECTIVE]]), ]
cat(sprintf("\nTop 5 by %s:\n", toupper(OBJECTIVE)))
print(head(res, 5), row.names = FALSE, digits = 4)
best <- res[1, ]

# ---- Sanity gate -----------------------------------------------------------
cat("\n=== Sanity check ===\n")
cat(sprintf("  best config MAE:  %.4f\n", best$mae))
cat(sprintf("  market baseline:  %.4f\n", mkt_mae))
n_beat <- sum(res$mae < mkt_mae)
beat_rate <- n_beat / nrow(res)
margin <- mkt_mae - best$mae
cat(sprintf("  configs beating the market: %d of %d (%.0f%%)\n",
            n_beat, nrow(res), 100 * beat_rate))
cat(sprintf("  best margin over market:    %+.4f MAE\n", margin))

# A single config squeaking past the market is noise, not signal. Require
# both a real margin AND a decent share of the grid clearing the bar.
gate <- if (margin <= 0) "fail" else if (margin < 0.01 || beat_rate < 0.25) "weak" else "pass"

if (margin <= 0) {
  cat("\n  !! FAIL: the best config does NOT beat the market on these folds.\n")
  cat("     Saved for inspection, but do NOT enable XGB_DIRECT on this basis.\n")
  cat("     load_xgb_direct_params() will refuse these and fall back.\n")
} else if (margin < 0.01 || beat_rate < 0.25) {
  cat("\n  !! WEAK: margin under 0.01 MAE and/or fewer than 25% of configs\n")
  cat("     clear the market. That is within window-to-window noise — three\n")
  cat("     separate windows this project has measured disagree on whether\n")
  cat("     XGB beats the market at all. Treat as NOT a real edge.\n")
} else {
  cat(sprintf("\n  OK — best config beats the market by %.4f MAE with %.0f%% of the\n",
              margin, 100 * beat_rate))
  cat("     grid clearing it. Still confirm on the full walk-forward before\n")
  cat("     changing production; sub-window results have not transferred.\n")
}

# ---- Save ------------------------------------------------------------------
out <- list(
  eta = best$eta, max_depth = as.integer(best$depth),
  min_child_weight = best$min_child, subsample = best$subsample,
  colsample_bytree = best$colsample, alpha = best$alpha,
  lambda = best$lambda, nrounds = as.integer(best$iters),
  monotone_spread_line = (length(sc) == 1),
  gate = gate, beat_rate = beat_rate, margin = margin,
  cv_mae = best$mae, cv_brier = best$brier, market_mae = mkt_mae,
  objective = OBJECTIVE, tune_seasons = range(TUNE_SEASONS),
  val_years = VAL_YEARS, fit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
)
saveRDS(out, file.path(DATA_DIR, "tune_best_xgb_direct.rds"))
write.csv(res, file.path(DATA_DIR, "tune_results_xgb_direct.csv"), row.names = FALSE)

cat("\nSaved:\n")
cat("  data/tune_best_xgb_direct.rds\n")
cat("  data/tune_results_xgb_direct.csv\n")
cat("\nUse with: XGB_DIRECT=1 (see ensemble_lib::train_xgb_direct)\n")
cat("Production default is GLM-only; these params only take effect if you\n")
cat("explicitly enable XGB_DIRECT / STACK_MODE.\n")
