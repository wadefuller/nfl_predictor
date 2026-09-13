# ============================================
# NFL Predictor: GLM (cv.glmnet) Alpha Sweep
# ============================================
# REWRITTEN 2026-09-02. The GLM is now the ENTIRE production model
# (STACK_MODE=glm_only), so its alpha is the single most consequential
# hyperparameter in the project. The previous version chose it badly:
#
#   1. SINGLE SPLIT. Trained 2008-2023, scored one 544-game window
#      (2024-25). This project has now been misled three separate times by
#      single-window selection — the XGB challenge (looked decisive on
#      2020-23, inverted on holdout), the stage-2 prune screen (predicted
#      -0.035 MAE, delivered -0.0002), and the NNLS stacker. Rolling-origin
#      folds instead.
#   2. THE SPREAD WAS NOISE. Brier across all five alphas spanned 0.00043,
#      and the winner beat the runner-up by 0.00016 — inside noise. A choice
#      that arbitrary should be reported as arbitrary, not silently saved.
#   3. WRONG ARCHITECTURE. Tuned 2026-05-24 against the multi-stacker, which
#      then amplified GLM predictions by 1.23x. That amplification is gone;
#      the optimal regularisation under pure GLM need not be the same.
#
# Adds a per-fold winner table so you can see whether the selected alpha is
# actually preferred consistently or is just winning an average of noise.
#
# Window policy matches tune_hyperparams.R: tune on 2008-2020, validate
# 2015-2020, leave 2021-2025 alone.
#
# Output: data/tune_best_glm.rds  ($alpha, $lambda, + diagnostics)
# Usage:  Rscript tune_glm.R                    (~4 min)
#         TUNE_OBJECTIVE=mae Rscript tune_glm.R
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
suppressPackageStartupMessages({library(glmnet); library(recipes)})

SEED       <- 989
VAL_YEARS  <- 2015:2020
ALPHA_GRID <- c(0.02, 0.05, 0.10, 0.15, 0.25, 0.35, 0.50, 0.65, 0.80, 0.95)
OBJECTIVE  <- Sys.getenv("TUNE_OBJECTIVE", "brier")
stopifnot(OBJECTIVE %in% c("brier", "mae"))

cat("Loading features...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv")) %>%
  filter(!is.na(home_result), !is.na(home_moneyline),
         (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
         season >= 2008, season <= 2020)
cat(sprintf("  %d games, seasons %d-%d\n", nrow(g), min(g$season), max(g$season)))
cat(sprintf("  folds: validate %s\n\n", paste(VAL_YEARS, collapse = ", ")))

# One fold = fit on everything before vy, score vy. cv.glmnet picks lambda by
# its own internal CV inside the training rows only, so no leakage.
fit_fold <- function(alpha, vy) {
  tr_raw <- g %>% filter(season <  vy)
  va_raw <- g %>% filter(season == vy)
  tr <- prep_for_train(tr_raw); va <- prep_for_train(va_raw)

  rec <- prep(recipe(home_result ~ ., data = tr) %>%
                step_nzv(all_numeric_predictors()) %>%
                step_impute_median(all_numeric_predictors()) %>%
                step_novel(all_nominal_predictors()) %>%
                step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
                step_normalize(all_numeric_predictors()),
              training = tr)
  btr <- bake(rec, new_data = tr); bva <- bake(rec, new_data = va)
  xtr <- as.matrix(btr %>% select(-home_result)); ytr <- btr$home_result
  xva <- as.matrix(bva %>% select(-home_result)); yva <- bva$home_result

  set.seed(SEED)
  fit <- cv.glmnet(xtr, ytr, alpha = alpha, nfolds = 5)
  pr  <- as.numeric(predict(fit, newx = xva, s = "lambda.min"))
  sig <- sd(ytr - as.numeric(predict(fit, newx = xtr, s = "lambda.min")))
  nz  <- sum(as.numeric(predict(fit, type = "coefficients", s = "lambda.min")) != 0) - 1
  list(mae = mean(abs(pr - yva)),
       brier = mean((pnorm(pr / sig) - as.integer(yva > 0))^2),
       lambda = fit$lambda.min, nz = nz,
       mkt = mean(abs(va$spread_line - yva)))
}

cat(sprintf("=== Sweeping %d alphas x %d folds (selecting on %s) ===\n",
            length(ALPHA_GRID), length(VAL_YEARS), toupper(OBJECTIVE)))
per_fold <- list(); rows <- data.frame(); t0 <- Sys.time()
for (a in ALPHA_GRID) {
  fr <- lapply(VAL_YEARS, function(vy) fit_fold(a, vy))
  per_fold[[as.character(a)]] <- sapply(fr, function(x) x[[OBJECTIVE]])
  rows <- rbind(rows, data.frame(
    alpha = a,
    mae    = mean(sapply(fr, `[[`, "mae")),
    brier  = mean(sapply(fr, `[[`, "brier")),
    lambda = mean(sapply(fr, `[[`, "lambda")),
    coefs  = round(mean(sapply(fr, `[[`, "nz")))))
  cat(sprintf("  alpha=%.2f  MAE=%.4f  Brier=%.5f  mean lambda=%.3f  mean coefs=%d\n",
              a, tail(rows$mae,1), tail(rows$brier,1), tail(rows$lambda,1), tail(rows$coefs,1)))
}
mkt_mae <- mean(sapply(VAL_YEARS, function(vy) {
  va <- g %>% filter(season == vy); mean(abs(va$spread_line - va$home_result)) }))
cat(sprintf("\nMarket baseline on these folds: MAE %.4f\n", mkt_mae))
cat(sprintf("Sweep took %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

rows <- rows[order(rows[[OBJECTIVE]]), ]
cat("\nRanked:\n"); print(rows, row.names = FALSE, digits = 5)
winner <- rows[1, ]

# --- Is the choice meaningful, or are we ranking noise? ---------------------
cat("\n=== Selection stability ===\n")
spread <- max(rows[[OBJECTIVE]]) - min(rows[[OBJECTIVE]])
margin <- rows[[OBJECTIVE]][2] - rows[[OBJECTIVE]][1]
cat(sprintf("  %s spread across alphas: %.5f\n", toupper(OBJECTIVE), spread))
cat(sprintf("  winner beats runner-up by: %.5f\n", margin))

pf <- do.call(rbind, per_fold)
best_per_fold <- ALPHA_GRID[apply(pf, 2, which.min)]
cat(sprintf("  per-fold best alpha: %s\n",
            paste(sprintf("%d:%.2f", VAL_YEARS, best_per_fold), collapse = "  ")))
n_distinct <- length(unique(best_per_fold))
cat(sprintf("  distinct winners across %d folds: %d\n", length(VAL_YEARS), n_distinct))

noise_thresh <- if (OBJECTIVE == "brier") 1e-4 else 5e-3
undetermined <- (margin < noise_thresh) || (n_distinct >= length(VAL_YEARS) - 1)

# The GLM alpha governs 100% of production. Do NOT churn it on a noise-level
# margin: when the sweep can't separate the candidates, keep whatever alpha is
# already in place and record the sweep for inspection only. Silently
# overwriting a production hyperparameter on a 0.00007 Brier margin is exactly
# how an arbitrary choice acquires the appearance of having been tuned.
out_path  <- file.path(DATA_DIR, "tune_best_glm.rds")
incumbent <- if (file.exists(out_path)) tryCatch(readRDS(out_path), error = function(e) NULL) else NULL

adopt_alpha  <- winner$alpha
adopt_lambda <- winner$lambda
adopted      <- "winner"

if (undetermined) {
  cat("\n  !! The alpha choice is NOT well-determined — the margin is inside\n")
  cat("     noise and/or the folds disagree.\n")
  if (!is.null(incumbent$alpha)) {
    adopt_alpha  <- incumbent$alpha
    adopt_lambda <- if (!is.null(incumbent$lambda)) incumbent$lambda else winner$lambda
    adopted      <- "incumbent (sweep undetermined)"
    cat(sprintf("     KEEPING incumbent alpha=%.2f rather than churning to %.2f.\n",
                adopt_alpha, winner$alpha))
    cat("     Sweep diagnostics are still saved. To force the sweep winner,\n")
    cat("     delete the file and re-run, or set the alpha by hand.\n")
  } else {
    cat(sprintf("     No incumbent on disk; adopting sweep winner alpha=%.2f by default.\n",
                winner$alpha))
  }
} else {
  cat(sprintf("\n  OK — alpha=%.2f wins by %.5f with folds broadly agreeing.\n",
              winner$alpha, margin))
}

out <- list(alpha = adopt_alpha, lambda = adopt_lambda,
            adopted = adopted, sweep_winner_alpha = winner$alpha,
            undetermined = undetermined,
            sweep_results = rows, per_fold_best = best_per_fold,
            objective = OBJECTIVE, spread = spread, margin = margin,
            market_mae = mkt_mae, val_years = VAL_YEARS,
            fit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
saveRDS(out, out_path)
cat(sprintf("\nWrote %s  (alpha=%.2f, source: %s)\n", out_path, adopt_alpha, adopted))
