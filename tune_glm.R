# ============================================
# NFL Predictor: GLM (cv.glmnet) Alpha Sweep
# ============================================
# Sweeps the elastic-net alpha mix (0 = pure L2 ridge, 1 = pure L1 lasso).
# Currently hardcoded at alpha=0.5; with the wider feature set (334 cols)
# the optimum may have shifted.
#
# Validation: train on 2008-2023, evaluate on 2024-2025 (matches the cal
# window used by train_and_save_ensemble.R). Scores by MAE + Brier.
#
# Output: data/tune_best_glm.rds with $alpha and $lambda (best from cv).
# Usage: Rscript tune_glm.R   (~3 min)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
library(glmnet)

ALPHA_GRID  <- c(0.05, 0.25, 0.50, 0.75, 0.95)
TRAIN_LAST  <- 2023
VAL_START   <- 2024

cat("Loading features...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
g <- g %>% filter(!is.na(home_result), !is.na(home_moneyline),
                   (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
                   season >= 2008)

train_raw <- g %>% filter(season <= TRAIN_LAST)
val_raw   <- g %>% filter(season >= VAL_START)
cat(sprintf("Train: %d rows (2008-%d)  Val: %d rows (%d+)\n",
            nrow(train_raw), TRAIN_LAST, nrow(val_raw), VAL_START))

# Same prep as ensemble_lib::train_glm_model
prep_glm <- function(df) {
  df %>%
    mutate(across(c(season, week, weekday, div_game), as.factor)) %>%
    select(-any_of(LEAKAGE_VARS_REG), -any_of("season"))
}

train_prep <- prep_glm(train_raw)
val_prep   <- prep_glm(val_raw)

cat(sprintf("Train cols after prep: %d\n", ncol(train_prep)))

# Bake recipe once on training (matches train_glm_model)
rec <- recipes::recipe(home_result ~ ., data = train_prep) %>%
  recipes::step_nzv(recipes::all_numeric_predictors()) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_novel(recipes::all_nominal_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

prep_rec <- recipes::prep(rec, training = train_prep)
train_baked <- recipes::bake(prep_rec, new_data = train_prep)
val_baked   <- recipes::bake(prep_rec, new_data = val_prep)

x_train <- as.matrix(train_baked %>% select(-home_result))
y_train <- train_baked$home_result
x_val   <- as.matrix(val_baked %>% select(-home_result))
y_val   <- val_baked$home_result

cat(sprintf("Feature matrix: %d × %d\n", nrow(x_train), ncol(x_train)))

# Sweep
cat("\n=== Alpha sweep ===\n")
results <- map_dfr(ALPHA_GRID, function(a) {
  set.seed(989)
  fit <- cv.glmnet(x_train, y_train, alpha = a, nfolds = 5)
  pred <- as.numeric(predict(fit, newx = x_val, s = "lambda.min"))
  sigma <- sd(y_train - as.numeric(predict(fit, newx = x_train, s = "lambda.min")))
  wp    <- pnorm(pred / sigma)
  win   <- as.integer(y_val > 0)
  brier <- mean((wp - win)^2)
  cat(sprintf("  alpha=%.2f  λ_min=%.4f  MAE=%.3f  Brier=%.4f  RMSE=%.3f\n",
              a, fit$lambda.min, mean(abs(pred - y_val)), brier,
              sqrt(mean((pred - y_val)^2))))
  tibble(alpha = a, lambda = fit$lambda.min,
         mae = mean(abs(pred - y_val)),
         rmse = sqrt(mean((pred - y_val)^2)),
         brier = brier)
})

# Pick winner: rank by Brier (matches "rank by core metrics" philosophy)
winner <- results %>% arrange(brier) %>% slice(1)
cat(sprintf("\nBest alpha: %.2f (Brier=%.4f, MAE=%.3f)\n",
            winner$alpha, winner$brier, winner$mae))

out <- list(alpha = winner$alpha, lambda = winner$lambda,
            sweep_results = results,
            fit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
saveRDS(out, file.path(DATA_DIR, "tune_best_glm.rds"))
cat(sprintf("\nWrote %s\n", file.path(DATA_DIR, "tune_best_glm.rds")))
