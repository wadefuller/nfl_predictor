# ============================================
# NFL Predictor: Random Forest (ranger) Hyperparameter Sweep
# ============================================
# Sweeps mtry × min_n. Currently hardcoded at mtry=sqrt(n_features)≈18,
# min_n=10. With 334 cols and ~5000 training rows, a smaller mtry+larger
# min_n may help (less overfitting) or larger mtry may help (more candidate
# splits per node).
#
# Validation: train on 2008-2023, evaluate on 2024-2025. Score by MAE+Brier.
#
# Output: data/tune_best_rf.rds with $mtry and $min_n.
# Usage: Rscript tune_rf.R   (~5-7 min — RF is the slow learner)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
library(ranger)

MTRY_GRID  <- c(15, 25, 40, 60)
MIN_N_GRID <- c(5, 10, 20)
TRAIN_LAST <- 2023
VAL_START  <- 2024
TREES      <- 500

cat("Loading features...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
g <- g %>% filter(!is.na(home_result), !is.na(home_moneyline),
                   (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
                   season >= 2008)

train_raw <- g %>% filter(season <= TRAIN_LAST)
val_raw   <- g %>% filter(season >= VAL_START)
cat(sprintf("Train: %d rows  Val: %d rows\n", nrow(train_raw), nrow(val_raw)))

# Same prep as ensemble_lib::train_rf_model
prep_rf <- function(df) {
  df %>%
    mutate(across(c(season, week, weekday, div_game), as.factor)) %>%
    select(-any_of(LEAKAGE_VARS_REG), -any_of("season"))
}

train_prep <- prep_rf(train_raw)
val_prep   <- prep_rf(val_raw)

rec <- recipes::recipe(home_result ~ ., data = train_prep) %>%
  recipes::step_nzv(recipes::all_numeric_predictors()) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_novel(recipes::all_nominal_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)

prep_rec <- recipes::prep(rec, training = train_prep)
train_baked <- recipes::bake(prep_rec, new_data = train_prep)
val_baked   <- recipes::bake(prep_rec, new_data = val_prep)

cat(sprintf("Feature matrix: %d × %d\n", nrow(train_baked), ncol(train_baked) - 1))

grid <- expand.grid(mtry = MTRY_GRID, min_n = MIN_N_GRID,
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

cat(sprintf("\n=== RF sweep: %d configs (%d trees each) ===\n", nrow(grid), TREES))

results <- pmap_dfr(grid, function(mtry, min_n) {
  set.seed(989)
  t0 <- Sys.time()
  fit <- ranger(home_result ~ ., data = train_baked,
                 num.trees = TREES, mtry = mtry, min.node.size = min_n,
                 seed = 989)
  pred <- predict(fit, data = val_baked)$predictions
  elapsed <- as.numeric(Sys.time() - t0)
  sigma <- sd(train_baked$home_result - predict(fit, data = train_baked)$predictions)
  wp    <- pnorm(pred / sigma)
  win   <- as.integer(val_baked$home_result > 0)
  brier <- mean((wp - win)^2)
  mae   <- mean(abs(pred - val_baked$home_result))
  cat(sprintf("  mtry=%2d  min_n=%2d  MAE=%.3f  Brier=%.4f  (%.1fs)\n",
              mtry, min_n, mae, brier, elapsed))
  tibble(mtry = mtry, min_n = min_n, mae = mae, brier = brier,
         elapsed_s = round(elapsed, 1))
})

winner <- results %>% arrange(brier) %>% slice(1)
cat(sprintf("\nBest RF: mtry=%d, min_n=%d (Brier=%.4f, MAE=%.3f)\n",
            winner$mtry, winner$min_n, winner$brier, winner$mae))

out <- list(mtry = winner$mtry, min_n = winner$min_n, trees = TREES,
            sweep_results = results,
            fit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
saveRDS(out, file.path(DATA_DIR, "tune_best_rf.rds"))
cat(sprintf("\nWrote %s\n", file.path(DATA_DIR, "tune_best_rf.rds")))
