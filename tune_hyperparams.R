# ============================================
# NFL Predictor: XGBoost Hyperparameter Tuning
# ============================================
# Tunes XGBoost hyperparameters for the regression model used by walk_forward.
# (The cv.glmnet and ranger base learners in the ensemble auto-tune internally
# or use sensible defaults — only XGB needs explicit tuning here.)
#
#   - Proper temporal CV (rolling_origin via manual_rset, no random k-fold)
#   - L1/L2 regularization (reg_alpha, reg_lambda) included in grid
#   - Bounded mtry range
#
# Tunes on 2008-2020 (13 seasons, 6-fold rolling origin), leaving 2021-2025
# as untouched honest holdout for walk-forward.
#
# Output: data/tune_best_params_reg.rds, tune_results_reg.csv
#
# Runtime: ~1 min (early-stopping makes fits very fast)
# ============================================

# Source the shared library — gives us the canonical LEAKAGE_VARS_REG.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")
library(doParallel)

# PROJECT_DIR / DATA_DIR come from ensemble_lib.R.
OUTPUT_DIR <- DATA_DIR

SEED         <- 989
TUNE_SEASONS <- 2008:2020
GRID_SIZE    <- 32   # LHS draws per model

# LEAKAGE_VARS_REG comes from ensemble_lib.R — single source of truth.

# ---- Load + prep -----------------------------------------------------------

cat("Loading data...\n")
g_raw <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))

fold_data <- g_raw %>%
  filter(season %in% TUNE_SEASONS,
         !is.na(result), !is.na(home_moneyline),
         (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021)) %>%
  arrange(as.integer(season), week)

# Single regression model targeting home_result (margin).
# Win probability is derived later via pnorm(pred / sigma).
train_reg <- fold_data %>%
  mutate(across(c(week, weekday, div_game), as.factor)) %>%
  select(-any_of(LEAKAGE_VARS_REG), -season)

cat(sprintf("Regression (target = home_result): %d rows × %d features\n",
            nrow(train_reg), ncol(train_reg) - 1))

# ---- Build rolling-origin folds by season ---------------------------------
# 6 folds: train 2008-(2014+k) → validate 2015+k for k=0..5
seasons_vec <- fold_data$season

make_season_folds <- function(data) {
  splits_list <- map(0:5, function(k) {
    val_season   <- 2015 + k
    train_idx    <- which(seasons_vec < val_season)
    val_idx      <- which(seasons_vec == val_season)
    make_splits(
      list(analysis = train_idx, assessment = val_idx),
      data = data
    )
  })
  manual_rset(splits = splits_list, ids = paste0("Fold", 1:6, "_val", 2015:2020))
}

folds_reg <- make_season_folds(train_reg)

cat("Fold sizes:\n")
for (i in seq_along(folds_reg$splits)) {
  s <- folds_reg$splits[[i]]
  cat(sprintf("  %s: train=%d, val=%d\n",
              folds_reg$id[i],
              length(s$in_id),
              length(s$out_id)))
}

# ---- Tuning spec & grid (shared between reg/class) -----------------------

build_param_set <- function(wf) {
  extract_parameter_set_dials(wf) %>%
    update(
      tree_depth     = tree_depth(range = c(3L, 10L)),
      learn_rate     = learn_rate(range = c(-2.5, -0.7)),     # log10: 0.003-0.20
      min_n          = min_n(range = c(5L, 30L)),
      loss_reduction = loss_reduction(range = c(-3, 0)),       # log10: 0.001-1
      sample_size    = sample_prop(range = c(0.5, 0.95)),
      mtry           = mtry(range = c(8L, 40L)),               # bounded
      reg_alpha      = penalty(range = c(-3, 0)),               # log10: 0.001-1
      reg_lambda     = penalty(range = c(-2, 1))                # log10: 0.01-10
    )
}

registerDoParallel()

# ---- Tune regression model ------------------------------------------------

cat("\n=== Tuning regression model ===\n")

spec_reg <- boost_tree(
  trees          = 800,
  tree_depth     = tune(),
  learn_rate     = tune(),
  min_n          = tune(),
  loss_reduction = tune(),
  sample_size    = tune(),
  mtry           = tune(),
  stop_iter      = 25
) %>%
  set_engine("xgboost",
             validation = 0.15,
             alpha  = tune("reg_alpha"),
             lambda = tune("reg_lambda")) %>%
  set_mode("regression")

rec_reg <- recipe(home_result ~ ., data = train_reg) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wf_reg    <- workflow() %>% add_recipe(rec_reg) %>% add_model(spec_reg)
ps_reg    <- build_param_set(wf_reg)

set.seed(SEED)
grid_reg  <- grid_latin_hypercube(ps_reg, size = GRID_SIZE)
cat(sprintf("  Grid: %d combos × %d folds = %d fits\n",
            nrow(grid_reg), length(folds_reg$splits),
            nrow(grid_reg) * length(folds_reg$splits)))

t0 <- Sys.time()
tune_reg <- wf_reg %>%
  tune_grid(
    resamples = folds_reg,
    grid      = grid_reg,
    metrics   = metric_set(rmse, mae),
    control   = control_grid(save_pred = FALSE, verbose = FALSE, allow_par = TRUE)
  )
cat(sprintf("  Regression tuning took %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

best_reg <- select_best(tune_reg, metric = "rmse")
cat("\nBest regression params (RMSE):\n")
print(as.data.frame(best_reg))

cat("\nTop 5 regression configs by RMSE:\n")
print(as.data.frame(show_best(tune_reg, metric = "rmse", n = 5)))

write_csv(collect_metrics(tune_reg), file.path(OUTPUT_DIR, "tune_results_reg.csv"))
saveRDS(best_reg, file.path(OUTPUT_DIR, "tune_best_params_reg.rds"))

cat("\nDone. Saved:\n")
cat("  data/tune_best_params_reg.rds\n")
cat("  data/tune_results_reg.csv\n")
cat("Classification model removed — win probability now derived from regression via pnorm().\n")
