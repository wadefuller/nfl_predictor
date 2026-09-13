# ============================================
# NFL Predictor: Ensemble Inference Library
# ============================================
# Shared helpers for training and predicting with the production ensemble.
# Sourced by:
#   - train_and_save_ensemble.R  (training + saving bundle)
#   - predict_week.rmd           (current-week picks for Google Sheet)
#   - export_json.R              (JSON export for the website)
#
# The "bundle" object packages everything needed to predict on new games:
#   - xgb, glm (recipe + cv.glmnet), rf — base learners
#   - stack    — NNLS stacking coefficients
#   - sigma    — residual SD of stacked predictions (for pnorm winprob)
#   - platt    — logistic recalibration coefficients
#   - leakage_vars — columns to drop from features at predict time
#   - metadata — training_seasons, trained_date, n_features
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(glmnet)
  library(ranger)
})

# ---- Canonical paths (env-var overridable for portability) ----------------
# Override via `NFL_PROJECT_DIR=/some/path Rscript ...` to relocate the repo
# without editing source. All scripts sourcing this file should use the
# constants below rather than re-defining their own PROJECT_DIR / DATA_DIR.
PROJECT_DIR <- Sys.getenv("NFL_PROJECT_DIR",
                          unset = path.expand("~/Projects/R/nfl_predictor"))
DATA_DIR    <- file.path(PROJECT_DIR, "data")
MODELS_DIR  <- file.path(PROJECT_DIR, "models")

# Columns that must NOT be features (outcomes, leakage). SINGLE source of
# truth — `walk_forward_eval.R` and `tune_hyperparams.R` import from here.
LEAKAGE_VARS_REG <- c(
  "game_id", "gameday", "gametime", "team", "opposing_team",
  "points_for", "points_against", "total_points_scored", "pct_points_scored",
  "result", "win", "home", "game_info_gain", "win_prob_captured",
  # Offensive current-game stats
  "off_epa", "off_pass_epa", "off_rush_epa", "success_rate", "qb_epa",
  # Defensive current-game stats (incl. splits added 2026-05-23)
  "def_epa", "def_pass_epa", "def_rush_epa", "def_success_rate",
  "def_sack_rate", "def_qb_hit_rate", "def_stuff_rate",
  "def_int_rate", "def_third_dn_rate",
  # Pace + total environment current-game (rolling versions are the features)
  "team_points_against", "team_total_env", "team_off_plays",
  "home_points", "away_points", "home_pct_points_scored", "away_pct_points_scored",
  "home_win", "spread_adjusted_result", "spread_cover",
  # SoS-adjusted EPA — disabled (experiment hurt every strategy 1-4pp ROI).
  # Kept in CSV for future use; dropped at recipe stage.
  "adj_off_epa_r4_home", "adj_off_epa_r4_away", "adj_off_epa_r4_home_diff",
  "adj_off_epa_r16_home", "adj_off_epa_r16_away", "adj_off_epa_r16_home_diff",
  "adj_def_epa_r4_home", "adj_def_epa_r4_away", "adj_def_epa_r4_home_diff",
  "adj_def_epa_r16_home", "adj_def_epa_r16_away", "adj_def_epa_r16_home_diff",
  # 2026-05-21 audit — dropped after measuring Gain < 0.001 in shipped XGB.
  # Rest features: model treats raw days as noise. Could re-engineer later
  # (back_to_back_road, short_week_after_long_travel) — kept in CSV for that.
  "rest_diff", "home_team_rest", "away_team_rest",
  # Venue dummies: is_dome / is_grass don't add beyond temp/wind/roof signal.
  "is_dome", "is_grass"
)

# ---- Dead-weight pruning (ON by default; PRUNE_DEAD=0 to disable) ----------
# These do NOT leak. They are columns the feature census (./run_pipeline.sh
# census) found to be dead weight in production: zero nonzero coefficients in
# the GLM and ~1.6% of XGB gain between all of them. 29 raw columns, but 117
# matrix columns after one-hot expansion — 30% of the design matrix.
#
# Measured effect on walk-forward 2016-2025 (2026-06-07):
#   MAE     9.7783 -> 9.7738  (-0.0045)
#   Brier   0.21033 -> 0.20986 (-0.00047, real)
#   LogLoss 0.60871 -> 0.60749 (-0.00122, real)
#   Cover   0.5336  -> 0.5360  (+0.24pp)
#   seasons beating Vegas: 4 -> 5; the five most recent all improved.
#
# NOTE the stated mechanism was WRONG. The hypothesis was that fewer candidate
# columns would let cv.glmnet pick a SMALLER lambda and retain more signal.
# Lambda actually went UP (1.58 -> 2.06) with the coefficient count unchanged
# at 19. So the gain is not coming from the regularisation path; the most
# likely explanation is reduced variance in the CV lambda selection itself
# (fewer columns -> more stable fold-to-fold choice). Unconfirmed.
PRUNED_VARS_REG <- c(
  # -> 66 team-identity dummies (XGB gain 0.2%, GLM coefs 0)
  "home_team", "away_team",
  # -> 30 schedule dummies (XGB gain 0.2%, GLM coefs 0)
  "week", "weekday", "div_game",
  # rest module: 18 raw -> 15 after nzv (XGB gain 0.3%, GLM coefs 0)
  "rest_short_week_home", "rest_extra_short_home", "rest_off_bye_home",
  "rest_advantage_home", "rest_b2b_road_home", "rest_b2b_home_home",
  "rest_short_week_away", "rest_extra_short_away", "rest_off_bye_away",
  "rest_advantage_away", "rest_b2b_road_away", "rest_b2b_home_away",
  "rest_short_week_diff", "rest_extra_short_diff", "rest_off_bye_diff",
  "rest_advantage_diff", "rest_b2b_road_diff", "rest_b2b_home_diff",
  # officials: 4 (XGB gain 0.6%, GLM coefs 0)
  "ref_career_n_games", "ref_career_home_win_pct",
  "ref_career_avg_total", "ref_career_avg_margin",
  # weather: 2 surviving after nzv (XGB gain 0.3%, GLM coefs 0)
  "temp", "wind"
)

if (Sys.getenv("PRUNE_DEAD", "1") != "0") {
  LEAKAGE_VARS_REG <- c(LEAKAGE_VARS_REG, PRUNED_VARS_REG)
}

# ---- Stage-2 block pruning (PRUNE_BLOCKS, default none) --------------------
# Larger feature blocks that also carry zero GLM coefficients. Unlike the
# stage-1 set these have real XGB gain, so they are opt-in and pattern-matched
# rather than pruned by default.
#
# TESTED AND NOT ADOPTED (2026-09-02). Left as opt-in infrastructure only.
#
# The holdout screen (prune_screen.R, 2024-25) looked strong — removing these
# lowers the lambda cv.glmnet picks and lets MORE coefficients survive:
#   baseline                        lambda 2.065  19 coefs  MAE 9.5833
#   -injuries                       lambda 1.423  25 coefs  MAE 9.5683
#   -injuries,snap                  lambda 1.297  25 coefs  MAE 9.5637
#   -injuries,snap,def_epa          lambda 0.981  29 coefs  MAE 9.5514
#   all four                        lambda 0.894  29 coefs  MAE 9.5482
# (def_epa / snap_counts / pythag alone are bit-identical to baseline; only
# the combined column count moves the CV path.)
#
# On the full 10-season walk-forward it evaporated:
#   arm            predicted MAE   actual MAE   Cover      seasons beating Vegas
#   -injuries         -0.0149       -0.0018    -0.43pp     5 -> 4
#   all four          -0.0351       -0.0002    +0.20pp     5 -> 4
# Off by 8x and 175x. The lambda effect is real but does not translate into
# better out-of-sample predictions: a single 2008-2023 fit is one draw of the
# CV path, and the walk-forward retrains per fold where it washes out.
#
# Arm B is a genuine wash on aggregate accuracy with 52% fewer predictors
# (308 -> 148), so it is tempting for fit speed. Not made default because it
# costs a season of beating Vegas, is worse in 6 of 10 seasons (the flat
# average rides on 2016 and 2024 alone), and strips EVERY defensive and
# injury feature — cheap on average, plausibly brittle in specific matchups.
#
# Usage (opt-in): PRUNE_BLOCKS=injuries,snap_counts,def_epa,pythag
PRUNE_BLOCK_PATTERNS <- c(
  def_epa     = "^def_(epa|pass_epa|rush_epa|success_rate|sack_rate|qb_hit_rate|stuff_rate|int_rate|third_dn_rate)",
  injuries    = "^inj_",
  snap_counts = "^snap_",
  pythag      = "^pythag"
)

expand_prune_blocks <- function(nms) {
  sel <- Sys.getenv("PRUNE_BLOCKS", "")
  if (!nzchar(sel)) return(character(0))
  keys <- trimws(strsplit(sel, ",")[[1]])
  bad  <- setdiff(keys, names(PRUNE_BLOCK_PATTERNS))
  if (length(bad)) {
    stop(sprintf("PRUNE_BLOCKS: unknown block(s) %s. Valid: %s",
                 paste(bad, collapse = ", "),
                 paste(names(PRUNE_BLOCK_PATTERNS), collapse = ", ")))
  }
  unique(unlist(lapply(keys, function(k)
    grep(PRUNE_BLOCK_PATTERNS[[k]], nms, value = TRUE))))
}

# Backward-compat alias — train_and_save_ensemble.R + older sites referenced
# ENSEMBLE_LEAKAGE_VARS. Both names point to the same vector.
ENSEMBLE_LEAKAGE_VARS <- LEAKAGE_VARS_REG

# ---- XGB hyperparameter loading (shared by walk_forward + train_and_save) --
DEFAULT_REG_PARAMS <- list(
  trees = 800, tree_depth = 5, learn_rate = 0.012, min_n = 18,
  loss_reduction = 0.01, sample_size = 0.755, stop_iter = 25
)

load_tuned_xgb_params <- function(path, defaults = DEFAULT_REG_PARAMS) {
  if (!file.exists(path)) {
    cat(sprintf("  [no tuned params at %s — using defaults]\n", basename(path)))
    return(defaults)
  }
  p <- readRDS(path)
  out <- list(
    trees          = defaults$trees,
    tree_depth     = as.integer(p$tree_depth),
    learn_rate     = p$learn_rate,
    min_n          = as.integer(p$min_n),
    loss_reduction = p$loss_reduction,
    sample_size    = p$sample_size,
    mtry           = if ("mtry" %in% names(p)) as.integer(p$mtry) else NULL,
    stop_iter      = defaults$stop_iter,
    reg_alpha      = if ("reg_alpha" %in% names(p)) p$reg_alpha else 0,
    reg_lambda     = if ("reg_lambda" %in% names(p)) p$reg_lambda else 1
  )
  cat(sprintf("  [loaded tuned XGB params from %s]\n", basename(path)))
  out
}

# Tuned GLM alpha (cv.glmnet elastic-net mix). Default 0.5 if no tune file.
DEFAULT_GLM_ALPHA <- 0.5
load_tuned_glm_alpha <- function(path = file.path(DATA_DIR, "tune_best_glm.rds"),
                                  default = DEFAULT_GLM_ALPHA) {
  if (!file.exists(path)) return(default)
  p <- readRDS(path)
  cat(sprintf("  [loaded tuned GLM alpha=%.2f from %s]\n", p$alpha, basename(path)))
  as.numeric(p$alpha)
}

# Tuned stacking λ (multi-objective RMSE vs LogLoss mix). Default 0.5.
DEFAULT_STACKING_LAMBDA <- 0.5
load_tuned_stacking_lambda <- function(path = file.path(DATA_DIR, "tune_best_lambda.rds"),
                                        default = DEFAULT_STACKING_LAMBDA) {
  if (!file.exists(path)) return(default)
  p <- readRDS(path)
  cat(sprintf("  [loaded tuned stacking λ=%.2f from %s]\n",
              p$lambda, basename(path)))
  as.numeric(p$lambda)
}

# Tuned RF (mtry, min_n). Defaults are sqrt(n_features) and 10.
DEFAULT_RF_PARAMS <- list(mtry = NULL, min_n = 10, trees = 500)
load_tuned_rf_params <- function(path = file.path(DATA_DIR, "tune_best_rf.rds"),
                                  default = DEFAULT_RF_PARAMS) {
  if (!file.exists(path)) return(default)
  p <- readRDS(path)
  out <- list(mtry = as.integer(p$mtry),
              min_n = as.integer(p$min_n),
              trees = if ("trees" %in% names(p)) as.integer(p$trees) else 500)
  cat(sprintf("  [loaded tuned RF params from %s: mtry=%d min_n=%d]\n",
              basename(path), out$mtry, out$min_n))
  out
}

# ---- Data prep ------------------------------------------------------------

# Training-time prep: applies the standard regular-season filter + drops
# outcome-derived columns. home_result is kept (it's the target).
prep_for_train <- function(data,
                            leakage_vars = ENSEMBLE_LEAKAGE_VARS) {
  drop <- c(leakage_vars, expand_prune_blocks(names(data)))
  data %>%
    filter(
      !is.na(result), !is.na(home_moneyline),
      (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021)
    ) %>%
    mutate(across(c(season, week, weekday, div_game), as.factor)) %>%
    select(-any_of(drop), -any_of("season"))
}

# Predict-time prep: no outcome filtering (live games have NA result).
# Drops leakage columns and the season factor. Keeps home_result if present
# (used as recipe outcome at predict time; tidymodels handles missing outcome
# but having the column with NA is safer).
prep_for_predict <- function(data,
                              leakage_vars = ENSEMBLE_LEAKAGE_VARS) {
  # Prediction-time column selection is driven ONLY by `leakage_vars`, which
  # for the production path is bundle$leakage_vars — the EFFECTIVE list
  # train_and_save stored (leakage + any expanded PRUNE_BLOCKS at train time).
  #
  # Deliberately does NOT consult PRUNE_BLOCKS. An earlier version added
  # expand_prune_blocks() here as "belt and braces" and it was a live bug: with
  # PRUNE_BLOCKS set in the serving environment, predicting from a bundle
  # trained WITHOUT it stripped columns the baked recipe required and threw
  # inside hardhat::validate_column_names. A bundle must predict identically
  # regardless of ambient env; the trained artifact is the source of truth.
  drop <- leakage_vars
  data %>%
    mutate(across(c(season, week, weekday, div_game), as.factor)) %>%
    select(-any_of(drop), -any_of("season")) %>%
    { if ("home_result" %in% names(.)) . else mutate(., home_result = NA_real_) }
}

# ---- Base learners --------------------------------------------------------

# Tuned direct-xgboost learner (2026-06-07). The tidymodels wrapper cannot
# express a monotone constraint, which turns out to matter a lot here: the
# target is ~linear in spread_line, and unconstrained trees approximate that
# with a staircase that collapses toward the training mean at the tails.
# Constraining spread_line to be monotone increasing, with depth-2 stumps and
# heavy L2, took XGB from WORSE than quoting Vegas (9.867 on 2020-23 folds)
# to better than it (9.774). Config selected by xgb_challenge.R.
#
# Enabled with XGB_DIRECT=1. Returns an object whose predict() method mimics
# the parsnip workflow interface (a tibble with .pred) so call sites in
# walk_forward_eval.R / train_and_save_ensemble.R need no changes.
XGB_DIRECT_DEFAULTS <- list(
  eta = 0.00778, max_depth = 2L, min_child_weight = 20,
  subsample = 0.816, colsample_bytree = 0.363,
  alpha = 0.0207, lambda = 13.079, nrounds = 441L
)

# Param sources, in priority order:
#   1. data/tune_best_xgb_direct.rds   — canonical, written by tune_hyperparams.R
#   2. data/xgb_challenge_results.rds  — the one-off sweep that first found the
#                                        monotone/depth-2 regime
#   3. XGB_DIRECT_DEFAULTS             — that sweep's winner, hardcoded
load_xgb_direct_params <- function() {
  tuned <- file.path(DATA_DIR, "tune_best_xgb_direct.rds")
  if (file.exists(tuned)) {
    p <- tryCatch(readRDS(tuned), error = function(e) NULL)
    # Refuse params that failed the tuner's market gate. Shipping params that
    # lose to simply quoting the closing line is the exact bug the rewrite
    # was meant to eliminate, so don't let them back in silently.
    if (!is.null(p$gate) && p$gate == "fail") {
      warning(sprintf(paste0("tune_best_xgb_direct.rds FAILED its market gate ",
                             "(cv MAE %.4f vs market %.4f); ignoring it and ",
                             "falling back. Re-tune before using XGB_DIRECT."),
                      p$cv_mae, p$market_mae), call. = FALSE)
      p <- NULL
    }
    if (!is.null(p$eta)) {
      cvm <- if (is.null(p$cv_mae)) NA_real_ else p$cv_mae
      mkm <- if (is.null(p$market_mae)) NA_real_ else p$market_mae
      message(sprintf("  [loaded tuned direct-XGB params: eta=%.5f depth=%d rounds=%d (cv MAE %.4f vs market %.4f)]",
                      p$eta, p$max_depth, p$nrounds, cvm, mkm))
      return(p[c("eta","max_depth","min_child_weight","subsample",
                 "colsample_bytree","alpha","lambda","nrounds")])
    }
  }
  f <- file.path(DATA_DIR, "xgb_challenge_results.rds")
  if (file.exists(f)) {
    b <- tryCatch(readRDS(f)$best, error = function(e) NULL)
    if (!is.null(b)) {
      message(sprintf("  [loaded direct-XGB params from challenge sweep: eta=%.5f depth=%d rounds=%d]",
                      b$eta, as.integer(b$depth), as.integer(b$iters)))
      return(list(eta = b$eta, max_depth = as.integer(b$depth),
                  min_child_weight = b$min_child, subsample = b$subsample,
                  colsample_bytree = b$colsample, alpha = b$alpha,
                  lambda = b$lambda, nrounds = as.integer(b$iters)))
    }
  }
  XGB_DIRECT_DEFAULTS
}

train_xgb_direct <- function(train_data, seed = 989,
                              params = load_xgb_direct_params()) {
  rec <- recipes::recipe(home_result ~ ., data = train_data) %>%
    recipes::step_nzv(recipes::all_numeric_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)
  prep_rec <- recipes::prep(rec, training = train_data)
  baked    <- recipes::bake(prep_rec, new_data = train_data)
  x <- as.matrix(baked %>% select(-any_of("home_result")))
  y <- baked$home_result

  mono <- rep(0L, ncol(x))
  sc   <- which(colnames(x) == "spread_line")
  if (length(sc) == 1) mono[sc] <- 1L

  xgb_params <- list(
    objective = "reg:squarederror", eval_metric = "mae",
    eta = params$eta, max_depth = params$max_depth,
    min_child_weight = params$min_child_weight, subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    alpha = params$alpha, lambda = params$lambda, nthread = 4,
    monotone_constraints = paste0("(", paste(mono, collapse = ","), ")")
  )
  set.seed(seed)
  fit <- xgboost::xgb.train(params = xgb_params,
                            data = xgboost::xgb.DMatrix(x, label = y),
                            nrounds = params$nrounds, verbose = 0)
  structure(list(fit = fit, recipe = prep_rec, cols = colnames(x)),
            class = "xgb_direct")
}

# parsnip-compatible predict shim so existing call sites keep working.
predict.xgb_direct <- function(object, new_data, ...) {
  baked <- recipes::bake(object$recipe, new_data = new_data)
  x <- as.matrix(baked %>% select(-any_of("home_result")))
  x <- x[, object$cols, drop = FALSE]
  tibble::tibble(.pred = as.numeric(stats::predict(object$fit, x)))
}

train_xgb_model <- function(train_data,
                             params = get0("REG_PARAMS", inherits = TRUE,
                                            ifnotfound = DEFAULT_REG_PARAMS),
                             seed = 989) {
  if (Sys.getenv("XGB_DIRECT", "0") == "1") {
    return(train_xgb_direct(train_data, seed = seed))
  }
  set.seed(seed)
  n_feats  <- ncol(train_data) - 1
  mtry_val <- if (!is.null(params$mtry)) params$mtry else max(1L, floor(sqrt(n_feats)))

  spec <- boost_tree(
    trees          = params$trees,
    tree_depth     = params$tree_depth,
    learn_rate     = params$learn_rate,
    min_n          = params$min_n,
    loss_reduction = params$loss_reduction,
    sample_size    = params$sample_size,
    mtry           = mtry_val,
    stop_iter      = params$stop_iter
  ) %>%
    set_engine("xgboost",
               validation = 0.15,
               alpha  = if (!is.null(params$reg_alpha))  params$reg_alpha  else 0,
               lambda = if (!is.null(params$reg_lambda)) params$reg_lambda else 1) %>%
    set_mode("regression")

  rec <- recipe(home_result ~ ., data = train_data) %>%
    step_nzv(all_numeric_predictors()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

  workflow() %>% add_recipe(rec) %>% add_model(spec) %>% fit(data = train_data)
}

train_glm_model <- function(train_data, seed = 989,
                              alpha = get0("GLM_ALPHA", inherits = TRUE,
                                            ifnotfound = load_tuned_glm_alpha())) {
  set.seed(seed)
  rec <- recipe(home_result ~ ., data = train_data) %>%
    step_nzv(all_numeric_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_normalize(all_numeric_predictors())

  prep_rec <- prep(rec, training = train_data)
  baked    <- bake(prep_rec, new_data = train_data)
  x_mat    <- as.matrix(baked %>% select(-home_result))
  y_vec    <- baked$home_result

  cv_fit <- cv.glmnet(x_mat, y_vec, alpha = alpha, nfolds = 5)
  list(recipe = prep_rec, model = cv_fit,
       lambda = cv_fit$lambda.min, alpha = alpha)
}

predict_glm <- function(glm_obj, new_data) {
  baked <- bake(glm_obj$recipe, new_data = new_data)
  x_new <- as.matrix(baked %>% select(-any_of("home_result")))
  as.numeric(predict(glm_obj$model, newx = x_new, s = "lambda.min"))
}

train_rf_model <- function(train_data, seed = 989,
                            rf_params = get0("RF_PARAMS", inherits = TRUE,
                                              ifnotfound = load_tuned_rf_params())) {
  set.seed(seed)
  n_feats <- ncol(train_data) - 1
  mtry_val <- if (!is.null(rf_params$mtry)) rf_params$mtry
              else max(1L, floor(sqrt(n_feats)))

  spec <- rand_forest(trees = rf_params$trees,
                       mtry = mtry_val, min_n = rf_params$min_n) %>%
    set_engine("ranger", seed = seed) %>%
    set_mode("regression")

  rec <- recipe(home_result ~ ., data = train_data) %>%
    step_nzv(all_numeric_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

  workflow() %>% add_recipe(rec) %>% add_model(spec) %>% fit(data = train_data)
}

# ---- Stacking + calibration -----------------------------------------------

fit_stacking_nnls <- function(xgb_pred, glm_pred, rf_pred, actual) {
  m_x <- mean(xgb_pred); m_g <- mean(glm_pred); m_r <- mean(rf_pred); m_a <- mean(actual)
  A <- cbind(xgb_pred - m_x, glm_pred - m_g, rf_pred - m_r)
  b <- actual - m_a
  fit <- nnls::nnls(A, b)
  w <- fit$x
  intercept <- m_a - w[1] * m_x - w[2] * m_g - w[3] * m_r
  list(intercept = intercept, w_xgb = w[1], w_glm = w[2], w_rf = w[3],
       method = "nnls")
}

# Logloss-based stacking: minimize log loss of pnorm-derived winprob, subject
# to non-negative weights. Directly optimizes what our ML betting strategies
# depend on (calibrated winprob) instead of margin RMSE.
# Uses a 2-step approach: pilot NNLS fit to estimate sigma, then constrained
# Nelder-Mead on 4 params (intercept + 3 non-negative weights).
fit_stacking_logloss <- function(xgb_pred, glm_pred, rf_pred, actual) {
  # Pilot NNLS for initial sigma + weight start
  pilot     <- fit_stacking_nnls(xgb_pred, glm_pred, rf_pred, actual)
  pilot_pred <- apply_stacking(pilot, xgb_pred, glm_pred, rf_pred)
  sigma_pilot <- sd(actual - pilot_pred, na.rm = TRUE)

  actual_win <- as.integer(actual > 0)

  loss_fn <- function(theta) {
    pred   <- theta[1] + theta[2] * xgb_pred + theta[3] * glm_pred + theta[4] * rf_pred
    raw_wp <- pnorm(pred / sigma_pilot)
    p      <- pmin(pmax(raw_wp, 1e-15), 1 - 1e-15)
    -mean(actual_win * log(p) + (1 - actual_win) * log(1 - p))
  }

  # Constraint matrix: each of the 3 weights must be ≥ 0; intercept unconstrained
  ui <- rbind(c(0, 1, 0, 0),
              c(0, 0, 1, 0),
              c(0, 0, 0, 1))
  ci <- c(0, 0, 0)

  # Strict-interior starting point (constrOptim requires init satisfies ui·θ > ci)
  init <- c(pilot$intercept,
            max(pilot$w_xgb, 0.01),
            max(pilot$w_glm, 0.01),
            max(pilot$w_rf, 0.01))

  res <- constrOptim(theta = init, f = loss_fn, grad = NULL,
                     ui = ui, ci = ci, method = "Nelder-Mead",
                     control = list(maxit = 5000, reltol = 1e-8))

  list(intercept = res$par[1],
       w_xgb     = res$par[2],
       w_glm     = res$par[3],
       w_rf      = res$par[4],
       cal_logloss = res$value,
       converged   = res$convergence == 0,
       method      = "logloss")
}

# Multi-objective: λ·(normalized RMSE) + (1-λ)·(normalized logloss).
# Each metric is normalized by its pilot value so both sit near 1 at the
# starting point. λ=1 → pure RMSE (NNLS-equivalent), λ=0 → pure logloss.
fit_stacking_multi <- function(xgb_pred, glm_pred, rf_pred, actual, lambda = 0.5) {
  pilot       <- fit_stacking_nnls(xgb_pred, glm_pred, rf_pred, actual)
  pilot_pred  <- apply_stacking(pilot, xgb_pred, glm_pred, rf_pred)
  sigma_pilot <- sd(actual - pilot_pred, na.rm = TRUE)
  actual_win  <- as.integer(actual > 0)

  # Normalization references (pilot RMSE = sigma_pilot; pilot logloss from pnorm)
  rmse_pilot <- sigma_pilot
  pilot_wp   <- pnorm(pilot_pred / sigma_pilot)
  p_pilot    <- pmin(pmax(pilot_wp, 1e-15), 1 - 1e-15)
  logloss_pilot <- -mean(actual_win * log(p_pilot) + (1 - actual_win) * log(1 - p_pilot))

  loss_fn <- function(theta) {
    pred    <- theta[1] + theta[2]*xgb_pred + theta[3]*glm_pred + theta[4]*rf_pred
    rmse    <- sqrt(mean((pred - actual)^2))
    raw_wp  <- pnorm(pred / sigma_pilot)
    p       <- pmin(pmax(raw_wp, 1e-15), 1 - 1e-15)
    logloss <- -mean(actual_win * log(p) + (1 - actual_win) * log(1 - p))
    lambda * (rmse / rmse_pilot) + (1 - lambda) * (logloss / logloss_pilot)
  }

  ui <- rbind(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1))
  ci <- c(0, 0, 0)
  init <- c(pilot$intercept,
            max(pilot$w_xgb, 0.01),
            max(pilot$w_glm, 0.01),
            max(pilot$w_rf, 0.01))

  res <- constrOptim(theta = init, f = loss_fn, grad = NULL,
                     ui = ui, ci = ci, method = "Nelder-Mead",
                     control = list(maxit = 5000, reltol = 1e-8))

  list(intercept = res$par[1], w_xgb = res$par[2], w_glm = res$par[3], w_rf = res$par[4],
       lambda = lambda, converged = res$convergence == 0, method = "multi")
}

# Dispatch: pick stacking objective by name. Defaults look up module-level
# `STACKING_METHOD` / `STACKING_LAMBDA` if set (walk_forward_eval.R defines
# these); otherwise falls back to safe defaults.
fit_stacking <- function(xgb_pred, glm_pred, rf_pred, actual,
                          method = get0("STACKING_METHOD", inherits = TRUE,
                                         ifnotfound = "multi"),
                          lambda = get0("STACKING_LAMBDA", inherits = TRUE,
                                         ifnotfound = 0.5)) {
  # GLM-only architecture (default since 2026-06-07). The cv.glmnet learner
  # is the only one carrying real signal in this dataset; XGB and RF both
  # got stacking weight 0 in every recent multi-stacked bundle. Worse, the
  # multi-stacker was AMPLIFYING the GLM by ~1.23× to over-fit cal-set
  # residuals — which catastrophically overshot magnitude on walk-forward,
  # especially on close games (the +0.225 MAE bucket that 6 feature
  # experiments tried and failed to fix).
  #
  # Switching to pure unamplified GLM dropped walk-forward MAE 9.832 → 9.778
  # (model now BEATS Vegas), Brier 0.21075 → 0.21033, LogLoss 0.60967 →
  # 0.60871. The close-game bucket went from +0.225 MAE worse than Vegas
  # to -0.036 MAE BETTER than Vegas (a 0.26 MAE swing). Big-road-favorites
  # 7.5-10 went from +0.273 to -0.043.
  #
  # STACK_MODE selects the architecture:
  #   "glm_only" (default) — pure unamplified GLM
  #   "xgb_only"           — pure XGB (pair with XGB_DIRECT=1 for the tuned one)
  #   "full"               — real multi-objective NNLS over all three
  # STACK_FULL=1 is kept as a legacy alias for STACK_MODE=full.
  stack_mode <- Sys.getenv("STACK_MODE", "")
  if (stack_mode == "" ) {
    stack_mode <- if (Sys.getenv("STACK_FULL", "0") == "1") "full" else "glm_only"
  }
  if (stack_mode == "glm_only") {
    return(list(intercept = 0, w_xgb = 0, w_glm = 1, w_rf = 0,
                method = "glm_only"))
  }
  if (stack_mode == "xgb_only") {
    return(list(intercept = 0, w_xgb = 1, w_glm = 0, w_rf = 0,
                method = "xgb_only"))
  }
  method <- match.arg(method, choices = c("nnls", "logloss", "multi"))
  if      (method == "nnls")    fit_stacking_nnls(xgb_pred, glm_pred, rf_pred, actual)
  else if (method == "logloss") fit_stacking_logloss(xgb_pred, glm_pred, rf_pred, actual)
  else                          fit_stacking_multi(xgb_pred, glm_pred, rf_pred, actual, lambda)
}

apply_stacking <- function(stack, xgb_pred, glm_pred, rf_pred) {
  stack$intercept + stack$w_xgb * xgb_pred + stack$w_glm * glm_pred + stack$w_rf * rf_pred
}

# ---- Margin calibration --------------------------------------------------
# Sits between stacking and pnorm(/σ). Learns actual_margin ≈ f(predicted)
# fit on the cal set. Addresses the "fat-tail" pattern: model overshoots
# magnitude on close games (MAE +0.225 on 0-3 actual) while being well-
# calibrated on blowouts (MAE -0.27 on 14+).
#
# Methods:
#   "linear"   — 2-param OLS shrinkage. Safe default; can't overfit.
#                Symmetric shrinkage toward 0 (if slope<1) + intercept shift.
#   "isotonic" — base R isoreg, monotone non-decreasing piecewise fit.
#                More flexible but OVERFIT THE CAL SET in our walk-forward
#                test (close-game gap actually widened); use only with
#                large cal samples (5+ seasons).
#
# Bundle stores a $fn (callable). apply_margin_calibration() applies it,
# falling through for NULL (back-compat with older bundles).
fit_margin_calibrator <- function(pred, actual, method = c("linear", "isotonic")) {
  method <- match.arg(method)
  ok <- !is.na(pred) & !is.na(actual)
  pred   <- pred[ok]
  actual <- actual[ok]
  if (method == "linear") {
    fit <- stats::lm(actual ~ pred)
    coefs <- stats::coef(fit)
    intercept <- unname(coefs[1])
    slope     <- unname(coefs[2])
    fn <- local({
      ic <- intercept; sl <- slope
      function(x) ic + sl * x
    })
    return(list(fn = fn, intercept = intercept, slope = slope,
                n_train = length(pred), method = "linear"))
  }
  # isotonic
  fit <- stats::isoreg(x = pred, y = actual)
  x_sorted <- pred[fit$ord]
  y_sorted <- fit$yf
  fn <- approxfun(x_sorted, y_sorted, method = "linear", rule = 2, ties = mean)
  list(fn = fn, x = x_sorted, y = y_sorted,
       n_train = length(pred), method = "isotonic")
}

apply_margin_calibration <- function(pred, cal) {
  if (is.null(cal) || is.null(cal$fn)) return(pred)  # back-compat pass-through
  out <- cal$fn(pred)
  ifelse(is.na(pred), NA_real_, out)
}

# Back-compat aliases (older code uses these names)
fit_margin_iso   <- function(pred, actual) fit_margin_calibrator(pred, actual, method = "linear")
apply_margin_iso <- apply_margin_calibration

fit_platt <- function(raw_wp, actual_win) {
  p_clamped <- pmin(pmax(raw_wp, 1e-6), 1 - 1e-6)
  fit <- glm(actual_win ~ qlogis(p_clamped), family = binomial())
  list(intercept = unname(coef(fit)[1]), slope = unname(coef(fit)[2]))
}

# Alias kept for backward-compat with walk_forward_eval.R
fit_platt_logits <- fit_platt

apply_platt <- function(raw_wp, platt) {
  p_clamped <- pmin(pmax(raw_wp, 1e-6), 1 - 1e-6)
  plogis(platt$intercept + platt$slope * qlogis(p_clamped))
}

# ---- Win probability: margin -> P(home win) -------------------------------
# Two routes, selected by WINPROB_METHOD. Default is the shipped behaviour.
#
#   "pnorm_platt" (default)  pnorm(margin / sigma) then a Platt logistic on the
#                            resulting probability. TWO transformations, and the
#                            first assumes one global residual SD applies to every
#                            game regardless of how uncertain that matchup is.
#
#   "logistic"    (arm C)    a single logistic fitted directly on predicted
#                            margin. One transformation, slope learned from data
#                            rather than inherited from a pooled sigma.
#
# WHY THE ALTERNATIVE EXISTS. On a 10-fold walk-forward (2016-2025, 2,638 games)
# the single logistic scored Brier 0.20909 / logloss 0.60525 against the shipped
# chain's 0.20982 / 0.60730, beating it in 7 of 10 seasons and beating the market
# baseline (0.21039) outright. The paired difference is -0.00073, 95% CI
# [-0.00148, +0.00001], p = 0.052 — suggestive, NOT established, which is exactly
# why this is a flag and not a replacement. Validate with a full
# `./run_pipeline.sh eval` before considering a default change.
#
# Note this was tested against a DIRECT CLASSIFIER too (xgboost binary:logistic
# on the same features): that scored 0.21808, worse than everything including the
# market. The binary label discards margin magnitude, and at this sample size
# that loss dominates. The transformation is the problem, not the regression.
WINPROB_METHOD <- Sys.getenv("WINPROB_METHOD", "pnorm_platt")

#' Fit the margin -> win-probability map on a calibration split.
fit_winprob <- function(cal_pred_margin, cal_actual_win, sigma,
                        method = WINPROB_METHOD) {
  if (identical(method, "logistic")) {
    fit <- stats::glm(cal_actual_win ~ cal_pred_margin, family = stats::binomial())
    return(list(method = "logistic",
                intercept = unname(coef(fit)[1]), slope = unname(coef(fit)[2])))
  }
  list(method = "pnorm_platt",
       platt = fit_platt(pnorm(cal_pred_margin / sigma), cal_actual_win))
}

#' Apply it. Falls back to the legacy sigma+platt path for bundles trained before
#' `winprob` existed, so an old ensemble_latest.rds keeps scoring identically.
apply_winprob <- function(pred_margin, winprob, sigma, platt = NULL) {
  if (is.null(winprob) || is.null(winprob$method))
    return(apply_platt(pnorm(pred_margin / sigma), platt))
  if (identical(winprob$method, "logistic"))
    return(plogis(winprob$intercept + winprob$slope * pred_margin))
  apply_platt(pnorm(pred_margin / sigma), winprob$platt)
}

# ---- Portfolio betting helpers --------------------------------------------
# Shared by portfolio_sim.R (backtest) and predict_week.rmd (live picks).
# Default params chosen from the portfolio sweep (best Sharpe at deployable DD).

PORTFOLIO_DEFAULTS <- list(
  # Note: min_ev now applies to CALIBRATED EV (see EV_CALIBRATION below).
  # Raw model EVs are systematically inflated (slope ≈ 0.36 vs realized).
  # Calibrated 0.05 ≈ raw 0.128 ≈ "5% real edge per $1".
  min_ev          = 0.05,
  kelly_mult      = 0.15,
  max_bet_pct     = 0.05,
  max_abs_odds    = 300,
  round_to        = 5,
  min_stake       = 5
)

# Linear calibration of predicted → realized EV.
# The default is a stale fit from 2026-05; production callers should pass a
# fresh calibration from the trained bundle's metadata (see
# `train_and_save_ensemble.R`, which calls `recalibrate_ev.R` after Stage 2
# and stores `ev_calibration` in `bundle$metadata`).
EV_CALIBRATION_DEFAULT <- list(intercept = 0.0039, slope = 0.3596)
# Kept for back-compat — older code references EV_CALIBRATION directly.
EV_CALIBRATION <- EV_CALIBRATION_DEFAULT

calibrate_ev <- function(ev_raw, cal = EV_CALIBRATION_DEFAULT) {
  cal$intercept + cal$slope * ev_raw
}

# Extract EV calibration from a trained bundle. Falls back to the default if
# the bundle was trained before EV cal was baked in.
ev_calibration_from_bundle <- function(bundle) {
  cal <- bundle$metadata$ev_calibration
  if (is.null(cal) || is.null(cal$intercept) || is.null(cal$slope)) {
    EV_CALIBRATION_DEFAULT
  } else {
    list(intercept = as.numeric(cal$intercept), slope = as.numeric(cal$slope))
  }
}

# Canonical production EV calibration = the deployed bundle's baked cal. Every
# betting caller should use this (or pass its own bundle's cal) rather than the
# stale EV_CALIBRATION_DEFAULT. Falls back to the default if the bundle is
# unreadable. Callers that already hold a bundle should prefer ev_calibration_from_bundle().
production_ev_calibration <- function(path = file.path(MODELS_DIR, "ensemble_latest.rds")) {
  tryCatch(ev_calibration_from_bundle(readRDS(path)),
           error = function(e) EV_CALIBRATION_DEFAULT)
}

american_to_decimal <- function(a) if_else(a > 0, a / 100 + 1, 100 / abs(a) + 1)

# Signed Kelly fraction — can be negative when the bet is -EV. Callers that
# only want long bets should `pmax(., 0)` themselves (or use
# `kelly_fraction_nonneg`). walk_forward_eval.R uses the signed form because
# downstream Kelly capping clamps to 0 anyway.
kelly_fraction <- function(prob, odds) {
  b <- american_to_decimal(odds) - 1
  (b * prob - (1 - prob)) / b
}

# Convenience non-negative variant — used by `filter_and_size_bets` where we
# only consider +EV positions.
kelly_fraction_nonneg <- function(prob, odds) pmax(kelly_fraction(prob, odds), 0)

# Alias kept for backward-compat with walk_forward_eval.R / portfolio_sim.R
kelly_f <- kelly_fraction

ev_per_dollar <- function(prob, odds) {
  d <- american_to_decimal(odds)
  prob * (d - 1) - (1 - prob)
}

pnl_for_win <- function(stake, odds) {
  if_else(odds > 0, odds * (stake / 100), stake * 100 / abs(odds))
}

# Settled P&L given outcome, odds, and stake. `correct` is 1 / 0 / NA (push).
# Pushes return $0 (stake refunded — net P&L is zero, not loss).
pnl_dollars <- function(correct, odds_american, wager_dollars) {
  dplyr::case_when(
    is.na(correct)    ~ 0,
    correct != 1L     ~ -wager_dollars,
    odds_american > 0 ~ odds_american * (wager_dollars / 100),
    TRUE              ~ wager_dollars * 100 / abs(odds_american)
  )
}

# For each row in `predictions` (one per game), generate 0-2 candidate bets:
# one spread bet on the side of positive line_diff, one ML bet on the side
# our pred_win_pct favors. Returns a tibble keyed by (game_id, bet_type).
# `predictions` must have: home_team, away_team, line_diff, cover_prob_home,
#                          pred_win_pct, home_spread_odds, away_spread_odds,
#                          home_moneyline, away_moneyline
# Optionally: cover_correct, ml_correct (used only for backtest scoring).
build_bet_candidates <- function(predictions) {
  pmap_dfr(predictions, function(...) {
    row <- list(...)
    out <- list()

    has_spread <- !is.null(row$home_spread_odds) && !is.null(row$away_spread_odds) &&
                  !is.na(row$home_spread_odds) && !is.na(row$away_spread_odds) &&
                  !is.na(row$cover_prob_home)
    if (has_spread) {
      if (row$line_diff > 0) {
        side <- "home"; pos <- "spread"; team <- row$home_team
        odds <- row$home_spread_odds; prob <- row$cover_prob_home
      } else {
        side <- "away"; pos <- "spread"; team <- row$away_team
        odds <- row$away_spread_odds; prob <- 1 - row$cover_prob_home
      }
      out[[length(out) + 1]] <- tibble(
        game_id  = row$game_id, season = row$season, week = row$week,
        home_team = row$home_team, away_team = row$away_team,
        bet_type = paste0(side, "_spread"), bet_team = team,
        odds = odds, our_prob = prob,
        correct = if (!is.null(row$cover_correct)) row$cover_correct else NA_integer_
      )
    }

    has_ml <- !is.null(row$home_moneyline) && !is.null(row$away_moneyline) &&
              !is.na(row$home_moneyline) && !is.na(row$away_moneyline) &&
              !is.na(row$pred_win_pct)
    if (has_ml) {
      if (row$pred_win_pct > 0.5) {
        side <- "home"; team <- row$home_team
        odds <- row$home_moneyline; prob <- row$pred_win_pct
      } else {
        side <- "away"; team <- row$away_team
        odds <- row$away_moneyline; prob <- 1 - row$pred_win_pct
      }
      out[[length(out) + 1]] <- tibble(
        game_id  = row$game_id, season = row$season, week = row$week,
        home_team = row$home_team, away_team = row$away_team,
        bet_type = paste0(side, "_ml"), bet_team = team,
        odds = odds, our_prob = prob,
        correct = if (!is.null(row$ml_correct)) row$ml_correct else NA_integer_
      )
    }

    if (length(out) == 0) tibble() else bind_rows(out)
  })
}

# Given candidates + bankroll + params, return only the bets that pass EV +
# odds filters, with Kelly-sized stake. Returns empty tibble if nothing passes.
filter_and_size_bets <- function(candidates, bankroll,
                                  params = PORTFOLIO_DEFAULTS,
                                  ev_cal = EV_CALIBRATION_DEFAULT) {
  if (nrow(candidates) == 0) return(candidates)
  candidates %>%
    mutate(
      ev_raw      = ev_per_dollar(our_prob, odds),
      ev          = calibrate_ev(ev_raw, ev_cal),  # calibrated EV (filter + sizing)
      kelly_frac  = kelly_fraction_nonneg(our_prob, odds),
      stake_target= pmin(kelly_frac * params$kelly_mult * bankroll,
                          bankroll * params$max_bet_pct)
    ) %>%
    filter(ev >= params$min_ev,
           abs(odds) <= params$max_abs_odds,
           stake_target >= params$min_stake) %>%
    mutate(
      stake = pmax(params$min_stake,
                   round(stake_target / params$round_to) * params$round_to),
      payout_if_win = pnl_for_win(stake, odds),
      expected_value = ev * stake     # calibrated EV × stake
    )
}

# Live bet card: one-shot helper combining candidates + filter + sizing.
# `predictions` should come from predict_with_ensemble() and include odds.
# Pass `bundle` to pick up the bundle-baked EV calibration; otherwise uses
# the stale default constants (with a warning when EV cal is missing).
generate_bet_card <- function(predictions, bankroll,
                               params = PORTFOLIO_DEFAULTS,
                               bundle = NULL) {
  ev_cal <- if (is.null(bundle)) EV_CALIBRATION_DEFAULT
            else ev_calibration_from_bundle(bundle)
  filter_and_size_bets(build_bet_candidates(predictions), bankroll, params, ev_cal) %>%
    arrange(desc(ev)) %>%
    select(game_id, week, home_team, away_team, bet_type, bet_team,
           odds, our_prob, ev, kelly_frac, stake, payout_if_win, expected_value)
}

# ---- Top-level inference --------------------------------------------------

# Given a saved bundle + raw game features, return predictions per game.
# Works for both historical (with outcomes) and live (without) games.
# Returns: tibble with game_id, identifiers, market lines, and all prediction
# columns (nfl_predictor_line, line_diff, pred_win_pct, raw/stacked outputs).
predict_with_ensemble <- function(bundle, raw_data) {
  prep_data <- prep_for_predict(raw_data, bundle$leakage_vars)

  xgb <- predict(bundle$xgb, new_data = prep_data)$.pred
  glm <- predict_glm(bundle$glm, prep_data)
  rf  <- predict(bundle$rf,  new_data = prep_data)$.pred

  nfl_pred_raw <- apply_stacking(bundle$stack, xgb, glm, rf)
  # Fallback if any base learner returns NA (e.g., glmnet on a missing feature)
  nfl_pred_raw <- if_else(is.na(nfl_pred_raw), xgb, nfl_pred_raw)

  # Margin calibration (no-op for bundles trained before margin_iso existed —
  # apply_margin_iso passes pred through unchanged when bundle$margin_iso is NULL)
  nfl_pred <- apply_margin_iso(nfl_pred_raw, bundle$margin_iso)

  # Routed through apply_winprob so a bundle carrying a `winprob` calibrator uses
  # it; bundles without one fall through to the identical legacy path.
  # `raw_wp` is still emitted as pred_win_pct_raw — it is the uncalibrated
  # pnorm probability, reported for diagnostics regardless of which calibration
  # route produced pred_win_pct.
  raw_wp  <- pnorm(nfl_pred / bundle$sigma)
  pred_wp <- apply_winprob(nfl_pred, bundle$winprob, bundle$sigma, bundle$platt)

  # Cover probability for spread bets: P(home covers) = pnorm((pred - spread)/σ).
  # Note: uses CALIBRATED nfl_pred so the spread comparison is on calibrated margin.
  line_diff       <- nfl_pred - raw_data$spread_line
  cover_prob_home <- pnorm(line_diff / bundle$sigma)

  tibble(
    game_id            = raw_data$game_id,
    season             = raw_data$season,
    week               = raw_data$week,
    home_team          = raw_data$home_team,
    away_team          = raw_data$away_team,
    spread_line        = raw_data$spread_line,
    total_line         = raw_data$total_line,
    home_moneyline     = raw_data$home_moneyline,
    away_moneyline     = raw_data$away_moneyline,
    nfl_predictor_line = nfl_pred,        # calibrated (post-isotonic)
    nfl_predictor_line_raw = nfl_pred_raw,  # pre-calibration (for diagnostics)
    line_diff          = line_diff,
    pred_win_pct       = pred_wp,
    pred_win_pct_raw   = raw_wp,
    cover_prob_home    = cover_prob_home,
    xgb_pred           = xgb,
    glm_pred           = glm,
    rf_pred            = rf
  )
}
