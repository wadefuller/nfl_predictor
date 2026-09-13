# ============================================
# NFL Predictor: Feature Census
# ============================================
# Answers two questions:
#   1. WHERE do the ~390 model features come from? (provenance by module)
#   2. HOW MANY does each learner actually use? (not the same number at all)
#
# Pipeline width is not the same as features used. The recipe expands 5
# categorical columns into ~90 dummies, then each learner prunes very
# differently: cv.glmnet's L1 is brutal, XGB uses whatever earns a split,
# ranger touches nearly everything.
#
# Usage: Rscript feature_census.R    (~1 min)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")
suppressPackageStartupMessages({library(recipes); library(ranger); library(xgboost)})

TRAIN_LAST <- 2023
SEED       <- 989

g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
train_raw <- g %>% filter(!is.na(home_result), !is.na(home_moneyline),
                          (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
                          season >= 2008, season <= TRAIN_LAST)
train_prep <- prep_for_train(train_raw)

# ---------------------------------------------------------------------
# PART 1 — Provenance: which module produced each column?
# ---------------------------------------------------------------------
cat("=====================================================================\n")
cat("PART 1: Where do the features come from?\n")
cat("=====================================================================\n")

classify <- function(nm) {
  if (grepl("^(spread_line|total_line|home_spread_odds|away_spread_odds|home_moneyline|away_moneyline|impl_prob|impl_win_prob)", nm)) return("market (odds/lines)")
  if (grepl("^(off_epa|off_pass_epa|off_rush_epa|success_rate|qb_epa)", nm))      return("off_epa")
  if (grepl("^(def_epa|def_pass_epa|def_rush_epa|def_success|def_sack|def_qb_hit|def_stuff|def_int|def_third)", nm)) return("def_epa")
  if (grepl("^(starter_|new_starter)", nm))                                       return("qb_starter")
  if (grepl("^pythag", nm))                                                       return("pythag")
  if (grepl("^(win_|result_|points_for_|spread_cover_|spread_adjusted_|game_info_gain|win_prob_captured)", nm)) return("trending")
  if (grepl("^team_(points_against|total_env|off_plays)", nm))                    return("pace_total")
  if (grepl("^inj_", nm))                                                         return("injuries")
  if (grepl("^snap_", nm))                                                        return("snap_counts")
  if (grepl("^coach_", nm))                                                       return("coaching")
  if (grepl("^rest_", nm))                                                        return("rest")
  if (grepl("^ref_", nm))                                                         return("officials")
  if (grepl("^(temp|wind|roof|surface|is_dome|is_grass)", nm))                    return("weather_venue")
  if (grepl("^(home_team|away_team)$", nm))                                       return("team identity (raw)")
  if (grepl("^(week|weekday|div_game)$", nm))                                     return("schedule categoricals")
  if (grepl("^games_played$", nm))                                                return("misc")
  "other/unclassified"
}
preds <- setdiff(names(train_prep), "home_result")
prov  <- table(sapply(preds, classify))
prov  <- sort(prov, decreasing = TRUE)
cat(sprintf("\nRaw CSV columns:                 %d\n", ncol(g)))
cat(sprintf("Dropped by prep_for_train:       %d  (leakage vars + season)\n", ncol(g) - ncol(train_prep)))
cat(sprintf("Predictors entering the recipe:  %d\n\n", length(preds)))
for (i in seq_along(prov)) cat(sprintf("  %-24s %4d\n", names(prov)[i], prov[i]))

cat("\nWhy so many per module: most team-level features exist in 3 pivoted\n")
cat("forms (_home, _away, _home_diff) x 2-3 rolling windows (r4/r12/r16).\n")
cat("One conceptual signal like 'offensive EPA' becomes ~15 columns.\n")

# ---------------------------------------------------------------------
# PART 2 — Recipe expansion
# ---------------------------------------------------------------------
cat("\n=====================================================================\n")
cat("PART 2: What the recipe does to that count\n")
cat("=====================================================================\n")

rec_xgb <- recipe(home_result ~ ., data = train_prep) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)
baked_xgb <- bake(prep(rec_xgb, training = train_prep), new_data = train_prep)

rec_glm <- recipe(home_result ~ ., data = train_prep) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(all_numeric_predictors())
baked_glm <- bake(prep(rec_glm, training = train_prep), new_data = train_prep)

# how many numeric predictors did step_nzv kill?
rec_nzv_only <- prep(recipe(home_result ~ ., data = train_prep) %>%
                       step_nzv(all_numeric_predictors()), training = train_prep)
n_after_nzv <- ncol(bake(rec_nzv_only, new_data = train_prep)) - 1

cat(sprintf("\n  predictors in:                 %d\n", length(preds)))
cat(sprintf("  after step_nzv:                %d   (dropped %d near-zero-variance)\n",
            n_after_nzv, length(preds) - n_after_nzv))
cat(sprintf("  after step_dummy (one_hot):    %d   (5 categoricals -> ~91 dummies)\n",
            ncol(baked_xgb) - 1))
cat(sprintf("\n  XGB design matrix width:       %d\n", ncol(baked_xgb) - 1))
cat(sprintf("  GLM design matrix width:       %d\n", ncol(baked_glm) - 1))
cat("\n  ^^^ THIS is the '~390 features' number. It is matrix width,\n")
cat("      not the number of distinct signals, and not the number used.\n")

# ---------------------------------------------------------------------
# PART 3 — Actual usage per learner
# ---------------------------------------------------------------------
cat("\n=====================================================================\n")
cat("PART 3: How many does each learner ACTUALLY use?\n")
cat("=====================================================================\n")

set.seed(SEED)
cat("\nFitting all three...\n")
xgb_fit <- train_xgb_model(train_prep)
glm_fit <- train_glm_model(train_prep)

# --- GLM: nonzero coefficients at lambda.min
co <- predict(glm_fit$model, type = "coefficients", s = "lambda.min")
co_names <- rownames(co)
co_vals  <- as.numeric(co)
nz_idx   <- which(co_vals != 0 & co_names != "(Intercept)")
n_glm_used <- length(nz_idx)
glm_width  <- ncol(baked_glm) - 1

# --- XGB: features appearing with nonzero Gain
xgb_eng <- extract_fit_engine(xgb_fit)
imp <- xgb.importance(model = xgb_eng)
n_xgb_used <- nrow(imp)
xgb_width  <- ncol(baked_xgb) - 1

# --- RF: refit with impurity importance so we can count what it touched
baked_rf <- bake(prep(recipe(home_result ~ ., data = train_prep) %>%
                        step_nzv(all_numeric_predictors()) %>%
                        step_impute_median(all_numeric_predictors()) %>%
                        step_novel(all_nominal_predictors()) %>%
                        step_dummy(all_nominal_predictors(), one_hot = TRUE),
                      training = train_prep), new_data = train_prep)
rf_params <- load_tuned_rf_params()
set.seed(SEED)
rf_imp <- ranger(home_result ~ ., data = baked_rf, num.trees = rf_params$trees,
                 mtry = rf_params$mtry, min.node.size = rf_params$min_n,
                 importance = "impurity", seed = SEED)
vi <- ranger::importance(rf_imp)
n_rf_used <- sum(vi > 0)
rf_width  <- ncol(baked_rf) - 1

cat(sprintf("\n  %-8s %-14s %-14s %s\n", "learner", "matrix width", "actually used", "% of matrix"))
cat(sprintf("  %-8s %-14d %-14d %.1f%%\n", "GLM", glm_width, n_glm_used, 100*n_glm_used/glm_width))
cat(sprintf("  %-8s %-14d %-14d %.1f%%\n", "XGB", xgb_width, n_xgb_used, 100*n_xgb_used/xgb_width))
cat(sprintf("  %-8s %-14d %-14d %.1f%%\n", "RF",  rf_width,  n_rf_used,  100*n_rf_used/rf_width))

cat("\n  GLM nonzero coefficients (all of them):\n")
gdf <- data.frame(feature = co_names[nz_idx], coef = co_vals[nz_idx])
gdf <- gdf[order(-abs(gdf$coef)), ]
for (i in seq_len(nrow(gdf)))
  cat(sprintf("    %-42s %+.4f  [%s]\n", gdf$feature[i], gdf$coef[i], classify(gdf$feature[i])))

cat("\n  XGB gain concentration:\n")
for (k in c(10, 20, 50, 100, n_xgb_used)) {
  if (k > n_xgb_used) next
  cat(sprintf("    top %-4d features hold %5.1f%% of total gain\n",
              k, 100 * sum(imp$Gain[1:k])))
}
cat("\n  RF impurity concentration:\n")
vs <- sort(vi, decreasing = TRUE); vs <- vs / sum(vs)
for (k in c(10, 20, 50, 100)) {
  if (k > length(vs)) next
  cat(sprintf("    top %-4d features hold %5.1f%% of total impurity reduction\n",
              k, 100 * sum(vs[1:k])))
}

# ---------------------------------------------------------------------
# PART 4 — Dead weight
# ---------------------------------------------------------------------
cat("\n=====================================================================\n")
cat("PART 4: How much is dead weight?\n")
cat("=====================================================================\n")
xgb_dead <- xgb_width - n_xgb_used
cat(sprintf("\n  Columns XGB never split on:      %d of %d (%.1f%%)\n",
            xgb_dead, xgb_width, 100*xgb_dead/xgb_width))
cat(sprintf("  Columns GLM zeroed out:          %d of %d (%.1f%%)\n",
            glm_width - n_glm_used, glm_width, 100*(glm_width-n_glm_used)/glm_width))

low_gain <- sum(imp$Gain < 0.001)
cat(sprintf("  XGB features with Gain < 0.001:  %d (of %d used)\n", low_gain, n_xgb_used))

team_dummies <- grep("^(home_team|away_team)_", colnames(baked_xgb), value = TRUE)
tm_used <- sum(team_dummies %in% imp$Feature)
tm_glm  <- sum(team_dummies %in% gdf$feature)
cat(sprintf("\n  Team-identity dummies:           %d columns\n", length(team_dummies)))
cat(sprintf("    used by XGB: %d    nonzero in GLM: %d\n", tm_used, tm_glm))

cat("\nDone.\n")
