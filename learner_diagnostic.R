# ============================================
# NFL Predictor: Base-Learner Diagnostic
# ============================================
# Answers: why does the GLM dominate the stack when the prior says XGB
# should win?
#
# Hypotheses tested:
#   H1 MARKET ANCHOR — the target is dominated by a near-perfectly LINEAR
#      relationship with spread_line. Trees approximate y=x with a staircase,
#      wasting capacity. GLM represents it in one coefficient.
#   H2 SIGNAL-TO-NOISE — 81% of target variance is irreducible after the
#      market. Trees fit that noise; heavy shrinkage does not.
#   H3 REDUNDANCY — if base learners correlate ~0.99, NNLS picks one and
#      zeroes the rest regardless of quality.
#
# The decisive test is RESIDUAL TARGETING: refit each learner on
# (home_result - spread_line) and add the market back at predict time. If
# XGB improves sharply, H1 is confirmed and the fix is structural, not
# hyperparameter tuning.
#
# Usage: Rscript learner_diagnostic.R    (~40 sec)
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")

TRAIN_LAST <- 2023
VAL_START  <- 2024
SEED       <- 989

cat("Loading features...\n")
g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
g <- g %>% filter(!is.na(home_result), !is.na(home_moneyline),
                  (week <= 17 & season <= 2020) | (week <= 18 & season >= 2021),
                  season >= 2008)
train_raw <- g %>% filter(season <= TRAIN_LAST)
val_raw   <- g %>% filter(season >= VAL_START)
cat(sprintf("Train: %d rows (2008-%d)   Val: %d rows (%d+)\n\n",
            nrow(train_raw), TRAIN_LAST, nrow(val_raw), VAL_START))

train_prep <- prep_for_train(train_raw)
val_prep   <- prep_for_train(val_raw)
actual     <- val_prep$home_result
mkt_val    <- val_prep$spread_line
mkt_train  <- train_prep$spread_line

metrics <- function(pred, actual, label) {
  mae  <- mean(abs(pred - actual))
  rmse <- sqrt(mean((pred - actual)^2))
  cov  <- mean(sign(pred) == sign(actual), na.rm = TRUE)
  cat(sprintf("  %-28s MAE=%.4f  RMSE=%.3f  cover=%.4f\n", label, mae, rmse, cov))
  invisible(c(mae = mae, rmse = rmse, cover = cov))
}

# =====================================================================
# PART 1 — Standalone base-learner performance (target = home_result)
# =====================================================================
cat("=====================================================================\n")
cat("PART 1: Standalone learners, STANDARD target (home_result)\n")
cat("=====================================================================\n")

set.seed(SEED)
xgb_fit <- train_xgb_model(train_prep)
glm_fit <- train_glm_model(train_prep)
rf_fit  <- train_rf_model(train_prep)

xgb_p <- predict(xgb_fit, new_data = val_prep)$.pred
glm_p <- predict_glm(glm_fit, val_prep)
rf_p  <- predict(rf_fit,  new_data = val_prep)$.pred

cat("\nVal 2024-2025 performance:\n")
m_mkt <- metrics(mkt_val, actual, "MARKET (spread_line alone)")
m_xgb <- metrics(xgb_p,   actual, "XGBoost")
m_glm <- metrics(glm_p,   actual, "GLM (cv.glmnet)")
m_rf  <- metrics(rf_p,    actual, "Random Forest")

cat("\nEdge over raw market (negative = better than market):\n")
cat(sprintf("  XGB:  %+.4f MAE\n", m_xgb["mae"] - m_mkt["mae"]))
cat(sprintf("  GLM:  %+.4f MAE\n", m_glm["mae"] - m_mkt["mae"]))
cat(sprintf("  RF:   %+.4f MAE\n", m_rf["mae"]  - m_mkt["mae"]))

# =====================================================================
# PART 2 — H3: are the learners redundant?
# =====================================================================
cat("\n=====================================================================\n")
cat("PART 2: H3 — prediction correlation structure\n")
cat("=====================================================================\n")
pm <- cbind(xgb = xgb_p, glm = glm_p, rf = rf_p, market = mkt_val, actual = actual)
cat("\nCorrelation matrix of predictions:\n")
print(round(cor(pm), 4))

cat("\nSD of each prediction (how much each learner 'moves' off the market):\n")
for (nm in c("xgb", "glm", "rf", "market", "actual")) {
  cat(sprintf("  %-8s sd=%6.3f\n", nm, sd(pm[, nm])))
}
cat("\nSD of (prediction - market)  — the actual 'edge signal' each emits:\n")
for (nm in c("xgb", "glm", "rf")) {
  cat(sprintf("  %-8s sd=%6.3f\n", nm, sd(pm[, nm] - mkt_val)))
}

# =====================================================================
# PART 3 — H1: can trees represent the linear market anchor?
# =====================================================================
cat("\n=====================================================================\n")
cat("PART 3: H1 — how well does each learner track the market's LINE?\n")
cat("=====================================================================\n")
cat("\nRegressing each learner's prediction on spread_line.\n")
cat("A learner that has internalised the market gives slope ~1.0, R^2 ~1.0.\n\n")
for (nm in c("xgb", "glm", "rf")) {
  f <- lm(pm[, nm] ~ mkt_val)
  cat(sprintf("  %-8s slope=%.4f  intercept=%+.3f  R^2=%.4f\n",
              nm, coef(f)[2], coef(f)[1], summary(f)$r.squared))
}

cat("\nStaircase test — mean prediction by spread_line bucket:\n")
cat("(trees should show flat steps; GLM should track the diagonal)\n\n")
buckets <- cut(mkt_val, breaks = c(-Inf, -10, -7, -3.5, -1, 1, 3.5, 7, 10, Inf))
tab <- data.frame(bucket = levels(buckets))
for (nm in c("market", "xgb", "glm", "rf", "actual")) {
  tab[[nm]] <- round(tapply(pm[, nm], buckets, mean)[levels(buckets)], 2)
}
tab$n <- as.integer(table(buckets)[levels(buckets)])
print(tab, row.names = FALSE)

# =====================================================================
# PART 4 — THE DECISIVE TEST: residual targeting
# =====================================================================
cat("\n=====================================================================\n")
cat("PART 4: DECISIVE TEST — refit on RESIDUAL target (result - spread)\n")
cat("=====================================================================\n")
cat("\nIf H1 is right, giving trees the market for free (so they only model\n")
cat("the deviation) should sharply improve XGB and RF.\n\n")

train_resid <- train_prep %>% mutate(home_result = home_result - mkt_train)
val_resid   <- val_prep   %>% mutate(home_result = home_result - mkt_val)

set.seed(SEED)
xgb_r <- train_xgb_model(train_resid)
glm_r <- train_glm_model(train_resid)
rf_r  <- train_rf_model(train_resid)

xgb_pr <- predict(xgb_r, new_data = val_resid)$.pred + mkt_val
glm_pr <- predict_glm(glm_r, val_resid)            + mkt_val
rf_pr  <- predict(rf_r,  new_data = val_resid)$.pred + mkt_val

cat("Val 2024-2025, RESIDUAL-TARGETED (market added back):\n")
m_xgb_r <- metrics(xgb_pr, actual, "XGBoost  (residual)")
m_glm_r <- metrics(glm_pr, actual, "GLM      (residual)")
m_rf_r  <- metrics(rf_pr,  actual, "RF       (residual)")

cat("\nDELTA vs standard target (negative = residual targeting HELPS):\n")
cat(sprintf("  XGB:  %+.4f MAE   (%.4f -> %.4f)\n",
            m_xgb_r["mae"] - m_xgb["mae"], m_xgb["mae"], m_xgb_r["mae"]))
cat(sprintf("  GLM:  %+.4f MAE   (%.4f -> %.4f)\n",
            m_glm_r["mae"] - m_glm["mae"], m_glm["mae"], m_glm_r["mae"]))
cat(sprintf("  RF:   %+.4f MAE   (%.4f -> %.4f)\n",
            m_rf_r["mae"]  - m_rf["mae"],  m_rf["mae"],  m_rf_r["mae"]))

# =====================================================================
# PART 5 — how much shrinkage does the residual signal deserve?
# =====================================================================
cat("\n=====================================================================\n")
cat("PART 5: Is the residual signal real? (OLS on predicted deviation)\n")
cat("=====================================================================\n")
cat("\nRegress actual deviation on each learner's predicted deviation.\n")
cat("slope ~1 = well-calibrated edge; slope ~0 = the 'edge' is noise;\n")
cat("slope <<1 = overconfident, predictions need shrinking.\n\n")
actual_dev <- actual - mkt_val
for (nm in c("XGB", "GLM", "RF")) {
  pr <- switch(nm, XGB = xgb_pr, GLM = glm_pr, RF = rf_pr)
  dev <- pr - mkt_val
  f <- lm(actual_dev ~ dev)
  cat(sprintf("  %-5s (residual-fit) slope=%+.4f  R^2=%.5f  sd(dev)=%.3f\n",
              nm, coef(f)[2], summary(f)$r.squared, sd(dev)))
}
for (nm in c("XGB", "GLM", "RF")) {
  pr <- switch(nm, XGB = xgb_p, GLM = glm_p, RF = rf_p)
  dev <- pr - mkt_val
  f <- lm(actual_dev ~ dev)
  cat(sprintf("  %-5s (standard-fit) slope=%+.4f  R^2=%.5f  sd(dev)=%.3f\n",
              nm, coef(f)[2], summary(f)$r.squared, sd(dev)))
}

cat("\nDone.\n")
