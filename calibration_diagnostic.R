# ============================================
# NFL Predictor: Win Probability Calibration Diagnostic
# ============================================
# Reads walk_forward_predictions.csv and produces a reliability analysis
# of pred_win_pct vs actual home_win outcomes.
#
# Output:
#   - Reliability table to stdout (predicted decile vs actual win rate)
#   - Calibration metrics (ECE, MCE, Brier decomposition)
#   - Comparison to Vegas implied prob if available
#
# Run after walk_forward_eval.R: Rscript calibration_diagnostic.R
# ============================================

# DATA_DIR + tidyverse from ensemble_lib.R.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

PREDS_FILE <- file.path(DATA_DIR, "walk_forward_predictions.csv")

if (!file.exists(PREDS_FILE)) {
  stop(sprintf("No predictions file at %s. Run walk_forward_eval.R first.", PREDS_FILE))
}

p <- read_csv(PREDS_FILE, show_col_types = FALSE)
cat(sprintf("Loaded %d predictions across %d seasons.\n\n", nrow(p), n_distinct(p$season)))

# Load Vegas implied prob for comparison if available
g <- read_csv(file.path(DATA_DIR, "game_w_features.csv"), show_col_types = FALSE)
p <- p %>%
  left_join(g %>% select(game_id, impl_prob_home), by = "game_id")

# If raw and calibrated probs both present, show both
has_raw <- "pred_win_pct_raw" %in% names(p)
if (has_raw) {
  cat("Detected pred_win_pct_raw — will compare raw pnorm vs Platt-calibrated.\n\n")
}

# ---- Reliability analysis ----

cat("=== RELIABILITY TABLE (decile bins) ===\n")
cat("Bin    | n   | Predicted avg | Actual win rate | Gap     | Vegas avg\n")
cat("-------|-----|---------------|-----------------|---------|----------\n")

bins <- p %>%
  filter(!is.na(pred_win_pct), !is.na(home_win_01)) %>%
  mutate(bin = ntile(pred_win_pct, 10)) %>%
  group_by(bin) %>%
  summarise(
    n             = n(),
    pred_min      = min(pred_win_pct),
    pred_max      = max(pred_win_pct),
    pred_mean     = mean(pred_win_pct),
    actual_rate   = mean(home_win_01),
    vegas_mean    = mean(impl_prob_home, na.rm = TRUE),
    gap           = actual_rate - pred_mean,
    .groups = "drop"
  )

for (i in seq_len(nrow(bins))) {
  b <- bins[i, ]
  cat(sprintf("%2d %.2f-%.2f | %4d | %.3f         | %.3f           | %+.3f  | %.3f\n",
              b$bin, b$pred_min, b$pred_max, b$n,
              b$pred_mean, b$actual_rate, b$gap, b$vegas_mean))
}

# ---- Aggregate calibration metrics ----

# Expected Calibration Error (ECE): weighted avg of |pred - actual| per bin
ece <- bins %>% summarise(ece = sum(n * abs(gap)) / sum(n)) %>% pull(ece)

# Maximum Calibration Error (MCE)
mce <- max(abs(bins$gap))

# Brier score decomposition: reliability + resolution + uncertainty
overall_rate <- mean(p$home_win_01, na.rm = TRUE)
brier <- mean((p$pred_win_pct - p$home_win_01)^2, na.rm = TRUE)
brier_vegas <- mean((p$impl_prob_home - p$home_win_01)^2, na.rm = TRUE)

# Reliability term: sum_b (n_b/N) * (pred_b - actual_b)^2
reliability <- bins %>%
  summarise(rel = sum(n * (pred_mean - actual_rate)^2) / sum(n)) %>% pull(rel)

# Resolution: sum_b (n_b/N) * (actual_b - overall)^2
resolution <- bins %>%
  summarise(res = sum(n * (actual_rate - overall_rate)^2) / sum(n)) %>% pull(res)

# Uncertainty: overall * (1 - overall)
uncertainty <- overall_rate * (1 - overall_rate)

cat(sprintf("\n=== CALIBRATION METRICS ===\n"))
cat(sprintf("Total games:                    %d\n", nrow(p)))
cat(sprintf("Overall actual home win rate:   %.3f\n", overall_rate))
cat(sprintf("Overall predicted win rate:     %.3f\n", mean(p$pred_win_pct, na.rm = TRUE)))
cat(sprintf("\nBrier (model):                  %.4f\n", brier))
cat(sprintf("Brier (Vegas implied prob):     %.4f\n", brier_vegas))
cat(sprintf("\nExpected Calibration Error:     %.4f\n", ece))
cat(sprintf("Maximum Calibration Error:      %.4f\n", mce))
cat(sprintf("\nBrier decomposition:\n"))
cat(sprintf("  Reliability (lower=better):   %.4f\n", reliability))
cat(sprintf("  Resolution  (higher=better):  %.4f\n", resolution))
cat(sprintf("  Uncertainty (constant):       %.4f\n", uncertainty))
cat(sprintf("  Sanity:  Brier ≈ Rel - Res + Unc = %.4f\n",
            reliability - resolution + uncertainty))

# ---- ASCII reliability plot ----

cat(sprintf("\n=== RELIABILITY PLOT (ASCII) ===\n"))
cat("Each row is a decile bin. '+' = predicted, '*' = actual.\n")
cat("Perfect calibration: + and * align.\n\n")
cat("        0.0       0.2       0.4       0.6       0.8       1.0\n")
cat("        |---------|---------|---------|---------|---------|\n")

for (i in seq_len(nrow(bins))) {
  b <- bins[i, ]
  line <- strrep(" ", 50)
  pred_pos <- max(1, min(50, round(b$pred_mean * 50)))
  actual_pos <- max(1, min(50, round(b$actual_rate * 50)))
  substr(line, pred_pos, pred_pos) <- "+"
  if (actual_pos == pred_pos) {
    substr(line, pred_pos, pred_pos) <- "X"
  } else {
    substr(line, actual_pos, actual_pos) <- "*"
  }
  cat(sprintf("Bin%2d | %s | gap %+.3f\n", b$bin, line, b$gap))
}

# ---- Diagnosis ----

cat(sprintf("\n=== DIAGNOSIS ===\n"))

# Check for S-curve: low bins over-predict, high bins under-predict (or vice versa)
low_gap  <- mean(bins$gap[bins$bin <= 3])   # actual - pred in low-prob bins
high_gap <- mean(bins$gap[bins$bin >= 8])   # actual - pred in high-prob bins
mid_gap  <- mean(bins$gap[bins$bin >= 4 & bins$bin <= 7])

cat(sprintf("Avg gap (actual - pred):\n"))
cat(sprintf("  Low bins  (1-3):  %+.3f  (positive = pred too low)\n", low_gap))
cat(sprintf("  Mid bins  (4-7):  %+.3f\n", mid_gap))
cat(sprintf("  High bins (8-10): %+.3f  (positive = pred too low)\n", high_gap))

if (abs(low_gap - high_gap) < 0.02 && abs(low_gap) > 0.02) {
  cat("\n→ UNIFORM BIAS: predictions consistently off by ~constant amount.\n")
  cat("  Likely σ scaling issue. Fix: better σ estimation (Option A).\n")
} else if (sign(low_gap) == sign(high_gap)) {
  cat("\n→ SYSTEMATIC BIAS in one direction.\n")
  cat("  Try Platt scaling (Option B).\n")
} else if (low_gap < -0.02 && high_gap > 0.02) {
  cat("\n→ OVER-CONFIDENT model: predicts too extreme.\n")
  cat("  Classic σ-too-small issue. Fix: increase σ or apply Platt.\n")
} else if (low_gap > 0.02 && high_gap < -0.02) {
  cat("\n→ UNDER-CONFIDENT model: predicts too close to 0.5.\n")
  cat("  Fix: decrease σ or apply Platt scaling.\n")
} else {
  cat("\n→ Calibration looks reasonably balanced.\n")
  cat("  Main miscalibration may be in extremes (look at MCE).\n")
}

# Also compare to Vegas
cat(sprintf("\nModel vs Vegas calibration:\n"))
cat(sprintf("  Model Brier:  %.4f\n", brier))
cat(sprintf("  Vegas Brier:  %.4f\n", brier_vegas))
cat(sprintf("  Difference:   %+.4f  (negative = model better)\n", brier - brier_vegas))

# ---- If raw vs calibrated both present, compare ---------------------------
if (has_raw) {
  brier_raw <- mean((p$pred_win_pct_raw - p$home_win_01)^2, na.rm = TRUE)
  ece_raw   <- p %>%
    filter(!is.na(pred_win_pct_raw), !is.na(home_win_01)) %>%
    mutate(bin = ntile(pred_win_pct_raw, 10)) %>%
    group_by(bin) %>%
    summarise(n = n(),
              pred = mean(pred_win_pct_raw),
              actual = mean(home_win_01),
              .groups = "drop") %>%
    summarise(ece = sum(n * abs(actual - pred)) / sum(n)) %>%
    pull(ece)

  cat(sprintf("\n=== RAW vs PLATT-CALIBRATED ===\n"))
  cat(sprintf("                  Raw pnorm    Platt-calibrated   Δ\n"))
  cat(sprintf("Brier:            %.4f       %.4f             %+.4f\n",
              brier_raw, brier, brier - brier_raw))
  cat(sprintf("ECE:              %.4f       %.4f             %+.4f\n",
              ece_raw, ece, ece - ece_raw))
}
