# ============================================
# NFL Predictor: Feature Test Suite
# ============================================
# Lightweight CI-style sanity checks on game_w_features.csv. Catches
# regressions before they hit walk-forward eval:
#
#   1. Schema lock         — column count + presence match expectations
#   2. NA tolerance        — per-column NA% by season within bounds
#   3. Leakage detection   — outcome-derived cols correctly dropped
#   4. Determinism check   — repeated builds produce byte-identical CSVs
#                            (skipped by default — slow; run with FULL_TESTS=1)
#   5. Distribution drift  — current-season percentile bands within historical
#
# Usage: Rscript tests/test_features.R   (or ./run_pipeline.sh test)
# Exit code: 0 = all pass, 1 = any failure
#
# Each check prints PASS / FAIL with the offending columns / values.
# ============================================

suppressPackageStartupMessages({
  source("~/Projects/R/nfl_predictor/ensemble_lib.R")     # DATA_DIR, LEAKAGE_VARS_REG
  source(file.path(PROJECT_DIR, "R/features/_registry.R"))  # MODULES
})

FEATURES_PATH <- file.path(DATA_DIR, "game_w_features.csv")

if (!file.exists(FEATURES_PATH)) {
  stop(sprintf("No features file at %s. Run ./run_pipeline.sh features first.",
               FEATURES_PATH))
}

# ---- Test infrastructure --------------------------------------------------

.fail_count <- 0L
.pass_count <- 0L

pass <- function(msg)  { .pass_count <<- .pass_count + 1L
                          cat(sprintf("  PASS  %s\n", msg)) }
fail <- function(msg)  { .fail_count <<- .fail_count + 1L
                          cat(sprintf("  FAIL  %s\n", msg)) }

section <- function(title) cat(sprintf("\n=== %s ===\n", title))

# ---- Load features --------------------------------------------------------

cat(sprintf("Loading %s...\n", FEATURES_PATH))
g <- read_csv(FEATURES_PATH, show_col_types = FALSE)
cat(sprintf("Loaded %s rows × %d cols (%d-%d seasons)\n\n",
            format(nrow(g), big.mark = ","), ncol(g),
            min(g$season), max(g$season)))

# ============================================================================
# Test 1: Schema lock
# ============================================================================
section("Schema lock — required columns present")

REQUIRED_COLS <- c(
  # Identity & lines
  "game_id", "season", "week", "home_team", "away_team",
  "spread_line", "total_line", "home_moneyline", "away_moneyline",
  # Outcome
  "home_result", "home_win", "home_points", "away_points",
  # Market-derived
  "impl_prob_home", "impl_prob_away",
  # Pythag (canonical pre-pivot)
  "pythag_win_pct_home", "pythag_win_pct_away", "pythag_win_pct_home_diff",
  # EPA (one rolling per group)
  "off_epa_r4_home", "off_epa_r4_away", "off_epa_r4_home_diff",
  "def_epa_r4_home", "def_epa_r4_away", "def_epa_r4_home_diff",
  # QB starter
  "starter_qb_epa_r8_home", "starter_qb_epa_r8_away",
  "new_starter_home", "new_starter_away",
  # Injuries
  "inj_qb_starter_out_home", "inj_qb_starter_out_away",
  "inj_out_cap_pct_home", "inj_out_cap_pct_away",
  # Weather
  "temp", "wind", "is_dome", "is_grass"
)

missing <- setdiff(REQUIRED_COLS, names(g))
if (length(missing) == 0) {
  pass(sprintf("All %d required cols present (CSV has %d cols total)",
               length(REQUIRED_COLS), ncol(g)))
} else {
  fail(sprintf("Missing %d required cols: %s",
               length(missing), paste(missing, collapse = ", ")))
}

# ============================================================================
# Test 2: NA tolerance — per col / per season
# ============================================================================
section("NA tolerance — per-column NA% within bounds")

# Per-feature-class NA bounds. Lenient default; tighten per known fragile cols.
NA_TOLERANCE <- list(
  # Modern (2010+) seasons
  modern_default = 0.05,  # most cols should be < 5% NA in 2010+
  # Pre-2010 — contracts/injuries sparse; rolling features may be NA
  pre_modern_default = 0.40
)

# Check 1: NA rate on completed regular-season games for KEY model cols
# (the things the model actually trains on)
completed_modern <- g %>% filter(season >= 2010, !is.na(home_result))
key_cols <- c("off_epa_r4_home", "def_epa_r4_home", "pythag_win_pct_home",
              "starter_qb_epa_r8_home", "spread_line", "home_moneyline",
              "impl_prob_home")
na_rates <- vapply(key_cols, function(c) mean(is.na(completed_modern[[c]])),
                    numeric(1))
problems <- names(na_rates)[na_rates > NA_TOLERANCE$modern_default]
if (length(problems) == 0) {
  pass(sprintf("Key model cols all < %.0f%% NA on 2010+ data",
               100 * NA_TOLERANCE$modern_default))
} else {
  for (col in problems) {
    fail(sprintf("Col %s has %.1f%% NA (>%.0f%% threshold)",
                  col, 100 * na_rates[[col]], 100 * NA_TOLERANCE$modern_default))
  }
}

# Check 2: per-season NA for snap counts (only available 2012+)
snap_cols <- grep("^snap_.*_home$", names(g), value = TRUE)
if (length(snap_cols) > 0) {
  snap_2012plus <- g %>% filter(season >= 2012, !is.na(home_result))
  snap_na <- mean(is.na(snap_2012plus[[snap_cols[1]]]))
  # Snap features are lag-shifted (week N uses week N-1 snaps) so wk 1 of
  # every season is NA. With 18 weeks × 14 yrs that's ~6% baseline NA;
  # bump tolerance to 20% to cover injured-reserve gaps + bye weeks.
  if (snap_na < 0.20) {
    pass(sprintf("snap_counts cols < 20%% NA in 2012+ (actual %.1f%%)",
                 100 * snap_na))
  } else {
    fail(sprintf("snap_counts NA %.1f%% in 2012+ (expected <20%%)",
                 100 * snap_na))
  }
}

# ============================================================================
# Test 3: Leakage — outcome-derived cols correctly flagged in ENSEMBLE_LEAKAGE_VARS
# ============================================================================
section("Leakage — outcome-derived cols in LEAKAGE_VARS_REG")

# Every current-game stat declared by a module should be in ensemble_lib's
# LEAKAGE_VARS_REG (since walk_forward / training drop those cols).
declared_leakage <- all_declared_leakage_vars()
not_in_ensemble_lib <- setdiff(declared_leakage, ENSEMBLE_LEAKAGE_VARS)
if (length(not_in_ensemble_lib) == 0) {
  pass(sprintf("All %d module-declared leakage cols are in ensemble_lib::LEAKAGE_VARS_REG",
               length(declared_leakage)))
} else {
  fail(sprintf("Module-declared leakage cols MISSING from ensemble_lib::LEAKAGE_VARS_REG: %s",
                paste(not_in_ensemble_lib, collapse = ", ")))
}

# Reverse-leakage check: any NON-leakage numeric col that correlates
# suspiciously with home_result (R² > 0.5) is a potential leak we missed.
# Cols already in LEAKAGE_VARS_REG are silently fine — the recipe drops them.
non_leakage_num_cols <- setdiff(
  names(g)[vapply(g, is.numeric, logical(1))],
  c(LEAKAGE_VARS_REG, "home_result", "spread_line")  # target + obvious priors
)
suspicious <- c()
for (col in non_leakage_num_cols) {
  pair <- na.omit(data.frame(x = g[[col]], y = g$home_result))
  if (nrow(pair) < 100) next
  r2 <- summary(lm(y ~ x, data = pair))$r.squared
  if (!is.na(r2) && r2 > 0.5) {
    suspicious <- c(suspicious, sprintf("%s (R²=%.3f)", col, r2))
  }
}
if (length(suspicious) == 0) {
  pass(sprintf("No non-leakage cols suspiciously correlate (R²>0.5) with home_result — %d numeric feature cols checked",
               length(non_leakage_num_cols)))
} else {
  for (s in suspicious) fail(sprintf("Possible leakage (R²>0.5 vs home_result): %s", s))
}

# ============================================================================
# Test 4: Distribution drift — current season vs historical bands
# ============================================================================
section("Distribution drift — current season within historical 1-99% bands")

# For top XGB-importance cols, current season's mean should fall within the
# historical (1%, 99%) percentile of season means. Catches: nflreadr schema
# change introducing different units, upstream data bug.
DRIFT_CHECK_COLS <- intersect(c(
  "spread_line", "home_moneyline", "impl_prob_home",
  "off_epa_r16_home", "def_epa_r16_home",
  "pythag_win_pct_home", "starter_qb_epa_r8_home"
), names(g))

current_season <- max(g$season[!is.na(g$home_result)])
historical <- g %>% filter(season < current_season, !is.na(home_result))
current    <- g %>% filter(season == current_season, !is.na(home_result))

drift_issues <- c()
for (col in DRIFT_CHECK_COLS) {
  hist_means <- historical %>% group_by(season) %>%
    summarise(m = mean(.data[[col]], na.rm = TRUE), .groups = "drop") %>% pull(m)
  if (length(hist_means) < 5) next
  band_low  <- quantile(hist_means, 0.01, na.rm = TRUE)
  band_high <- quantile(hist_means, 0.99, na.rm = TRUE)
  cur_mean  <- mean(current[[col]], na.rm = TRUE)
  if (cur_mean < band_low || cur_mean > band_high) {
    drift_issues <- c(drift_issues,
                       sprintf("%s: current %.4f vs historical band [%.4f, %.4f]",
                               col, cur_mean, band_low, band_high))
  }
}
if (length(drift_issues) == 0) {
  pass(sprintf("All %d drift-checked cols within historical 1-99%% band for season %d",
               length(DRIFT_CHECK_COLS), current_season))
} else {
  for (msg in drift_issues) fail(msg)
}

# ============================================================================
# Test 5: Determinism check (opt-in — slow)
# ============================================================================
if (Sys.getenv("FULL_TESTS", "0") == "1") {
  section("Determinism — repeated build produces identical CSV")
  cat("  Running second build_features...\n")
  snapshot_path <- file.path(DATA_DIR, ".test_features_snapshot.csv")
  file.copy(FEATURES_PATH, snapshot_path, overwrite = TRUE)
  rc <- system2("Rscript", "build_features.R",
                stdout = file.path(tempdir(), "determinism.log"),
                stderr = file.path(tempdir(), "determinism.log"))
  if (rc != 0) {
    fail("Second build_features.R failed")
  } else {
    h1 <- tools::md5sum(snapshot_path)
    h2 <- tools::md5sum(FEATURES_PATH)
    if (unname(h1) == unname(h2)) {
      pass("Two builds produce byte-identical CSV")
    } else {
      fail(sprintf("Builds differ: md5 was %s, now %s", h1, h2))
    }
  }
  file.remove(snapshot_path)
} else {
  cat("\n[skipped determinism test — set FULL_TESTS=1 to enable]\n")
}

# ============================================================================
# Summary
# ============================================================================
section("Summary")
total <- .pass_count + .fail_count
cat(sprintf("%d of %d checks passed (%d failed)\n",
            .pass_count, total, .fail_count))
if (.fail_count > 0) {
  cat("\n⚠ FEATURE TESTS FAILED — investigate above before training.\n")
  quit(status = 1)
} else {
  cat("\n✓ All feature tests passed.\n")
  quit(status = 0)
}
