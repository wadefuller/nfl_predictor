# ============================================
# NFL Predictor: Bundle-vs-Bundle Feature-Importance Diff
# ============================================
# Compares XGB feature importance between two saved bundles. Tells you in
# 5 seconds:
#   - What new features the new bundle picked up
#   - Which features moved rank significantly (≥10 places)
#   - Which features went DEAD (gain dropped from >0.005 to <0.001) — the
#     "displaced signal" pattern that bit us repeatedly with coaching, NGS,
#     def_splits this session
#   - Which top-30 features fell out of the top 30
#   - How concentrated the new model is vs the old (gain distribution)
#
# Usage:
#   Rscript feature_diff.R <old.rds> <new.rds>
#   Rscript feature_diff.R models/ensemble_2026-05-22.rds models/ensemble_latest.rds
# ============================================

suppressPackageStartupMessages({
  source("~/Projects/R/nfl_predictor/ensemble_lib.R")
  library(xgboost)
  library(workflows)
})

# ---- Arg parsing ----------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript feature_diff.R <old_bundle.rds> <new_bundle.rds>")
}
old_path <- args[[1]]
new_path <- args[[2]]
stopifnot(file.exists(old_path), file.exists(new_path))

cat(sprintf("Comparing:\n  old: %s\n  new: %s\n\n", old_path, new_path))

# ---- Load importance from each bundle ------------------------------------

load_importance <- function(path) {
  b <- readRDS(path)
  xgb_fit <- extract_fit_engine(b$xgb)
  imp <- xgboost::xgb.importance(model = xgb_fit) %>% as_tibble() %>%
    arrange(desc(Gain)) %>%
    mutate(rank = row_number())
  list(
    importance = imp,
    metadata   = b$metadata,
    n_features = nrow(imp)
  )
}

old <- load_importance(old_path)
new <- load_importance(new_path)

# ---- Bundle-level summary -------------------------------------------------

cat(sprintf("Bundles:\n"))
cat(sprintf("  old: trained %s, %d features in XGB, cal-set Brier=%.4f\n",
            old$metadata$trained_date %||% "?",
            old$n_features,
            old$metadata$cal_set_metrics$brier %||% NA_real_))
cat(sprintf("  new: trained %s, %d features in XGB, cal-set Brier=%.4f\n",
            new$metadata$trained_date %||% "?",
            new$n_features,
            new$metadata$cal_set_metrics$brier %||% NA_real_))

# ---- Join + classify changes ---------------------------------------------

joined <- full_join(
  old$importance %>% select(Feature, gain_old = Gain, rank_old = rank),
  new$importance %>% select(Feature, gain_new = Gain, rank_new = rank),
  by = "Feature"
) %>%
  mutate(
    in_old = !is.na(gain_old),
    in_new = !is.na(gain_new),
    gain_delta = coalesce(gain_new, 0) - coalesce(gain_old, 0),
    rank_delta = rank_new - rank_old   # NA if not in both
  )

# ---- 1. New features (in new, not in old) --------------------------------

cat("\n=== NEW FEATURES (in new bundle only) ===\n")
new_feats <- joined %>% filter(!in_old, in_new) %>% arrange(desc(gain_new))
if (nrow(new_feats) == 0) {
  cat("  (none)\n")
} else {
  print(new_feats %>%
    select(Feature, rank_new, gain_new) %>%
    mutate(gain_new = round(gain_new, 4)) %>%
    head(15) %>% as.data.frame())
}

# ---- 2. Removed features (in old, not in new) ----------------------------

cat("\n=== REMOVED FEATURES (in old bundle only, top 10 by old gain) ===\n")
removed <- joined %>% filter(in_old, !in_new) %>% arrange(desc(gain_old))
if (nrow(removed) == 0) {
  cat("  (none)\n")
} else {
  print(removed %>%
    select(Feature, rank_old, gain_old) %>%
    mutate(gain_old = round(gain_old, 4)) %>%
    head(10) %>% as.data.frame())
}

# ---- 3. Dead features (was >0.005 gain, now <0.001) ----------------------

cat("\n=== WENT DEAD (gain >0.005 → <0.001 — \"displaced signal\") ===\n")
dead <- joined %>%
  filter(in_old, in_new, gain_old > 0.005, gain_new < 0.001) %>%
  arrange(desc(gain_old))
if (nrow(dead) == 0) {
  cat("  (none)\n")
} else {
  print(dead %>%
    select(Feature, rank_old, rank_new, gain_old, gain_new) %>%
    mutate(across(c(gain_old, gain_new), ~round(., 4))) %>%
    head(15) %>% as.data.frame())
}

# ---- 4. Big rank movers (rank changed by 10+) ----------------------------

cat("\n=== BIG RANK MOVERS (|Δ rank| ≥ 10, both in old and new) ===\n")
movers <- joined %>%
  filter(in_old, in_new, abs(rank_delta) >= 10) %>%
  arrange(rank_delta)   # most-promoted first (negative rank_delta)
if (nrow(movers) == 0) {
  cat("  (none)\n")
} else {
  promoted <- movers %>% filter(rank_delta < 0) %>% head(10)
  demoted  <- movers %>% filter(rank_delta > 0) %>% arrange(desc(rank_delta)) %>% head(10)
  if (nrow(promoted) > 0) {
    cat("  -- Promoted (rank moved up) --\n")
    print(promoted %>%
      transmute(Feature, rank_old, rank_new, rank_delta,
                gain_delta = round(gain_delta, 4)) %>%
      as.data.frame())
  }
  if (nrow(demoted) > 0) {
    cat("\n  -- Demoted (rank moved down) --\n")
    print(demoted %>%
      transmute(Feature, rank_old, rank_new, rank_delta,
                gain_delta = round(gain_delta, 4)) %>%
      as.data.frame())
  }
}

# ---- 5. Top-30 churn (which top-30 cols changed?) ------------------------

cat("\n=== TOP-30 CHURN ===\n")
old_top30 <- old$importance %>% slice_head(n = 30) %>% pull(Feature)
new_top30 <- new$importance %>% slice_head(n = 30) %>% pull(Feature)

fell_out <- setdiff(old_top30, new_top30)
came_in  <- setdiff(new_top30, old_top30)
cat(sprintf("  %d of 30 top features changed\n", length(fell_out)))
if (length(fell_out) > 0) {
  cat("  -- Fell out of top 30 (now ranked):\n")
  for (f in fell_out) {
    r <- joined$rank_new[joined$Feature == f]
    cat(sprintf("    %s (was %d, now %s)\n", f,
                old$importance$rank[old$importance$Feature == f][1],
                if (length(r) == 0 || is.na(r)) "GONE" else as.character(r)))
  }
  cat("  -- New to top 30:\n")
  for (f in came_in) {
    r <- joined$rank_old[joined$Feature == f]
    cat(sprintf("    %s (was %s, now %d)\n", f,
                if (length(r) == 0 || is.na(r)) "NEW" else as.character(r),
                new$importance$rank[new$importance$Feature == f][1]))
  }
}

# ---- 6. Concentration ----------------------------------------------------

cat("\n=== CONCENTRATION (cumulative gain by rank) ===\n")
conc_table <- tibble(
  top_n = c(5, 10, 20, 30, 50, 100),
  old_pct = vapply(c(5, 10, 20, 30, 50, 100), function(n) {
    sum(head(old$importance$Gain, n)) * 100
  }, numeric(1)),
  new_pct = vapply(c(5, 10, 20, 30, 50, 100), function(n) {
    sum(head(new$importance$Gain, n)) * 100
  }, numeric(1))
) %>% mutate(delta = new_pct - old_pct,
             across(c(old_pct, new_pct, delta), ~round(., 1)))
print(as.data.frame(conc_table))
if (mean(conc_table$delta[1:3]) > 5) {
  cat("\n  ⚠ NEW MODEL IS MORE CONCENTRATED — top features now carry more weight\n")
} else if (mean(conc_table$delta[1:3]) < -5) {
  cat("\n  ✓ NEW MODEL IS MORE DISTRIBUTED — signal spread across more features\n")
}

cat("\nDone.\n")
