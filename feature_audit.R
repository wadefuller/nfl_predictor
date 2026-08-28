# ============================================
# NFL Predictor: Feature Space Audit
# ============================================
# Sanity-checks game_w_features.csv for missing data, outliers, duplicates,
# cross-feature inconsistencies, and per-season coverage.
#
# Usage: Rscript feature_audit.R
# ============================================

# DATA_DIR + tidyverse from ensemble_lib.R.
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

g <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))

cat("=== OVERVIEW ===\n")
cat(sprintf("Rows: %s\n", format(nrow(g), big.mark = ",")))
cat(sprintf("Cols: %d\n", ncol(g)))
cat(sprintf("Seasons: %d-%d\n", min(g$season), max(g$season)))
cat(sprintf("Mem footprint: %.1f MB\n", as.numeric(object.size(g)) / 1024^2))

# ---- Duplicate check ------------------------------------------------------
cat("\n=== DUPLICATES ===\n")
dup_ids <- g %>% count(game_id) %>% filter(n > 1)
cat(sprintf("Duplicate game_ids: %d\n", nrow(dup_ids)))
if (nrow(dup_ids) > 0) print(head(dup_ids, 10))

# ---- NA rates by column (top offenders) -----------------------------------
cat("\n=== TOP 20 COLUMNS BY NA RATE ===\n")
na_rates <- g %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "na_rate") %>%
  arrange(desc(na_rate))
print(na_rates %>% head(20) %>% as.data.frame())

cat("\nFeatures with 0% NA:", sum(na_rates$na_rate == 0), "/", nrow(na_rates), "\n")
cat("Features with >50% NA:", sum(na_rates$na_rate > 0.5), "\n")
cat("Features with >90% NA:", sum(na_rates$na_rate > 0.9), "\n")

# ---- NA patterns by season for key features ------------------------------
cat("\n=== NA RATE BY SEASON for KEY FEATURES ===\n")
key_features <- c("spread_line", "total_line", "home_moneyline", "off_epa",
                  "off_epa_r16_home", "def_epa_r16_home",
                  "starter_qb_epa_r8_home", "new_starter_home",
                  "temp", "wind", "is_dome",
                  "pythag_win_pct_home", "result")
key_features <- intersect(key_features, names(g))

by_season_na <- g %>%
  group_by(season) %>%
  summarise(
    games = n(),
    across(any_of(key_features), ~ round(mean(is.na(.)), 3))
  )
print(by_season_na %>% as.data.frame(), row.names = FALSE)

# ---- Numeric feature ranges (extreme outliers) ---------------------------
cat("\n=== EXTREME NUMERIC OUTLIERS (top 15 columns by max abs Z) ===\n")
numeric_summary <- g %>%
  select(where(is.numeric), -game_id, -season, -week) %>%
  summarise(across(everything(),
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE),
                        mean = ~mean(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("col", "stat"), names_sep = "__",
               values_to = "val") %>%
  pivot_wider(names_from = "stat", values_from = "val") %>%
  mutate(
    abs_z_min = if_else(sd > 0, abs((min - mean) / sd), 0),
    abs_z_max = if_else(sd > 0, abs((max - mean) / sd), 0),
    max_abs_z = pmax(abs_z_min, abs_z_max)
  ) %>%
  arrange(desc(max_abs_z))
print(numeric_summary %>% head(15) %>%
        select(col, min, max, mean, sd, max_abs_z) %>%
        as.data.frame(), row.names = FALSE)

# ---- Cross-feature consistency checks ------------------------------------
cat("\n=== CROSS-FEATURE CONSISTENCY CHECKS ===\n")

# 1. home_result = home_points - away_points (when both present)
hr_check <- g %>%
  filter(!is.na(home_points), !is.na(away_points), !is.na(home_result)) %>%
  mutate(implied_hr = home_points - away_points,
         diff = home_result - implied_hr)
cat(sprintf("home_result vs (home_points-away_points): %d rows checked, %d mismatches\n",
            nrow(hr_check), sum(hr_check$diff != 0)))
if (sum(hr_check$diff != 0) > 0) {
  print(hr_check %>% filter(diff != 0) %>% select(game_id, season, week, home_points, away_points, home_result, implied_hr) %>% head(5))
}

# 2. home_win = (home_result > 0)
hw_check <- g %>%
  filter(!is.na(home_win), !is.na(home_result)) %>%
  mutate(implied_hw = as.integer(home_result > 0),
         diff = home_win - implied_hw)
cat(sprintf("home_win vs (home_result>0): %d rows checked, %d mismatches\n",
            nrow(hw_check), sum(hw_check$diff != 0)))

# 3. total_points_scored = home_points + away_points (where both present)
ts_check <- g %>%
  filter(!is.na(home_points), !is.na(away_points), !is.na(total_points_scored)) %>%
  mutate(implied_ts = home_points + away_points,
         diff = total_points_scored - implied_ts)
cat(sprintf("total_points_scored vs (home+away): %d rows checked, %d mismatches\n",
            nrow(ts_check), sum(ts_check$diff != 0)))

# 4. spread_line + result = "cover margin from home perspective"; check sign consistency with spread_cover
# (this is a derived field worth sanity-checking)
sc_check <- g %>%
  filter(!is.na(spread_line), !is.na(home_result), !is.na(spread_cover)) %>%
  mutate(
    home_covers = as.integer(home_result > spread_line),
    diff = spread_cover - home_covers
  )
cat(sprintf("spread_cover vs (home_result>spread_line): %d rows checked, %d mismatches\n",
            nrow(sc_check), sum(sc_check$diff != 0)))
if (sum(sc_check$diff != 0) > 0) {
  cat("  → Confirms spread_cover was computed from a DIFFERENT cover convention than what we use.\n")
  cat("  Sample mismatches:\n")
  print(sc_check %>% filter(diff != 0) %>% select(game_id, season, week, spread_line, home_result, spread_cover, home_covers) %>% head(5))
}

# 5. impl_prob_home + impl_prob_away (should sum to >1 due to vig, but in this range)
ip_check <- g %>%
  filter(!is.na(impl_prob_home), !is.na(impl_prob_away)) %>%
  mutate(sum_p = impl_prob_home + impl_prob_away)
cat(sprintf("impl_prob_home + impl_prob_away: range [%.3f, %.3f], mean %.3f (expected ~1.04 due to vig)\n",
            min(ip_check$sum_p), max(ip_check$sum_p), mean(ip_check$sum_p)))

# 6. Differential consistency: home_off_epa_r16_home_diff = home_off_epa_r16_home - off_epa_r16_away? (we'd need both cols populated)
# Skip if not all cols present
if (all(c("off_epa_r16_home", "off_epa_r16_away", "off_epa_r16_home_diff") %in% names(g))) {
  diff_check <- g %>%
    filter(!is.na(off_epa_r16_home), !is.na(off_epa_r16_away), !is.na(off_epa_r16_home_diff)) %>%
    mutate(implied_diff = off_epa_r16_home - off_epa_r16_away,
           gap = off_epa_r16_home_diff - implied_diff)
  cat(sprintf("off_epa_r16_home_diff vs (home - away): %d rows, max abs gap %.6f\n",
              nrow(diff_check), max(abs(diff_check$gap), na.rm = TRUE)))
}

# ---- Negative/impossible values ------------------------------------------
cat("\n=== IMPOSSIBLE VALUE CHECKS ===\n")
neg_points <- g %>% filter(home_points < 0 | away_points < 0)
cat(sprintf("Negative point totals: %d rows\n", nrow(neg_points)))

# weather: temp -50 to 130 reasonable, wind 0-50
weather_outliers <- g %>%
  filter((temp < -20 | temp > 110 | wind < 0 | wind > 50) & !is.na(temp))
cat(sprintf("Weather outliers (temp <-20 or >110 or wind <0 or >50): %d rows\n",
            nrow(weather_outliers)))
if (nrow(weather_outliers) > 0) {
  print(weather_outliers %>% select(game_id, season, gameday, temp, wind) %>% head(5))
}

# rest days reasonable: 3-14
rest_outliers <- g %>%
  filter(!is.na(home_team_rest), home_team_rest < 0 | home_team_rest > 21)
cat(sprintf("Rest day outliers (home_team_rest <0 or >21): %d rows\n", nrow(rest_outliers)))

# ---- Team distribution check ---------------------------------------------
cat("\n=== TEAM DISTRIBUTION ===\n")
team_counts <- g %>% filter(!is.na(home_team)) %>% count(home_team) %>% arrange(n)
cat(sprintf("Distinct home teams: %d\n", nrow(team_counts)))
cat("Teams with fewest home games (might indicate renaming issues):\n")
print(head(team_counts, 5))

# ---- Pre-2008 coverage gap ----------------------------------------------
cat("\n=== KEY-FEATURE COVERAGE BEFORE 2008 (where model doesn't train) ===\n")
pre_2008 <- g %>% filter(season < 2008)
cat(sprintf("Rows before 2008: %d\n", nrow(pre_2008)))
if (nrow(pre_2008) > 0) {
  pre_2008_na <- pre_2008 %>%
    summarise(
      spread_line     = mean(is.na(spread_line)),
      home_moneyline  = mean(is.na(home_moneyline)),
      off_epa_r16_home = mean(is.na(off_epa_r16_home)),
      starter_qb_epa_r8_home = mean(is.na(starter_qb_epa_r8_home)),
      temp            = mean(is.na(temp))
    )
  print(pre_2008_na)
  cat("  → These rows pad the CSV but are excluded by the model's min_season filter.\n")
}

# ---- Highly correlated feature pairs -------------------------------------
cat("\n=== TOP 15 HIGHEST FEATURE-PAIR CORRELATIONS ===\n")
numeric_cols <- g %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("game_id", "season", "week"))) %>%
  select(where(~ sd(., na.rm = TRUE) > 0 & mean(is.na(.)) < 0.5))
if (ncol(numeric_cols) > 100) {
  # Limit to a manageable subset to keep correlation matrix < 10k cells
  numeric_cols <- numeric_cols %>% select(1:100)
}
cor_mat <- cor(numeric_cols, use = "pairwise.complete.obs")
cor_long <- cor_mat %>%
  as.data.frame() %>% rownames_to_column("a") %>%
  pivot_longer(-a, names_to = "b", values_to = "r") %>%
  filter(a < b, !is.na(r), abs(r) > 0.95) %>%
  arrange(desc(abs(r)))
cat(sprintf("Feature pairs with |r| > 0.95 (among first %d numeric cols): %d\n",
            ncol(numeric_cols), nrow(cor_long)))
print(cor_long %>% head(15) %>% as.data.frame(), row.names = FALSE)

cat("\nDone.\n")
