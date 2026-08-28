# ============================================
# NFL Predictor: Model Success/Failure Diagnostic
# ============================================
# Slices walk_forward_predictions by game characteristics to identify where
# the model beats Vegas and where it loses. Use after every eval cycle to:
#   - Confirm a new feature actually closed its target failure-mode gap
#   - Spot new failure modes to target with the next feature
#
# Reads:  data/walk_forward_predictions.csv + data/game_w_features.csv
# Output: stdout — table per slicing dimension
# Usage:  Rscript model_diagnostic.R
# ============================================

source("~/Projects/R/nfl_predictor/ensemble_lib.R")

preds <- read_csv(file.path(DATA_DIR, "walk_forward_predictions.csv"),
                  show_col_types = FALSE)
g <- read_csv(file.path(DATA_DIR, "game_w_features.csv"),
              show_col_types = FALSE)

df <- preds %>%
  left_join(g %>% select(game_id, total_line, div_game, weekday, gametime,
                          temp, wind, is_dome, high_wind, cold_game,
                          home_team_rest, away_team_rest), by = "game_id") %>%
  mutate(
    model_err   = abs(nfl_predictor_line - home_result),
    vegas_err   = abs(spread_line - home_result),
    edge        = model_err - vegas_err,        # negative = model wins
    pick_size   = abs(spread_line),
    actual_mag  = abs(home_result),
    is_upset    = sign(home_result) != sign(spread_line) & sign(spread_line) != 0
  )

slice_summary <- function(grouping_expr, label, min_n = 50) {
  cat(sprintf("\n=== %s ===\n", label))
  out <- df %>% group_by(!!enquo(grouping_expr)) %>%
    summarise(
      n         = n(),
      model_mae = round(mean(model_err), 3),
      vegas_mae = round(mean(vegas_err), 3),
      edge      = round(mean(edge), 3),
      .groups   = "drop"
    ) %>%
    filter(n >= min_n) %>%
    arrange(edge)
  print(as.data.frame(out))
}

# Each slice surfaces a different failure-mode hypothesis
slice_summary(cut(pick_size, c(-Inf, 2.5, 5, 7.5, 10, Inf),
                  labels=c("0-2.5","2.5-5","5-7.5","7.5-10","10+")),
              "By Vegas pick size")
slice_summary(cut(actual_mag, c(-Inf, 3, 7, 14, 21, Inf),
                  labels=c("0-3","3-7","7-14","14-21","21+")),
              "By actual margin (game closeness)")
slice_summary(case_when(spread_line > 3  ~ "home_fav_>3",
                         spread_line > 0  ~ "home_fav_0-3",
                         spread_line < -3 ~ "road_fav_>3",
                         spread_line < 0  ~ "road_fav_0-3",
                         TRUE             ~ "pickem"),
              "By favorite direction")
slice_summary(cut(total_line, c(-Inf, 40, 45, 48, 52, Inf),
                  labels=c("<40","40-45","45-48","48-52","52+")),
              "By total line")
slice_summary(case_when(is_dome == 1L    ~ "dome",
                         high_wind == 1L  ~ "high_wind",
                         cold_game == 1L  ~ "cold",
                         TRUE             ~ "neutral"),
              "By weather/venue")
slice_summary(div_game, "By divisional game", min_n = 100)
slice_summary(weekday, "By day of week")
slice_summary(cut(week, c(0,4,9,14,Inf), labels=c("W1-4","W5-9","W10-14","W15+")),
              "By week-of-season")
slice_summary(is_upset, "By upset (favorite lost outright)")
slice_summary(case_when(home_team_rest <= 5 ~ "home_short",
                         away_team_rest <= 5 ~ "away_short",
                         home_team_rest >= 13 ~ "home_off_bye",
                         away_team_rest >= 13 ~ "away_off_bye",
                         TRUE                 ~ "normal"),
              "By rest situation", min_n = 50)
