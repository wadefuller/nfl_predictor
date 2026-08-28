# Defensive EPA + split metrics per (game_id, team) + rolling r4/r12/r16.
# Splits (sack/qb_hit/stuff/int/third_dn) ON by default; toggle via
# DEF_SPLITS_ENABLED=0 to ablate.

generate_def_epa <- function(plys) {
  enable_splits <- Sys.getenv("DEF_SPLITS_ENABLED", "1") == "1"
  agg <- plys %>%
    filter(!is.na(defteam)) %>%
    group_by(game_id, season, week, defteam) %>%
    summarise(
      def_epa          = mean(epa, na.rm = TRUE),
      def_pass_epa     = mean(epa[qb_dropback == 1], na.rm = TRUE),
      def_rush_epa     = mean(epa[qb_dropback == 0], na.rm = TRUE),
      def_success_rate = mean(success, na.rm = TRUE),
      def_sack_rate     = if (enable_splits) mean(sack[qb_dropback == 1], na.rm = TRUE) else NA_real_,
      def_qb_hit_rate   = if (enable_splits) mean(qb_hit[qb_dropback == 1], na.rm = TRUE) else NA_real_,
      def_stuff_rate    = if (enable_splits) mean(tackled_for_loss[qb_dropback == 0], na.rm = TRUE) else NA_real_,
      def_int_rate      = if (enable_splits) mean(interception[qb_dropback == 1], na.rm = TRUE) else NA_real_,
      def_third_dn_rate = if (enable_splits) mean(third_down_converted[down == 3 & !is.na(down)], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    rename(team = defteam)

  if (!enable_splits) {
    agg <- agg %>% select(-def_sack_rate, -def_qb_hit_rate, -def_stuff_rate,
                           -def_int_rate, -def_third_dn_rate)
  }
  agg
}

DEF_EPA_MODULE <- feature_module(
  name        = "def_epa",
  description = "Defensive EPA + splits (sack/qb_hit/stuff/int/third_dn rates); rolling r4/r12/r16",
  level       = "team_game_pre_pivot",
  needs       = c("pbp"),
  build       = function(seasons, deps) {
    f_def <- generate_def_epa(deps$pbp)
    cols  <- intersect(
      c("def_epa", "def_pass_epa", "def_rush_epa", "def_success_rate",
        "def_sack_rate", "def_qb_hit_rate", "def_stuff_rate",
        "def_int_rate", "def_third_dn_rate"),
      names(f_def)
    )
    roll_epa(f_def, cols)
  },
  leakage_vars = c("def_epa", "def_pass_epa", "def_rush_epa", "def_success_rate",
                    "def_sack_rate", "def_qb_hit_rate", "def_stuff_rate",
                    "def_int_rate", "def_third_dn_rate")
)
