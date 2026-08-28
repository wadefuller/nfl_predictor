# Offensive EPA per (game_id, team) + rolling r4/r12/r16. The current-game
# values (off_epa, off_pass_epa, etc.) are leakage and dropped at recipe
# stage via ensemble_lib::LEAKAGE_VARS_REG. The rolling versions are the
# real features.

generate_off_epa <- function(plys) {
  plys %>%
    group_by(game_id, season, week, posteam) %>%
    summarise(
      off_epa      = mean(epa, na.rm = TRUE),
      off_pass_epa = mean(epa[qb_dropback == 1], na.rm = TRUE),
      off_rush_epa = mean(epa[qb_dropback == 0], na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      qb_epa       = mean(qb_epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(team = posteam)
}

OFF_EPA_MODULE <- feature_module(
  name        = "off_epa",
  description = "Offensive EPA (per-game + rolling r4/r12/r16): off_epa, off_pass_epa, off_rush_epa, success_rate, qb_epa",
  level       = "team_game_pre_pivot",
  needs       = c("pbp"),
  build       = function(seasons, deps) {
    f_off <- generate_off_epa(deps$pbp)
    roll_epa(f_off, c("off_epa", "off_pass_epa", "off_rush_epa",
                       "success_rate", "qb_epa"))
  },
  leakage_vars = c("off_epa", "off_pass_epa", "off_rush_epa",
                    "success_rate", "qb_epa")
)
