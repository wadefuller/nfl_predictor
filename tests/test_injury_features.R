suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("../shared/contracts.R")
source("R/infra/team_normalize.R")
source("R/features/_module_base.R")
source("R/features/injuries.R")

roster <- tibble::tibble(season = 2026L, week = 3L, team = "CHI",
  gsis_id = c("wr1", "wr2", "wr3", "qb"),
  position = c("WR", "WR", "WR", "QB"), game_type = "REG")
cap <- tibble::tibble(season = 2026L, gsis_id = roster$gsis_id,
                      apy = c(10, 2, 8, 1), apy_cap_pct = c(.1, .02, .08, .01))
team_cap <- tibble::tibble(season = 2026L, team = "CHI",
                           team_total_apy = 100, top5_apy_threshold = 5)
inj <- tibble::tibble(season = 2026L, week = 3L, team = "CHI",
  gsis_id = c("wr2", "qb"), position = c("WR", "QB"),
  game_type = "REG", report_status = "Out")
expected <- tibble::tibble(season = 2026L, week = 3L, team = "CHI",
                           expected_id = "qb")
x <- extract_injury_features(2026L, cap, team_cap, expected,
                             roster_weekly = roster, inj_data = inj)
stopifnot(nrow(x) == 1, x$inj_wrte_num_out == 1,
          x$inj_wrte_star_out == 0,
          x$inj_qb_starter_out == 1)
cat("PASS: injury star rank uses full weekly roster and QB flag uses expected identity\n")
