suppressPackageStartupMessages(library(dplyr))
source("R/features/_module_base.R")
source("R/features/snap_counts.R")

games <- tibble::tibble(season = 2026L, week = 1:3,
                        home_team = "A", away_team = "B")
snaps <- tibble::tibble(season = 2026L, week = 1:2, team = "A",
                        snap_qb_pct = c(0.8, 0.9))
x <- lag_snap_features(snaps, games)
stopifnot(is.na(x$snap_qb_pct_lastwk[x$team == "A" & x$week == 1]),
          x$snap_qb_pct_lastwk[x$team == "A" & x$week == 2] == 0.8,
          x$snap_qb_pct_lastwk[x$team == "A" & x$week == 3] == 0.9,
          is.na(x$snap_qb_pct_lastwk[x$team == "B" & x$week == 3]))
cat("PASS: scheduled future games receive prior-game snaps without current-game data\n")
