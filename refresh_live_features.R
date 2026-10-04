# Refresh injury reports and prior-game snaps for pending games in one slate.
# This is intentionally much cheaper than rebuilding the historical matrix.
# Historical/completed rows are never rewritten with today's information.
# Run from this script's directory, like its siblings capture_qb_availability.R
# and refresh_qb_history.R. launchd starts every scheduled run in
# /Users/wadefuller/Projects/R, so the relative sources below failed with
# "cannot open file 'ensemble_lib.R'" and took down every daily refresh on
# 2026-09-27 after 08:58 (the `upcoming` step is critical).
script_path <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
setwd(dirname(normalizePath(script_path)))
source("ensemble_lib.R")
source("../shared/contracts.R")
source("R/infra/team_normalize.R")
source("R/infra/data_loaders.R")
source("R/infra/attach.R")
source("R/features/_module_base.R")
source("R/features/qb_starter.R")
source("R/features/injuries.R")
source("R/features/snap_counts.R")

refresh_live_features <- function(season, week, now = Sys.time()) {
  path <- file.path(DATA_DIR, "game_w_features.csv")
  g <- read.csv(path)
  kickoff <- as.POSIXct(paste(g$gameday, g$gametime), tz = "America/New_York")
  idx <- which(g$season == season & g$week == week & is.na(g$home_result) &
                 !is.na(kickoff) & as.numeric(kickoff) > as.numeric(now))
  if (!length(idx)) {
    message("No future games to refresh; feature matrix unchanged")
    return(invisible(g))
  }
  target <- g[idx, c("game_id", "season", "week", "home_team", "away_team")]
  teams <- unique(c(target$home_team, target$away_team))
  snap <- qb_availability_snapshot(file.path(
    DATA_DIR, sprintf("qb_availability_%d_wk%02d.json", season, week)), now)
  expected <- tibble(team = teams, season = season, week = week,
                     expected_id = NA_character_)
  if (!is.null(snap) && identical(as.integer(snap$season), as.integer(season)) &&
      identical(as.integer(snap$week), as.integer(week)) &&
      "starter_id" %in% names(snap$players)) {
    for (i in seq_along(teams)) {
      q <- snap$players[snap$players$team == teams[i] &
                          !is.na(snap$players$depth) & snap$players$depth == 1, , drop = FALSE]
      if (nrow(q) == 1 && !is.na(q$starter_id)) expected$expected_id[i] <- q$starter_id
    }
  }

  player_cap <- load_player_cap_by_season(season)
  team_cap <- build_team_cap_aggregates(season, player_cap)
  injury <- extract_injury_features(season, player_cap, team_cap, expected) %>%
    filter(.data$week == !!week)
  snaps <- extract_snap_features(season, g %>% filter(.data$season == !!season)) %>%
    filter(.data$week == !!week)

  # Clear any prior snapshot first. A failed or absent current report must not
  # leave yesterday's non-missing value masquerading as today's data.
  live_cols <- grep("^(inj_|snap_)", names(g), value = TRUE)
  for (col in live_cols) g[idx, col] <- NA
  for (features in list(injury, snaps)) {
    if (!nrow(features)) next
    joined <- attach_per_team_features(target, features)
    stopifnot(nrow(joined) == length(idx), identical(joined$game_id, target$game_id))
    fresh_cols <- setdiff(names(joined), names(target))
    missing <- setdiff(fresh_cols, names(g))
    if (length(missing)) stop("Live feature schema mismatch: ", paste(missing, collapse = ", "))
    for (col in fresh_cols) g[idx, col] <- joined[[col]]
  }
  required <- c("inj_out_cap_pct_home", "inj_out_cap_pct_away")
  if (week > 1) required <- c(required, "snap_qb_pct_lastwk_home", "snap_qb_pct_lastwk_away")
  ready <- stats::complete.cases(g[idx, required, drop = FALSE])
  tmp <- paste0(path, ".tmp")
  readr::write_csv(g, tmp)
  stopifnot(file.rename(tmp, path))
  message(sprintf("Refreshed %d future games; %d have current injuries and prior-game snaps",
                  length(idx), sum(ready)))
  invisible(g[idx, , drop = FALSE])
}

if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  stopifnot(length(args) == 2L)
  refresh_live_features(as.integer(args[1]), as.integer(args[2]))
}
