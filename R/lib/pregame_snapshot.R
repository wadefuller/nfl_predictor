# Append-only pregame forecast ledger. The grader selects the last observation
# strictly before kickoff; it never re-runs a model trained after the game.
pregame_snapshot_path <- function(data_dir, season, week) {
  dir <- file.path(data_dir, "pregame_snapshots")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, sprintf("%d-wk%02d.csv", season, week))
}

record_pregame_snapshot <- function(predictions, games, bundle, data_dir,
                                    now = Sys.time()) {
  kickoff <- as.POSIXct(paste(games$gameday, games$gametime),
                         tz = "America/New_York")
  context <- tibble::tibble(
    game_id = games$game_id, kickoff_at = format(kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    kickoff_epoch = as.numeric(kickoff),
    home_spread_odds = games$home_spread_odds,
    away_spread_odds = games$away_spread_odds,
    impl_prob_home = games$impl_prob_home,
    impl_prob_away = games$impl_prob_away
  )
  cal <- ev_calibration_from_bundle(bundle)
  rows <- predictions %>%
    dplyr::left_join(context, by = "game_id") %>%
    dplyr::filter(!is.na(kickoff_epoch), kickoff_epoch > as.numeric(now),
                  is.finite(nfl_predictor_line), is.finite(pred_win_pct)) %>%
    dplyr::transmute(
      snapshot_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      snapshot_epoch = as.numeric(now), kickoff_at, kickoff_epoch,
      game_id, season, week, home_team, away_team, spread_line, total_line,
      home_moneyline, away_moneyline, home_spread_odds, away_spread_odds,
      impl_prob_home, impl_prob_away,
      nfl_predictor_line, line_diff, pred_win_pct, cover_prob_home,
      availability_ok, availability_reason, availability_checked_at,
      ev_cal_intercept = cal$intercept, ev_cal_slope = cal$slope
    )
  if (!nrow(rows)) return(invisible(0L))
  path <- pregame_snapshot_path(data_dir, rows$season[1], rows$week[1])
  # A repeated invocation within one second is idempotent. Existing rows are
  # never edited, even if later forecasts move before kickoff.
  if (file.exists(path)) {
    existing <- readr::read_csv(path, show_col_types = FALSE,
                                col_types = readr::cols(snapshot_at = readr::col_character()))
    rows <- rows %>% dplyr::anti_join(
      existing %>% dplyr::select(game_id, snapshot_at),
      by = c("game_id", "snapshot_at"))
  }
  if (nrow(rows)) {
    readr::write_csv(rows, path, append = file.exists(path))
    # The local ledger grades the week; the deployed copy is a recoverable
    # pregame record if this laptop's ignored data directory is ever lost.
    mirror_dir <- file.path(dirname(data_dir), "website", "public", "data",
                            "pregame_snapshots")
    dir.create(mirror_dir, recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(path, file.path(mirror_dir, basename(path)), overwrite = TRUE))
      stop("Could not mirror the frozen pregame forecast into the site snapshot")
  }
  message(sprintf("  archived %d pregame forecast(s) in %s", nrow(rows), path))
  invisible(nrow(rows))
}

read_frozen_pregame <- function(data_dir, season, week, game_ids = NULL) {
  path <- pregame_snapshot_path(data_dir, season, week)
  if (!file.exists(path)) stop(sprintf(
    "No pregame forecast ledger for %d week %d; refusing in-sample grading", season, week))
  rows <- readr::read_csv(path, show_col_types = FALSE)
  # Optional `provenance` column (absent from ledgers this file writes itself):
  #   "backfill_preview_archive" — written by backfill_pregame_ledger.R for weeks
  #       that predate the ledger. snapshot_at is the archive file's REAL write
  #       time, which can fall after kickoff; the numbers were produced by a
  #       bundle verified not to have trained on the game. Accepted by name
  #       rather than by forging a pre-kickoff timestamp.
  #   "excluded" — no honest pregame forecast exists for this game. It is
  #       recorded as deliberately ungraded instead of failing the whole week.
  if (!"provenance" %in% names(rows)) rows$provenance <- NA_character_
  excluded <- unique(rows$game_id[rows$provenance %in% "excluded"])
  rows <- rows %>%
    dplyr::filter(!game_id %in% excluded,
                  snapshot_epoch < kickoff_epoch |
                    provenance %in% "backfill_preview_archive") %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice_max(snapshot_epoch, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  if (!is.null(game_ids)) {
    missing <- setdiff(game_ids, c(rows$game_id, excluded))
    if (length(missing)) stop(sprintf(
      "Missing frozen pregame predictions for %d/%d games: %s",
      length(missing), length(game_ids), paste(missing, collapse = ", ")))
    keep <- game_ids[game_ids %in% rows$game_id]
    rows <- rows[match(keep, rows$game_id), , drop = FALSE]
    excluded <- intersect(excluded, game_ids)
  }
  attr(rows, "excluded") <- excluded
  rows
}

# Game ids a week's ledger marks as deliberately ungraded (see above).
pregame_excluded_ids <- function(data_dir, season, week) {
  path <- pregame_snapshot_path(data_dir, season, week)
  if (!file.exists(path)) return(character(0))
  rows <- readr::read_csv(path, show_col_types = FALSE)
  if (!"provenance" %in% names(rows)) return(character(0))
  unique(rows$game_id[rows$provenance %in% "excluded"])
}
