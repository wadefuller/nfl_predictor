# Serving and grading have different sources: only future games use the live fit.
export_forecasts <- function(games, current_season, data_dir, predict_live,
                             now = Sys.time()) {
  numeric_fields <- c("nfl_predictor_line", "line_diff", "pred_win_pct",
                      "cover_prob_home", "xgb_pred", "glm_pred", "rf_pred")
  character_fields <- c("availability_reason", "availability_checked_at",
    "qb_features_reason", "expected_qb_id_home", "expected_qb_id_away",
    "expected_qb_name_home", "expected_qb_name_away")
  out <- games
  for (nm in numeric_fields) out[[nm]] <- NA_real_
  for (nm in character_fields) out[[nm]] <- NA_character_
  out$availability_ok <- out$qb_features_ready <- FALSE
  out$forecast_source <- "uncovered"
  if (anyDuplicated(games$game_id)) stop("Duplicate export game IDs")
  completed <- !is.na(games$home_result)
  kickoff <- as.POSIXct(paste(games$gameday, games$gametime),
                       tz = "America/New_York")
  future <- !completed & !is.na(kickoff) & as.numeric(kickoff) > as.numeric(now)
  overlay <- function(rows, source) {
    if (!nrow(rows)) return(invisible(NULL))
    if (anyDuplicated(rows$game_id)) stop("Duplicate archived forecast game IDs")
    idx <- match(rows$game_id, out$game_id)
    keep <- !is.na(idx)
    idx <- idx[keep]; rows <- rows[keep, , drop = FALSE]
    # Outcomes always come from the current result feed. Forecasts and archived
    # decision-time prices override only their matching fields.
    fields <- intersect(names(rows), c(numeric_fields, character_fields,
      "availability_ok", "qb_features_ready", "spread_line", "total_line",
      "home_moneyline", "away_moneyline", "home_spread_odds", "away_spread_odds",
      "impl_prob_home", "impl_prob_away"))
    for (nm in fields) {
      valid <- !is.na(rows[[nm]])
      out[[nm]][idx[valid]] <<- rows[[nm]][valid]
    }
    out$forecast_source[idx] <<- source
  }
  historical <- games[completed & games$season < current_season, , drop = FALSE]
  if (nrow(historical)) {
    path <- file.path(data_dir, "walk_forward_predictions.csv")
    if (!file.exists(path)) stop("Historical export requires saved walk-forward predictions")
    wf <- readr::read_csv(path, show_col_types = FALSE)
    regular <- historical$week <= ifelse(historical$season <= 2020, 17, 18)
    missing <- setdiff(historical$game_id[regular], wf$game_id)
    if (length(missing)) stop("Missing historical OOF forecasts: ", paste(missing, collapse = ", "))
    overlay(wf[wf$game_id %in% historical$game_id, , drop = FALSE], "walk_forward")
  }
  current <- games[!future & games$season == current_season, , drop = FALSE]
  for (wk in unique(current$week)) {
    path <- file.path(data_dir, "pregame_snapshots",
                      sprintf("%d-wk%02d.csv", current_season, wk))
    if (!file.exists(path)) next
    frozen <- read_frozen_pregame(data_dir, current_season, wk)
    frozen <- frozen[frozen$game_id %in% current$game_id, , drop = FALSE]
    backfill <- frozen$provenance %in% "backfill_preview_archive"
    overlay(frozen[!backfill, , drop = FALSE], "frozen_pregame")
    overlay(frozen[backfill, , drop = FALSE], "pregame_backfill_archive")
  }
  if (any(future)) {
    live <- predict_live(games[future, , drop = FALSE])
    if (anyDuplicated(live$game_id) ||
        !setequal(live$game_id, games$game_id[future]))
      stop("Live export prediction coverage differs from future slate")
    overlay(live, "live")
  }
  out$line_diff <- out$nfl_predictor_line - out$spread_line
  out$forecast_graded <- completed & is.finite(out$nfl_predictor_line) &
    is.finite(out$pred_win_pct) & out$pred_win_pct >= 0 & out$pred_win_pct <= 1
  out
}
