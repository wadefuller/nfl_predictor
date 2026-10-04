# Pregame QB inputs. No live feed is used to rewrite completed games.
QB_FEATURES <- c("starter_qb_epa_r4", "starter_qb_epa_r8",
  "starter_pass_epa_r4", "starter_pass_epa_r8", "starter_career_games",
  "starter_games_team_season", "new_starter")

qb_features_for_starter <- function(history, starter_id, team, season, week) {
  # Strictly earlier weeks, including prior-team history; never target outcomes.
  h <- history[history$season < season |
    (history$season == season & history$week < week), , drop=FALSE]
  h <- h[order(h$season, h$week, h$game_id), , drop=FALSE]
  p <- h[h$starter_id == starter_id, , drop=FALSE]
  t <- h[h$team == team, , drop=FALSE]
  avg <- function(col, n) if (!nrow(p)) NA_real_ else mean(tail(p[[col]], n))
  c(starter_qb_epa_r4=avg("starter_qb_epa",4),
    starter_qb_epa_r8=avg("starter_qb_epa",8),
    starter_pass_epa_r4=avg("starter_pass_epa",4),
    starter_pass_epa_r8=avg("starter_pass_epa",8),
    starter_career_games=nrow(p),
    starter_games_team_season=sum(p$team == team & p$season == season),
    new_starter=if (!nrow(t)) 0 else as.integer(tail(t$starter_id,1) != starter_id))
}

refresh_qb_features <- function(games, data_dir, now=Sys.time()) {
  # Result missing alone is not enough: do not apply today's depth chart to
  # a past or in-progress game whose result has not arrived yet.
  kickoff <- if (all(c("gameday","gametime") %in% names(games)))
    as.POSIXct(paste(games$gameday,games$gametime),tz="America/New_York") else
    rep(as.POSIXct(NA),nrow(games))
  pending <- if ("home_result" %in% names(games)) is.na(games$home_result) else rep(TRUE,nrow(games))
  idx <- which(pending & !is.na(kickoff) & as.numeric(kickoff) > as.numeric(now))
  games$qb_features_ready <- FALSE
  games$qb_features_reason <- "No pregame QB feature refresh"
  for (side in c("home","away")) {
    games[[paste0("expected_qb_id_",side)]] <- NA_character_
    games[[paste0("expected_qb_name_",side)]] <- NA_character_
  }
  if (!length(idx)) return(games)
  # Match combine_home_away_differentials() and the trained recipe exactly.
  # Never silently create an alias that the production model does not consume.
  required <- unlist(lapply(c("home","away","home_diff"), function(side) paste0(QB_FEATURES,"_",side)))
  missing <- setdiff(required,names(games))
  if(length(missing)) stop("QB feature schema mismatch: missing ",paste(missing,collapse=", "))
  cache <- tryCatch(readRDS(file.path(data_dir,"qb_start_history.rds")),error=function(e) NULL)
  valid <- !is.null(cache) && identical(cache$schema,1L) &&
    identical(cache$first_season,2000L) && is.data.frame(cache$starts) &&
    all(c("game_id","season","week","team","starter_id","starter_qb_epa","starter_pass_epa") %in% names(cache$starts)) &&
    !anyNA(cache$starts[c("game_id","season","week","team","starter_id")]) &&
    !anyDuplicated(cache$starts[c("game_id","team")])
  schedule <- tryCatch(read.csv(file.path(data_dir,"game_w_features.csv")),error=function(e) NULL)
  for (i in idx) {
    g <- games[i,,drop=FALSE]
    reasons <- character()
    snap <- qb_availability_snapshot(file.path(data_dir,sprintf("qb_availability_%d_wk%02d.json",g$season,g$week)),now)
    matched <- !is.null(snap) && snap$season == g$season && snap$week == g$week &&
      "starter_id" %in% names(snap$players)
    history_ok <- valid && all(2000:(g$season-1L) %in% cache$seasons) &&
      all(2000:(g$season-1L) %in% unique(cache$starts$season))
    # Verify *every* previous scheduled game through kickoff, not merely the
    # selected QB's last appearance (he might have missed several games).
    if (is.null(schedule) || !all(c("gameday","gametime") %in% names(schedule))) history_ok <- FALSE
    if (history_ok) {
      prior <- schedule[schedule$season == g$season & schedule$week < g$week,,drop=FALSE]
      required <- c(paste(prior$game_id,prior$home_team),paste(prior$game_id,prior$away_team))
      history_ok <- all(required %in% paste(cache$starts$game_id,cache$starts$team))
    }
    if (!history_ok) reasons <- c(reasons,"QB history missing or incomplete through the previous week")
    for (side in c("home","away")) {
      # Never silently retain the previous team starter's EPA on failure.
      for (f in QB_FEATURES) games[i,paste0(f,"_",side)] <- NA_real_
      team <- g[[paste0(side,"_team")]]
      q <- if (matched) snap$players[!is.na(snap$players$depth) & snap$players$depth==1 & snap$players$team==team,,drop=FALSE] else NULL
      if (is.null(q) || nrow(q)!=1 || is.na(q$starter_id) || !nzchar(q$starter_id)) {
        reasons <- c(reasons,paste(team,"expected QB identity unresolved")); next
      }
      games[i,paste0("expected_qb_id_",side)] <- q$starter_id
      games[i,paste0("expected_qb_name_",side)] <- q$name
      if (!history_ok) next
      values <- qb_features_for_starter(cache$starts,q$starter_id,team,g$season,g$week)
      for (f in QB_FEATURES) games[i,paste0(f,"_",side)] <- values[[f]]
      if (any(!is.finite(values))) reasons <- c(reasons,paste(team,"expected QB lacks usable starting history"))
    }
    for (f in QB_FEATURES) games[i,paste0(f,"_home_diff")] <- games[i,paste0(f,"_home")] - games[i,paste0(f,"_away")]
    games$qb_features_ready[i] <- !length(reasons)
    games$qb_features_reason[i] <- paste(unique(reasons),collapse=" · ")
  }
  games
}
