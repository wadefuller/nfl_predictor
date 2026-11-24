# Serving-only guard. A fresh feed and rebuilt player-keyed QB features are
# necessary. Historical cap-weighted injury fields are not a live QB-status feed.
resolve_qb_gsis <- function(player, ids) {
  direct <- if(is.null(player$gsis_id)) character() else player$gsis_id
  cross <- if(all(c("sleeper_id","gsis_id") %in% names(ids)) && !is.null(player$player_id))
    ids$gsis_id[!is.na(ids$sleeper_id) & ids$sleeper_id==as.character(player$player_id)] else character()
  # Sleeper can pad otherwise valid GSIS IDs (e.g. Murray and Jones).
  # Normalize transport whitespace before detecting genuine identity conflicts.
  candidates <- unique(trimws(as.character(c(direct,cross))))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if(length(candidates)==1) candidates else NA_character_
}

annotate_qb_availability <- function(games, data_dir) {
  checks <- lapply(seq_len(nrow(games)),function(i) {
    g <- games[i,,drop=FALSE]
    path <- file.path(data_dir,sprintf("qb_availability_%d_wk%02d.json",g$season,g$week))
    assess_qb_availability(g,qb_availability_snapshot(path))
  })
  if(!nrow(games)) return(cbind(games,assess_qb_availability(games)))
  cbind(games,do.call(rbind,checks))
}

qb_availability_snapshot <- function(path, now = Sys.time()) {
  tryCatch({
    x <- jsonlite::fromJSON(path)
    age <- as.numeric(difftime(now, as.POSIXct(x$observed_at, tz="UTC"), units="hours"))
    if (!is.finite(age) || age < 0 || age > 6 || !is.data.frame(x$players) ||
        !all(c("team","name","depth","status") %in% names(x$players))) return(NULL)
    x
  }, error=function(e) NULL)
}

assess_qb_availability <- function(games, snapshot=NULL) {
  rows <- lapply(seq_len(nrow(games)), function(i) {
    g <- games[i,,drop=FALSE]
    reasons <- character()
    if (!isTRUE(g$qb_features_ready)) reasons <- c(reasons,
      if(length(g$qb_features_reason) && nzchar(g$qb_features_reason)) g$qb_features_reason else
        "QB features not verified for the expected starters")
    for (side in c("home","away")) {
      team <- g[[paste0(side,"_team")]]
      q <- if (!is.null(snapshot) && identical(as.integer(snapshot$season),as.integer(g$season)) &&
                identical(as.integer(snapshot$week),as.integer(g$week)))
        snapshot$players[snapshot$players$team==team,,drop=FALSE] else NULL
      if (is.null(q) || !nrow(q)) reasons <- c(reasons,paste(team,"QB status not freshly verified"))
      else {
        starter <- q[!is.na(q$depth) & q$depth==1,,drop=FALSE]
        if(nrow(starter)!=1) reasons <- c(reasons,paste(team,"starting QB unresolved"))
        if(nrow(starter)==1) {
          modeled <- g[[paste0("expected_qb_id_",side)]]
          if(length(modeled)!=1 || is.na(modeled) || !"starter_id" %in% names(starter) ||
             is.na(starter$starter_id) || modeled != starter$starter_id)
            reasons <- c(reasons,paste(team,"forecast does not match expected starter"))
          # Backup injuries are not evidence that the modeled starter is out.
          # A replacement is supported only after his own features were rebuilt.
          if(is.na(starter$status)) reasons <- c(reasons,paste(team,"starter status unknown"))
          else if(!tolower(trimws(starter$status)) %in% c("", "healthy", "active"))
            reasons <- c(reasons,paste(starter$name,starter$status))
        }
      }
    }
    data.frame(availability_ok=length(reasons)==0,
      availability_reason=paste(unique(reasons),collapse=" · "),
      availability_checked_at=if(is.null(snapshot)) NA_character_ else snapshot$observed_at)
  })
  if (!length(rows)) return(data.frame(availability_ok=logical(),availability_reason=character(),availability_checked_at=character()))
  do.call(rbind,rows)
}
