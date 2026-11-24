source("R/lib/qb_availability.R")
g <- data.frame(season=2026,week=3,home_team="CHI",away_team="PHI",qb_features_ready=TRUE,
 expected_qb_id_home="chi",expected_qb_id_away="phi",
 inj_qb_starter_out_home=0,new_starter_home=0,inj_qb_starter_out_away=0,new_starter_away=0)
s <- list(season=2026L,week=3L,observed_at="2026-09-26 10:00:00",
 players=data.frame(team=c("CHI","PHI"),name=c("Chicago QB","Philadelphia QB"),starter_id=c("chi","phi"),depth=c(1,1),status=c("","")))
stopifnot(assess_qb_availability(g,s)$availability_ok)
g$qb_features_ready <- FALSE
stopifnot(!assess_qb_availability(g,s)$availability_ok)
g$qb_features_ready <- TRUE
stopifnot(!assess_qb_availability(g,NULL)$availability_ok)
s$players$status[1] <- "Out"
stopifnot(!assess_qb_availability(g,s)$availability_ok)
s$players$status[1] <- ""
g$new_starter_home <- 1
stopifnot(assess_qb_availability(g,s)$availability_ok)
g$inj_qb_starter_out_home <- 1
stopifnot(assess_qb_availability(g,s)$availability_ok)
g$inj_qb_starter_out_home <- 0
g$new_starter_home <- NA
g$inj_qb_starter_out_home <- NA
stopifnot(assess_qb_availability(g,s)$availability_ok)
backup <- s$players[1,];backup$depth<-3;backup$status<-"Out";backup$starter_id<-"old"
s$players<-rbind(s$players,backup)
stopifnot(assess_qb_availability(g,s)$availability_ok)
g$expected_qb_id_home<-"old"
stopifnot(!assess_qb_availability(g,s)$availability_ok)
g$expected_qb_id_home<-"chi"
s$players$status[1]<-"Questionable"
stopifnot(!assess_qb_availability(g,s)$availability_ok)
s$players$status[1]<-""
s$week <- 2L
stopifnot(grepl("not freshly verified",assess_qb_availability(g,s)$availability_reason))
f <- tempfile(fileext=".json")
jsonlite::write_json(s,f,auto_unbox=TRUE)
stopifnot(is.null(qb_availability_snapshot(f,as.POSIXct("2026-09-26 17:00:00",tz="UTC"))))
stopifnot(is.null(qb_availability_snapshot(f,as.POSIXct("2026-09-26 09:00:00",tz="UTC"))))
cat("PASS: unknown, absent, changed, wrong-week, stale and future status safeguards\n")
