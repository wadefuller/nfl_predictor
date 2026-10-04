suppressPackageStartupMessages(library(dplyr))
source("qb_availability.R")
source("qb_features.R")
source("R/features/_module_base.R")
source("R/features/qb_starter.R")
ids <- data.frame(sleeper_id=c("1","2","2"),gsis_id=c("a","b","c"))
stopifnot(identical(resolve_qb_gsis(list(player_id="1"),ids),"a"),
  is.na(resolve_qb_gsis(list(player_id="2"),ids)),
  is.na(resolve_qb_gsis(list(player_id="1",gsis_id="wrong"),ids)),
  is.na(resolve_qb_gsis(list(player_id="missing"),ids)))
real_ids <- data.frame(sleeper_id=c("5849","5870"),
  gsis_id=c("00-0035228","00-0035710"))
stopifnot(
  identical(resolve_qb_gsis(list(player_id="5849",gsis_id=" 00-0035228"),real_ids),"00-0035228"),
  identical(resolve_qb_gsis(list(player_id="5870",gsis_id=" 00-0035710 \t"),real_ids),"00-0035710"),
  identical(resolve_qb_gsis(list(player_id="5849",gsis_id="  "),real_ids),"00-0035228"),
  is.na(resolve_qb_gsis(list(player_id="5849",gsis_id=" 00-0035710"),real_ids)))
h <- data.frame(game_id=paste0("g",1:8),season=c(rep(2025,5),rep(2026,3)),
  week=c(1:5,1:3),team=c("OLD","OLD","OLD","OLD","OLD","CHI","CHI","CHI"),
  starter_id=c(rep("backup",5),"old","backup","backup"),
  starter_qb_epa=c(1:5,10,6,999),starter_pass_epa=c(1:5,10,6,999)/10,
  dropbacks=30)
x <- qb_features_for_starter(h,"backup","CHI",2026,3)
stopifnot(x[["starter_qb_epa_r4"]]==4.5,x[["starter_career_games"]]==6,
  x[["starter_games_team_season"]]==1,x[["new_starter"]]==0)
# Exact parity with the existing training definition when identity is known.
legacy <- qb_team_stability(roll_qb_player(h))
expected <- unlist(legacy[legacy$game_id=="g8",QB_FEATURES],use.names=FALSE)
stopifnot(isTRUE(all.equal(unname(x),expected,check.attributes=FALSE)))
stopifnot(qb_features_for_starter(h,"backup","CHI",2026,2)[["new_starter"]]==1)
rookie <- qb_features_for_starter(h,"rookie","CHI",2026,3)
stopifnot(rookie[["starter_career_games"]]==0,is.na(rookie[["starter_qb_epa_r4"]]))

# A holdout game's postgame primary passer must not become its pregame QB.
mock_pbp <- h %>% transmute(game_id,season,week,posteam=team,
  passer_player_id=starter_id,qb_dropback=1L,qb_epa=starter_qb_epa,
  epa=starter_pass_epa)
mock_schedule <- h %>% transmute(game_id,season,week,team,
  gameday=sprintf("%04d-09-%02d",season,week))
pregame <- historical_pregame_qb_features(mock_schedule,mock_pbp)
g7 <- pregame[pregame$game_id=="g7",]
g8 <- pregame[pregame$game_id=="g8",]
stopifnot(g7$starter_qb_epa_r4==10, # old QB, despite backup's game-7 dropbacks
          g7$starter_career_games==1,
          g8$starter_qb_epa_r4==4.5,
          g8$starter_career_games==6)

td <- tempfile();dir.create(td)
now <- as.POSIXct("2026-09-25 12:00:00",tz="UTC")
g <- data.frame(game_id=c("target","completed"),season=2026,week=3,
  home_team="CHI",away_team="PHI",home_result=c(NA,7),
  gameday="2026-09-28",gametime="20:15")
for(f in QB_FEATURES) for(side in c("home","away","home_diff")) g[[paste0(f,"_",side)]]<-123
s <- list(season=2026,week=3,observed_at="2026-09-25 11:00:00",
 players=data.frame(team=c("CHI","PHI"),name=c("Backup","Other"),
  starter_id=c("backup","other"),depth=1,status=""))
jsonlite::write_json(s,file.path(td,"qb_availability_2026_wk03.json"),auto_unbox=TRUE)
# Complete prior-week coverage for both teams, with traded player's own history.
extra <- h[h$game_id %in% c("g6","g7"),];extra$team<-"PHI";extra$starter_id<-"other"
hist <- rbind(h,extra)
# The history cache must actually contain every claimed historical season.
old <- h[rep(1,25),];old$season<-2000:2024;old$game_id<-paste0("old",old$season)
old$starter_id<-"historical";hist<-rbind(old,hist)
cache <- list(schema=1L,first_season=2000L,seasons=2000:2026,starts=hist)
saveRDS(cache,file.path(td,"qb_start_history.rds"))
schedule <- data.frame(game_id=c("g6","g7"),season=2026,week=1:2,
  home_team="CHI",away_team="PHI",gameday=c("2026-09-10","2026-09-17"),gametime="20:15")
write.csv(schedule,file.path(td,"game_w_features.csv"),row.names=FALSE)
y<-refresh_qb_features(g,td,now)
bad_schema <- g[,setdiff(names(g),"starter_qb_epa_r4_home_diff")]
stopifnot(inherits(try(refresh_qb_features(bad_schema,td,now),silent=TRUE),"try-error"))
stopifnot(y$qb_features_ready[1],!y$qb_features_ready[2],y$expected_qb_id_home[1]=="backup",
  y$starter_qb_epa_r4_home[1]==4.5,
  y$starter_qb_epa_r4_home_diff[1]==y$starter_qb_epa_r4_home[1]-y$starter_qb_epa_r4_away[1],
  identical(g[2,],y[2,names(g)]))
for(f in QB_FEATURES) stopifnot(
  y[[paste0(f,"_home_diff")]][1] == y[[paste0(f,"_home")]][1]-y[[paste0(f,"_away")]][1],
  !paste0(f,"_diff") %in% names(y))
# Missing previous game, unknown ID, stale feed and in-progress games fail safe.
cache$starts<-cache$starts[cache$starts$game_id!="g7",]
saveRDS(cache,file.path(td,"qb_start_history.rds"))
z<-refresh_qb_features(g,td,now)
stopifnot(!z$qb_features_ready[1],is.na(z$starter_qb_epa_r4_home[1]))
stopifnot(all(is.na(z[1,paste0(QB_FEATURES,"_home_diff")])))
saveRDS(list(schema=1L,first_season=2000L,seasons=2000:2026,starts=hist),file.path(td,"qb_start_history.rds"))
z<-refresh_qb_features(g,td,now+7*3600)
stopifnot(!z$qb_features_ready[1],is.na(z$starter_qb_epa_r4_home[1]))
s$players$starter_id[1]<-NA_character_
jsonlite::write_json(s,file.path(td,"qb_availability_2026_wk03.json"),auto_unbox=TRUE,na="null")
stopifnot(!refresh_qb_features(g,td,now)$qb_features_ready[1])
s$players$starter_id[1]<-"backup"
s$players<-rbind(s$players,s$players[1,])
jsonlite::write_json(s,file.path(td,"qb_availability_2026_wk03.json"),auto_unbox=TRUE)
stopifnot(!refresh_qb_features(g,td,now)$qb_features_ready[1])
z<-refresh_qb_features(g,td,now+5*86400)
stopifnot(identical(g,z[,names(g)]))
cat("PASS: expected-starter identity, latest game, trades, training parity, cutoff, coverage, missing IDs, stale feeds, completed/in-progress preservation\n")
