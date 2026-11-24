# Public Sleeper status/depth snapshot for expected-starter inputs and safety.
# Current expected-starter identity; never a historical feature source.
script_path <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
setwd(dirname(dirname(normalizePath(script_path))))  # project root
source("R/lib/qb_availability.R")
suppressPackageStartupMessages({library(jsonlite);library(httr)})
args <- commandArgs(TRUE)
stopifnot(length(args)==2)
season <- as.integer(args[1]); week <- as.integer(args[2])
response <- GET("https://api.sleeper.app/v1/players/nfl", timeout(20))
stop_for_status(response)
players <- fromJSON(content(response,"text",encoding="UTF-8"),simplifyVector=FALSE)
qbs <- Filter(function(p) identical(p$position,"QB") && !is.null(p$team),players)
# Join only stable IDs. Missing/ambiguous mappings stay unresolved, never use
# fuzzy names or infer identity from which QB last played for the team.
ids <- tryCatch(as.data.frame(nflreadr::load_ff_playerids()),error=function(e) data.frame())
rows <- lapply(qbs,function(p) data.frame(
  team=if(p$team=="LAR") "LA" else p$team,
  name=if(is.null(p$full_name)) paste(p$first_name,p$last_name) else p$full_name,
  starter_id=resolve_qb_gsis(p,ids),
  sleeper_id=if(is.null(p$player_id)) NA_character_ else as.character(p$player_id),
  depth=if(is.null(p$depth_chart_order)) NA_real_ else as.numeric(p$depth_chart_order),
  status=if(is.null(p$injury_status)) "" else p$injury_status))
d <- do.call(rbind,rows)
stopifnot(length(unique(d$team))>=30)
out <- list(season=season,week=week,
  observed_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S",tz="UTC"),
  source="https://api.sleeper.app/v1/players/nfl",
  identity_source="nflreadr::load_ff_playerids (Sleeper ID to GSIS ID)",players=d)
path <- sprintf("data/qb_availability_%d_wk%02d.json",season,week)
tmp <- paste0(path,".tmp")
write_json(out,tmp,auto_unbox=TRUE,pretty=TRUE,na="null")
stopifnot(file.rename(tmp,path))
cat("Wrote",path,"\n")
