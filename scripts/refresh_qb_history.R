# Compact, resumable QB history refresh: no ensemble retraining required.
# Rscript scripts/refresh_qb_history.R 2026
script_path <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
setwd(dirname(dirname(normalizePath(script_path))))  # project root
suppressPackageStartupMessages({library(dplyr);library(nflreadr)})
source("R/infra/team_normalize.R")
source("R/infra/data_loaders.R")
source("R/features/_module_base.R")
source("R/features/qb_starter.R")
args <- commandArgs(TRUE)
season <- if(length(args)) as.integer(args[1]) else nflreadr::most_recent_season()
stopifnot(is.finite(season),season>=2000)
dir.create("data/qb_history",showWarnings=FALSE)
parts <- lapply(2000:season,function(s) {
  path <- sprintf("data/qb_history/%d.rds",s)
  if (s < season && file.exists(path)) return(readRDS(path))
  message("Refreshing QB history: ",s)
  p <- read_pbp(s)   # fantasy_db/pbp.db via shared/pbp.R (sourced by data_loaders.R)
  stopifnot(nrow(p)>0,all(p$season==s))
  starts <- identify_starters(filter_model_pbp(p))
  stopifnot(nrow(starts)>0)
  saveRDS(starts,paste0(path,".tmp")); stopifnot(file.rename(paste0(path,".tmp"),path))
  starts
})
h <- bind_rows(parts)
stopifnot(!anyDuplicated(h[c("game_id","team")]))
out <- list(schema=1L,first_season=2000L,seasons=2000:season,
  observed_at=format(Sys.time(),tz="UTC"),starts=h)
saveRDS(out,"data/qb_start_history.rds.tmp")
stopifnot(file.rename("data/qb_start_history.rds.tmp","data/qb_start_history.rds"))
cat("QB history refreshed:",nrow(h),"team-games\n")
