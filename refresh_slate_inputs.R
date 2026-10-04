# Refresh pending-game schedule/market inputs without rebuilding historical features.
# Usage: Rscript refresh_slate_inputs.R SEASON WEEK
script_path <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
setwd(dirname(normalizePath(script_path)))
suppressPackageStartupMessages(library(dplyr))
source("R/infra/team_normalize.R")
source("R/infra/data_loaders.R")
args <- commandArgs(TRUE);stopifnot(length(args)==2)
season <- as.integer(args[1]);week <- as.integer(args[2])
g <- read.csv("data/game_w_features.csv")
fresh <- normalize_team_names(fetch_games_rds())
stopifnot(!anyDuplicated(fresh$game_id))
kickoff <- as.POSIXct(paste(g$gameday,g$gametime),tz="America/New_York")
i <- which(g$season==season & g$week==week & is.na(g$home_result) &
  !is.na(kickoff) & as.numeric(kickoff)>as.numeric(Sys.time()))
j <- match(g$game_id[i],fresh$game_id)
stopifnot(length(i)>0,!anyNA(j),all(g$home_team[i]==fresh$home_team[j]),
          all(g$away_team[i]==fresh$away_team[j]))
# The wide matrix and upstream games use the same positive-home-favorite sign.
cols <- c("gameday","gametime","weekday","spread_line","total_line",
          "home_moneyline","away_moneyline","home_spread_odds","away_spread_odds")
stopifnot(all(cols %in% names(fresh)),all(cols %in% names(g)))
for(col in cols) g[i,col] <- fresh[[col]][j]
readr::write_csv(g,"data/game_w_features.csv.tmp")
stopifnot(file.rename("data/game_w_features.csv.tmp","data/game_w_features.csv"))
cat("Refreshed schedule and market inputs for",length(i),"pregame matchups\n")
