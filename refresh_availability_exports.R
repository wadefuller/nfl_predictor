# Refresh only safety metadata in existing exports, preserving all forecast
# numbers and historical grades. Useful without refitting or fetching markets.
source("qb_availability.R")
args <- commandArgs(TRUE); stopifnot(length(args)==2)
season <- as.integer(args[1]); week <- as.integer(args[2])
raw <- readr::read_csv("data/game_w_features.csv",show_col_types=FALSE)
raw <- raw[raw$season==season & raw$week==week,,drop=FALSE]
checks <- annotate_qb_availability(raw,"data")
get_check <- function(id) {
  i <- match(id,checks$game_id)
  list(eligible=!is.na(i) && isTRUE(checks$availability_ok[i]),
       reason=if(is.na(i)) "QB availability unverified" else checks$availability_reason[i],
       checkedAt=if(is.na(i)) NA_character_ else checks$availability_checked_at[i])
}
save <- function(j,path) {
  tmp <- paste0(path,".tmp")
  jsonlite::write_json(j,tmp,auto_unbox=TRUE,pretty=TRUE,null="null",na="null",digits=NA)
  stopifnot(file.rename(tmp,path))
}
p <- sprintf("website/public/data/seasons/%d.json",season)
j <- jsonlite::fromJSON(p,simplifyVector=FALSE)
j$games <- lapply(j$games,function(g) {
  if(g$week==week && is.null(g$result)) {
    g$availability <- get_check(g$gameId)
    if(!g$availability$eligible) g["bet"] <- list(NULL)
  }
  g
})
save(j,p)
folder <- sprintf("website/public/data/weekly/%d-wk%02d",season,week)
for(kind in c("polymarket_bets","kalshi_lines","kalshi_win_probs","bet_card")) {
  p <- file.path(folder,paste0(kind,".json")); if(!file.exists(p)) next
  j <- jsonlite::fromJSON(p,simplifyVector=FALSE)
  key <- if(kind=="bet_card") "bets" else if(kind=="kalshi_lines") "lines" else "markets"
  j[[key]] <- lapply(j[[key]],function(r) {
    id <- r$game_id
    if(is.null(id) && !is.null(r$game)) {
      matchup <- paste(checks$away_team,"@",checks$home_team)
      id <- checks$game_id[match(r$game,matchup)]
    }
    if(is.null(id)) id <- NA_character_
    r$availability <- get_check(id)
    if(!r$availability$eligible) r$qualifies <- FALSE
    r
  })
  if(kind=="bet_card") {
    j$bets <- Filter(function(r) isTRUE(r$availability$eligible),j$bets)
    j$n_bets <- length(j$bets)
  }
  save(j,p)
}
cat("Refreshed availability metadata; forecast values unchanged.\n")
