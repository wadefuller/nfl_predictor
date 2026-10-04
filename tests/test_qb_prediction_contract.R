# Local production-bundle integration check; no downloads or output writes.
source("ensemble_lib.R")
bundle <- readRDS(file.path(MODELS_DIR,"ensemble_latest.rds"))
raw <- read.csv(file.path(DATA_DIR,"game_w_features.csv"))
historical <- tail(raw[!is.na(raw$home_result),],20)
current <- predict_with_ensemble(bundle,historical)
# Reconstruct the pre-repair numerical prediction path, retaining metadata only
# so the output shape remains comparable. No input feature is changed.
repaired <- refresh_qb_features
refresh_qb_features <- function(games,...) {
  out <- repaired(games,...)
  out[names(games)] <- games
  out
}
baseline <- predict_with_ensemble(bundle,historical)
refresh_qb_features <- repaired
cols <- c("nfl_predictor_line","nfl_predictor_line_raw","pred_win_pct",
  "cover_prob_home","xgb_pred","glm_pred","rf_pred")
stopifnot(identical(current[,cols],baseline[,cols]))
empty <- generate_bet_card(current[0,],1000,bundle=bundle)
stopifnot(nrow(empty)==0,all(c("game_id","ev","stake") %in% names(empty)))
cat("PASS: historical production predictions identical; empty bet card preserves schema\n")

# Exercise the real serving path and trained recipe, not invented feature names.
# Time and starter status are fixtures; historical PBP remains cutoff at week 3.
local({
  game <- raw[raw$game_id=="2026_03_PHI_CHI",,drop=FALSE]
  stopifnot(nrow(game)==1)
  # The saved matrix now contains the final score. Recreate the pre-kickoff
  # row this fixture represents; completed games intentionally bypass live QB
  # replacement in production.
  game$home_result <- NA_real_
  clock <- as.POSIXct("2026-09-26 12:00:00",tz="UTC")
  td <- tempfile();dir.create(td)
  history <- readRDS(file.path(DATA_DIR,"qb_start_history.rds"))
  saveRDS(history,file.path(td,"qb_start_history.rds"))
  write.csv(raw,file.path(td,"game_w_features.csv"),row.names=FALSE)
  snapshot <- list(season=2026L,week=3L,observed_at="2026-09-26 11:00:00",
    players=data.frame(team=c("CHI","PHI"),depth=1,status="",
      name=c("Caleb Williams","Jalen Hurts"),starter_id=c("00-0039918","00-0036389")))
  write_snapshot <- function() jsonlite::write_json(snapshot,
    file.path(td,"qb_availability_2026_wk03.json"),auto_unbox=TRUE)
  original_refresh <- refresh_qb_features
  original_annotate <- annotate_qb_availability
  on.exit({assign("refresh_qb_features",original_refresh,.GlobalEnv)
           assign("annotate_qb_availability",original_annotate,.GlobalEnv)},add=TRUE)
  assign("refresh_qb_features",function(games,data_dir) original_refresh(games,td,clock),.GlobalEnv)
  assign("annotate_qb_availability",function(games,data_dir)
    cbind(games,assess_qb_availability(games,snapshot)),.GlobalEnv)
  write_snapshot()
  caleb <- predict_with_ensemble(bundle,game)
  snapshot$players$name[1]<-"Tyson Bagent"
  snapshot$players$starter_id[1]<-"00-0038416"
  write_snapshot()
  inputs <- original_refresh(game,td,clock)
  bagent <- predict_with_ensemble(bundle,game)
  diff_cols <- paste0(setdiff(QB_FEATURES,"new_starter"),"_home_diff")
  stopifnot(all(diff_cols %in% names(raw)),
    all(diff_cols %in% bundle$glm$recipe$term_info$variable),
    !"new_starter_home_diff" %in% bundle$glm$recipe$term_info$variable)
  for(f in QB_FEATURES) stopifnot(isTRUE(all.equal(inputs[[paste0(f,"_home_diff")]],
    inputs[[paste0(f,"_home")]]-inputs[[paste0(f,"_away")]])))
  # Independent expected row: replace both sides from player history and compute
  # canonical differentials without invoking refresh_qb_features.
  expected <- game
  for(side in c("home","away")) {
    id <- if(side=="home") "00-0038416" else "00-0036389"
    values <- qb_features_for_starter(history$starts,id,game[[paste0(side,"_team")]],2026,3)
    for(f in QB_FEATURES) expected[[paste0(f,"_",side)]]<-values[[f]]
  }
  for(f in QB_FEATURES) expected[[paste0(f,"_home_diff")]]<-
    expected[[paste0(f,"_home")]]-expected[[paste0(f,"_away")]]
  actual_prep <- prep_for_predict(inputs[,names(game)],bundle$leakage_vars)
  expected_prep <- prep_for_predict(expected,bundle$leakage_vars)
  stopifnot(identical(bake(bundle$glm$recipe,new_data=actual_prep),
                      bake(bundle$glm$recipe,new_data=expected_prep)))
  untouched <- setdiff(names(game),unlist(lapply(c("home","away","home_diff"),
    function(side)paste0(QB_FEATURES,"_",side))))
  print(data.frame(scenario=c("Williams","Bagent"),
    home_margin=c(caleb$nfl_predictor_line,bagent$nfl_predictor_line),
    home_win=c(caleb$pred_win_pct,bagent$pred_win_pct)))
  stopifnot(identical(inputs[,untouched],game[,untouched]),
    bagent$expected_qb_id_home=="00-0038416",bagent$availability_ok,
    bagent$nfl_predictor_line<caleb$nfl_predictor_line,
    bagent$pred_win_pct<caleb$pred_win_pct,
    abs(bagent$nfl_predictor_line-caleb$nfl_predictor_line)>0)
  cat("PASS: canonical QB columns reach baked production recipe; QB-only scenario affects margin and win probability\n")
})
