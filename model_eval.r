###################################
### Investigate the accuracy of the model by:
### - Comparing to Fivethirtyeight's model and Vegas' opening lines
### - Comparing to other individuals participating in Lee Sharpe's NFL Prediction Game (https://nflgamedata.com/predict/picks.php)
### - Observing model accuracy at different prediction thresholds
###################################

library(tidyverse)
library(tidymodels)
library(recipeselectors)
library(knitr)
library(RCurl)
library(data.table)
library(stringr)
library(ggplot2)
knitr::opts_knit$set(root.dir = "~/Documents/Scripts/R/")

wd <- "~/Documents/Scripts/R/pred/"
# source("~/Documents/Scripts/R/pred/feature_engineering.rmd")
game_w_features <- read.csv("~/Documents/Scripts/R/pred/game_w_features.csv")
nfl_predictor_model <- readRDS("~/Documents/Scripts/R/pred/nfl_predictor_model.rds")
nfl_predictor_model_class <- readRDS("~/Documents/Scripts/R/pred/nfl_predictor_model_class.rds")

season_dta <- game_w_features %>% filter(week <= 18 & season == 2021)
season_dta %>% summarize(win_pct = mean(home_win, na.rm = TRUE))

season_dta <- game_w_features %>% filter(!is.na(home_win) & season >= 2015)
season_dta_reg <- season_dta %>% mutate(week = as.factor(week), season = as.factor(season), div_game = as.factor(div_game))
season_dta_class <- season_dta %>% mutate(week = as.factor(week), season = as.factor(season), div_game = as.factor(div_game))
season_dta_preds <- predict(nfl_predictor_model, new_data = season_dta_reg)
season_dta_preds_class <- predict(nfl_predictor_model_class, new_data = season_dta_class, type = 'prob')
season_dta_preds_class <- season_dta_preds_class %>% transmute(pred_win_pct = .pred_1)
season_dta_pred <- season_dta %>% bind_cols(season_dta_preds) %>% bind_cols(season_dta_preds_class) %>% 
  rename(score_pred = .pred) %>%
  transmute(
    game_id
    ,home_team
    ,away_team
    ,pred_winner = if_else(pred_win_pct>.5,home_team,away_team)
    ,pred_win_pct_adj = if_else(pred_win_pct>.5,pred_win_pct,1-pred_win_pct)
    ,home_win_pct = pred_win_pct
    ,pred_spread_adj_winner = if_else(score_pred + spread_line > 0,home_team,away_team)
    ,score_pred = round(score_pred,1)
    ,spread_line
    ,total_line
  )
season_dta_pred %>% View()
pred_names <- season_dta_pred %>% names()
season_dta_fin <- season_dta_pred %>% inner_join(season_dta)
season_dta_fin %>% 
  mutate(pred_threshold = .5
         ,pred_winner = if_else(home_win_pct>pred_threshold,as.character(home_team),if_else(1-home_win_pct>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_cont = if_else(score_pred>pred_threshold,as.character(home_team),if_else(-score_pred>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_fte = if_else(elo_prob1>pred_threshold,as.character(home_team),if_else(1-elo_prob1>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_veg = if_else(spread_line<0,as.character(home_team),if_else(spread_line>0,as.character(away_team),as.character('No Pred')))
         ,pred_active = if_else(pred_winner == 'No Pred',F,T)
         ,pred_active_cont = if_else(pred_winner_veg == 'No Pred',F,T)
         ,pred_active_fte = if_else(pred_winner_fte == 'No Pred',F,T)
         ,pred_active_veg = if_else(pred_winner_veg == 'No Pred',F,T)
         ,actual_winner = if_else(home_win == 1,home_team,away_team)
         # ,pred_correct = if_else(pred_active == 1 & pred_winner == actual_winner,1,if_else(pred_active == 0,NA,0))
         ,pred_correct = case_when(
           pred_active & pred_winner == actual_winner ~ 1,
           pred_active & pred_winner != actual_winner ~ 0,
           !pred_active ~ as.double(NA)
         )
         ,pred_correct_fte = case_when(
           pred_active_fte & pred_winner_fte == actual_winner ~ 1,
           pred_active_fte & pred_winner_fte != actual_winner ~ 0,
           !pred_active_fte ~ as.double(NA)
         )
         ,pred_correct_veg = case_when(
           pred_active_veg & pred_winner_veg == actual_winner ~ 1,
           pred_active_veg & pred_winner_veg != actual_winner ~ 0,
           !pred_active_veg ~ as.double(NA)
         )
         ,pred_correct_cont = case_when(
           pred_active_cont & pred_winner_cont == actual_winner ~ 1,
           pred_active_cont & pred_winner_cont != actual_winner ~ 0,
           !pred_active_cont ~ as.double(NA)
         )
  ) %>%
  select(pred_correct , pred_correct_fte, pred_winner, pred_winner_fte, pred_correct_veg, pred_winner_veg, pred_correct_cont, pred_winner_cont, actual_winner, pred_names, pred_threshold) %>%
  summarise(win_pct = mean(pred_correct,na.rm = TRUE)
            ,win_pct_cont = mean(pred_correct_cont,na.rm = TRUE)
            ,win_pct_fte = mean(pred_correct_fte,na.rm = TRUE)
            ,win_pct_veg = mean(pred_correct_veg,na.rm = TRUE)
            ,tot_gms=n()
            ,n_active_wins=sum(pred_correct,na.rm=T)
            ,n_active_gms=sum(!is.na(pred_correct))
            ,pct_games_active = n_active_gms/tot_gms
            ,n_active_wins_fte=sum(pred_correct_fte,na.rm=T)
            ,n_active_gms_fte=sum(!is.na(pred_correct_fte))
            ,pct_games_active_fte = n_active_gms_fte/tot_gms
            ,n_active_wins_veg=sum(pred_correct_veg,na.rm=T)
            ,n_active_gms_veg=sum(!is.na(pred_correct_veg))
            ,pct_games_active_veg = n_active_gms_veg/tot_gms
            )
lst <- list()
thresholds <- seq(.5, .99, by=.01)
for (i in 1:length(thresholds)) {
n_thr <- i * .01 + .5
thr_dta <-season_dta_fin %>% 
  mutate(pred_threshold = n_thr
         ,pred_winner = if_else(home_win_pct>pred_threshold,as.character(home_team),if_else(1-home_win_pct>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_fte = if_else(elo_prob1>pred_threshold,as.character(home_team),if_else(1-elo_prob1>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_active = if_else(pred_winner == 'No Pred',F,T)
         ,pred_active_fte = if_else(pred_winner_fte == 'No Pred',F,T)
         ,actual_winner = if_else(home_win == 1,home_team,away_team)
         # ,pred_correct = if_else(pred_active == 1 & pred_winner == actual_winner,1,if_else(pred_active == 0,NA,0))
         ,pred_correct = case_when(
           pred_active & pred_winner == actual_winner ~ 1,
           pred_active & pred_winner != actual_winner ~ 0,
           !pred_active ~ as.double(NA)
         )
         ,pred_correct_fte = case_when(
           pred_active_fte & pred_winner_fte == actual_winner ~ 1,
           pred_active_fte & pred_winner_fte != actual_winner ~ 0,
           !pred_active_fte ~ as.double(NA)
         )
  ) %>%
  select(pred_correct , pred_correct_fte, pred_winner, pred_winner_fte, actual_winner, pred_names, pred_threshold) %>%
  summarise(win_pct = mean(pred_correct,na.rm = TRUE)
            ,tot_gms=n()
            ,n_active_wins=sum(pred_correct,na.rm=T)
            ,n_active_gms=sum(!is.na(pred_correct))
            ,pct_games_active = n_active_gms/tot_gms
            ,win_pct_fte = mean(pred_correct_fte,na.rm = TRUE)
            ,n_active_wins_fte=sum(pred_correct_fte,na.rm=T)
            ,n_active_gms_fte=sum(!is.na(pred_correct_fte))
            ,pct_games_active_fte = n_active_gms_fte/tot_gms)
thr_dta$pred_threshold <- n_thr
# lst <- append(lst,thr_dta)
lst[[i]] <- thr_dta
}
thr_agg <- dplyr::bind_rows(lst)
thr_agg %>%
  ggplot() +
  geom_line(aes(x = pred_threshold, y = win_pct)) +
  geom_line(aes(x = pred_threshold, y = win_pct_fte)) + 
  labs(x = 'Prediction Threshold', y = 'Prediction Accuracy') +
  scale_x_continuous(labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  theme_minimal()

mx <- max(thr_agg$n_active_gms)
thr_agg %>%
  ggplot() +
  geom_bar(aes(x = pred_threshold, y = n_active_gms),fill = 'lightgray',stat="identity") +
  geom_line(aes(x = pred_threshold, y = win_pct*mx),stat='identity', size = .5) +
  labs(x = 'Prediction Probability', y = 'Number of Games') +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,1,by=.1)) +
  scale_y_continuous(sec.axis = sec_axis(~./mx, name = 'Prediction Accuracy', labels = percent_format(accuracy = 1), breaks = seq(0, 1, by = .1))) +
  # theme_minimal() +
  ggtitle("Model Prediction Probability vs Accuracy") +
  labs(caption = '(using latest xgb model + feature set rolled out in wk 6)')

###################################
### LEE SHARPE'S PREDICTION GAME ##
###################################

x <- getURL("https://raw.githubusercontent.com/nflverse/nfldata/master/data/predictions.csv")
game_w_features <- read.csv("~/Documents/Scripts/R/pred/game_w_features.csv")
y <- read.csv(text = x)
market <- y %>%
  group_by(game_id) %>%
  summarise(mkt_pred = round(mean(prediction),0))

pred_dta <- y %>% 
  # filter(grepl('WadeFuller',screen_name,fixed = FALSE)) %>%
  inner_join(market) %>%
  inner_join(game_w_features) %>%
  transmute( season
             ,week
             ,game_id
             ,screen_name
             ,sn_pred = prediction
             ,mkt_pred
             ,vegas_pred = if_else(spread_line<0,1,0)
             ,fte_pred = if_else(elo_prob1>.5,1,0)
             ,vegas_correct = if_else(vegas_pred == home_win,1,0)
             ,fte_correct = if_else(fte_pred == home_win,1,0)
             ,sn_correct = if_else((home_win == 1 & sn_pred > 50) | (home_win == 0 & sn_pred < 50),1,0)
             ,mkt_correct = if_else((home_win == 1 & mkt_pred > 50) | (home_win == 0 & mkt_pred < 50),1,0)
             ,sn_error = sn_pred - home_win*100
             ,mkt_error = mkt_pred - home_win*100
             ,mkt_error_pct = prediction - mkt_pred
             ,mkt_error_outcome = if_else((prediction > 50 & mkt_pred < 50) | (prediction < 50 & mkt_pred > 50),1,0)
             ,spread_line
             ,home_win
  ) %>%
  group_by(screen_name) %>%
  summarise( n = n()
             ,n_preds = sum(sn_pred!=50)
             ,n_correct = sum(sn_correct)
             ,sn_accuracy = n_correct / n_preds
             ,mkt_accuracy = mean(mkt_correct)
             ,vegas_accuracy = mean(vegas_correct)
             ,fte_accuracy = mean(fte_correct)
             ,tot_sn_error = sum(sn_error)
             ,avg_sn_error = mean(sn_error)
             ,rmse_sn = sqrt(mean(sn_error))
             )

personal_dta <- pred_dta %>%
  filter(grepl('WadeFuller',screen_name,fixed = FALSE))
personal_dta

ptrns <- c('greerreNFL','WadeFuller')

pred_dta %>%
  filter(grepl(paste(ptrns,collapse='|'),screen_name,fixed = FALSE)) %>%
  View()

pred_dta_cln <- pred_dta %>%
  mutate(tot_gms = max(n)) %>%
  filter(n_preds >= tot_gms*.8)
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
quantiles <- quantile(pred_dta_cln$sn_accuracy, prob=probs)
pred_dta_cln %>% summarise(max(sn_accuracy), min(sn_accuracy), n())

pred_dta %>%
  mutate(tot_gms = max(n)) %>%
  filter(n_preds >= tot_gms*.8) %>% 
  ggplot(aes(x = sn_accuracy)) +
  geom_density() +
  scale_y_continuous(name = "Number of Participants") +
  scale_x_continuous(labels = percent_format(accuracy = 1), name = "Prediction Accuracy",  breaks = seq(0, 1, by = .025)) +
  theme_minimal() +
  ggtitle("Model Prediction Accuracy: Active Prediction Game Participants",subtitle = "2021 season, weeks 1-16") + 
  labs(caption = "(active participants are those w/ predictions of >50% for >80% of games)") +
  theme(plot.caption.position =  "plot")

pred_dta %>%
  mutate(tot_gms = max(n)) %>%
  filter(n_preds >= tot_gms*.8) %>% 
  ggplot(aes(x = sn_accuracy)) +
  geom_density() +
  geom_vline(aes(xintercept=quantiles[[1]]), size = .5, colour="#d0dce5",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[2]]), size = .5, colour="#a1bacb",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[3]]), size = .5, colour="#7397b1",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[4]]), size = .5, colour="#447597",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[5]]), size = .5, colour="#16537e",linetype="dashed") +
  annotate("text",x=quantiles[[1]]+.0035, label="10th Percentile", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[2]]+.0035, label="25th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[3]]+.0035, label="50th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[4]]+.0035, label="75th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[5]]+.0035, label="90th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  # geom_vline(aes(xintercept=personal_dta$sn_accuracy), size = 1, colour="black",linetype=1) +
  # geom_vline(aes(xintercept=personal_dta$fte_accuracy), size = 1, colour="black",linetype=1) +
  # geom_vline(aes(xintercept=personal_dta$vegas_accuracy), size = 1, colour="black",linetype=1) +
  # annotate("text",x=personal_dta$sn_accuracy+.0085, label="@wadefuller", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  # annotate("text",x=personal_dta$vegas_accuracy-.013, label="Vegas opening line", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  # annotate("text",x=personal_dta$fte_accuracy-.01, label="Fivethirtyeight", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  scale_y_continuous(name = "Number of Participants") +
  scale_x_continuous(labels = percent_format(accuracy = 1), name = "Prediction Accuracy",  breaks = seq(0, 1, by = .025)) +
  theme_minimal() +
  ggtitle("Model Prediction Accuracy: Active Prediction Game Participants",subtitle = "2021 season, weeks 1-16") + 
  labs(caption = "(active participants are those w/ predictions of >50% for >80% of games)") +
  theme(plot.caption.position =  "plot")


pred_dta %>%
  mutate(tot_gms = max(n)) %>%
  filter(n_preds > tot_gms*.8) %>% 
  ggplot(aes(x = sn_accuracy)) +
  geom_density() +
  geom_vline(aes(xintercept=quantiles[[1]]), size = .5, colour="#d0dce5",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[2]]), size = .5, colour="#a1bacb",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[3]]), size = .5, colour="#7397b1",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[4]]), size = .5, colour="#447597",linetype="dashed") +
  geom_vline(aes(xintercept=quantiles[[5]]), size = .5, colour="#16537e",linetype="dashed") +
  annotate("text",x=quantiles[[1]]-.006, label="10th Percentile", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[2]]+.0035, label="25th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[3]]+.0035, label="50th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[4]]+.0035, label="75th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  annotate("text",x=quantiles[[5]]+.0035, label="90th", y=0, colour="#565656", family = 'sans', fontface = 'plain', angle=0, size=2.5) +
  geom_vline(aes(xintercept=personal_dta$sn_accuracy), size = 1, colour="black",linetype=1) +
  geom_vline(aes(xintercept=personal_dta$mkt_accuracy), size = 1, colour="black",linetype=1) +
  geom_vline(aes(xintercept=personal_dta$fte_accuracy), size = 1, colour="black",linetype=1) +
  geom_vline(aes(xintercept=personal_dta$vegas_accuracy), size = 1, colour="black",linetype=1) +
  annotate("text",x=personal_dta$sn_accuracy+.007, label="@wadefuller", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  annotate("text",x=personal_dta$mkt_accuracy+.018, label="Prediction Game Market", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  annotate("text",x=personal_dta$vegas_accuracy-.017, label="Vegas opening line", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  annotate("text",x=personal_dta$fte_accuracy-.008, label="Fivethirtyeight", y=15, colour="#565656", family = 'sans', fontface = 'bold', angle=0, size=2.5) +
  scale_y_continuous(name = "Number of Participants") +
  scale_x_continuous(labels = percent_format(accuracy = 1), name = "Prediction Accuracy",  breaks = seq(0, 1, by = .025)) +
  theme_minimal() +
  ggtitle("Model Prediction Accuracy: Active Prediction Game Participants",subtitle = "2021 season, weeks 1-16") + 
  labs(caption = "(active participants are those w/ predictions of >50% for >80% of games)") +
  theme(plot.caption.position =  "plot")



#### EVAL WEEK BY WEEK ####
season_dta_fin

weekly_perf <- season_dta_fin %>% 
  mutate( season_week_raw = paste0(season,"_",week)
          ,season_week = if_else(nchar(season_week_raw)<7,paste0(substr(season_week_raw, 1,5),"0",substr(season_week_raw,6,6)),season_week_raw)
  ) %>%
  group_by(season,week) %>%
  mutate(pred_threshold = .5
         ,pred_winner = if_else(home_win_pct>pred_threshold,as.character(home_team),if_else(1-home_win_pct>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_cont = if_else(score_pred>pred_threshold,as.character(home_team),if_else(-score_pred>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_fte = if_else(elo_prob1>pred_threshold,as.character(home_team),if_else(1-elo_prob1>pred_threshold,as.character(away_team),as.character('No Pred')))
         ,pred_winner_veg = if_else(spread_line<0,as.character(home_team),if_else(spread_line>0,as.character(away_team),as.character('No Pred')))
         ,pred_active = if_else(pred_winner == 'No Pred',F,T)
         ,pred_active_cont = if_else(pred_winner_veg == 'No Pred',F,T)
         ,pred_active_fte = if_else(pred_winner_fte == 'No Pred',F,T)
         ,pred_active_veg = if_else(pred_winner_veg == 'No Pred',F,T)
         ,actual_winner = if_else(home_win == 1,home_team,away_team)
         # ,pred_correct = if_else(pred_active == 1 & pred_winner == actual_winner,1,if_else(pred_active == 0,NA,0))
         ,pred_correct = case_when(
           pred_active & pred_winner == actual_winner ~ 1,
           pred_active & pred_winner != actual_winner ~ 0,
           !pred_active ~ as.double(NA)
         )
         ,pred_correct_fte = case_when(
           pred_active_fte & pred_winner_fte == actual_winner ~ 1,
           pred_active_fte & pred_winner_fte != actual_winner ~ 0,
           !pred_active_fte ~ as.double(NA)
         )
         ,pred_correct_veg = case_when(
           pred_active_veg & pred_winner_veg == actual_winner ~ 1,
           pred_active_veg & pred_winner_veg != actual_winner ~ 0,
           !pred_active_veg ~ as.double(NA)
         )
         ,pred_correct_cont = case_when(
           pred_active_cont & pred_winner_cont == actual_winner ~ 1,
           pred_active_cont & pred_winner_cont != actual_winner ~ 0,
           !pred_active_cont ~ as.double(NA)
         )
  ) %>%
  select(pred_correct , pred_correct_fte, pred_winner, pred_winner_fte, pred_correct_veg, pred_winner_veg, pred_correct_cont, pred_winner_cont, actual_winner, pred_names, pred_threshold) %>%
  summarise(win_pct = mean(pred_correct,na.rm = TRUE)
            ,win_pct_cont = mean(pred_correct_cont,na.rm = TRUE)
            ,win_pct_fte = mean(pred_correct_fte,na.rm = TRUE)
            ,win_pct_veg = mean(pred_correct_veg,na.rm = TRUE)
            ,tot_gms=n()
            ,n_active_wins=sum(pred_correct,na.rm=T)
            ,n_active_gms=sum(!is.na(pred_correct))
            ,pct_games_active = n_active_gms/tot_gms
            ,n_active_wins_fte=sum(pred_correct_fte,na.rm=T)
            ,n_active_gms_fte=sum(!is.na(pred_correct_fte))
            ,pct_games_active_fte = n_active_gms_fte/tot_gms
            ,n_active_wins_veg=sum(pred_correct_veg,na.rm=T)
            ,n_active_gms_veg=sum(!is.na(pred_correct_veg))
            ,pct_games_active_veg = n_active_gms_veg/tot_gms
  ) %>% ungroup()

weekly_perf_cml <- weekly_perf %>% 
  filter(season >= 2000) %>%
  transmute(
     season
    ,week
    ,cml_active_gms = cumsum(n_active_gms)
    ,cml_active_wins = cumsum(n_active_wins)
    ,cml_win_pct = cml_active_wins / cml_active_gms
    ,cml_active_gms_veg = cumsum(n_active_gms_veg)
    ,cml_active_wins_veg = cumsum(n_active_wins_veg)
    ,cml_win_pct_veg = cml_active_wins_veg / cml_active_gms_veg
    ,cml_active_gms_fte = cumsum(n_active_gms_fte)
    ,cml_active_wins_fte = cumsum(n_active_wins_fte)
    ,cml_win_pct_fte = cml_active_wins_fte / cml_active_gms_fte
    ,cml_delta = cml_win_pct - cml_win_pct_veg
  )

weekly_perf_cml %>%
  ggplot(aes(x=interaction(season, week, lex.order = TRUE), group = 1)) +
  geom_line(aes(y = cml_win_pct, colour = 'cml_win_pct')) +
  geom_line(aes(y = cml_win_pct_veg, colour = 'cml_win_pct_veg')) +
  geom_line(aes(y = cml_win_pct_fte, colour = 'cml_win_pct_fte')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Model Accuracy: NFLPredictor vs Vegas vs Fivethirtyeight",subtitle = "2015-2021 seasons") +
  labs(y= "Cumulative Performance", x = "Season",caption = "(active participants are those w/ predictions of >50% for >80% of games)") +
  scale_colour_manual("", 
    values = c("cml_win_pct"="steelblue", "cml_win_pct_veg"="darkred", "cml_win_pct_fte"="darkgreen"))

weekly_perf_cml %>%
  ggplot(aes(x=interaction(season, week, lex.order = TRUE), group = 1)) +
  geom_line(aes(y = cml_win_pct, colour = 'cml_win_pct')) +
  geom_line(aes(y = cml_win_pct_veg, colour = 'cml_win_pct_veg')) +
  geom_line(aes(y = cml_win_pct_fte, colour = 'cml_win_pct_fte')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Model Accuracy: NFLPredictor vs Vegas vs Fivethirtyeight",subtitle = "2015-2021 seasons") +
  labs(y= "Cumulative Performance", x = "Season",caption = "(active participants are those w/ predictions of >50% for >80% of games)") +
  scale_colour_manual("", values = c("cml_win_pct"="steelblue", "cml_win_pct_veg"="darkred", "cml_win_pct_fte"="darkgreen"))

weekly_perf_cml %>%
  filter(week <= 17) %>%
  ggplot(aes(x=week, group = 1)) +
  geom_line(aes(y = cml_win_pct, colour = 'NFL Predictor Accuracy')) +
  geom_line(aes(y = cml_win_pct_veg, colour = 'Vegas Accuracy')) +
  geom_line(aes(y = cml_win_pct_fte, colour = 'Fivethirtyeight Accuracy')) +
  facet_grid(~ season, switch = "x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Model Accuracy: NFLPredictor vs Pinnacle vs Fivethirtyeight",subtitle = "2015-2021 seasons") +
  labs(y= "Cumulative Performance (% of games accurately predicted)", x = "Season / Week") +
  scale_colour_manual("", values = c("NFL Predictor Accuracy"="steelblue", "Vegas Accuracy"="darkred", "Fivethirtyeight Accuracy"="darkgreen"))

weekly_perf_cml %>%
  ggplot(aes(x=interaction(season, week, lex.order = TRUE), group = 1)) +
  geom_line(aes(y = cml_delta)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Model Prediction Accuracy: NFLPredictor vs Vegas",subtitle = "2015-2021 seasons") +
  labs(caption = "(active participants are those w/ predictions of >50% for >80% of games)")
