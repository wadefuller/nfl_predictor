## Improvement log
## // Move to a machine learning style model
## //// Add preprocessing and hyper-parameter tuning
## // Evaluation accuracy of model on past seasons
## // Improve model with new features
## //// Offense & Defensive rankings
##
## Inspired by the excellent work here: https://www.nflfastr.com/articles/nflfastR.html

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(lfe)
library(zoo)
library(caret)

options(scipen = 9999)

## PBP Data
# data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
# dim(data)
# View(data)

## Game Data
games <- readRDS(url("http://www.habitatring.com/games.rds"))
str(games)
## Home teams only
home <- games %>%
  filter(game_type == 'REG' & season >= 2000 & is.na(result) == FALSE ) %>%
  select(game_id,season, week, home_team, weekday, gametime, home_rest, away_rest, div_game, spread_line, total_line,result) %>%
  rename(team = home_team) %>%
  mutate(win = if_else(result > 0, 1, 0))
## Historical results with rolling means (home)
home_results <- home %>%
  select(game_id,team,season,week,result,win) %>%
  arrange(team,season,week) %>%
  group_by(team) %>%
  mutate(home_win_pct_lg = rollapply(win, width = list(-(1:1)), mean, fill = NA),
         home_win_pct_2 = rollapply(win, width = list(-(2:1)), mean, fill = NA),
         home_win_pct_3 = rollapply(win, width = list(-(3:1)), mean, fill = NA),
         home_win_pct_4 = rollapply(win, width = list(-(4:1)), mean, fill = NA),
         home_win_pct_5 = rollapply(win, width = list(-(5:1)), mean, fill = NA),
         home_win_pct_6 = rollapply(win, width = list(-(6:1)), mean, fill = NA),
         home_win_pct_7 = rollapply(win, width = list(-(7:1)), mean, fill = NA),
         home_win_pct_8 = rollapply(win, width = list(-(8:1)), mean, fill = NA)) %>%
  mutate(home_win_pct = coalesce(home_win_pct_8,home_win_pct_7,home_win_pct_6,home_win_pct_5,home_win_pct_4,home_win_pct_3,home_win_pct_2,home_win_pct_lg,0)) %>%
  select(game_id,team,home_win_pct)


## Overall (Home and away combined)
home_games <- games %>%
  filter(game_type == 'REG' & season >= 2000 & is.na(result) == FALSE) %>%
  select(game_id, season, week, home_team, weekday, gametime, home_rest, away_rest, div_game, spread_line, total_line, home_rest,away_rest,result) %>%
  rename(team = home_team) %>%
  mutate(home = 1,
         rest_diff = home_rest - away_rest,
         primetime = if_else(weekday == 'Thursday' | weekday == 'Monday' | (weekday == 'Sunday' & gametime > '20:00'),1,0))
away_games <- games %>%
  filter(game_type == 'REG' & season >= 2000 & is.na(result) == FALSE) %>%
  select(game_id, season, week, away_team, weekday, gametime, home_rest, away_rest, div_game, spread_line, total_line,home_rest,away_rest,result) %>%
  rename(team = away_team) %>%
  mutate(home = 0,
         rest_diff = home_rest - away_rest,
         primetime = if_else(weekday == 'Thursday' | weekday == 'Monday' | (weekday == 'Sunday' & gametime > '20:00'),1,0))
str(home_games) # check n rows
str(away_games) # check n rows
game_split <- rbind(home_games,away_games)
str(game_split) # check that these are the sum of those above
game_split$win <- if_else(game_split$home == 1 & game_split$result > 0,1,if_else(game_split$home == 0 & game_split$result < 0,1,0))
# View(game_split)
tr <- game_split %>%
  select(game_id,team,season,week,result,rest_diff,home_rest,div_game,primetime,weekday,gametime,spread_line,total_line,result,win) %>%
  arrange(team,season,week) %>%
  group_by(team) %>%
  mutate(win_pct_lg = rollapply(win, width = list(-(1:1)), mean, fill = NA),
         win_pct_last_2 = rollapply(win, width = list(-(2:1)), mean, fill = NA),
         win_pct_last_3 = rollapply(win, width = list(-(3:1)), mean, fill = NA),
         win_pct_last_4 = rollapply(win, width = list(-(4:1)), mean, fill = NA),
         win_pct_last_5 = rollapply(win, width = list(-(5:1)), mean, fill = NA),
         win_pct_last_6 = rollapply(win, width = list(-(6:1)), mean, fill = NA),
         win_pct_last_7 = rollapply(win, width = list(-(7:1)), mean, fill = NA),
         win_pct_last_8 = rollapply(win, width = list(-(8:1)), mean, fill = NA),
         point_diff_lg = rollapply(result, width = list(-(1:1)), sum, fill = NA),
         point_diff_l2 = rollapply(result, width = list(-(2:1)), sum, fill = NA),
         point_diff_l3 = rollapply(result, width = list(-(3:1)), sum, fill = NA),
         point_diff_l4 = rollapply(result, width = list(-(4:1)), sum, fill = NA),
         point_diff_l5 = rollapply(result, width = list(-(5:1)), sum, fill = NA),
         point_diff_l6 = rollapply(result, width = list(-(6:1)), sum, fill = NA),
         point_diff_l7 = rollapply(result, width = list(-(7:1)), sum, fill = NA),
         point_diff_l8 = rollapply(result, width = list(-(8:1)), sum, fill = NA)) %>% ## coalesce these win pcts
  mutate(win_pct = coalesce(win_pct_last_8,win_pct_last_7,win_pct_last_6,win_pct_last_5,win_pct_last_4,win_pct_last_3,win_pct_last_2,win_pct_lg,0),
         point_diff = coalesce(point_diff_l8,point_diff_l7,point_diff_l6,point_diff_l5,point_diff_l4,point_diff_l3,point_diff_l2,point_diff_lg,0))
tr_trim <- tr %>%
  select(game_id,team,win_pct,point_diff)
# View(tr)

gm_aug <- games %>%
  inner_join(tr_trim,by=c('game_id'='game_id','home_team'='team')) %>%
  rename(home.win_pct = win_pct,
         home.point_diff = point_diff) %>%
  inner_join(tr_trim,by=c('game_id'='game_id','away_team'='team')) %>%
  rename(away.win_pct = win_pct,
         away.point_diff = point_diff) %>%
  inner_join(home_results,by=c('game_id'='game_id','home_team'='team')) %>%
  mutate(win = if_else(result > 0, 1, 0),
         rest_diff = home_rest - away_rest,
         primetime = if_else(weekday == 'Thursday' | weekday == 'Monday' | (weekday == 'Sunday' & gametime > '20:00'),1,0)) 

## Modeling
summary(lm(result ~ spread_line, gm_aug))
summary(lm(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug))
summary(lm(win ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug))
summary(lm(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug))

## Final Models
model_a <-    lm(win ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug)
model_b <- lm(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug)

## Playing with some more interesting methods for improving the model. Non-linear model will ultimately be a more accurate way to go once additional features are added.
# input_data <- gm_aug %>%
#   select(result, game_id, win, week , home_win_pct , div_game , rest_diff , home_rest , primetime , spread_line , total_line, home.point_diff, home.win_pct, away.point_diff, away.win_pct)
# id_clean <- input_data[!rowSums(is.na(input_data)) > 0,]
# lapply(id_clean, function(x) any(is.na(x)))
# 
# trainIndex <- createDataPartition(id_clean$result, p = .7, 
#                                   list = FALSE, 
#                                   times = 1)
# train <- id_clean[ trainIndex,]
# test  <- id_clean[-trainIndex,]
# 
# rf.tune <- train(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct
#                  ,data = train
#                  ,method = "ranger")
# rf.tune.2 <- train(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct
#                  ,data = train
#                  ,method = "ranger"
#                  ,preProcess = c("center", "scale"))
#,ntrees = treeNum
#,tuneLength = tunelen)
# ,trControl = trControl)
# lm.tune <- train(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct
#                  ,data = train
#                  ,method = "lm")
# lm.tune.2 <- train(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct
#                  ,data = train
#                  ,method = "lm"
#                  ,preProcess = c("center", "scale"))
# model_b_rf <- predict(rf.tune, test)
# model_b_lm <- predict(lm.tune, test)

## Evaluate models 
# model_list <- list(rf = rf.tune, rf2 = rf.tune.2, lm = lm.tune, lm2 = lm.tune.2)
# res <- resamples(model_list)
# summary(res)
# compare_models(rf.tune, lm.tune)
