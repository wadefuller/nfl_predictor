## Improvement log
## // Move to a machine learning style model
#### // Add preprocessing and hyper-parameter tuning
## // Evaluation accuracy of model on past seasons
#### // A.Rsq = .0709
#### // B.Rsq = .1025
#### // C.Rsq = .1027
## // Improve model with new features
#### // Rankings: Passing / Rushing Offense & Defense
#### // Deltas on either side of the ball
#### // Strength of win
##
## Inspired by the beautiful work here: https://www.nflfastr.com/articles/nflfastR.html
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(lfe)
library(zoo)
library(caret)
library(corrplot)
library(ggplot2)


options(scipen = 9999)

## PBP Data - Use these for future feature development
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
         primetime = if_else(weekday == 'Thursday' | weekday == 'Monday' | (weekday == 'Sunday' & gametime > '20:00'),1,0),
         pt_diff = home.point_diff - away.point_diff,
         win_pct_diff = home.win_pct - away.win_pct) 

glimpse(gm_aug)
model_only <- gm_aug %>%
  select (result,week,home_win_pct,div_game,rest_diff,home_rest,away_rest,primetime,total_line,home.point_diff,home.win_pct,away.point_diff,away.win_pct,win_pct_diff,pt_diff)

# Investigate the data correlations manually
corr_matrix <- cor(model_only) # create a correlation matrix
summary(corr_matrix[upper.tri(corr_matrix)]) # check out the correlation summary
sum(abs(corr_matrix[upper.tri(corr_matrix)]) > .999) # check for nearly perfectly correlated predictors (should output zero)
findCorrelation(corr_matrix, cutoff = .75) # check for highly correlated predictors
ggcorrplot(corr_matrix, 
           hc.order = FALSE, 
           type = "lower",
           lab = TRUE) # visually inspect correlations
nearZeroVar(corr_matrix, saveMetrics= TRUE) # check that there are no zero-variance predictors
# pairs(model_only) # check out the distributions of the predictors <- this may crash your R session

## Simple Modeling
## I want to create features that don't need to leverage the spread in order to produce an accurate result. 
## Theoretically, a model with high performance should be able to eventually 

# Result ~ Spread: .1798 adjusted R-squared
summary(lm(result ~ spread_line, gm_aug)) 
# Result ~ Non-spread features : .1027
summary(lm(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug))
# Win ~ Non-spread features : .0796
summary(lm(win ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug))
# Result ~ Non-spread features + season, away_rest, win_pct_diff : .1036
summary(lm(result ~ season + week + home_win_pct + div_game + rest_diff + home_rest + away_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct + win_pct_diff, gm_aug))

# Trim down the data frame to only include the target features
id_clean <- model_only[!rowSums(is.na(model_only)) > 0,] # scrub out rows with NAs
lapply(id_clean, function(x) any(is.na(x))) # make sure none are TRUE - these all need to be non-NA

# Split training and test data 70/30
trainIndex <- createDataPartition(id_clean$result, p = .7,
                                  list = FALSE,
                                  times = 1)
train <- id_clean[ trainIndex,]
test  <- id_clean[-trainIndex,]

## Simple linear model using Caret. R-Sq: .1059
lm.tune <- train(result ~ .
                 ,data = train
                 ,method = "lm")
summary(lm.tune)

# Use some ML techniques and try a few modeling methods
# Set up 10 fold cross validation (repeating 10 times)
fitControl <- trainControl(method = "repeatedcv"
                          ,number = 10
                          ,repeats = 10
                          ,search = 'random')
mtry <- ncol(train) - 1

rfGrid <-  expand.grid(mtry=c(1:mtry)
                       ,splitrule = "variance"
                       ,min.node.size = 10)

rf.tune <- train(result ~ .
                 ,data = train
                 ,trControl = fitControl
                 ,tuneGrid = rfGrid
                 ,tuneLength = 15
                 ,importance = TRUE
                 ,method = "ranger")
summary(rf.tune)
result_pred <- predict(rf.tune, test)
postResample(pred = result_pred, obs = test$result)
plot(rf.tune) # 1 predictor produces the best RMSE (likely the win diff pct)

## Implement gradient boosting
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9)
                        ,n.trees = (1:30)*50
                        ,shrinkage = 0.1
                        ,n.minobsinnode = 20)
gbm.tune <- train(result ~ .
                 ,data = train
                 ,method = "gbm"
                 ,verbose = FALSE
                 ,trControl = fitControl
                 ,tuneGrid = gbmGrid)
summary(gbm.tune) # Observe the important features
ggplot(gbm.tune) # Visualize the grid of parameters

# Compare the models to each other
result_pred_gbm <- predict(gbm.tune, test)
result_pred_lm <- predict(lm.tune, test)
result_pred_rf <- predict(rf.tune, test)
postResample(pred = result_pred_gbm, obs = test$result) # RMSE: 13.9744 // R-Sq: .0990
postResample(pred = result_pred_rf, obs = test$result) # RMSE: 14.0845 // R-Sq: .0871
postResample(pred = result_pred_lm, obs = test$result) # RMSE: 13.9865 // R-Sq: .0970

## Final Models
model_a <- lm(win ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug)
model_b <- lm(result ~ week + home_win_pct + div_game + rest_diff + home_rest + primetime + total_line + home.point_diff + home.win_pct + away.point_diff + away.win_pct, gm_aug)
model_c <- gbm.tune
