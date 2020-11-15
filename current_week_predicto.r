## Replace this with wherever your game_predicto.r file is stored
## source('/nfl_predictor/game_predicto.r')

## This week's predictions
latest_data_ovr <-  tr %>%
  group_by(team) %>%
  summarise(win_pct=last(win_pct),
            point_diff = last(point_diff))

latest_data_home <- home_results %>%
  group_by(team) %>%
  summarise(home_win_pct=last(home_win_pct))

tm_data <- latest_data_ovr %>%
  inner_join(latest_data_home,by='team')

## Change the week number in this line
this_week <- games %>%
  filter(game_type == 'REG' & season == 2020 & week == 10 & is.na(result) == TRUE) %>%
  inner_join(tm_data,by=c('home_team'='team')) %>%
  rename(home.win_pct = win_pct,
         home.point_diff = point_diff) %>%
  inner_join(tm_data,by=c('away_team'='team')) %>%
  rename(away.win_pct = win_pct,
         away.point_diff = point_diff,
         home_win_pct = home_win_pct.x) %>%
  mutate(win = if_else(result > 0, 1, 0),
         rest_diff = home_rest - away_rest,
         primetime = if_else(weekday == 'Thursday' | weekday == 'Monday' | (weekday == 'Sunday' & gametime > '20:00'),1,0)) 
this_week.inputs_only <- this_week %>%
  select(game_id, win, week , home_win_pct , div_game , rest_diff , home_rest , primetime , spread_line , total_line, home.point_diff, home.win_pct, away.point_diff, away.win_pct)

## Model A: Predict winners by game
score_outcome <- predict.lm(model_a,this_week)
current_week_preds_a <- cbind(score_outcome,this_week)
current_week_preds_a$predicted_winner <- if_else(current_week_preds_a$score_outcome >= 0,current_week_preds_a$home_team,current_week_preds_a$away_team)
current_week_preds_a <- current_week_preds_a %>% relocate(predicted_winner, .before = score_outcome) %>% relocate(spread_line, .after = score_outcome) %>% relocate(total_line, .after = spread_line)
View(current_week_preds_a)

## Model B: Predict score outcomes by game
score_outcome <- predict.lm(model_b,this_week)
current_week_preds_b <- cbind(score_outcome,this_week)
current_week_preds_b$predicted_winner <- if_else(current_week_preds_b$score_outcome >= 0,current_week_preds_b$home_team,current_week_preds_b$away_team)
current_week_preds_b <- current_week_preds_b %>% relocate(predicted_winner, .before = score_outcome) %>% relocate(spread_line, .after = score_outcome) %>% relocate(total_line, .after = spread_line)
View(current_week_preds_b)
