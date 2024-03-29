### `nfl_predictor`
Lightweight model designed to predict NFL game outcomes using only team performance and market data that is available at kickoff.

2022 Predictions + Performance [here](https://docs.google.com/spreadsheets/d/1bBT9MqBJO8FUqHRNk33IfHDlvAMWZS-NBgruz5EKhe8/edit#gid=1401699762).


### Repo construction
1. `feature_engineering.rmd`: Build the model inputs.
2. `build_model.rmd`: Leverage Tidymodels to build an xgboost classifier.
3. `prediction_visuals.rmd`: Run some diagnostics on the predictions.
4. `predict_week.rmd`: Derive predictions for any given NFL week.

### Model Features

140 features that fall into the following buckets:

Metadata:
- season: which season the game was played.
- week: which week of the season the game was played.
- div_game: boolean indicating whether the teams share a division.
- home_rest: the number days since the team's last game.
- rest_diff: the difference in days of rest between the home and away team.

Market data:
- spread_line: the predicted game score from Pinnacle sports book.
- elo_prob1: 538's probability that the home team will win. ([538's model explained](https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/))
- elo1_pre: 538's pregame elo score for the home team.
- elo2_pre: 538's pregame elo score for the away team.
- moneyline: the [moneyline](https://theathletic.com/2514849/2022/01/25/moneyline-bets-in-sports-what-are-they-how-to-place-a-moneyline-bet-odds-payouts-favorites-vs-underdogs-and-examples/) for either team in Pinnacle sports book.
- implied win probability: the transformation of either team's moneyline rate to a percentage (vig included)

Performance data:
- home_point_diff_trend: the trailing 8 wk point differential (sum of win margin for the given team).
- home_win_pct_trend: the trailing 8 wk win percentage.
- point_diff_ats_season: the season-long difference between the spread expected game result and the actual game result.
- point_diff_trend_diff: the trailing 8 wk point differential for the home team minus the same figure for the away team.
- win_pct_trend_diff: the trailing 8 wk average win percentage at home for the home team minus the same figure for the away team.
- pythag_wins: adapted from the work of Bill James to predict NFL wins based on points for and points against. ([pythagorean win expecatation explained](https://www.pro-football-reference.com/blog/indexf6a9.html?p=337))
- off_epa: a team's expected points added from the offense. ([expected points added](https://www.espn.com/nfl/story/_/id/8379024/nfl-explaining-expected-points-metric))
- off_pass_epa: a team's EPA from passing plays.
- off_rush_epa: a team's EPA from rushing plays.


Model Roadmap
----
- Model engine explorations (Torch, Tabnet, etc)
- Additional feature set explorations
- Re-introduce regularization + feature selection
- Explore player performance models
