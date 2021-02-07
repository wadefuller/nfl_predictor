### `nfl_predictor`
Lightweight model designed to predict NFL game outcomes using team performance and market data.

### Model Features

Metadata:
- season: which season the game was played.
- week: which week of the season the game was played.
- div_game: boolean indicating whether the teams share a division.
- home_rest: the days of rest for the home team.
- rest_diff: the difference in days of rest between the home and away team.

Market data:
- spread_line: the predicted number of points that the betting markets thinks will be scored.

Performance data:
- home_point_diff_trend: the trailing 8 wk point differential for the home team (sum of win margin for the given team).
- home_win_pct_trend: the trailing 8 wk win percentage for the home team.
- point_diff_ats_season: the season-long difference between the market expected points scored and the actual points scored for the home team. 
- point_diff_trend_diff: the trailing 8 wk point differential for the home team minus the same figure for the away team.
- win_pct_trend_diff: the trailing 8 wk average win percentage at home for the home team minus the same figure for the away team.


Model Summary
----
```
══ Workflow ═════════════════════════════════════════════════════════════════════════════════════
Preprocessor: Formula
Model: boost_tree()

── Preprocessor ─────────────────────────────────────────────────────────────────────────────────
home_result ~ .

── Model ────────────────────────────────────────────────────────────────────────────────────────
Boosted Tree Model Specification (regression)

Main Arguments:
  mtry = 13
  trees = 1000
  min_n = 40
  tree_depth = 6
  learn_rate = 0.00275162416666921
  loss_reduction = 5.6634051459418e-07
  sample_size = 0.717018291268032

Computational engine: xgboost 

rsq  =  0.1479
rmse = 13.6909
```

Cumulative Model MAE vs market spread:

<img src="https://github.com/wadefuller/nfl_predictor/blob/main/img/cum_model_error_season.jpg" width="700" height="700">

Model Performance
----
In addition to offline performance evaluations, I am also experimenting with this model live in the 2020 season.

[Live results here](httpst://docs.google.com/spreadsheets/d/1xBt9tT3g6OpUco3cZJhfPTVls3azX_ZlFwWvIU3gjMA/edit?usp=sharing).

Further improvements
----
A few feature improvements that are on the backlog:
- [Expected Points Added](https://www.espn.com/nfl/story/_/id/8379024/nfl-explaining-expected-points-metric): Team EPA, Starting Quarterback EPA, Defensive EPA, Offensive EPA
- [Pythagorean wins](http://grantland.com/features/breaking-best-nfl-stats/)
- Market Signals: Moneylines & Odds
