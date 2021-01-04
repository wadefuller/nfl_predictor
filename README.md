# nfl_predictor
Lightweight and inaccurate model designed to predict NFL game outcomes.

### Model Features
Features:
- season: which season the game was played.
- week: which week of the season the game was played.
- spread_line: the predicted number of points that Vegas thinks will be scored.
- home_win_pct: the trailing 8 wk average win percentage at home for the home team.
- home_rest: the days of rest for the home team.
- rest_diff: the difference in days of rest between the home and away team.
- home_point_diff: the aggregate point differential for the home team (sum of win margin for the given season).
- pt_diff: the difference in aggregate point differential of the two teams (home minus away).
- home.win_pct: the win percentage for the home team that season.
- win_pct_diff: the difference in win percentage of the two teams (home minus away).


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
  mtry = 14
  trees = 1000
  min_n = 9
  tree_depth = 13
  learn_rate = 0.00300016862336822
  loss_reduction = 2.09922836365263e-06
  sample_size = 0.292054141357075

Computational engine: xgboost

rsq = 0.1357
rmse = 13.7193
```

Model Performance
----
In addition to offline performance evaluations, I am also experimenting with this model live in the 2020 season.

[Live results here](https://docs.google.com/spreadsheets/d/1xBt9tT3g6OpUco3cZJhfPTVls3azX_ZlFwWvIU3gjMA/edit?usp=sharing).
