# nfl_predictor
Lightweight and inaccurate model used to predict NFL game outcomes.

### Model Features
Features:
- week: which week of the season the game was played.
- home_win_pct: the home teams trailing 8 wk average win percentage.
- div_game: whether the game was played within a division.
- rest_diff: the difference in days of rest between the home and away team.
- home_rest: the days of rest that the home team had.
- primetime: whether or not the game was played in the evening (MNF, SNF, TNF).
- total_line: the predicted number of points that Vegas thinks will be scored.
- home.point_diff: the home team's point differential over the past 8 games.
- home.win_pct: the home team's win percentage over the past 8 games.
- away.point_diff: the away team's point differential over the past 8 games.
- away.win_pct: the away team's win percentage over the past 8 games.

Model Summary
----
```Call:
lm(formula = result ~ week + home_win_pct + div_game + rest_diff + 
    home_rest + primetime + total_line + home.point_diff + home.win_pct + 
    away.point_diff + away.win_pct, data = gm_aug)

Residuals:
    Min      1Q  Median      3Q     Max 
-47.176  -8.740  -0.212   8.424  51.208 

Coefficients:
                  Estimate Std. Error t value             Pr(>|t|)    
(Intercept)       1.007554   2.023869   0.498              0.61862    
week              0.042382   0.039075   1.085              0.27814    
home_win_pct      6.345792   1.272276   4.988          0.000000631 ***
div_game         -1.062778   0.399584  -2.660              0.00784 ** 
rest_diff         0.223811   0.099094   2.259              0.02395 *  
home_rest        -0.093945   0.125639  -0.748              0.45465    
primetime         0.239780   0.520941   0.460              0.64533    
total_line        0.004226   0.039950   0.106              0.91577    
home.point_diff  -0.014643   0.005467  -2.679              0.00742 ** 
home.win_pct     10.740953   1.224223   8.774 < 0.0000000000000002 ***
away.point_diff  -0.001946   0.004987  -0.390              0.69639    
away.win_pct    -13.466940   0.857539 -15.704 < 0.0000000000000002 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.95 on 5226 degrees of freedom
Multiple R-squared:  0.104,	Adjusted R-squared:  0.1021 
F-statistic: 55.15 on 11 and 5226 DF,  p-value: < 0.00000000000000022```
