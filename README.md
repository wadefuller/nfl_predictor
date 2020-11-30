# nfl_predictor
Lightweight and inaccurate model designed to predict NFL game outcomes.

### Model Features
Features:
- season: which season the game was played.
- week: which week of the season the game was played.
- total_line: the predicted number of points that Vegas thinks will be scored.
- div_game: whether the game was played within a division.
- primetime: whether or not the game was played in the evening (MNF, SNF, TNF).
- home_win_pct: the trailing 8 wk average win percentage at home for the home team.
- home_rest: the days of rest for the home team.
- rest_diff: the difference in days of rest between the home and away team.
- home.point_diff: the aggregate point differential for the home team (sum of win margin for the given season).
- pt_diff: the difference in aggregate point differential of the two teams (home minus away).
- home.win_pct: the win percentage for the home team that season.
- win_pct_diff: the difference in win percentage of the two teams (home minus away).


Model Summary
----
```Call:
lm(formula = result ~ season + week + total_line + div_game + 
    primetime + home_win_pct + home_rest + rest_diff + home.point_diff + 
    pt_diff + home.win_pct + win_pct_diff, data = gm_aug)

Residuals:
    Min      1Q  Median      3Q     Max 
-46.190  -8.714  -0.178   8.355  50.816 

Coefficients:
                  Estimate Std. Error t value             Pr(>|t|)    
(Intercept)     188.416604  70.532464   2.671              0.00758 ** 
season           -0.094235   0.035416  -2.661              0.00782 ** 
week              0.038329   0.039011   0.983              0.32588    
total_line        0.053418   0.043236   1.236              0.21670    
div_game         -1.103347   0.397780  -2.774              0.00556 ** 
primetime         0.233918   0.518356   0.451              0.65181    
home_win_pct      6.396367   1.266165   5.052          0.000000453 ***
home_rest        -0.084625   0.124804  -0.678              0.49776    
rest_diff         0.204131   0.097912   2.085              0.03713 *  
home.point_diff  -0.018460   0.007268  -2.540              0.01112 *  
pt_diff           0.002871   0.004971   0.578              0.56360    
home.win_pct     -3.032467   1.484198  -2.043              0.04109 *  
win_pct_diff     13.633647   0.853599  15.972 < 0.0000000000000002 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.93 on 5266 degrees of freedom
Multiple R-squared:  0.1057,	Adjusted R-squared:  0.1036 
F-statistic: 51.84 on 12 and 5266 DF,  p-value: < 0.00000000000000022
```

Model Performance
----
In addition to offline performance evaluations, I am also experimenting with this model live in the 2020 season.

[Live results here](https://docs.google.com/spreadsheets/d/1xBt9tT3g6OpUco3cZJhfPTVls3azX_ZlFwWvIU3gjMA/edit?usp=sharing).
