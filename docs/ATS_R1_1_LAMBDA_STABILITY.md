# ATS R1.1 — lambda-selection stability

**Status:** completed 2026-09-22; retain the incumbent `lambda.min` rule.

## Question

Does making elastic-net lambda selection more stable improve honest
out-of-sample ATS margin predictions? This experiment changed only the lambda
selection policy. Features, alpha (`0.25`), preprocessing, outer walk-forward
folds, calibration, and scoring stayed fixed.

The evaluated feature panel had SHA-256
`34ce50458ce81d8f800aff9cf957e3b45368964713bcc46e943f152e4c64618d`.

## Arms

1. incumbent: `lambda.min` from one deterministic five-fold CV;
2. conservative: `lambda.1se` from the same CV;
3. repeated: median `lambda.min` across five deterministic CV partitions;
4. temporal: lambda minimizing MAE on four rolling, past-only inner seasons;
5. fixed-prior: median lambda selected by the five most recent prior outer
   folds, with no current-holdout information.

The temporal and fixed-prior implementations rebuild preprocessing inside each
historical fold. They do not use later seasons to impute, normalize, choose
columns, or select lambda.

## Progressive results

The repeated-CV arm produced exactly the same 2025 predictions as the
incumbent and stopped at probe. `lambda.1se` failed the four-season screen.

| Screen rule | MAE delta | Brier delta | Log-loss delta | ATS delta | Outcome |
|---|---:|---:|---:|---:|---|
| `lambda.1se` | +0.0818 | +0.00131 | +0.00272 | -0.87pp | stop |
| temporal | -0.0057 | +0.00028 | +0.00066 | +0.00pp | confirm |
| fixed-prior | +0.0149 | +0.00039 | +0.00091 | -0.97pp | confirm per predeclared gate |

Both past-only arms met the literal screen gate: MAE improved in three of four
screen seasons, coefficient-set instability fell, and aggregate Brier
degradation stayed below `0.0005`. Confirmation was therefore run even though
the fixed-prior aggregate MAE was already unfavorable.

## Ten-season confirmation

| Rule | MAE | Brier | Log loss | ATS accuracy | MAE seasons won |
|---|---:|---:|---:|---:|---:|
| incumbent | 9.7678 | 0.20962 | 0.60690 | 53.94% | — |
| temporal | 9.7753 | 0.20984 | 0.60735 | 53.01% | 7 of 10 |
| fixed-prior | 9.7734 | 0.20973 | 0.60718 | 53.40% | 7 of 10 |

Paired candidate-minus-incumbent deltas:

| Rule | MAE delta | 90% season-block interval | Brier delta | Log-loss delta | ATS delta |
|---|---:|---:|---:|---:|---:|
| temporal | +0.0075 | [-0.0054, +0.0238] | +0.00022 | +0.00046 | -0.93pp |
| fixed-prior | +0.0056 | [-0.0052, +0.0207] | +0.00011 | +0.00028 | -0.54pp |

Neither candidate improved any aggregate confirmation metric. The probability
of a real MAE improvement in the season-block bootstrap was only 21.6% for the
temporal rule and 28.0% for fixed-prior.

## Mechanism diagnostic

The stability hypothesis was partly right, but the performance hypothesis was
wrong.

| Rule | Mean pairwise coefficient-set Jaccard distance | SD of nonzero count |
|---|---:|---:|
| incumbent | 0.389 | 2.23 |
| temporal | 0.376 | 2.12 |
| fixed-prior | 0.373 | 1.55 |

Lower is more stable. Both alternatives made the chosen coefficient set more
consistent and won more seasons than they lost. Their few losing seasons were
larger, however: temporal selection was notably worse in 2021 and 2024, while
fixed-prior suffered a large 2016 regression. This is why season win counts
cannot replace weighted paired performance.

## Decision

Keep `lambda.min` in production. The experiment controls and diagnostics remain
available through `GLM_LAMBDA_RULE` for future research, but none of these rules
should be enabled in the production environment.

Research artifacts live under
`data/ats_experiments/lambda-stability/{probe,screen,confirm}/`. Each completed
run includes predictions, per-fold lambda and coefficient diagnostics,
coefficient selection rates, and paired diagnostic charts. These generated
artifacts are intentionally ignored by Git.
