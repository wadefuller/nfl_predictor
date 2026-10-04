# R1.2 protocol — declared 2026-09-25 before results

Freeze inputs, features, GLM alpha and lambda.min selection, calibration method,
and evaluation games. Compare expanding equal-weight, trailing 8/12 seasons,
exponential half-lives 4/8/12 seasons, and half-life 4 with weight floor 0.25.
Age is measured from the latest training season separately for pre-calibration
and final fits. Calibration retains the usual two earlier seasons, unweighted.
Weights enter GLM fitting and CV loss; recipe preprocessing remains unweighted.

Screen: 2016, 2019, and all 2022–2025 years. Advance only with recent mean paired
MAE improvement >=0.01, at least 3/4 recent years improving, early mean MAE
regression <=0.05, no year worse by >0.20, and nonpositive aggregate Brier and
log-loss deltas. Require improvement when 2024 is omitted, effective training
sample >=750 in every stage, and log-lambda standard deviation no more than
twice the incumbent (floor 0.1). No tuning of these arms or cutoffs after results.

Confirm survivors on 2016–2025 with the usual promotion standard; screen never
authorizes production. Existing upstream tuning/market provenance caveats apply.
