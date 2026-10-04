# R1.2 — Training windows and observation weights

Completed 2026-09-25. **All six candidates stopped after screen. Keep the
expanding, equal-weight incumbent.** No production configuration was changed.

The [predeclared protocol](ATS_R1_2_PROTOCOL.md) specifies thresholds and arms.
Screen covered 2016, 2019, 2022, 2023, 2024, and 2025 (1,599 paired games).
This deliberately includes all four recent years, not just 2024. Pre-calibration
and final GLMs each apply their own trailing window or season-age weights;
calibration remains on the two historical years before each outer holdout.
The fixed hybrid uses a four-season half-life with minimum weight 0.25.
GLM fitting and cross-validation loss use the weights; recipe preprocessing is
unchanged/unweighted. Regularization remains lambda.min with existing alpha.

## Results

MAE deltas are candidate minus incumbent; **negative is better**. Recent and
early figures average season-level deltas; overall weights each game equally.

| Candidate | Overall MAE delta | 2022–25 MAE delta | 2016/19 MAE delta | Recent years improved | Brier delta |
|---|---:|---:|---:|---:|---:|
| Last 8 seasons | +0.00538 | −0.00144 | +0.01989 | 3/4 | +0.000858 |
| Last 12 seasons | +0.00670 | +0.00983 | 0.00000 | 1/4 | +0.000085 |
| Half-life 4 | +0.00235 | −0.00141 | +0.01033 | 3/4 | +0.000493 |
| Half-life 8 | +0.00443 | +0.00450 | +0.00430 | 1/4 | +0.000219 |
| Half-life 12 | +0.00496 | +0.00566 | +0.00347 | 2/4 | +0.000189 |
| Half-life 4, floor 0.25 | +0.00403 | +0.00147 | +0.00945 | 3/4 | +0.000306 |

All candidates worsened aggregate MAE, Brier, and log loss. Neither of the two
tiny recent improvements reached the predeclared 0.01-point threshold. Minimum
effective training samples ranged from 1,344 to 1,451, above the 750 floor;
all passed the log-lambda stability check. Thus sample collapse or unstable
lambda was not the reason for rejection: the predictive benefit was absent.
The 12-season window equals expanding in the early screen years because fewer
than 12 seasons of eligible history exist then.

No candidate advanced, so no full confirmation was run and no claim about
every 2016–2021 season is warranted. This is evidence against these fixed arms,
not proof that every possible recency-weighting design fails. Do not retune
half-lives on these results. Next priority is prospective provenance/ledger
work, then new decision-time information rather than another historical
weighting sweep.

## Verification and artifacts

- Refreshed incumbent margins are exactly equal to prior R1.4 cached baseline
  predictions on all paired screen games; outcomes also match exactly.
- Regression tests pass for existing lambda rules, uniform-weight equivalence,
  and rejection of malformed weights.
- Training arm is part of fold-cache identity; production defaults remain
  expanding/equal-weight. Nondefault arms require GLM-only and weighted fits
  require lambda.min, preventing silently unsupported combinations.
- Source, input-panel, tuning, and per-fold fingerprints are saved by the
  evaluator. Existing upstream tuning and market-timestamp provenance caveats
  still apply. These reused years are not an untouched prospective test.

Generated artifacts: `data/ats_experiments/training-window/`. Each arm contains
forecast and calibration files plus lambda/coefficient diagnostics. Each
`compare-*` directory contains paired metrics, bootstrap intervals, error
distributions, error CDFs, and per-season delta charts. `screen_decisions.csv`
is the R1.2-specific decision (the generic comparator has different thresholds);
`sample_audit.csv` records row counts and effective sample size by fit stage.

```sh
# Repeat for expanding, window8, window12, half4, half8, half12, hybrid:
ATS_TRAINING_ARM=half4 ATS_PROFILE=screen ATS_HOLDOUT_SEASONS=2016,2019,2022-2025 ATS_WORKERS=2 STACK_MODE=glm_only ATS_OUTPUT_DIR=data/ats_experiments/training-window/half4 Rscript walk_forward_eval.R
Rscript ats_compare.R --incumbent data/ats_experiments/training-window/expanding --candidate data/ats_experiments/training-window/half4 --out data/ats_experiments/training-window/compare-half4 --bootstrap 2000
# After all six comparisons:
Rscript ats_window_review.R
Rscript tests/test_glm_lambda_rules.R
```
