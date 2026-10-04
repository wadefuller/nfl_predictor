# Elastic-net search after the 2026-09-27 feature repair

**Decision: retain production alpha = 0.25.** The search did not produce a
repeatable improvement. No production tuning file or model bundle was changed.

## Design

The repaired `data/game_w_features.csv` was frozen for this comparison. The
architecture remained GLM-only; `lambda.min` was selected by five-fold CV
within each past-only training window. The pre-calibration, final fit, and
probability calibration all used the candidate alpha. Four representative
outer seasons (2016, 2019, 2022, 2025) screened alphas 0.05, 0.10, 0.25,
0.50, and 0.75. The best screen challenger (0.10) and incumbent (0.25) then
ran on all 2016–2025 outer holdouts. No 2026 result was used to choose alpha.

`ATS_GLM_ALPHA` is an experimental override in `walk_forward_eval.R`. It is
validated and included in the fold-cache fingerprint, so different alpha arms
cannot silently reuse one another's fitted models. Results are isolated under
`data/ats_experiments/feature-repair-alpha/`.

## Four-season screen (1,055 games)

| Alpha | Margin MAE | Brier | Log loss | ATS cover accuracy |
|---:|---:|---:|---:|---:|
| 0.05 | 9.43085 | 0.21508 | 0.61783 | 51.21% |
| 0.10 | **9.42472** | **0.21505** | **0.61773** | **51.80%** |
| 0.25 incumbent | 9.43412 | 0.21521 | 0.61813 | 50.63% |
| 0.50 | 9.43754 | 0.21526 | 0.61823 | 50.83% |
| 0.75 | 9.43807 | 0.21521 | 0.61812 | 50.34% |

The screen advantage for alpha 0.10 was small enough to require confirmation.

## Ten-season confirmation (2,638 games)

| Alpha | Margin MAE | Brier | Log loss | ATS cover accuracy |
|---:|---:|---:|---:|---:|
| 0.10 challenger | 9.79031 | 0.211000 | 0.610312 | 52.27% |
| 0.25 incumbent | 9.79047 | 0.211007 | **0.610283** | 51.57% |

The challenger-minus-incumbent difference was **-0.00016 MAE** and
**-0.000006 Brier**—operationally zero. Its log loss was slightly worse.
It improved MAE in only 4 of 10 seasons and Brier in only 3 of 10. Across
2022–2025 it was worse by 0.0041 MAE and 0.000087 Brier. A 10,000-draw
season-block bootstrap gave a 90% interval of approximately
[-0.0094, +0.0094] for the paired MAE difference.

Alpha 0.10 selected an average of 26 nonzero GLM coefficients per outer fold,
versus 19.05 for alpha 0.25. The additional complexity did not produce a
credible gain. The apparent ATS accuracy improvement is not enough to select
a margin-model hyperparameter when the primary margin and probability metrics
are tied or mixed; the betting rule also needs independent prospective proof.

## Reproduction

```bash
ATS_PROFILE=screen ATS_GLM_ALPHA=0.10 ATS_OUTPUT_DIR=data/ats_experiments/feature-repair-alpha/screen-010 Rscript walk_forward_eval.R
ATS_PROFILE=confirm ATS_GLM_ALPHA=0.10 ATS_OUTPUT_DIR=data/ats_experiments/feature-repair-alpha/confirm-010 Rscript walk_forward_eval.R
ATS_PROFILE=confirm ATS_GLM_ALPHA=0.25 ATS_OUTPUT_DIR=data/ats_experiments/feature-repair-alpha/confirm-025 Rscript walk_forward_eval.R
```

This result supersedes any alpha preference inferred from the pre-repair
feature matrix. Further parameter search should have a new hypothesis or new
decision-time data, rather than widening this grid around a flat result.
