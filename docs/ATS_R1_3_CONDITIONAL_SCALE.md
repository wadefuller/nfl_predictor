# ATS R1.3 — conditional residual scale

**Status:** completed 2026-09-23; retain the global normal sigma for cover
probabilities.

## Question

Does modeling matchup-specific residual width improve the conversion from the
frozen margin forecast to a probability of covering the spread?

This was a calibration-only experiment. The margin model was trained once per
outer fold, its predictions were frozen, and all five scale rules were scored
on the same games. A hard assertion verified that every arm retained exactly
one identical margin per `game_id`.

## Arms

1. global normal sigma, estimated from the two prior calibration seasons;
2. spread/total buckets, with 75-game shrinkage toward global variance;
3. a Gamma log-linear variance GLM using spread, total, QB uncertainty, rest,
   weather, dome, and late-season context;
4. a Student-t residual distribution with maximum-likelihood scale and degrees
   of freedom;
5. a shrinkage conformal residual distribution by spread and QB regime.

Pushes were excluded from probability scores. The four-season screen contains
1,029 graded cover outcomes from 2016, 2019, 2022, and 2025.

## Four-season screen

| Rule | Brier | Delta | Log loss | Delta | ECE | Calibration slope | Brier seasons won |
|---|---:|---:|---:|---:|---:|---:|---:|
| global normal | 0.249199 | — | 0.691560 | — | 0.02347 | 0.792 | — |
| bucket shrinkage | 0.249274 | +0.000075 | 0.691713 | +0.000153 | 0.02430 | 0.754 | 1 of 4 |
| Student-t | 0.249369 | +0.000170 | 0.691914 | +0.000355 | 0.02563 | 0.692 | 1 of 4 |
| variance GLM | 0.249436 | +0.000237 | 0.692047 | +0.000488 | 0.02395 | 0.698 | 1 of 4 |
| conformal regime | 0.250306 | +0.001107 | 0.693825 | +0.002265 | 0.03456 | 0.400 | 1 of 4 |

Lower is better for Brier, log loss, and ECE; calibration slope is ideally one.
Every conditional rule regressed both proper probability scores and moved the
aggregate slope farther from one. Each candidate's only Brier win was 2016.
The variance GLM regression was the clearest statistically: its 90% week-block
bootstrap interval for Brier delta was `[+0.000046, +0.000435]`.

## Interval diagnostic

The Student-t arm did produce narrower, more accurate central intervals:

| Rule | 50% coverage / width | 80% coverage / width | 90% coverage / width |
|---|---:|---:|---:|
| global normal | 58.4% / 18.7 | 85.5% / 35.4 | 92.8% / 45.5 |
| Student-t | 53.6% / 16.9 | 82.5% / 32.8 | 91.6% / 43.0 |

That is useful diagnostic evidence that the global normal intervals are mildly
too wide. It is not enough to ship Student-t for betting probabilities: its
Brier, log loss, ECE, and calibration slope all worsened. Learned degrees of
freedom also varied materially across screen folds (`8.6` to `34.0`), so the
tail estimate is not especially stable.

## Interpretation

Conditional variance exists in the calibration residuals, but estimating it
from two seasons does not improve the decision probability. The cover target is
close to irreducible at this resolution: even the incumbent Brier is near
`0.25`, and more responsive scale rules make already-overconfident probability
bands more extreme. The dominant opportunity is therefore more likely in the
conditional mean or in an explicit model-trust gate, not in a more elaborate
residual width model.

## Decision

No arm advances to ten-season confirmation. Keep the global normal sigma in
production. Preserve Student-t as an interval-display research lead only; do
not use it for cover probabilities without new evidence.

Generated artifacts are under `data/ats_experiments/conditional-scale/` and
include frozen calibration/holdout rows, per-fold scale diagnostics,
reliability tables, interval coverage, paired bootstrap results, and charts.
The reusable evaluator is `research/ats_scale_experiment.R`.
