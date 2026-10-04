# R1.4 — Cross-fitted model-trust gate

Research completed 2026-09-23. **Do not promote.** The learned error-based
gate is a promising selection policy, not a demonstrated improvement yet.
Production forecasts and betting rules are unchanged.

## Design

The [predeclared protocol](ATS_R1_4_PROTOCOL.md) freezes the GLM-only,
lambda.min incumbent. Earlier outer-fold predictions from 2012–2015 warm up
the gate; each 2016–2025 evaluation uses only earlier seasons' outer-fold
predictions and outcomes. Each year refits a ridge logistic gate with fixed
lambda 0.1. Imputation and scaling use training rows only.

Two targets were tested: whether the model beats the market's absolute margin
error, and whether its proposed ATS side wins. Features are model/market
disagreement, spread tiers, total, season/week, rest difference, division, and
the fold's historically calibrated global sigma. No current-fold outcomes enter
selection. Abstention uses the market margin for forecasts and makes no bet.

Simple controls use 1-point or 2.5-point disagreement, a historical uncertainty
ceiling, and the combination of disagreement and uncertainty. These thresholds
were not retuned after viewing results.

## Screen and confirmation

On the four-season screen (2016, 2019, 2022, 2025), only the error-based learned
gate and the 1-point disagreement rule qualified for ten-season confirmation.
The cover-target gate failed consistency. The 2.5-point rule had only 14 graded
bets in its sparsest season; uncertainty policies selected no games in one
season and failed the minimum count. Global sigma varies by year, not by game,
so an uncertainty ceiling can exclude a whole season.

| Policy, 2016–2025 | Graded bets | ATS accuracy | Recorded-odds ROI | Fixed -110 ROI | Full-slate MAE change |
|---|---:|---:|---:|---:|---:|
| Incumbent, all games | 2,573 | 53.94% | 4.04% | 2.91% | — |
| Learned error gate | 1,714 | 54.67% | 5.39% | 4.24% | +0.0122 points |
| Disagreement ≥1 point | 1,063 | 54.66% | 5.11% | 4.26% | +0.0051 points |

Positive MAE change is worse. ROI is profit divided by flat one-unit stakes,
including refunded pushes in the denominator. It is not the deployed portfolio's
return. The gate placed 1,765 bets including 51 pushes, selecting 66.9% of games;
the baseline placed 2,638 including 65 pushes. Total simulated profit fell from
106.68 to 95.11 units while less capital was staked.

The learned gate improved ROI against the all-game incumbent in **8/10 seasons**,
had at least 120 graded bets every season, and no season accounted for more than
23.5% of gross positive season profit. But its +1.35-percentage-point ROI gain
had a **90% paired season-block interval of −0.11 to +2.76 percentage points**.
Its full-slate MAE change interval was −0.0216 to +0.0493 points.

The simple disagreement rule improved ROI in only 4/10 seasons. Its ROI-gain
interval was −2.51 to +5.10 percentage points; its MAE-change interval was
−0.0068 to +0.0173 points. Neither passed confirmation.

The learned gate's selected predictions beat the market's MAE in 7/10 seasons,
but that does not establish a better gate: the incumbent also contains useful
predictions among the discarded games. Full-slate comparison catches that loss.
Predicting the *chance* of beating the market also ignores the *size* of a win
or loss in absolute error.

## Interpretation and next step

Keep the incumbent. Preserve this fixed learned gate as a candidate for
prospective shadow evaluation rather than searching these same ten seasons for
a more flattering threshold. A separately predeclared experiment could predict
the magnitude of model-versus-market loss difference instead of a binary win,
to align the target with the decision to defer.

These ten years include the screen years and have been reused in other research;
confirmation is broader historical evidence, not an untouched final test.
Bootstrap intervals are not adjusted for the full research search. The earlier
OOF restriction prevents direct gate leakage, but existing upstream tuning and
market timestamp provenance still require review before any promotion. Archived
odds are not proof those prices were executable at the decision time.

## Reproduction and checks

`ats_trust_gate.R` reads frozen fold artifacts; it does not retrain the game
model for each gate. The baseline refresh had 4 cache hits and 10 misses to
populate all 14 years with the calibration metadata required by this experiment.
All policy arms then reused those same forecasts.

```sh
ATS_PROFILE=confirm ATS_HOLDOUT_SEASONS=2012-2025 ATS_WORKERS=2 STACK_MODE=glm_only GLM_LAMBDA_RULE=min ATS_OUTPUT_DIR=data/ats_experiments/trust-gate/base Rscript walk_forward_eval.R
Rscript ats_trust_gate.R --run data/ats_experiments/trust-gate/base --out data/ats_experiments/trust-gate/probe --profile probe
Rscript ats_trust_gate.R --run data/ats_experiments/trust-gate/base --out data/ats_experiments/trust-gate/screen --profile screen
Rscript ats_trust_gate.R --run data/ats_experiments/trust-gate/base --out data/ats_experiments/trust-gate/confirm --profile confirm --screen data/ats_experiments/trust-gate/screen
Rscript tests/test_ats_trust_gate.R
```

Tests verify that changing current/future outcomes or future features cannot
change an earlier gate's selections, that all-game margins remain identical,
and that pushes and abstention are scored correctly.

Local artifacts under `data/ats_experiments/trust-gate/` include per-game
selections, aggregate/season/regime metrics, coefficient histories, training-year
audits, bootstrap intervals, decisions, source fingerprints, and a seasonal
ROI/sample-size chart. Large generated research artifacts are ignored by Git;
the protocol, runner, tests, and this report preserve the reproducible record.
