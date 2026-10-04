# ATS experiment process

This is the default path for spread/margin model changes. It borrows the useful
parts of the weekly fantasy research engine—successive halving, exact fold
caches, bounded parallelism, layered evaluation, and paired diagnostics—without
weakening the final evidence standard.

## Current honest baseline

Frozen walk-forward evaluation, 2016–2025:

| Metric | Model | Market | Difference |
|---|---:|---:|---:|
| Margin MAE | 9.7678 | 9.7868 | -0.0190 points |
| Brier | 0.20962 | 0.21118 | -0.00156 |
| Log loss | 0.60690 | 0.60983 | -0.00294 |
| ATS accuracy | 53.94% | 52.38% break-even at -110 | +1.56pp |

The signal is promising, not overwhelming. ATS is 1,388–1,185 excluding 65
pushes (2,573 graded decisions), with a 95% binomial interval of 52.00%–55.88%.
It is clearly above a 50% coin flip (one-sided p=0.000034), but not yet clearly
above the 52.38% -110 break-even rate (one-sided p=0.058). The theoretical
flat-bet return at exactly -110 is +2.99% per dollar; the saved historical
flat-spread simulation, using its recorded odds, returned +4.0%.

The model beats the market on margin MAE in 6 of 10 seasons, Brier in 7 of 10,
and clears -110 break-even ATS accuracy in 8 of 10. A season-block bootstrap
still includes zero for the paired MAE edge (-0.0190, 95% CI -0.0744 to
+0.0324); the Brier edge is narrowly below zero (-0.00156, 95% CI -0.00309 to
-0.00003). Treat the model as a small, plausible edge with meaningful regime
risk—not as a solved market.

These numbers end in 2025. The live bundle is trained through 2025. Its first
2026 slate was weak ATS—40% on 15 graded decisions (one push), with 11.53 margin
MAE—but Brier remained 0.2114. One 16-game week is not enough evidence to revise
the historical strength assessment; it is an early warning to monitor, not a
reason to retune on the same outcomes.

## Progressive profiles

| Profile | Holdouts | Purpose | Promotion authority |
|---|---|---|---|
| `probe` | 2025 | catch broken or clearly weak ideas in seconds | no |
| `screen` | 2016, 2019, 2022, 2025 | span distinct regimes at ~40% of full cost | no |
| `confirm` | 2016–2025 | final honest walk-forward evidence | yes |

Run them with:

```bash
./run_pipeline.sh ats-probe
./run_pipeline.sh ats-screen
./run_pipeline.sh ats-confirm
```

`ATS_HOLDOUT_SEASONS=2018,2021-2023` overrides the fold list for diagnosis, but
an overridden run never has promotion authority. `ATS_WORKERS` controls bounded
season-level parallelism (default one for probe and two for screen/confirm).

## What is cached

Each outer season writes an atomic fold artifact containing the full raw
prediction frame and provenance. Its training key includes:

- the content hash of `game_w_features.csv`;
- learner source and tuned-parameter hashes;
- architecture, calibration method, feature-pruning controls, seed, training
  start, calibration window, GLM lambda-selection controls, and holdout season.

The default cache is `data/ats_experiment_cache/folds/` and is ignored by Git.
Use `ATS_CACHE_MODE=refresh` after intentionally changing training semantics;
`off` is available for a clean audit. A cache hit rescans metrics and policies
without refitting.

Evaluation and reporting code is deliberately outside the training fingerprint.
Changing a chart, decision threshold, or betting rule should not retrain a GLM.
Changing the panel, feature routing, learner, or calibration correctly creates a
new fold key.

Fold artifacts also retain the two past calibration seasons' raw residuals and
pregame scale context. `scale_calibration_rows.csv` and
`scale_holdout_rows.csv` expose those rows for calibration-only experiments.
They let residual-distribution rules be compared while holding the fitted
margin prediction exactly fixed.

## The production fast path

Production is a pure, unamplified GLM (`stack = 0 + 1·GLM`). The old evaluator
still trained XGBoost and random forest twice in every fold and then multiplied
both predictions by zero. The new runner trains only active learners.

Measured on the 2025 probe on 2026-09-20:

| Runner | Fold fit time | Prediction equality |
|---|---:|---|
| legacy three-learner path | 22.8s | reference |
| active GLM-only path | 3.6s | bit-for-bit identical |

That is a 6.3× fold-fit speedup before parallelism or warm-cache reuse. Set
`ATS_FORCE_UNUSED_LEARNERS=1` only to repeat the equivalence audit.

The refreshed 10-season confirmation completed with two fold workers; rerunning
the same confirmation from exact fold caches took 5.4 seconds end to end,
including package startup, metric aggregation, betting rescoring, and output.

## Cheap comparison and diagnostics

Once incumbent and candidate runs exist, compare them without fitting again:

```bash
Rscript ats_compare.R \
  --incumbent path/to/incumbent-run \
  --candidate path/to/candidate-run \
  --out path/to/comparison
```

The comparison fails closed if game populations, outcomes, or market lines do
not match. It writes overall and per-season paired deltas, block-bootstrap
uncertainty, a machine-readable decision, signed-error distributions, absolute-
error CDFs, and season-level gain charts.

Every GLM run also writes selected lambdas, nonzero coefficient sets, selection
rates, and a fold-to-fold Jaccard-instability summary. These are mechanism
diagnostics: greater coefficient stability is useful only when paired holdout
predictions also improve.

Bet thresholds, Kelly sizing, EV calibration, and reporting belong after this
step. Rescore `walk_forward_predictions.csv` with `strategy_sweep.R` or the
portfolio tools; do not retrain the game model for a policy-only change.

## Promotion standard

Probe and screen can stop a candidate for futility or advance it, but can never
change production. A candidate reaches production only after full confirmation
and should satisfy all of the following:

1. paired MAE improves by at least 0.01 points and its season-block bootstrap
   interval supports a real gain;
2. Brier and log loss do not regress;
3. MAE improves in at least 6 of 10 seasons, without a concentrated failure in
   recent seasons;
4. ATS accuracy and selected-bet economics do not deteriorate materially;
5. the result is leak-free, reproducible from cached artifacts, and documented
   in the baseline or ruled-out experiment log.

The thresholds are declared before reading candidate results. A strong screen
is a reason to spend confirmation compute, not evidence to lower the bar.

The prioritized research portfolio and experiment backlog live in
`docs/ATS_RESEARCH_ROADMAP.md`.

## Cached trust-policy experiments

`ats_trust_gate.R` evaluates R1.4 without retraining the margin model per policy.
Populate earlier outer-fold forecasts for gate warm-up, then fit each evaluation
year's gate on strictly earlier OOF seasons. Never use the current evaluation
fold to train, normalize, or choose thresholds. Confirmation requires the
screen decision artifact and evaluates only advancing policies. The protocol
and reproduction commands are in `docs/ATS_R1_4_PROTOCOL.md` and
`docs/ATS_R1_4_MODEL_TRUST_GATE.md`. Selection-policy returns are not production
portfolio returns; report exposure, pushes, price sensitivity, complete-slate
error, and paired uncertainty alongside selected-subset gains.
