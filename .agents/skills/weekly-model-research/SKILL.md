---
name: weekly-model-research
description: Run rigorous, reproducible research on the weekly fantasy distribution model. Use for feature ideas, ablations, architecture comparisons, hyperparameter searches, calibration changes, model promotion decisions, and analysis of weekly projection quality. Enforces pre-kickoff feature availability, temporal outer folds, seed robustness, paired uncertainty, and a durable experiment ledger.
---

# Weekly Model Research

Use this workflow for changes under `/Users/wadefuller/Projects/R/weekly_fantasy`.

## Start with a research contract

Before changing training code, write down:

- one falsifiable hypothesis;
- the incumbent and the single primary metric;
- the feature availability timestamp;
- the experiment stage and outer evaluation seasons;
- the dimensions allowed to vary;
- the promotion threshold.

Read [research-protocol.md](references/research-protocol.md) before selecting folds or interpreting results.

## Preserve temporal integrity

Treat a player-week feature as legal only if its value could have been produced from a timestamped snapshot available before that game's kickoff. A prior-season export is not automatically leak-safe: inspect how it was fitted and whether it used later seasons.

Use expanding-season outer folds. Any hyperparameter selection, recalibration, imputation, early stopping, feature selection, or ensemble weighting must occur using rows strictly earlier than the outer test season. Never use a future-tuned parameter file to score an earlier outer fold.

## Run in stages

1. `smoke`: one recent season and one seed; verifies mechanics only.
2. `screen`: broad, cheap search on discovery folds. Reject obvious losers.
3. `confirm`: narrowed candidates, at least three seeds, all eligible outer folds, paired season-week uncertainty.
4. `prospective`: freeze the winner and collect live results without retuning it.

Do not call a smoke or screen result an improvement.

## Change one research axis at a time

For feature work, compare both marginal addition to the incumbent and leave-one-group-out removal. For architecture work, hold the feature set and evaluation contract constant. For tuning work, hold the architecture and feature set constant.

Use `/Users/wadefuller/Projects/R/weekly_fantasy/run_research.R` and its manifest rather than one-off scripts when the adapter supports the trial. Add a new adapter to `R/research.R` when introducing a genuinely new architecture.

For a multi-state hurdle, define every state and threshold before the run. Same-game snaps, routes, touches, and participation are legal as historical labels only; never supply the realized outer-test state to prediction. Predict `P(state | pre-kickoff features)` and marginalize the conditional outcome forecasts across states. Select thresholds and tune state models inside each outer fold. Record class balance, missing-label behavior, multiclass Brier/log loss and calibration, minimum state counts, rare-state fallback, and Week-1 behavior. A new adapter is not promotion-eligible until it emits the standard prediction schema and has leakage tests.

## Judge the whole forecast

Use mean pinball/CRPS as the primary distribution score. Also inspect:

- MAE and bias of the mean projection;
- 50% and 90% interval coverage;
- active-probability Brier score and log loss;
- weekly within-position rank correlation and top-N hit rate;
- performance by position, season, early/late season, and startable tier;
- seed spread and paired season-week bootstrap intervals.

Prefer the simpler candidate when the paired interval includes zero or the gain is isolated to one season or position.

## Promotion gate

Define paired improvement as `incumbent pinball - candidate pinball`, so positive values favor the candidate. Promote only when the candidate:

- improves the predeclared primary metric on confirmation folds;
- has a paired 95% interval whose harmful tail is within the tolerance in the protocol;
- does not materially degrade any position, calibration, active classification, or startable tier;
- repeats across seeds;
- passes the invariant suite;
- records its manifest, source/data fingerprints, predictions, and metrics.

Keep prospective results separate from research results. Never overwrite the opening-week or season-long locked snapshots during experimentation.

Do not change production after smoke or screen. When promotion is explicitly authorized, update a versioned production configuration only after confirmation, preserve the incumbent as a rollback path, run invariants, and write a decision artifact naming both immutable run hashes.
