# Weekly model research protocol

## Evaluation topology

The historical record has already been inspected repeatedly, so no historical season is a pristine lockbox. Use nested time-aware evaluation to limit further selection bias, and reserve 2026 as a prospective record.

| Stage | Purpose | Default outer seasons | Seeds | Interpretation |
|---|---|---:|---:|---|
| smoke | validate code and artifacts | 2025 | 1 | no model claim |
| screen | eliminate weak ideas cheaply | 2019-2022 | 1 | directional only |
| confirm | promotion-quality comparison | 2017-2025 | 3+ | paired, uncertainty-aware |
| prospective | observe a frozen choice | 2026 onward | frozen | never feed back into that version |

Within every outer fold for season `S`, fit preprocessing and models on seasons `< S`. If tuning is needed, tune on an inner expanding split that also ends before `S`. Parameter files selected using 2022-2024 are not eligible for outer folds through 2024.

## Primary metric and tolerances

Primary: mean pinball loss across the standardized 0.05-0.95 grid (equivalently one-half of the approximated CRPS). Pair candidates and incumbent on identical player-week predictions, then bootstrap season-week blocks.

Default promotion tolerances:

- confirmation pinball improvement greater than `0.0020` (roughly CRPS `0.0040`);
- paired 95% interval excludes zero in the candidate's favor by default;
- no position worsens by more than `0.0050` pinball;
- 90% coverage remains within `[0.87, 0.93]` and 50% coverage within `[0.47, 0.58]` unless a predeclared calibration objective supersedes this;
- active Brier score cannot worsen by more than `0.0020`;
- startable-tier weekly Spearman cannot fall by more than `0.005`.

These are defaults, not facts of nature. Change them before running a trial, never after seeing its result.
An explicitly predeclared non-inferiority study may instead allow a lower interval bound of `-0.0005`, but the mean gain must still exceed `0.0020`; do not choose this allowance after inspecting results.

## Feature availability contract

For each new feature, record:

- source system and raw fields;
- event time and ingestion time;
- earliest reliable season;
- join keys and expected coverage;
- missingness behavior at prediction time;
- whether a historical value is a true snapshot or a later reconstruction;
- whether the feature can be recomputed using only prior rows.

Reject features derived from final weekly totals, end-of-season roster labels, hindsight injury designations, closing lines unavailable at the intended prediction time, or season projections fitted with the test season's outcomes.

## Experiment design

- Compare a feature as `incumbent + candidate` and `incumbent - family` where applicable.
- Use a small factorial design only for plausible interactions; do not promote from a giant undisciplined grid.
- Search architecture and feature space separately first, then test interactions among finalists.
- Keep seed, folds, simulation draws, feature set, and preprocessing identical in paired architecture comparisons.
- Use successive halving: smoke, screen, then full confirmation for the top few candidates.
- Correct expectations for multiple comparisons. A winner among dozens of tiny gains needs a fresh confirmation run or prospective evidence.

## Required artifacts

Every durable run should retain:

- normalized experiment specification and unique hash;
- code and data fingerprints;
- per-player-week predictions;
- fold, position, and aggregate metrics;
- runtime and failure state;
- paired comparison against the named incumbent;
- a short decision note: promote, retain for more evidence, or reject.

Never identify a run only by a friendly name; names can be reused, hashes cannot.
