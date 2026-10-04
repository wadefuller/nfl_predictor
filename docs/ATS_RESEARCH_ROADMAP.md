# ATS model research roadmap

Last updated: 2026-09-28

> Feature-integrity update (2026-09-27): baseline metrics and experiment
> deltas below predate the away-team perspective and postgame-QB repairs.
> Treat them as historical research records, not promotion evidence. The
> corrected 2016–2025 walk-forward is MAE 9.79047 (market 9.78677) and
> Brier 0.21101 (market 0.21118). Compare new ATS candidates against the
> rebuilt matrix and recalibrated bundle.

## Mission

Build an ATS system that adds a small but repeatable edge to the market, knows
when that edge is unreliable, and can be evaluated without fitting stories to
noise. The target is not simply a lower aggregate margin MAE. The useful system
must jointly improve:

1. margin accuracy relative to the market;
2. win and cover probability calibration;
3. stability across seasons and identifiable regimes;
4. prospective betting decisions made from information actually available at
   the stated decision time.

The research process is defined in `docs/ATS_EXPERIMENT_PROCESS.md`. Probe and
screen results allocate compute; only a full 2016–2025 confirmation can support
a production change.

## Baseline and diagnosis

Production is a pure elastic-net GLM anchored by the market spread. Its honest
2016–2025 walk-forward record is:

| Metric | Model | Market / threshold | Assessment |
|---|---:|---:|---|
| Margin MAE | 9.7737 | 9.7868 | 0.013-point edge; statistically unresolved |
| Brier | 0.20986 | 0.21118 | better in 7 of 10 seasons |
| Log loss | 0.60749 | 0.60983 | small calibration edge |
| ATS | 53.60% | 52.38% at -110 | plausible +2.3% theoretical ROI |

The core conclusion is **small real-looking signal, large uncertainty**. The
model is not failing because it needs a larger generic feature matrix. Multiple
additive feature families, nonlinear learners, learned stacking, carryover
decay, and post-hoc margin calibration have already failed. The two largest
wins came from simplification: removing the destructive stack and pruning dead
columns.

Current failure slices, expressed as model MAE minus market MAE:

| Slice | Games | Excess MAE | Maximum aggregate lift if fully fixed* |
|---|---:|---:|---:|
| Non-divisional games | 1,679 | +0.029 | 0.018 points |
| Favorites of 5–7.5 | 666 | +0.056 | 0.014 points |
| Weeks 10–14 | 732 | +0.046 | 0.013 points |
| Home team on short rest | 168 | +0.154 | 0.010 points |
| Thursday games | 178 | +0.122 | 0.008 points |
| Favorites of 10+ | 267 | +0.079 | 0.008 points |

\*The slices overlap and are discovered post hoc. These values are upper bounds,
not expected gains. They show why an experiment must either improve a broad
population or produce a trustworthy selective-betting rule.

The first 2026 slate was 40% ATS on 15 graded decisions with 11.53 MAE. Record
it prospectively, but do not tune to one week.

## Research principles

1. **Improve the learning problem before adding columns.** Regularization and
   era stability are the most credible unresolved mechanisms.
2. **Separate mean prediction from uncertainty.** A matchup can have the same
   expected margin and a very different probability of covering.
3. **Treat market time as part of the target.** A model using a closing line is
   a kickoff model. An early-week model requires an early-week line snapshot.
4. **Prefer cross-fitted selection to post-hoc buckets.** A slice is a hypothesis
   generator, not permission to bet that slice.
5. **Optimize prediction first, policy second.** Kelly, thresholds, and EV
   calibration rescore frozen predictions and never justify a weaker model.
6. **Demand broad confirmation.** Averages carried by one or two seasons do not
   promote, even when the aggregate metric clears a threshold.

## Prioritized portfolio

| Priority | Project | Why now | First stage | Promotion target |
|---:|---|---|---|---|
| 0 | Prospective data and line-timestamp contract | Historical validity depends on what was knowable when | audit + ledger | zero timing ambiguity; immutable weekly snapshots |
| 1 | Regularization stability | Current lambda selection is noisy; pruning helped for an unexplained stability reason | probe/screen | ≥0.01 MAE gain, no probability regression, 6/10 seasons |
| 2 | Training-era weighting | Equal weight from 2008 may dilute current rule/market regimes | screen | recent lift without early-fold collapse |
| 3 | Conditional uncertainty / cover calibration | Global residual sigma ignores matchup uncertainty | post-process screen | Brier/log loss and calibration improve; ATS selection stable |
| 4 | Cross-fitted trust model | Aggregate edge is thin and concentrated unevenly | prediction-only screen | fewer bets with repeatable lift on untouched seasons |
| 5 | Decision-time market model | Closing-line residual is nearly saturated | data audit + separate target | beat the market available at the actual bet time |
| 6 | QB/availability shock family | Largest plausible new information family, especially before market convergence | data gate + probe | intended late-change cohort improves without broad harm |
| 7 | Compact schedule interactions | Thursday/short-rest weaknesses remain, but generic rest features failed | probe | targeted cohort gain and aggregate non-regression |
| 8 | Distributional architecture challenge | Useful if uncertainty work succeeds but simple variance model is insufficient | screen | beat GLM on mean and distributional scores |

## Tranche 0 — make the evidence prospective

### R0.1 Line provenance and decision-time audit — do first

**Question:** Is every historical `spread_line` and price genuinely available at
the time the model claims to make a decision?

**Work:**

- document source, book, timestamp, and whether each field is opening, snapshot,
  or closing;
- store point spread and price together rather than mixing a closing point with
  an earlier vig;
- fail closed when a historical timestamp cannot be established;
- version the prediction-time contract as `kickoff`, `24h`, `6h`, or another
  explicit horizon;
- add this contract to the fold fingerprint and run metadata.

**Exit condition:** a randomly sampled season can be reconstructed with no
look-ahead and every prediction can name its market timestamp. Do not begin an
early-line model until this is true.

### R0.2 Immutable 2026 prospective ledger

Before each kickoff, freeze model version, feature hash, line/odds, predicted
margin, cover probability, proposed side, and whether the play passed policy.
After grading, append outcomes without rewriting the prediction fields.

Report weekly and cumulative:

- MAE and paired market MAE;
- ATS record with pushes separated;
- Brier/log loss and calibration slope;
- closing-line value from the actual bet timestamp;
- results by predeclared edge tier, not newly discovered slices.

**Review cadence:** observe weekly; formally reassess after Weeks 6, 12, and 18.
No in-season retune from a single bad week.

## Tranche 1 — highest-value model experiments

### R1.1 Stabilize lambda selection — completed, retain incumbent

**Hypothesis:** the model's remaining variance comes partly from selecting
`lambda.min` on one random five-fold split. Dead-column pruning improved the
model without changing the surviving coefficient count, suggesting stability
of regularization—not more signal—may be the mechanism.

**Arms:**

1. incumbent `lambda.min`;
2. `lambda.1se`;
3. median lambda across five deterministic repeated CV partitions;
4. rolling temporal inner CV with the lambda chosen on prior seasons;
5. fixed lambda derived from prior outer folds only.

Keep alpha, features, outer folds, and calibration fixed. Record coefficient
selection frequency and prediction dispersion in addition to normal metrics.

**Advance:** candidate improves at least three of four screen seasons, reduces
fold-to-fold coefficient instability, and does not worsen Brier by 0.0005.

**Stop:** result changes coefficient count but not paired predictions, or gains
are isolated to one season.

**Result (2026-09-22):** no candidate earned promotion. All comparisons used
the same features, alpha, outer folds, calibration, and 2,638-game population.
The feature-panel SHA-256 was
`34ce50458ce81d8f800aff9cf957e3b45368964713bcc46e943f152e4c64618d`.

| Rule | Furthest stage | MAE delta | Brier delta | Log-loss delta | ATS delta | Decision |
|---|---|---:|---:|---:|---:|---|
| `lambda.1se` | screen | +0.0818 | +0.00131 | +0.00272 | -0.87pp | stop |
| five-CV median | probe | 0.0000 | 0.00000 | 0.00000 | 0.00pp | stop: identical predictions |
| temporal inner CV | confirm | +0.0075 | +0.00022 | +0.00046 | -0.93pp | do not promote |
| fixed from five prior outer folds | confirm | +0.0056 | +0.00011 | +0.00028 | -0.54pp | do not promote |

Both past-only rules looked deceptively good by season count: each lowered MAE
in 7 of 10 seasons and modestly reduced mean pairwise coefficient-set Jaccard
instability (`0.389` incumbent, `0.376` temporal, `0.373` fixed-prior). But
their losses were larger than their wins. Temporal selection was especially
hurt in 2021 and 2024; fixed-prior was badly hurt in 2016. Full paired metrics,
season tables, bootstrap intervals, coefficient diagnostics, and charts are in
`data/ats_experiments/lambda-stability/` (ignored experiment artifacts).
The durable report is
[`ATS_R1_1_LAMBDA_STABILITY.md`](ATS_R1_1_LAMBDA_STABILITY.md).

**Conclusion:** single-split `lambda.min` remains the production rule. Lambda
selection noise is measurable, but smoothing it does not improve out-of-sample
predictions. Do not revisit this family without a materially different
regularization objective or a larger change to the learner.

### R1.2 Training-window and sample-weight challenge — completed, retain incumbent

**Hypothesis:** relationships learned from 2008 may be stale under modern rules,
coaching, quarterback usage, and market efficiency.

**Arms:**

- expanding window, equal weight (incumbent);
- most recent 8 and 12 seasons;
- exponential observation half-lives of 4, 8, and 12 seasons;
- a conservative hybrid with a minimum floor weight for older games.

This is observation weighting inside the GLM, not the previously rejected
feature carryover decay. Calibration seasons remain strictly before the outer
holdout.

**Advance:** improvement in the 2022–2025 folds plus no material collapse in
2016–2021. Do not select purely on recent seasons.

**Stop:** effective sample size becomes too small, lambda becomes unstable, or
the gain disappears outside 2024.

**Result (2026-09-25):** all six candidates failed the six-season screen
(2016, 2019, 2022–2025). Every arm worsened aggregate MAE, Brier, and log loss.
The 8-season window and 4-season half-life improved recent MAE by only about
0.0014 points, below the predeclared 0.01 threshold. Effective sample size and
lambda stability passed; lack of predictive improvement was the blocker.
No confirmation or production change. See
[`ATS_R1_2_TRAINING_WINDOWS.md`](ATS_R1_2_TRAINING_WINDOWS.md).

### R1.3 Conditional residual scale — completed, retain global sigma

**Hypothesis:** one global residual sigma is too crude. Spread magnitude, total,
QB uncertainty, rest asymmetry, dome/weather, and late-season context may change
the width of the outcome distribution even when expected margin is unchanged.

Fit a simple cross-fitted scale model on absolute or squared calibration
residuals. Compare:

- global normal sigma (incumbent);
- spread/total buckets with shrinkage toward global sigma;
- log-linear variance GLM;
- Student-t residuals with learned degrees of freedom;
- conformal residual bands by predeclared regime.

The mean margin prediction must remain frozen for this project. Train once and
evaluate every scale rule from cached raw predictions.

**Primary metrics:** cover-probability Brier/log loss, ECE, calibration slope,
and reliability by probability band. ATS accuracy alone is not sufficient.

**Predeclared progression gate (2026-09-23):** stop at probe only for a clear
joint regression (`Brier > +0.001` and worse log loss). Advance from the
four-season screen only if aggregate Brier and log loss both improve, Brier
improves in at least three of four seasons, ECE does not worsen by more than
0.005, and calibration slope moves closer to one. Promotion requires aggregate
Brier/log-loss improvement, at least six of ten Brier season wins, and a
negative upper bound on the 90% season-block bootstrap interval for paired
Brier delta. Mean margin predictions must be byte-identical across arms.

**Result (2026-09-23):** no conditional-scale arm passed the four-season
screen. Versus global normal sigma, Brier deltas were `+0.000075` for
spread/total bucket shrinkage, `+0.000170` for Student-t, `+0.000237` for the
variance GLM, and `+0.001107` for regime conformal. All four also worsened log
loss, won only one of four screen seasons, and moved aggregate calibration slope
farther from one. Mean margins were identical across arms. Student-t produced
better-calibrated, narrower central prediction intervals, but its cover
probabilities were worse, so it remains an interval-only research lead rather
than a production candidate. See
[`ATS_R1_3_CONDITIONAL_SCALE.md`](ATS_R1_3_CONDITIONAL_SCALE.md).

### R1.4 Cross-fitted model-trust gate

**Hypothesis:** the model should defer to the market in regimes where its small
deviation is historically unreliable.

For each outer fold, train a gate only on earlier out-of-fold predictions. Its
target is whether the model beats the market in absolute error or whether the
signed residual supports the proposed ATS side. Start with a transparent
regularized logistic model using only pregame variables and model diagnostics:
absolute model-market disagreement, spread tier, total, season/week, rest,
division, and predicted uncertainty.

Compare it with simpler predeclared rules such as minimum disagreement and
uncertainty ceilings. Never feed current-fold outcomes into the gate.

**Success:** improved paired error or ROI on the selected subset in at least 6
of 10 seasons, adequate bet count, and no single slice accounting for most P&L.

**Failure mode to avoid:** training a classifier on the same OOS predictions it
then selects. That is policy overfit, not cross-fitting.

**Result (2026-09-23):** tested six fixed policies with 2012–2015 warm-up OOF
forecasts and past-only gate training. The error-target logistic gate and a
1-point disagreement rule reached confirmation; neither passed. The learned
gate improved historical ROI from 4.04% to 5.39% in aggregate and won 8/10
seasons, but its ROI-gain interval crossed zero (−0.11 to +2.76 percentage
points) and full-slate MAE worsened by 0.0122 points. Keep production unchanged;
retain the fixed gate as a prospective shadow-evaluation lead. See
[`ATS_R1_4_MODEL_TRUST_GATE.md`](ATS_R1_4_MODEL_TRUST_GATE.md).

### R1.5 Time-weighted prior-game WP control share — screened, not promoted

**Question:** does recent game control add predictive information beyond the
pregame market and current ATS features? For each completed prior game, measure
the fraction of regulation game-clock seconds during which the team's live
win probability was above 50%; use the trailing-four-game mean for each team
and the home-away difference. WP is held over each interval between distinct
play-clock observations, so this is weighted by game-clock time rather than
play count. Overtime is excluded because its clock is not on the same 0–3,600
second regulation scale.

**Result (2026-09-28):** the four-season representative screen (2016, 2019,
2022, 2025; 1,055 games) did not select any control-share feature in the GLM.
Candidate predictions were identical to the incumbent to machine precision,
with no change in aggregate MAE, Brier, or log loss. Play-by-play supplied
both teams' live WP for 6,764 games; 99.8% had the full regulation clock. Stop
this as a direct ATS margin-model addition. The metric remains useful as a
descriptive diagnostic for distinguishing wins despite little control from
losses after sustained control; reconsider it in a trust-gate experiment only
with a predeclared hypothesis. Implementation and research output:
[`ats_wp_control_feature.R`](../ats_wp_control_feature.R) and
`../data/ats_experiments/feature-candidates/wp-control-prior4.csv` (ignored).

## Tranche 2 — new information, not more versions of old information

### R2.1 Decision-time market panel

The prior opening-line experiment asked whether opening/line movement should be
added to a closing-line model; it failed because opening was 93% redundant with
closing. The new question is different: **can the model make a useful prediction
before the closing line exists?**

Build a multi-snapshot panel—open, 48h, 24h, 6h, and final where available—with
book consensus, dispersion, price, and movement velocity. Train a separate
model for each horizon using only data available then. Evaluate both game
outcomes and closing-line value.

Do not call this a re-test of the rejected additive line feature. It is a new
decision-time target and requires the R0.1 provenance contract first.

### R2.2 QB and availability shocks

Replace broad injury aggregates with a small point-in-time shock family:

- expected starter versus confirmed starter;
- change in starter quality relative to the prior expectation;
- backup experience and team familiarity;
- offensive-line continuity and missing starter snaps;
- skill-position opportunity removed, weighted by prior snaps/routes;
- time between status change and market snapshot.

The intended cohort is games where availability changed after the prior market
snapshot. If only closing lines are available, expect much of this information
to be absorbed by the market and deprioritize accordingly.

**Data gate:** historical status timestamps and depth charts must be complete
enough to reconstruct what was known. No inferred final inactive list may enter
an earlier snapshot.

### R2.3 Compact schedule-discontinuity interactions

Generic rest binaries and travel features failed. Test only interactions tied
to the remaining diagnostic:

- Thursday × prior overtime / prior play count;
- short-rest home team × travel/time-zone return;
- rest disadvantage × offensive-line continuity;
- Weeks 10–14 × cumulative starter continuity;
- favorite size × rest asymmetry.

Limit the family to a predeclared handful of columns. Require improvement in the
intended cohort and the aggregate; a cohort-only win is useful for a trust gate,
not necessarily for the mean model.

## Tranche 3 — architecture challenges

These are conditional on Tranches 1–2. The current data has repeatedly favored
a simple linear model, so architecture work must earn its complexity.

### R3.1 Direct cover-probability model with market offset

Test a regularized logistic model for `P(home covers)` with the market-implied
break-even probability as an offset and a small set of model-market residual
features. This differs from the rejected XGBoost win classifier: it targets ATS,
uses a linear low-capacity learner, and starts from the market rather than zero.

It must beat the conditional-scale approach on cross-fitted cover Brier/log loss
and preserve calibration. Do not optimize classification accuracy at 0.5.

### R3.2 Distributional margin model

If conditional-scale GLM leaves systematic tail errors, challenge it with:

- penalized quantile regression at q10/q25/q50/q75/q90;
- a location-scale Student-t model;
- distributional boosting constrained around the market mean.

Evaluate CRPS/pinball, interval coverage and width, mean MAE, cover calibration,
and season stability. Quantile models are not justified solely by fixing the
old close-game overshoot; GLM-only already mostly solved that problem.

### R3.3 Dynamic coefficients / partial pooling

Last, test whether a few coefficients drift by era while remaining partially
pooled toward the global GLM. Candidate varying effects: spread anchor, total,
QB differential, and late-season context. Use a small hierarchical or state-
space model—not a broad neural network.

Proceed only if R1.2 shows clear recency effects. Otherwise this project lacks
an empirical premise.

## Tranche 4 — betting policy research

Policy research starts from frozen confirmed predictions and never triggers a
model retrain.

1. Refit cover and EV calibration with strict temporal cross-fitting.
2. Evaluate thresholds as curves, not a single winning grid cell.
3. Bootstrap by season and report effective bet count, turnover, ROI, maximum
   drawdown, and probability of positive return.
4. Separate spreads and moneylines; their calibration and tail risks differ.
5. Benchmark against no bet, flat stake, and capped fractional Kelly.
6. Lock a policy before the next prospective block and report every qualifying
   play, including skipped or unavailable prices.

A policy may reduce volume to improve reliability. It may not use future line
movement or select only the historically profitable seasons.

## Explicitly parked

Do not spend another cycle on these without the stated new evidence:

- XGBoost/RF/full NNLS stack: revisit only with materially more data or a new
  feature family showing nonlinear residual signal.
- Generic additive PFR, QBR, NGS, Elo, travel, coaching, rest, or line-movement
  blocks: already tested and rejected.
- GLM alpha sweep: objective-dependent and flat; the incumbent is retained.
- Stage-2 block pruning as an accuracy project: full confirmation was a wash.
- Isotonic or linear post-hoc mean calibration: previously failed.
- Vanilla DNN: inappropriate for the current sample size and signal-to-noise.
- Repeated parameter grids ranked by ROI on all ten OOS seasons: this converts
  the evaluation set into a training set.

## Execution backlog

### Now

- [ ] R0.1 Write and test the market timestamp/provenance contract.
- [ ] R0.2 Start the immutable 2026 prospective ledger.
- [x] R1.1 Implement deterministic lambda-selection arms and run probe/screen.
- [x] Add coefficient-selection stability and prediction-dispersion outputs to
  the ATS comparison artifact.

### Next

- [x] R1.2 Implement observation weights and rolling training windows — no arm advanced.
- [x] R1.3 Export calibration residual substrates and run conditional-scale grid.
- [x] R1.4 Build the nested cross-fitted trust gate.
- [x] R1.5 Screen trailing-four time-weighted live-WP control share; no ATS lift.
- [ ] Review prospective performance after 2026 Week 6 without retuning to it.

### After evidence or data acquisition

- [ ] R2.1 Acquire and validate historical multi-book timestamped lines.
- [ ] R2.2 Audit historical status/depth-chart timestamps before engineering
  availability shocks.
- [ ] R2.3 Test the compact schedule interactions only if the same-week
  diagnostic persists prospectively.
- [ ] R3 architecture challenges only after their prerequisite signal appears.

## Recommended sequence

R1.1–R1.5 are now complete without a production promotion. Prioritize
**R0.1/R0.2: timestamp provenance and an immutable prospective ledger**. Keep
the fixed R1.4 gate as a shadow-evaluation lead. Next model research should
target new decision-time information (R2.1/R2.2), after the required data audit,
rather than retuning historical windows or thresholds on these same seasons.
