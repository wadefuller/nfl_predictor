# Experiments ruled out (don't redo)

Sorted newest first. Each entry: **what was tried**, **measured outcome**, **why it failed**, **conditions under which you'd revisit**.

---

## Making XGB / RF competitive with the GLM (residual targeting, shrinkage, mixed-regime stacking)

**Tried (2026-06-07):** Full diagnostic investigation into why the stack is
GLM-dominated when the usual tabular-ML prior favours XGBoost. Reproduce with
`./run_pipeline.sh learners`.

**The measurements (honest 3-way split: train 2008-21 | cal 2022-23 | val 2024-25):**

Standalone val MAE — *XGB and RF are worse than simply quoting Vegas*:

| Learner | MAE | vs market |
|---|---|---|
| MARKET (`spread_line`) | 9.666 | — |
| GLM (cv.glmnet) | **9.589** | **−0.078** |
| XGBoost | 9.685 | +0.019 |
| Random Forest | 9.733 | +0.067 |

NNLS zeroing XGB and RF is therefore **correct**, not a bug.

**Root cause 1 — the linear market anchor.** `spread_line` alone gives
R²=0.189 with slope 1.053: the target is a straight line through the market
plus noise. Trees approximate `y=x` with a staircase and cannot extrapolate.
Regressing each learner's prediction on `spread_line`: GLM slope 0.995 /
R²=0.972, XGB slope 0.951 / **R²=0.882**. At the tails XGB collapses toward
the training mean — for `spread_line > 10` the market said 13.3, XGB said
**12.0**, GLM said 12.9, and the actual mean was 18.0.

**Root cause 2 — signal-to-noise.** 81% of target variance is irreducible
after the market. The real edge has R² of only 0.4%–2.6%. XGB emits sd=**2.08**
points of deviation-from-market when OLS says only ~0.38 of it is real; GLM
emits sd=**1.00**. Trees have far more capacity to manufacture spurious edge,
and at n≈4,900 with 393 features that is exactly what they do. Elastic-net's
L1 penalty is a much better-matched regularizer for "almost nothing is real."

**Root cause 3 — redundancy.** Base-learner predictions correlate 0.96–0.98,
so NNLS has little reason to use more than one.

**Fixes tested:**

1. **Residual targeting** (train on `home_result - spread_line`, add the
   market back at predict time). This is a *real* fix for root cause 1 and
   replicated across two independent splits: XGB **−0.071 / −0.075 MAE**,
   RF −0.019 / −0.032. It also flips the stacker — XGB's weight goes from
   0.000 to **1.000**. But it *hurts* the GLM (+0.013) and the resulting
   ensemble is worse overall (9.669 vs 9.620 same-regime, vs 9.605 GLM-only).

2. **Edge shrinkage** (scale deviation-from-market by an OLS slope fit on cal).
   Fitted slopes were **0.000 for XGB, 0.009 for GLM, 0.000 for RF** on the
   2022-23 cal window — i.e. that window says the entire edge is noise, and
   shrinkage collapses every learner back onto the market exactly.

3. **Mixed-regime stacking** (GLM on standard target, trees on residual —
   each learner in its own best regime). This *did* decorrelate them:
   `cor(xgb_dev, glm_dev)` fell from 0.60 to **0.28**. But NNLS still picked
   100% XGB and landed at 9.669. Equal-weight blends did better than the
   stacker but still failed to beat pure GLM: best was 0.25·XGBres +
   0.75·GLMstd at **9.609** vs GLM-only **9.605** (blends did win slightly on
   cover: 0.689 vs 0.686).

**Why none of it helps — the decisive number:** when *both* learners are
handed the market for free and asked only to model the deviation, GLM-residual
(9.618) still beats XGB-residual (9.669) by 0.051. **There is no meaningful
non-linear structure in the residual.** The deviation from the closing line is
(barely) linearly predictable and everything else is noise. Trees are not
mis-configured here; they have nothing to contribute.

**Bonus finding — why the old 1.23× amplification was so damaging.** The
edge-calibration slope is wildly unstable across adjacent windows: GLM's
deviation slope measured **1.56 on 2024-25** but **0.009 on 2022-23**. Same
quantity, adjacent two-year windows, ~544 games each. The multi-stacker was
fitting an amplification coefficient with enormous sampling variance, which is
exactly how it landed on 1.23 and wrecked walk-forward magnitude. This
independently validates the GLM-only decision.

**Revisit if:**
- Training data grows a lot (another ~10 seasons, or adding CFB/preseason).
  Trees need far more data to beat a well-regularized linear model at this SNR.
- You find a feature with genuinely non-linear interaction structure — the
  test is cheap: does XGB-on-residual beat GLM-on-residual? Today it loses by
  0.051.
- You start predicting something other than margin-vs-closing-line (e.g.
  opening lines, totals, or player props) where the market anchor is weaker.

---

## PFR advstats v2 (re-test under GLM-only architecture)

**Tried (2026-06-07):** After the GLM-only architecture change (which made
the model BEAT Vegas), re-tested PFR advstats against the new baseline. The
hypothesis: cv.glmnet's elastic-net does feature selection naturally, so
features that XGB would displace under the multi-stacker might instead
coexist as small coefficients under GLM-only.

**Result vs new GLM-only baseline (MAE 9.7783 / Brier 0.21033 / LogLoss 0.60871 / Cover 53.36%):**
- MAE +0.0004 (noise)
- Brier -0.00002 (basically identical)
- LogLoss -0.00008 (tiny improvement)
- Cover -0.04pp (noise)

**Why it failed:** elastic-net pruned 41 of 42 PFR cols. Only
`pfr_yac_avg_r4_away` survived at coef -0.010 (a microscopic value).
Worse, the extra 42 cols distorted CV-fold regularization: lambda increased
from 1.58 to 1.91, pruning the bundle from 29 to 21 GLM coefs. Net: lost
8 existing useful coefs, gained 1 tiny PFR coef. Wash.

**Implication:** Under GLM-only architecture, the existing 29-coef GLM is
the actual signal extraction. Additional features that don't earn nonzero
coefs in elastic-net are pure noise that distorts CV regularization.

**Revisit if:**
- You add a single PFR feature in isolation (not the full 7-metric module).
  The pressure rate and bad throw pct may earn coefs without the noise
  contribution from broken tackles / def metrics.
- You change the GLM alpha to be less aggressive (e.g., alpha=0.05 = mostly
  ridge), letting more small coefs through. Risk: overfitting.

---

## REPLACEMENT pattern: travel features + drop starter_qb_epa_r8

**Tried (2026-06-06):** Hypothesis-of-record from the prior 5 ADDITION
failures: maybe new features fail because of substitution-cancellation —
they earn rank but displace existing features that were carrying real
signal. Test by FORCING the displaced feature out via `LEAKAGE_VARS_REG`
so the new feature has no choice but to fill the gap.

Implementation: re-added the travel module (5 cols × 4 versions, 12 pivoted
total) AND added `starter_qb_epa_r8_home`, `starter_qb_epa_r8_away`,
`starter_qb_epa_r8_home_diff` to `LEAKAGE_VARS_REG`. The travel ADDITION
attempt earlier in the session had shown `starter_qb_epa_r8_home` going
from gain 0.0071 → 0.0005 when travel was added — the cleanest single
displacement we'd seen.

**Result vs baseline (MAE 9.8324 / Brier 0.21075 / LogLoss 0.60967 / Cover 52.91%):**
- MAE +0.0018 (noise)
- Brier +0.00024 (marginal real)
- LogLoss **+0.00061 (REAL)**
- Cover **−1.05pp (REAL)** — a full point of cover acc lost

**Why it failed (key insight):** The displacement pattern was NOT
"duplicate signal between features". When BOTH travel and starter_qb_epa_r8
were available (ADDITION attempt), XGB's tree-split greediness chose
travel at each particular node but still benefited from having QB EPA
available for OTHER splits — combinatorial use that doesn't show up in
the importance score. Forcing QB EPA out entirely removed that
combinatorial benefit and travel alone couldn't fill the gap.

Cover acc lost a full point. That's a real signal that QB EPA r8 carries
unique information at SOME splits even when it lost rank.

**Implication for the saturation diagnosis:** The 5 consecutive ADDITION
reverts (lines v1/v2, PFR, QBR, travel) AND the REPLACEMENT revert (now
6 total this session) together tell a clearer story:
- The current 374-feature set is at the **information-density** ceiling of
  what XGB + GLM + RF can extract from this data
- The ceiling isn't "features measuring the same thing" — it's "every new
  feature competes for tree splits that existing features were also
  contributing to combinatorially"
- The path forward is NOT more or different features. It's **architecture
  change**: quantile regression head, seasonal stacking, residual
  specialization, or a different ensemble shape entirely

**Revisit if:**
- You change the ensemble architecture first (then re-test ADDITION of any
  promising feature against the new baseline)
- You replace a LOWER-importance displaced feature (rank 60+ where the
  combinatorial value is presumably smaller). This experiment used a
  rank-25 displacement target.

---

## Travel distance + timezone shift features

**Tried (2026-06-06):** Added `R/features/travel.R` + `data/external/team_locations.csv`
(hardcoded lat/lon/tz_offset for 32 teams). Features per (game_id, team):
- `travel_distance` — great-circle miles from team's home to venue (home = 0)
- `tz_shift` — abs hours difference between team's tz and venue tz
- `travel_distance_r4` — rolling 4-game travel (fatigue proxy)
- `travel_distance_season` — cumulative season-to-date travel

This was the highest-prior queued item: the ONLY remaining candidate that
captured a genuinely new spatial dimension (not substitution-prone).

**Result vs baseline (MAE 9.8324 / Brier 0.21075 / LogLoss 0.60967 / Cover 52.91%):**
- MAE +0.0023 (noise)
- Brier +0.00003 (noise)
- LogLoss +0.00020 (noise)
- Cover −0.43pp (noise, close to threshold)

0 of 4 improved, but none crossed real-regression thresholds.

**Targeted failure-bucket movement** (from `./run_pipeline.sh diagnose`):
- home_short_rest: +0.310 → +0.303 MAE (tiny improvement on the targeted bucket)
- big road favorites 7.5-10pt: +0.273 → +0.300 (worse)
- 10+ pt favorites: +0.161 → +0.171 (worse)

**Feature importance:** 6 of 12 travel cols earned real XGB rank
(travel_distance_r4_away rank 48 gain 0.0028; travel_distance_season_away
rank 49 gain 0.0025; 4 more in top-100). tz_shift cols all dropped by step_nzv
(only 7 distinct integer values). `starter_qb_epa_r8_home` went DEAD
(gain 0.0071 → 0.0005) — model found travel partly explained QB variance.

Cal-set Brier marginally improved (0.2016 → 0.2015) but walk-forward
(broader temporal coverage) marginally regressed.

**Why it failed:** Same substitution pattern as the prior 4 reverts (lines
v1/v2, PFR, QBR), just from a different angle. Even though travel is
spatial/orthogonal in the FEATURE space, in the SIGNAL space it captures
a dimension already partly absorbed by `starter_qb_epa_r8_home` (QB
performance variance correlates with travel/schedule). The skill's noise
thresholds aren't crossed in either direction — this is "no clear effect".

**Revisit if:**
- You build the "REPLACEMENT not addition" variant (drop `starter_qb_epa_*`
  from LEAKAGE_VARS_REG so it's used, then add travel as the replacement).
  Likely the cleanest test of whether travel carries unique signal that
  QB-EPA misses.
- You drop the unused tz_shift cols and the always-zero travel_distance_home
  cols, leaving only `travel_distance_away`, `travel_distance_r4_away`,
  `travel_distance_season_away`. Maybe a leaner 3-col module avoids the
  displacement cost.
- You add a SUBSET-conditional feature — `travel_distance_x_short_rest` —
  to hit the home_short_rest bucket directly rather than slate-wide.
- You add stadium coordinates and route through actual stadium IDs to
  handle neutral-site games (London, Mexico City, São Paulo) which are
  currently ignored. Adds correctness but unlikely to flip the result.

Module deleted on revert; `data/external/team_locations.csv` KEPT for future
v2 attempts (cuts the build cost from ~1 hr to ~20 min).

---

## ESPN QBR (charting-based all-in-one QB metric)

**Tried (2026-06-06):** Added `R/features/qbr.R` via
`nflreadr::load_espn_qbr(summary_type="week")`, exposing `qbr_total` and
`qbr_pts_added` rolled r4/r12/r16 — same lens family as our existing
`starter_qb_epa_*` features but charting-derived and context-weighted
(clutch plays, score state).

Coverage: 2018+ (100% NA pre-2018). NA% in 2018+ falls 12.7% → 5.6% by 2025.

**Pre-flight correlation check (worth knowing before any future re-try):**
- cor(qbr_r4, starter_qb_epa_r4_home) = **0.675** — high displacement risk
- cor(qbr_r4, qb_epa_r4_home)        = **0.786** — substitution territory
- Univariate cor with home_result: 0.188 (QBR) vs 0.168 (starter_qb_epa) —
  QBR slightly more predictive standalone
- Marginal R² of QBR over qb_epa in OLS: +0.010 (1pp of R²) — small but
  measurable orthogonal signal

The pre-flight made this a fence-sit experiment: high substitution risk
but measurable marginal R². Ran it to confirm direction.

**Result vs baseline (MAE 9.8324 / Brier 0.21075 / LogLoss 0.60967 / Cover 52.91%):**
- MAE +0.0097 (borderline real)
- Brier +0.00033 (marginal real)
- LogLoss **+0.00063 (REAL)**
- Cover **−0.59pp (REAL)**

**0 of 4 improved; 2 of 4 crossed real-regression thresholds.** This is the
clearest revert of any feature attempt in this session. 2023 regressed by
+0.090 MAE — that's a season already losing to Vegas by 0.149 and QBR made
it materially worse.

**Why it failed:** The pre-flight 0.79 correlation accurately predicted
substitution. The marginal R² in OLS doesn't survive XGB's actual feature
competition — when the new feature is highly correlated with multiple
existing ones, XGB rearranges weight without adding net signal, and the
rearrangement itself adds variance.

**Pattern recognized:** This is now the third experiment in this session
(lines v1, PFR, QBR) where a feature that earns importance in XGB still
hurts aggregate metrics because it displaces existing features measuring
the same underlying factor. The shared diagnosis is: **the feature set has
saturated the available signal under the current ensemble architecture.**

**Revisit if:**
- You build the "REPLACEMENT not ADDITION" variant: drop `starter_qb_epa_*`
  from `LEAKAGE_VARS_REG` so they're not features, then add QBR. Cleanest
  test of whether QBR carries more signal than starter_qb_epa per-unit.
- You add SUBSET-conditional QBR (e.g. `qbr_total_when_starter_new = qbr_total
  * (new_starter == 1)`) — targets specific contexts where QBR's charting
  context-weighting may matter more than EPA's outcome-weighting.
- Pre-2018 historical QBR becomes available (currently fold quality is
  diluted by NA-imputed earlier folds).

---

## PFR advanced stats (pressure, YBC/YAC, broken tackles, missed tackles)

**Tried (2026-06-06):** Added `R/features/pfr_advstats.R` via
`nflreadr::load_pfr_advstats(summary_level="week")` for pass/rush/rec/def.
Aggregated per-player charting data to team-game level, rolled r4/r16, and
exposed 7 metrics:
- `pfr_press_pct` (% dropbacks pressured — OL/QB pass-pro)
- `pfr_bad_throw_pct` (QB decision-making, outcome-independent)
- `pfr_ybc_avg` / `pfr_yac_avg` (rushing yards before/after contact)
- `pfr_brktkl` (sum of broken tackles, rushing+receiving)
- `pfr_def_pressures` / `pfr_def_miss_tkl_pct` (defensive)

Coverage: weekly data 2018+ (0% NA from 2019, 100% NA pre-2018).

**Result vs baseline (MAE 9.8324 / Brier 0.21075 / LogLoss 0.60967 / Cover 52.91%):**
- MAE +0.0046 (noise)
- Brier +0.00013 (marginal real)
- LogLoss **+0.00037 (REAL regression)**
- Cover +0.12pp (noise, right direction)

1 of 4 improved (and only in noise band); LogLoss crossed the real threshold.
XGB DID accept the features (213 vs 189 in importance): `pfr_press_pct_r4_home`
landed at rank 41, plus three more in top-100. But the new signal displaced
`starter_pass_epa_r4_home_diff` (gain 0.0152 → 0.0009, went DEAD).

**Why it failed:** Classic signal substitution. PFR's pressure rate is a more
direct measure of OL/QB protection than the outcome-based `starter_pass_epa`
the model was using, but the new measure captures roughly the same underlying
factor. Substitution, not addition. Net signal: zero, slight regression from
the displacement cost. The defensive PFR metrics (`pfr_def_pressures`,
`pfr_def_miss_tkl_pct`) didn't earn any importance rank — defensive aggregation
of player-level data is too noisy without snap-count weighting.

**Revisit if:**
- You add snap-count-weighted aggregation for the defensive metrics (current
  module uses naive `def_tackles_combined` weighting which is itself noisy)
- You replace `starter_pass_epa` features with the PFR pressure features
  entirely (so substitution doesn't have a displacement cost). Risk: pre-2018
  walk-forward folds become NA-heavy.
- You add a SUBSET of features that pass `feature_audit.R`'s gain threshold
  (drop the 5 PFR cols that landed at rank > 100 in v1)
- PFR releases a longer historical archive (back to 2010+) — coverage from
  2018 cuts walk-forward fold quality on the earliest seasons.

---

## Opening line + line-movement features (CLV proxy)

**Tried (2026-06-06):** Added `R/features/lines.R` joining `nfelo_games.csv`
from github.com/greerreNFL/nfelo (full-coverage opening + closing spreads
1999-2024). Two variants:
- **v1 (8 features):** `line_open`, `line_move`, `line_move_abs`,
  `line_move_toward_home_fav`/`_road_fav`, `juice_open`, `juice_close`,
  `juice_move`.
- **v2 (5 features, movement-only):** dropped `line_open` and the two sparse
  juice cols after diagnosing redundancy.

**Result vs baseline (MAE 9.8319 / Brier 0.21075 / LogLoss 0.60966 / Cover 52.91%):**
- v1: MAE +0.0062, Brier +0.00008, LogLoss +0.00016, Cover -0.24pp. All 4 in
  noise band but all wrong direction. `line_open` did earn XGB **rank 7,
  gain 0.038** — a top-10 feature.
- v2: MAE +0.0089, Brier +0.00017, LogLoss **+0.00050** (real), Cover **-0.66pp**
  (real). Strictly worse than v1.

**Why it failed:** v1's massive `line_open` importance was substitution, not
new signal. `cor(line_open, spread_line) = 0.97` and `line_open ≈ spread_line
- line_move` by identity → line_open is **93.4% redundant** with our existing
closing spread. XGB swapped it in at rank 7 but pushed two previously-
important market-derived features into "dead" territory:
- `game_info_gain_4gm_trend_home_diff` (gain 0.010 → 0.0004)
- `starter_qb_epa_r8_home` (gain 0.007 → 0.0005)

v2 isolated the genuinely additive dimension (`line_move`, cor 0.18 with
spread cover) but movement alone wasn't enough to overcome the noise it
contributed at the per-game level. The 0.18 cover correlation is real but
substantially already captured by the existing market features
(`spread_line`, `game_info_gain`, trending suite).

**Revisit if:**
- You have a much CLEANER line-movement signal — book-specific timing (sharp
  $ enters at 3am Wednesday), reverse line movement vs public bet %, etc.
  Aggregate close-minus-open is too noisy to help.
- You build SUBSET-conditional features (e.g., `line_move_in_big_road_fav`
  bucket only) to attack the specific +0.273 MAE bucket directly rather
  than as a slate-wide feature.
- You replace `spread_line` with `line_open` in the feature set entirely
  (would need to also drop `game_info_gain`-style derived cols) — testing
  the hypothesis that XGB prefers opening over closing as its market anchor.

Code preserved as the v2 module in `R/features/lines.R` and ETL helper
`etl_nfelo_lines.R` were deleted on revert. nfelo_games.csv stays at
`data/external/nfelo_games.csv` for future experiments. Resurrection cost:
~30 min (we know the API + cor structure now).

---

## Hyperparameter re-tune on 374-col feature set

**Tried (2026-06-04):** Re-ran all 4 tune scripts after the pace_total
feature add. Previous tune was on the 334-col feature set (May 24). Suspected
the optima had shifted.

**Result:** All 4 walk-forward metrics REGRESSED (MAE 9.832 → 9.839, Brier
0.21075 → 0.21094, LogLoss 0.60966 → 0.61017, Cover 52.91 → 52.65). Each
delta was small but consistent.

**Why it failed:** The new XGB tune chose much heavier regularization
(learn_rate 0.023 → 0.0035, reg_lambda 0.046 → 0.40, sample_size 0.84 →
0.66) optimizing the 6-fold rolling-origin CV RMSE. That single-metric
optimization on the **2008-2020** tune window doesn't reliably translate to
walk-forward performance on **2016-2025**. Specifically, the heavier
regularization improved CV RMSE by 0.013 but hurt out-of-sample Brier — the
two metrics aren't aligned at the margins.

**Revisit if:**
- You change the tune harness to optimize Brier directly (matches the
  evaluation philosophy)
- You change the tune window to overlap with the walk-forward window
  (currently disjoint by design to keep walk-forward honest)
- The feature set grows by >50 cols again (current tune is on 374 cols;
  modest changes shouldn't need retune)

The May 24 tune params are restored; no code changes were needed for revert.

---

## Margin calibration layer (isotonic + linear)

**Tried (2026-05-31):** Added `fit_margin_calibrator` between stacking and `pnorm(/σ)` to address the "close-game overshoot" diagnostic (model MAE +0.225 vs Vegas on 0-3 actual margin).

**Result:**
- Isotonic: cal-set MAE 9.69 → 9.41 (looks great), walk-forward MAE 9.84 → **9.99** (regressed), close-game gap **widened** to +0.425.
- Linear OLS: slope ≈ 1.0, intercept ≈ 0 → no-op. Confirms ensemble has no systematic global bias to recover.

**Why it failed:** Isotonic with ~100 step changes overfits 544-point cal-set noise. Linear can't move because OLS finds the ensemble output is already unbiased on average. The close-game overshoot is **structural** — the model leans into magnitude (wins on blowouts -0.27 MAE, loses on close games +0.22 MAE). Trade-off is inseparable without features that distinguish close-game scenarios upfront.

**Revisit if:** You add features that flag close-game scenarios (pace/total-driven), or want to try quantile regression / magnitude-conditional shrinkage. Helpers preserved in `ensemble_lib::fit_margin_calibrator`.

---

## Rest features v2 (interactions + prior-game state + cumulative)

**Tried (2026-05-30):** Added 8 features on top of v1 rest module: `rest_short_at_home`, `rest_short_on_road`, `rest_off_bye_at_home`, `rest_short_div`, `prior_won`, `prior_margin`, `prior_blowout_loss`, `prior_rest_days`, plus 2 cumulative season counts. Aimed to fix the home-short-rest failure (+0.265 MAE).

**Result:** XGB rejected 8 of 9 new features (only `prior_margin` survived, rank 54, gain 0.002). Targeted home-short-rest gap **widened** from +0.265 to +0.346. All aggregate metrics regressed.

**Why it failed:**
- Binary AND-interaction flags (e.g., `rest_short_at_home`) are **redundant for XGB** — the model can already learn AND interactions from the raw binaries (`rest_short_week` + `was_home`).
- Cumulative season counts (`rest_short_cum_season`) duplicate `games_played` signal.
- The home-short-rest gap is a real failure mode but the signal isn't there in the rest features at this granularity.

**Revisit if:** You add **travel distance** / **timezone change** features (we don't have stadium coordinates). That's a genuinely new dimension. Just adding more rest binaries won't help.

---

## Next Gen Stats (CPOE, RYOE, separation)

**Tried (2026-05-22):** `nflreadr::load_nextgen_stats` for passing/rushing/receiving. 24 features added across 8 dimensions.

**Result:** Added 3.5% of XGB gain. Walk-forward MAE regressed +0.008, Brier +0.00016, cover -0.04pp. Cal-set OOS improved.

**Why it failed:** "Displaces signal" pattern. Model is feature-saturated — new features push out old ones without adding net signal. Per-fold stacking weights shifted toward NGS-driven splits, but the existing EPA features were already capturing the underlying signal.

**Revisit if:** Hyperparam retune (the May 22 tune was on the larger feature set). Or test with feature pruning of the lowest-gain existing features. Code preserved; toggle via `NGS_ENABLED=1`.

---

## Carryover decay (per-class half-life)

**Tried (2026-05-21):** `apply_team_carryover_decay()` blends prior-season end-state with within-season values via `α(t) = exp(-(season_age) / half_life)`. Tested HL ∈ {Inf, 16, 12, 8, 6, 4, 2}.

**Result:** Inf (no decay) wins on every core metric (MAE 9.843 vs HL=6 at 9.890; Brier 0.21086 vs HL=6 at 0.21215). 2026-05-23 re-test with new feature set confirmed.

**Why it failed:** Decay introduces noise — current-season rolling EPA/Pythagorean already capture team-strength evolution. Prior-season end-state is mostly redundant with the within-season rolling features.

**Revisit if:** You add features that *specifically benefit* from prior-season context (e.g., free-agency turnover percentage). Code preserved; toggle via `CARRYOVER_HL_JSON='{"team_epa":6,...}'`.

---

## Elo (team + QB-adjusted)

**Tried (2026-05-21):** `build_elo.R` walks all games chronologically. Tested team-only, QB-adjusted with various K_TEAM, K_QB, HOME_ADV, regression rates.

**Result:** Team-only Elo edged baseline by 0.015 MAE / 0.0004 Brier. QB-adjusted variants worse than baseline. Even team-only didn't justify the added complexity.

**Why it failed:** Vegas market + Pythagorean + EPA already capture team strength. Elo is correlated with them; adds noise without adding net signal.

**Revisit if:** You drop market features (e.g., predict opening lines instead of beating closing lines). Code preserved; toggle via `ELO_DISABLED=0`.

---

## Coaching features (new HC, interim, tenure)

**Tried (2026-05-22):** `R/features/coaching.R` — `coach_new_hc`, `coach_is_interim`, `coach_team_games`, `coach_team_seasons`.

**Result:** 2.0% of XGB gain. Walk-forward metrics regressed marginally; 2025 MAE gap improved from -0.212 to -0.150 vs Vegas.

**Status:** KEPT (user direction). Helps recent-season adaptation but hurts aggregate. Code is in `R/features/coaching.R`.

---

## "Dead features" pruned at recipe stage

**Audit (2026-05-21):** 42 of 191 features had Gain < 0.001 in XGB importance.

**Removed via `LEAKAGE_VARS_REG`:**
- `rest_diff`, `home_team_rest`, `away_team_rest` (raw rest features — model treats as noise)
- `is_dome`, `is_grass` (don't add beyond `temp`, `wind`, `roof`)

**Notable still-dead but kept (low risk to leave):**
- 26 team identity dummies (all `home_team_*`, `away_team_*`) — XGB uses rolling team-strength stats instead. Dropped via `step_nzv` automatically.

---

## How to add new "ruled out" entries

When an experiment fails, document it here BEFORE the next session forgets:

```markdown
## [Name of thing tried]

**Tried (YYYY-MM-DD):** [one sentence]

**Result:** [metric changes]

**Why it failed:** [root cause]

**Revisit if:** [conditions under which the experiment becomes worth retrying]
```
