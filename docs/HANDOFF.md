# NFL Predictor — Session Handoff

Last updated: 2026-06-07

## Where things stand right now

- **Walk-forward 2016-2025:** MAE **9.7783** (BEATS Vegas 9.7868 by 0.0085), Brier 0.21033 (beats Vegas by 0.00085), LogLoss 0.60871 (beats Vegas by 0.00112), Cover 53.36%. **Model beats Vegas in 4 of 10 seasons** (2016, 2018, 2020, 2024).
- **Cal-set OOS (2024–2025):** MAE 9.57, Cover 0.549.
- **Architecture change (2026-06-07):** **GLM-only stack** is now the default. The cv.glmnet learner is the only one carrying real signal; XGB and RF were getting stacking weight 0 in every recent bundle. The multi-stacker was also AMPLIFYING GLM by ~1.23× which over-fit cal-set residuals — disastrous on walk-forward, especially close games. Switching to pure unamplified GLM dropped MAE 9.832 → 9.778, Brier 0.21075 → 0.21033, LogLoss 0.60967 → 0.60871, Cover 52.91% → 53.36% — **the largest single improvement in project history**. The structural close-game overshoot (+0.225 MAE) that survived 7 feature experiments is now **−0.036 MAE BETTER than Vegas** in that bucket. Set `STACK_FULL=1` to fall back.
- **Production bundle defaults (post-2026-06-07):** `R/features/pace_total.R` (kept 2026-06-01), GLM-only stack, no carryover, no Elo, no NGS. XGB and RF still train per-fold but their predictions are discarded by the stacker (~3-4 min of wasted compute per eval; left in for STACK_FULL fallback).
- **Pipeline:** `./run_pipeline.sh {full|eval|train|features|predict|test|diagnose|learners}` is the entry point. Don't run the four `Rscript` commands manually. `learners` explains *why* the stack is GLM-dominated (see EXPERIMENTS_RULED_OUT.md).
- **Features:** modular under `R/features/`. Add one = new file + one line in `_registry.R`. See `docs/HOW_TO_ADD_A_FEATURE.md`.
- **Tuning:** XGB, GLM alpha, RF mtry/min_n, stacking λ all have their own scripts. Auto-loaded by `ensemble_lib::load_tuned_*`.
- **Live model serving:** `predict_week.rmd` for weekly bet card, `export_json.R` for website. Both use bundle-baked EV calibration.

## First commands to run in a new session

```bash
# 1. Get current model state
./run_pipeline.sh diagnose            # 2 sec — see where model beats/loses to Vegas
Rscript -e 'b <- readRDS("models/ensemble_latest.rds"); str(b$metadata)' | head -20

# 2. If anything looks stale (>1 week old):
./run_pipeline.sh test                # validate features still pass schema/NA/leakage
./run_pipeline.sh eval                # walk_forward + retrain + auto-bake EV cal (~15 min)
```

## What's queued for next session

**GLM-only architecture (2026-06-07) is the new baseline.** Confirmed pace_total
remains valuable (all 4 metrics regress slightly without it). PFR v2 re-test
under GLM-only also failed (1 of 42 cols got tiny coef, distorted CV-fold
regularization). The remaining queue:

In priority order:

1. **Re-test lines, QBR, travel against new GLM-only baseline** (12 min each) —
   PFR v2 failed under GLM-only with the same "elastic-net prunes them" pattern.
   The remaining three (lines, QBR, travel) likely fail similarly, but worth
   confirming. Each gives a clean 0-or-1-coef signal on whether the data
   carries useful info that the existing 29-coef GLM doesn't already extract.

2. **Subset-conditional features** (1-2 hr) — the home_short_rest (+0.134) and
   Monday (+0.054) buckets remain open. Bucket-specific features could attack
   them directly; the leaner GLM may absorb a targeted coef without the noise.
   `data/external/team_locations.csv` cached.

3. **Drop XGB+RF training entirely** (1 hr cleanup) — they still train but
   are discarded under GLM-only. Removing the calls cuts ~3 min per eval.
   Pure cleanup; no metric change expected.

4. **Quantile regression head** (3-4 hr) — DEPRIORITIZED. The close-game
   overshoot it was designed to fix is **already solved** by GLM-only
   (+0.225 → −0.036 MAE on the 0-3 bucket). Other remaining buckets
   (Thursday +0.111, home_short_rest +0.134) may benefit but lift ceiling is
   much lower.

5. **Re-tune `tune_glm.R`** (5 min) — current `alpha=0.25, lambda=1.88` was
   tuned against the multi-stacker. Marginal expected improvement (all alphas
   in the prior sweep landed within Brier ±0.0001).

Do NOT try: re-testing PFR (just done, failed cleanly).

Closed since last handoff:
- **Pace/total close-game features** (2026-06-01) — landed as `R/features/pace_total.R`. Improved all 4 core metrics. Didn't directly close the close-game gap but broad-lifted the slate.
- **Hyperparameter re-tune** (2026-06-04, REVERTED) — see EXPERIMENTS_RULED_OUT.md. CV-RMSE-tuned XGB params don't translate to walk-forward Brier.
- **Wide-deep DNN POC** (2026-06-04, NOT WIRED) — `dnn_poc.R` test showed DNN can't match GLM on this dataset (MAE 9.69 vs 9.59). Existing stack already gives XGB+RF weight 0 (GLM dominates) — non-linear models add no signal here. POC file remains as reference.
- **Opening line / line-movement features via nfelo** (2026-06-06, REVERTED both v1 and v2) — see EXPERIMENTS_RULED_OUT.md. `line_open` is 93% redundant with `spread_line`; `line_move` alone too noisy at slate level.
- **PFR advanced stats** (2026-06-06, REVERTED) — see EXPERIMENTS_RULED_OUT.md. Pressure rate landed at rank 41 (real signal) but displaced `starter_pass_epa` features; substitution, not net addition. LogLoss crossed real-regression threshold.
- **ESPN QBR** (2026-06-06, REVERTED) — see EXPERIMENTS_RULED_OUT.md. 0.79 cor with `qb_epa_r4_home` predicted substitution; result confirmed (0 of 4 metrics improved, LogLoss and Cover both crossed real-regression threshold). Pre-flight correlation check now established as a discipline.
- **Travel distance + timezone shift** (2026-06-06, REVERTED) — see EXPERIMENTS_RULED_OUT.md. 6 of 12 cols earned real XGB rank but displaced `starter_qb_epa_r8_home` (gain 0.0071 → 0.0005). 0 of 4 metrics improved, all in noise band. Targeted home_short_rest bucket moved by only -0.007 MAE. Even a spatially-orthogonal feature hits the saturation wall.
- **REPLACEMENT-not-addition (travel + drop starter_qb_epa_r8)** (2026-06-06, REVERTED) — see EXPERIMENTS_RULED_OUT.md. Forced QB EPA out via LEAKAGE_VARS_REG to test whether displacement was the issue. Result: WORSE than ADDITION. Cover lost a full point (52.91% → 51.86%), LogLoss crossed real threshold. **Key finding**: the displacement pattern is XGB's combinatorial use of multiple features at different tree splits, not duplicate signal. Forcing displaced features out loses combinatorial value.
- **GLM-only architecture** (2026-06-07, **KEPT — largest improvement in project history**). MAE 9.832 → 9.778 (model BEATS Vegas), Brier 0.21075 → 0.21033, LogLoss 0.60967 → 0.60871, Cover 52.91% → 53.36%. The 1.23 amplification on GLM that the multi-stacker added was overfitting cal-set residuals and overshooting magnitude on walk-forward — disastrous for close games. Pure unamplified GLM is now the default. Set `STACK_FULL=1` to fall back.
- **pace_total re-eval under GLM-only** (2026-06-07, CONFIRMED keep). Disabled pace_total to verify; all 4 metrics regressed slightly. The 2 pace_total coefs in the 29-coef GLM (`team_points_against_r4_home` -0.19, `team_points_against_r16_home` -0.09) are real signal.
- **PFR v2 re-test under GLM-only** (2026-06-07, REVERTED). Elastic-net pruned 41 of 42 PFR cols; the surviving one had a tiny coef. Added 42 noise cols distorted CV regularization (lambda 1.58 → 1.91, dropped 8 useful coefs). See EXPERIMENTS_RULED_OUT.md.

## What NOT to try (proven dead ends)

See `docs/EXPERIMENTS_RULED_OUT.md` for details. Quick list:

- Post-hoc margin calibration (isotonic OR linear) — won't fix close-game overshoot.
- More rest binaries / interaction flags — XGB already extracts them; cumulative counts duplicate `games_played`.
- Re-enabling NGS — overfits when added on top of current feature set.
- Carryover decay — hurts pure prediction metrics in every sweep.
- More team identity dummies — all 26 are dead in importance ranking.
- Opening line as a standalone feature — 93% redundant with closing `spread_line`; XGB substitutes it for `spread_line` and displaces market-derived features into "dead" territory.
- Slate-wide line-movement features — `cor(line_move, cover) = 0.18` is real but already captured by existing market features.
- Re-tuning XGB via CV RMSE — the tune objective doesn't align with walk-forward Brier; heavier-reg params lose net.
- Vanilla DNN as 4th base learner — can't match GLM on this small (~5k row) tabular dataset; stacker already gives XGB+RF weight 0.
- PFR advstats stacked ON TOP of existing features — pressure rate displaces `starter_pass_epa_*` (substitution not addition). The path that might still work is REPLACEMENT not addition (drop the displaced features explicitly).
- ESPN QBR ON TOP of `starter_qb_epa_*` — substitution at 0.79 cor; 0 of 4 metrics improved. Verified the pre-flight correlation discipline: anything > 0.7 cor with an existing feature should be tried as REPLACEMENT first, not addition.
- Travel distance/tz_shift ON TOP of existing features — even a spatially-orthogonal signal displaces `starter_qb_epa_r8_home` because travel partly explains QB performance variance. **Saturation pattern**: the additive-feature path is exhausted at this ensemble architecture. Future work needs REPLACEMENT or ARCHITECTURE CHANGE, not addition.
- REPLACEMENT pattern (drop the displaced feature, add the new one) — confirmed to be WORSE than ADDITION. The "displacement" was XGB combinatorial use, not duplicate signal. Future work needs ARCHITECTURE CHANGE only.
- Reviving XGB/RF via residual targeting, edge shrinkage, or mixed-regime stacking — all measured, none beat pure GLM. Run `./run_pipeline.sh learners` for the full diagnostic. Decisive number: handed the market for free, GLM-on-residual (9.618) still beats XGB-on-residual (9.669). **There is no non-linear structure in the residual.**

## Loose ends

- **Last full retrain:** 2026-06-06 (sanity re-eval after lines revert, baseline restored). Production bundle = `models/ensemble_latest.rds` (~148 MB), trained on 2008-2025. Stacking weights: 0.00·xgb + 1.23·glm + 0.00·rf (GLM dominates).
- **Cached external data:** `data/external/nfelo_games.csv` (3.5 MB, 1999-2024 opening + closing spreads). Kept for the queued subset-conditional line-movement experiment. Refresh via re-running the deleted `etl_nfelo_lines.R` pattern (URL: github.com/greerreNFL/nfelo, file `nfelo/Data/Formatted Data/nfelo_games.csv`).
- **Failure buckets pace_total did NOT fix** (despite aggregate-metric gains): home_short rest (+0.310 MAE), big road favorites 7.5-10pt (+0.273), close games 0-3 actual margin (+0.244), Thursday games (+0.257). Each is an open lead for targeted future work.
- **`dnn_poc.R`** at repo root — standalone DNN attempt from 2026-06-04. Reference for any future DNN exploration.

## File map for quick orientation

```
build_features.R                  # 127-line orchestrator — reads R/features/_registry.R
walk_forward_eval.R               # 10-fold walk-forward eval
train_and_save_ensemble.R         # production bundle trainer (auto-bakes EV cal)
ensemble_lib.R                    # canonical helpers (paths, LEAKAGE_VARS_REG,
                                  #   load_tuned_*, fit_stacking, predict_with_ensemble)
sweep_harness.R                   # shared sweep machinery
recalibrate_ev.R                  # refit EV calibration constants from current preds
tune_hyperparams.R                # XGB tune (~1 min)
tune_glm.R                        # GLM alpha tune
tune_rf.R                         # RF mtry/min_n tune
tune_stacking_lambda.R            # stacking objective mix tune
run_pipeline.sh                   # entry point — use this, not raw Rscript
model_diagnostic.R                # ./run_pipeline.sh diagnose
feature_diff.R                    # Rscript feature_diff.R old.rds new.rds
tests/test_features.R             # ./run_pipeline.sh test
R/features/                       # one file per feature group, registered in _registry.R
R/infra/                          # shared infra (data_loaders, rolling, carryover, etc.)
docs/                             # this file + sibling docs
```

## Key env vars for experiments

| Env var | Effect |
|---|---|
| `NFL_PROJECT_DIR` | Relocate the repo |
| `CARRYOVER_HL_JSON='{"team_epa":6,...}'` | Override carryover half-lives |
| `ELO_DISABLED=0` | Re-enable Elo features |
| `ELO_PARAMS_JSON='{...}'` | Override Elo hyperparameters |
| `NGS_ENABLED=1` | Re-enable NGS features |
| `DEF_SPLITS_ENABLED=0` | Disable defensive splits |
| `EV_CAL_INTERCEPT_OVERRIDE`, `EV_CAL_SLOPE_OVERRIDE` | Override EV calibration |
| `STACKING_TUNE_OBJECTIVE=mae` | Tune stacking λ for MAE instead of Brier |
| `WF_SEASONS=2024,2025` | (not implemented yet — see backlog) per-season walk-forward subset |
