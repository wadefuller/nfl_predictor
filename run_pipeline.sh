#!/usr/bin/env bash
# ============================================================================
# NFL Predictor: Pipeline Orchestrator
# ============================================================================
# Single entry point that replaces the 4-command Rscript chain we run for
# each model iteration. Set -euo pipefail for fail-fast — if any step errors,
# the pipeline halts before re-saving stale outputs.
#
# Usage:
#   ./run_pipeline.sh              # default mode: full
#   ./run_pipeline.sh full         # build_features + walk_forward + train + portfolio_sim
#   ./run_pipeline.sh eval         # walk_forward + train (assumes features fresh)
#   ./run_pipeline.sh train        # train_and_save_ensemble only
#   ./run_pipeline.sh features     # build_features only
#   ./run_pipeline.sh predict      # render predict_week.rmd to HTML
#   ./run_pipeline.sh test         # run tests/test_features.R sanity checks
#   ./run_pipeline.sh diagnose     # slice walk_forward results by game type
#   ./run_pipeline.sh ats-probe    # 2025 fast triage; cannot promote
#   ./run_pipeline.sh ats-screen   # four-regime screen; cannot promote
#   ./run_pipeline.sh ats-confirm  # full 2016-2025 promotion evidence
#   ./run_pipeline.sh fantasy      # fantasy market index + league export -> website
#
# Env vars honored (passed through to each Rscript):
#   NFL_PROJECT_DIR             — relocate the repo (default ~/Projects/R/nfl_predictor)
#   CARRYOVER_HL_JSON           — override default carryover half-lives
#   ELO_DISABLED, ELO_PARAMS_JSON
#   NGS_ENABLED, DEF_SPLITS_ENABLED
#   EV_CAL_{INTERCEPT,SLOPE}_OVERRIDE  — override stored EV calibration
#   FANTASY_TTL_HOURS           — league API cache TTL, default 6 (0 forces refresh)
#
# Examples:
#   ELO_DISABLED=0 ./run_pipeline.sh full
#   NGS_ENABLED=1 ./run_pipeline.sh features
#   ./run_pipeline.sh full 2>&1 | tee /tmp/pipeline.log
# ============================================================================

set -euo pipefail
cd "$(dirname "$0")"

MODE="${1:-full}"

run_step() {
  local script="$1"
  local label="$2"
  printf '\n\033[1m>>> %s\033[0m  (Rscript %s)\n' "$label" "$script"
  local t0=$SECONDS
  Rscript "$script"
  printf '\033[2m    done in %ds\033[0m\n' "$((SECONDS - t0))"
}

render_rmd() {
  local rmd="$1"
  local label="$2"
  printf '\n\033[1m>>> %s\033[0m  (render %s)\n' "$label" "$rmd"
  local t0=$SECONDS
  Rscript -e "rmarkdown::render('$rmd', quiet = FALSE)"
  printf '\033[2m    done in %ds\033[0m\n' "$((SECONDS - t0))"
}

case "$MODE" in
  full)
    # build_features reads play-by-play from fantasy_db/pbp.db; refresh its
    # current season first (seconds). set -e makes a failed refresh stop the
    # build rather than silently building on last week's plays.
    run_step ../fantasy_db/build_pbp_db.R "Refresh play-by-play store"
    run_step build_features.R         "Build features"
    run_step walk_forward_eval.R      "Walk-forward eval"
    run_step train_and_save_ensemble.R "Train + save bundle"
    run_step portfolio_sim.R          "Portfolio simulation"
    ;;
  eval)
    run_step walk_forward_eval.R      "Walk-forward eval"
    run_step train_and_save_ensemble.R "Train + save bundle"
    ;;
  train)
    run_step train_and_save_ensemble.R "Train + save bundle"
    ;;
  features)
    # build_features reads play-by-play from fantasy_db/pbp.db; refresh its
    # current season first (seconds). set -e makes a failed refresh stop the
    # build rather than silently building on last week's plays.
    run_step ../fantasy_db/build_pbp_db.R "Refresh play-by-play store"
    run_step build_features.R         "Build features"
    ;;
  predict)
    render_rmd predict_week.rmd       "Render predict_week"
    ;;
  test)
    run_step tests/test_game_split.R       "Team-perspective regression"
    run_step tests/test_qb_features.R      "Pregame QB regression"
    run_step tests/test_injury_features.R  "Injury identity/rank regression"
    run_step tests/test_snap_features.R    "Prior-game snap regression"
    run_step tests/test_calibration_seasons.R "Complete-season calibration regression"
    run_step tests/test_qb_prediction_contract.R "Production QB prediction contract"
    run_step tests/test_features.R    "Feature test suite"
    ;;
  diagnose)
    run_step model_diagnostic.R       "Model success/failure diagnostic"
    ;;
  ats-probe)
    ATS_PROFILE=probe run_step walk_forward_eval.R "ATS fast probe"
    ;;
  ats-screen)
    ATS_PROFILE=screen run_step walk_forward_eval.R "ATS representative-season screen"
    ;;
  ats-confirm)
    ATS_PROFILE=confirm run_step walk_forward_eval.R "ATS full confirmation"
    ;;
  learners)
    run_step learner_diagnostic.R     "Base-learner diagnostic (why GLM dominates)"
    ;;
  census)
    run_step feature_census.R         "Feature census (provenance + per-learner usage)"
    ;;
  fantasy)
    run_step export_fantasy.R         "Fantasy market index + league export"
    ;;
  *)
    echo "Unknown mode: $MODE" >&2
    echo "Usage: $0 [full|eval|train|features|predict|test|diagnose|ats-probe|ats-screen|ats-confirm|learners|census|fantasy]" >&2
    exit 2
    ;;
esac

printf '\n\033[32m\033[1m✓ pipeline complete (%s)\033[0m\n' "$MODE"
