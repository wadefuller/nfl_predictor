# How to add a feature group

The whole point of the `R/features/` module pattern is that adding a feature is a routine ~30 min task, not a refactor. Here's the routine.

## 1. Decide what level

Each module declares a `level` controlling how outputs are joined:

- **`team_game_pre_pivot`** — one row per (`game_id`, `season`, `week`, `team`). Joined onto game_split, gets carryover decay, then pivoted to `_home`/`_away`/`_diff` via `DIFFERENTIAL_COLS`. Used for: EPA, Pythagorean, QB starter, trending.
- **`team_week_post_pivot`** — one row per (`season`, `week`, `team`). Attached *after* pivot via `attach_per_team_features` (also produces `_home`/`_away`/`_diff`). Used for: injuries, snap counts, coaching, rest, NGS.
- **`game`** — one row per `game_id`. Left-joined directly. Used for: weather, Elo, officials.

Choose the level that matches what your raw data naturally provides. If it's already per-team-per-week, use `team_week_post_pivot` — it's the easiest.

## 2. Write the module file

```r
# R/features/your_feature.R

extract_your_feature <- function(deps_you_need) {
  # Build the per-key tibble
  tibble(
    season, week, team,           # OR game_id for level="game"
    your_feature_1, your_feature_2, ...
  )
}

YOUR_FEATURE_MODULE <- feature_module(
  name        = "your_feature",
  description = "One-line description for docs",
  level       = "team_week_post_pivot",     # or team_game_pre_pivot, or game
  enabled     = function() TRUE,            # or env-var gated
  needs       = c("games"),                 # subset of deps; see below
  build       = function(seasons, deps) extract_your_feature(deps$games),
  leakage_vars = character(0)               # any current-game cols here
)
```

Available `deps` (set up by `build_features.R`):
- `games` — full game schedule from habitatring (home_score, away_score, gameday, etc.)
- `game_split` — long-format per-team rows (one per team per game)
- `pbp` — filtered nflfastR play-by-play
- `player_cap` — per-player APY (from `load_contracts`)
- `team_cap` — per-team total APY + top-5 threshold

If you need other nflreadr data (e.g., `load_pfr_advstats`), call it inside your `build` function.

## 3. Register the module

Edit `R/features/_registry.R`:

```r
source(file.path(PROJECT_DIR, "R/features/your_feature.R"))   # add source line

MODULES <- list(
  ...
  your_feature = YOUR_FEATURE_MODULE,                           # add to list
  ...
)
```

## 4. (If pre-pivot only) Add output cols to DIFFERENTIAL_COLS

If your module is `team_game_pre_pivot` and you want `_home`/`_away`/`_diff` columns, add the output cols to `R/infra/combine_pivot.R::DIFFERENTIAL_COLS`. Post-pivot modules don't need this — `attach_per_team_features` handles the pivot automatically.

## 5. (If applicable) Add leakage cols

If your module produces current-game stats that shouldn't be model features (e.g., `your_current_game_stat`), declare them in `leakage_vars = c("your_current_game_stat")`. Mirror them in `ensemble_lib::LEAKAGE_VARS_REG`.

## 6. Validate

```bash
./run_pipeline.sh test                # ~2 sec — schema/NA/leakage checks
```

If it fails, the test output names which column / which check failed.

## 7. Full cycle

```bash
cp models/ensemble_latest.rds /tmp/bundle_pre_X.rds   # snapshot for diff
./run_pipeline.sh features            # ~7 min
./run_pipeline.sh eval                # ~15 min — walk_forward + retrain
./run_pipeline.sh diagnose            # ~2 sec — slice-by-slice MAE breakdown
Rscript feature_diff.R /tmp/bundle_pre_X.rds models/ensemble_latest.rds
```

## 8. Decide: keep or revert

Check core metrics in this order (per evaluation philosophy):

1. **Brier** (most important — drives EV via probability calibration)
2. **LogLoss**
3. **Cover acc**
4. **MAE**

A feature is a clear win if 3+ of 4 improve and none regress meaningfully. If mixed, look at `./run_pipeline.sh diagnose` to see if it closed a specific failure mode you were targeting.

If you decide to revert: just remove the registry entry + delete the file, then re-run features + eval. The codebase will stay clean — no scattered conditional logic.

## Common pitfalls

- **`fit$x` from `isoreg` is NOT sorted** despite docs claiming otherwise. Use `pred[fit$ord]` to get the sorted x aligned with `fit$yf`. (Burned us in the margin calibration attempt.)
- **`load_officials` uses `old_game_id` (YYYYMMDD)**, not the modern `game_id`. Join via `games$old_game_id`.
- **XGB rejects redundant interaction-flag binaries** (e.g., `rest_short_at_home = rest_short_week AND was_home`). It can learn AND interactions natively from raw flags. Don't add explicit AND-flags.
- **Cumulative season counts duplicate `games_played`.** XGB will reject them.
- **Sourcing `build_features.R` is safe** — the auto-run is gated by `sys.nframe() == 0L`. Same for `build_elo.R`.

## Reference modules to copy from

- **Simplest team-week module:** `R/features/coaching.R` (50 lines)
- **Simplest game-keyed module:** `R/features/officials.R` (60 lines)
- **Pre-pivot with rolling:** `R/features/off_epa.R` (35 lines)
- **Complex multi-source module:** `R/features/injuries.R` (80 lines)
