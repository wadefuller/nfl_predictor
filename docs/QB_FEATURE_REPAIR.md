# Expected-starter QB feature repair

> 2026-09-27 update: the archived training features now use the team's last
> observed primary passer **before** each game, not that game's postgame
> primary passer. The live path still uses a fresh expected-QB snapshot.
> Historical `new_starter` flags and observed game-weather fields are excluded
> from the retrained model because they lack equivalent archived pregame data.
> The older Chicago margins quoted below describe the retired bundle.

## What changed

The historical history source identifies each completed game's most-used passer.
The training row now uses the prior observed passer and his strictly earlier
starts. Previously it used the current game's most-used passer, while upcoming
rows had no such passer, so the team carryover
step copied the previous starter's **already lagged** inputs. This was both an
identity error when starters changed and a one-game freshness error otherwise.

`predict_with_ensemble()` now rebuilds seven QB inputs for games that have not
kicked off: QB/pass EPA over 4 and 8 prior starts, career starts, starts with this
team this season, and starter change. Home-minus-away differentials are rebuilt
too. The source matrix and completed/in-progress game inputs are not rewritten.

- Expected identity: current Sleeper QB1, joined by Sleeper ID to GSIS ID using
  the public nflverse/ffverse crosswalk. No name-based matching. Missing or
  conflicting IDs and multiple QB1s are unresolved, not guessed.
- History: the same filtered play-by-play and most-dropbacks definition as the
  training module, cached as `data/qb_start_history.rds`. All historical seasons
  from 2000 must be present; every team-game in earlier weeks of the current
  season must be covered. Historic provider gaps remain the same as training
  (e.g. two games missing in 2000); this is not a claim of perfect source data.
- Cutoff: only seasons/weeks strictly before the target week; follows a player
  across teams. Current-week outcomes cannot enter the rolling values.
- Freshness: a matching season/week snapshot fetched within six hours. This is
  retrieval age, not a guarantee that the provider updated its depth chart.
- Failure: remove carried-over QB inputs, mark the forecast unverified and
  withhold recommendations. No invented replacement-player EPA or injury zeros.

`export_upcoming.R` refreshes status/IDs and current-season history before
prediction, and writes `data/upcoming_qb_features.csv` for inspection. Historical
seasons are cached individually; the daily refresh requires no retraining. A full
feature build also persists the compact history directly from its PBP input.
The central predictor applies the same repair to website exports.

## Chicago check (2026 week 3)

The observed depth feed resolves Tyson Bagent (`00-0038416`) as QB1 and Caleb
Williams as Out. Bagent is Questionable. The repaired inputs contain 5 career
starts, 0 Chicago starts this season, `new_starter=1`, QB EPA r4 approximately
-0.055 and r8 -0.265. Previously they contained 37 starts and +0.198/+0.188 EPA.
The first repair mistakenly wrote `_diff` aliases instead of the trained
`_home_diff` columns, leaving model-used differences stale. This naming bug is
now corrected for all seven QB features, including failure-path NA values.
Missing canonical input columns raise a schema error rather than silently
creating unused predictors.

The retired bundle moved from roughly PHI -1.7 to PHI -2.8 in the controlled
Williams/Bagent comparison. After honest historical retraining, the same
fixture moves from PHI -3.08 / Chicago win probability 35.42% to PHI -3.16 /
35.12%. The smaller response is the learned signal after removing hindsight,
not a claim that the QB switch has no real-world impact.

## Guardrails and remaining work

September 27 identity normalization: Sleeper provided Murray's `00-0035228`
and Jones's `00-0035710` with a leading space. The resolver now trims GSIS IDs
before deduplication/conflict checks. These are verified Sleeper/ffverse/nflverse
ID matches, not name-based overrides. Regression tests retain genuine-conflict
rejection and cover padded IDs and blank direct IDs with valid crosswalk matches.

September 27 correction: eligibility now requires freshly verified QB1 identity
to match the player whose features were rebuilt, plus a healthy QB1 status.
Injured backups no longer veto an otherwise verified starter. Questionable/unavailable QB1s,
ambiguous IDs and missing starter history remain blocked. The browser separately
labels expired checks as "Forecast needs refresh", not a QB injury warning.
Pregame orchestration now exports the main game cards immediately after upcoming
predictions, including lightweight upcoming-only runs.

Live injury and prior-game snap inputs are now refreshed before each slate
export. Missing feeds withhold recommendations rather than allowing a stale
value to masquerade as current. Historical injury importance uses the full
weekly roster, and the injured-starter flag matches the pregame QB proxy by
player ID. The full matrix, model weights and 2024–2025 calibration have been
rebuilt. Archived pregame QB announcements remain unavailable for older
seasons; the prior-passer proxy is conservative, not a perfect reconstruction
of what was known before every kickoff.

## Validation

Run `Rscript tests/test_qb_features.R` and
`Rscript tests/test_qb_availability.R`. Tests cover agreement with the training
formula, latest-game inclusion, transfers, replacement starters, missing history,
missing IDs, stale snapshots, differential recomputation and preservation of
completed/in-progress inputs. A production-bundle check confirms the live QB
refresh does not rewrite completed-game inputs within the rebuilt model.
With a local production bundle, run `Rscript tests/test_qb_prediction_contract.R`
for numerical preservation and empty-card export checks. It also uses the real
input schema, matches the actual baked production recipe against independently
constructed home-minus-away QB values, checks that non-QB inputs are unchanged,
and verifies the Williams/Bagent effect through `predict_with_ensemble()`.
The synthetic feature test was corrected to use canonical `_home_diff` names;
it was run against the buggy implementation first and failed as expected.
