# Pregame quarterback availability safeguard

Update: the expected-starter input repair is now implemented; see
[QB_FEATURE_REPAIR.md](QB_FEATURE_REPAIR.md). Its verified starter-change flag
alone no longer blocks a game. Missing injury inputs and unresolved injury
statuses still block. The original implementation notes below describe the
initial guard before that repair.

Implemented 2026-09-26. This is a serving safety gate, **not a new QB-adjusted
margin model**. It does not change the historical training/evaluation features
or numerically reprice a backup quarterback.

`export_upcoming.R` captures public Sleeper NFL player status/depth data before
prediction. The season/week-specific snapshot records source and UTC retrieval
time. Six hours is the maximum retrieval age, not a guarantee that the provider
updated an injury within six hours. Failed requests retain the prior snapshot,
which must still pass age and season/week validation.

Predictions carry `availability_ok`, reason, and checked time. Both teams need
complete model QB-out/change fields with no outstanding flag, a fresh matched
snapshot, and exactly one depth-chart QB1. Any nonempty nonhealthy QB injury
status blocks the game, including a backup's designation: this is deliberately
conservative because an injured starter may already have been demoted to QB3.
We do not infer healthy from missing model inputs. This guard is not proof of
point-in-time starter identity alignment; that remains work for the QB-state
feature pipeline before restoring affected recommendations.

The live sportsbook/Polymarket/Kalshi path fails closed. Historical graded
records remain unchanged. Site cards, top bets, and market boards also reject
missing/expired availability metadata, including old exports. Numerical model
probabilities remain visible with an unverified warning, not a recommended edge.

The current PHI–CHI snapshot reports Caleb Williams Out and Tyson Bagent
Questionable. Both teams have missing model QB inputs. **Other games with those
missing inputs are withheld too**, not just Chicago. Resolving the input coverage
and validating replacement-QB effects is required to restore full coverage;
do not clear flags manually or substitute zeros for missing values.

`refresh_availability_exports.R SEASON WEEK` updates existing derived web files
without changing forecasts or historical grades. This is for refreshing safety
metadata without a full model/export run. Regular exports persist the same
metadata through `export_json.R` and `weekly_run/run_pre.R`.

Validation: `Rscript tests/test_qb_availability.R`, production prediction guard
on PHI–CHI, and website build. This change does not yet suppress unavailable
players in fantasy lineup optimization; that separate serving path still needs
its own player-level availability integration.
