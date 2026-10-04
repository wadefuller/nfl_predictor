# R1.4 model-trust gate protocol

Declared before reading gate results, 2026-09-23.

- Freeze GLM-only incumbent forecasts. Generate 2012–2015 outer holdouts as
  warm-up data, then evaluate 2016–2025. Each gate sees only outer predictions
  from seasons strictly before its evaluation season. Never train on in-sample
  margin residuals. Refit the gate once per season.
- Ridge logistic regression, alpha 0 and fixed lambda 0.1, without a tuning
  sweep. Median imputation and standardization use gate-training rows only.
- Pregame inputs: absolute model/market disagreement, spread magnitude and
  tiers, total, calendar year, week, rest difference, division, and the
  original fold's global residual sigma. Exclude observed weather, outcomes,
  and postgame statistics from the gate inputs.
- Learned arms: probability of beating market absolute error > 0.50; and
  probability the proposed ATS side wins > its odds break-even + 0.02.
  Exclude pushes when fitting the ATS classifier, refund them when scoring bets.
- Simple arms: disagreement >= 1 point; disagreement >= 2.5 points;
  sigma <= median sigma in earlier OOF games; disagreement >= 1 combined
  with that uncertainty ceiling. Include an all-games incumbent reference.
- Abstention means using the market margin for prediction and placing no bet.
  Report selected model-minus-market MAE, complete-slate gated MAE against
  incumbent and market, bet counts, flat one-unit recorded-odds ROI, and
  fixed -110 ROI sensitivity. No Kelly sizing or threshold retuning.
- Probe: 2025. Screen: 2016, 2019, 2022, 2025. Advance if >= 3/4 seasons
  have lower selected-subset MAE than market OR higher selected ROI than the
  all-games incumbent, >= 25 graded bets per season, and aggregate full-slate
  MAE improves OR ROI improves. Confirmation: same conditions in >= 6/10
  seasons, >= 500 total graded bets, no season supplies > 50% of gross positive
  season P&L, and a 90% season-block paired interval supports the claimed gain.
- Stop after screen if no arm qualifies. Report simple policies alongside
  learned gates; a simple-policy result does not validate the learned gate.
- This is historical research with existing feature/tuning provenance. Earlier
  OOF predictions protect the gate from direct look-ahead, but do not establish
  that upstream tuning or market snapshots were contemporaneous. Promotion
  requires a separate provenance review and prospective evidence.
