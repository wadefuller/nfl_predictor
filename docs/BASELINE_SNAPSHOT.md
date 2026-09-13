# Baseline snapshot — GLM-only + dead-column pruning

Frozen 2026-09-02. Supersedes the 2026-06-07 pre-prune snapshot
(MAE 9.7783 / Brier 0.21033 / LogLoss 0.60871 / Cover 0.5336, 4 seasons).
Any future change is measured against these numbers.

Architecture: GLM-only stack (STACK_MODE=glm_only), dead-column pruning ON
(PRUNE_DEAD default 1). 275 matrix columns, down from 392.

## Walk-forward 2016-2025 (n=2638)

| Metric | Model | Vegas | Edge |
|---|---|---|---|
| MAE | 9.7738 | 9.7868 | -0.0130 |
| Brier | 0.20986 | 0.21118 | -0.00132 |
| LogLoss | 0.60749 | 0.60983 | -0.00235 |
| Cover | 0.5360 | 0.4841 | +0.0519 |

Seasons beating Vegas on MAE: 5 of 10

## Per-season

| Season | Model MAE | Vegas MAE | Beat | Model Brier | Vegas Brier |
|---|---|---|---|---|---|
| 2016 | 8.8875 | 9.0195 | YES | 0.21695 | 0.22118 |
| 2017 | 10.1201 | 10.0804 |  | 0.20143 | 0.19968 |
| 2018 | 9.9104 | 9.9785 | YES | 0.21029 | 0.21064 |
| 2019 | 10.3255 | 10.2109 |  | 0.21460 | 0.21425 |
| 2020 | 9.8254 | 9.8301 | YES | 0.20088 | 0.20364 |
| 2021 | 10.7782 | 10.7812 | YES | 0.21594 | 0.21960 |
| 2022 | 8.7489 | 8.7417 |  | 0.21271 | 0.21006 |
| 2023 | 9.9733 | 9.9007 |  | 0.21764 | 0.21921 |
| 2024 | 9.4518 | 9.6103 | YES | 0.19669 | 0.20040 |
| 2025 | 9.7258 | 9.7224 |  | 0.21114 | 0.21269 |

## Bundle

trained 2026-09-02 | stack glm_only | drop-list 84 cols
cal-set OOS: MAE=9.583 Brier=0.2015 LogLoss=0.5872 cover=0.6967
