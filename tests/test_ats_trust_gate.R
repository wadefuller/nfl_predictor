source("ats_trust_gate.R")
d <- load_gate_rows("data/ats_experiments/trust-gate/base",
                    "data/ats_experiment_cache/folds")
a <- gate_fold(d, 2019)
changed <- d
ix <- changed$season >= 2019
changed$model_error[ix] <- 999
changed$market_error[ix] <- 0
changed$win[ix] <- 1 - changed$win[ix]
changed$home_result[ix] <- -999
b <- gate_fold(changed, 2019)
for (field in c("selected", "gate_mae_probability", "gate_cover_probability"))
  stopifnot(identical(a$predictions[[field]], b$predictions[[field]]))
changed[changed$season > 2019, "total_line"] <- 999
c <- gate_fold(changed, 2019)
stopifnot(identical(b$predictions$selected, c$predictions$selected),
          a$audit$train_max < 2019)
base <- a$predictions[a$predictions$arm == "all_games", ]
stopifnot(identical(base$gated_margin, base$nfl_predictor_line))
fixture <- base[1:3, ]
fixture$selected <- TRUE
fixture$win <- c(1, 0, NA)
fixture$pnl <- fixture$policy_pnl <- c(100/110, -1, 0)
s <- score_gate(fixture)
stopifnot(s$bets == 3, s$graded_bets == 2, s$pushes == 1,
          abs(s$roi - (100/110 - 1)/3) < 1e-12)
fixture$selected <- FALSE
fixture$policy_pnl <- 0
s <- score_gate(fixture)
stopifnot(s$bets == 0, is.na(s$roi), s$pnl == 0)
cat("Trust gate: temporal isolation, frozen margins, pushes and abstention passed.\n")
