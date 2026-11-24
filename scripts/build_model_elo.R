# ============================================
# NFL Predictor: Model-Elo (ratings that speak for the model)
# ============================================
# Two per-team numbers, both in POINTS OF MARGIN (Elo-scale = 1500 + 25 * pts):
#
#   power  — what the model believes. Elo-style tracker whose "result" is the
#            model's own pre-game line, not the score:
#               implied = power[home] - power[away] + HFA
#               err     = model_line - implied
#               power[home] += K_P * err ;  power[away] -= K_P * err
#            K_P, HFA and offseason regression are tuned so the rating BEFORE
#            each game reproduces the model's line for that game (one-step
#            ahead). Outcomes never enter this rating.
#
#   form   — how a team is doing RELATIVE TO THE MODEL. Elo-style on surprise:
#               surprise = actual_margin - model_line      (capped at +/-21)
#               d = surprise - (form[home] - form[away])
#               form[home] += K_F * d ;  form[away] -= K_F * d
#            K_F is tuned on whether form predicts the NEXT game's surprise.
#            K_F = 0 is in the grid: if form has no predictive power the tuner
#            is allowed to say so, and the rating is then descriptive only.
#
# LEAKAGE RULE: every model line must be out-of-sample for its game.
#   2016-2025 : data/walk_forward_predictions.csv (per-fold OOS)
#   2026+     : archived pre-game website/public/data/weekly/<s>-wkNN/games_preview.json
# The production bundle's predictions are NOT used for played games — its
# training span includes the current season's completed games.
# Archived lines are frozen into data/model_elo_lines.csv once a result
# exists, so a later preview rewrite (e.g. after a manual mid-week retrain)
# cannot replace an honest line with an in-sample one.
#
# Known exposure: the 2026 wk3 preview was rewritten 2026-09-28 by the
# 2026-09-27 bundle, which had trained on that week's Thursday game. One game.
#
# Usage:  Rscript scripts/build_model_elo.R          # tune + write all outputs (~30 s)
#         source() it and call model_elo_current() to refresh with saved params
# Output: data/model_elo_params.rds    tuned params + diagnostics
#         data/model_elo_history.csv   per-game pre/post ratings
#         data/model_elo_current.csv   current per-team table
#         data/model_elo_lines.csv     frozen OOS line ledger (2026+)
# ============================================

local({
  r_root <- Sys.getenv("NFL_STACK_ROOT", unset = path.expand("~/Projects/R"))
  source(file.path(r_root, "shared/paths.R"))
  source(file.path(r_root, "shared/contracts.R"))
})
suppressPackageStartupMessages({library(dplyr); library(jsonlite)})

ELO_PER_POINT  <- 25
SURPRISE_CAP   <- 21
TRAIN_SEASONS  <- 2017:2022   # 2016 is burn-in (all ratings start at 0)
VALID_SEASONS  <- 2023:2100

p_model_elo <- function(x) file.path(dir_nfl(), "data", x)

# ---- Lines: OOS model predictions + results ---------------------------------
load_model_lines <- function() {
  wf <- read.csv(p_walkforward(), stringsAsFactors = FALSE)
  expect_frame(wf, cols = c("game_id", "season", "week", "home_team", "away_team",
                            "nfl_predictor_line"), min_rows = 2000,
               label = "walk_forward_predictions")
  wf <- wf %>% transmute(game_id, season, week, home_team, away_team,
                         model_line = nfl_predictor_line, market_line = spread_line,
                         source = "walkforward_oos")

  # Archived pre-game previews for games walk-forward does not cover.
  arch_files <- Sys.glob(file.path(p_web_data(), "weekly", "*-wk*", "games_preview.json"))
  arch <- bind_rows(lapply(arch_files, function(f) {
    j <- tryCatch(fromJSON(f), error = function(e) NULL)
    if (is.null(j) || !is.data.frame(j$games) || !nrow(j$games)) return(NULL)
    j$games %>% transmute(game_id, season = as.integer(j$season), week = as.integer(j$week),
                          home_team, away_team, model_line = as.numeric(pred_margin),
                          market_line = as.numeric(spread_line), source = "preview_archive")
  }))
  if (nrow(arch)) arch <- arch %>% filter(!game_id %in% wf$game_id, !is.na(model_line))

  # Ledger: a frozen line (result already known when recorded) always wins.
  ledger_path <- p_model_elo("model_elo_lines.csv")
  if (file.exists(ledger_path) && nrow(arch)) {
    led <- read.csv(ledger_path, stringsAsFactors = FALSE) %>% filter(frozen)
    arch <- bind_rows(led %>% select(game_id, season, week, home_team, away_team,
                                     model_line, market_line, source),
                      arch %>% filter(!game_id %in% led$game_id))
  }

  # Timestamped pre-game snapshots (pregame_snapshot.R, from 2026 wk5 on) beat
  # the preview archive: each row carries snapshot_at and kickoff_at, so "the
  # last forecast made BEFORE kickoff" is leakage-proof by construction rather
  # than inferred from file mtimes. Precedence for a game_id:
  #   walk-forward OOS > pre-kickoff snapshot > frozen ledger > preview archive
  snap_files <- Sys.glob(file.path(dir_nfl(), "data", "pregame_snapshots", "*.csv"))
  if (length(snap_files)) {
    snap <- bind_rows(lapply(snap_files, read.csv, stringsAsFactors = FALSE))
    expect_frame(snap, cols = c("game_id", "season", "week", "home_team", "away_team",
                                "nfl_predictor_line", "spread_line",
                                "snapshot_epoch", "kickoff_epoch"),
                 label = "pregame_snapshots")
    # Honour the ledger's provenance tags exactly as grade_week.R does:
    # "excluded" games have no honest forecast and are dropped from the ratings
    # entirely (never fall back to the archive for them); backfilled rows are
    # accepted by name even though their real write time is after kickoff.
    if (!"provenance" %in% names(snap)) snap$provenance <- NA_character_
    excluded_ids <- unique(snap$game_id[snap$provenance %in% "excluded"])
    arch <- arch %>% filter(!game_id %in% excluded_ids)
    snap <- snap %>%
      filter(!game_id %in% excluded_ids,
             snapshot_epoch < kickoff_epoch | provenance %in% "backfill_preview_archive",
             !is.na(nfl_predictor_line), !game_id %in% wf$game_id) %>%
      group_by(game_id) %>% slice_max(snapshot_epoch, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(game_id, season = as.integer(season), week = as.integer(week),
                home_team, away_team, model_line = nfl_predictor_line,
                market_line = spread_line, source = "pregame_snapshot")
    arch <- bind_rows(snap, arch %>% filter(!game_id %in% snap$game_id))
  }

  feats <- read.csv(p_features(), stringsAsFactors = FALSE) %>%
    select(game_id, gameday, home_result)
  expect_frame(feats, cols = c("game_id", "gameday", "home_result"), min_rows = 1000,
               label = "game_w_features")

  lines <- bind_rows(wf, arch) %>%
    left_join(feats, by = "game_id") %>%
    arrange(gameday, game_id)
  if (any(is.na(lines$gameday)))
    stop(sprintf("[model_elo] %d lines have no gameday in game_w_features (unmatched game_id)",
                 sum(is.na(lines$gameday))))

  # Persist / freeze the archive-sourced rows.
  if (nrow(arch)) {
    led_out <- lines %>% filter(source == "preview_archive") %>%
      mutate(frozen = !is.na(home_result)) %>%
      select(game_id, season, week, home_team, away_team, model_line, market_line,
             source, frozen)
    write.csv(led_out, ledger_path, row.names = FALSE)
  }
  lines
}

# ---- Core pass ---------------------------------------------------------------
run_model_elo <- function(lines, k_p, hfa, reg_p, k_f, reg_f, history = TRUE) {
  teams <- sort(unique(c(lines$home_team, lines$away_team)))
  P <- numeric(length(teams)); F <- P
  h <- match(lines$home_team, teams); a <- match(lines$away_team, teams)
  m <- lines$model_line; y <- lines$home_result; s <- lines$season
  n <- length(m)
  M <- matrix(0, n, 10)   # integer-indexed matrix: the tuner calls this ~1,100 times
  prev <- s[1]
  for (i in seq_len(n)) {
    if (s[i] != prev) { P <- P * reg_p; F <- F * reg_f; prev <- s[i] }
    hi <- h[i]; ai <- a[i]
    imp <- P[hi] - P[ai] + hfa
    ps  <- F[hi] - F[ai]
    M[i, 1] <- P[hi]; M[i, 2] <- P[ai]; M[i, 3] <- imp
    M[i, 4] <- F[hi]; M[i, 5] <- F[ai]; M[i, 6] <- ps
    e <- m[i] - imp
    P[hi] <- P[hi] + k_p * e; P[ai] <- P[ai] - k_p * e
    if (!is.na(y[i])) {
      sur <- max(-SURPRISE_CAP, min(SURPRISE_CAP, y[i] - m[i]))
      d <- sur - ps
      F[hi] <- F[hi] + k_f * d; F[ai] <- F[ai] - k_f * d
    }
    M[i, 7] <- P[hi]; M[i, 8] <- P[ai]; M[i, 9] <- F[hi]; M[i, 10] <- F[ai]
  }
  colnames(M) <- c("pre_power_home", "pre_power_away", "implied_line",
                   "pre_form_home", "pre_form_away", "pred_surprise",
                   "post_power_home", "post_power_away", "post_form_home", "post_form_away")
  hist <- if (history) bind_cols(lines, as.data.frame(M)) else as.data.frame(M)
  list(history = hist, power = setNames(P, teams), form = setNames(F, teams))
}

# ---- Tuning ------------------------------------------------------------------
tune_model_elo <- function(lines) {
  played <- !is.na(lines$home_result)
  tr <- lines$season %in% TRAIN_SEASONS & played
  va <- lines$season %in% VALID_SEASONS & played
  surprise <- lines$home_result - lines$model_line

  # Stage A: power tracker. Form params irrelevant here (set inert).
  ga <- expand.grid(k_p = seq(0.05, 0.50, 0.05), hfa = seq(0, 3, 0.25),
                    reg_p = seq(0.3, 1.0, 0.1))
  ga$mse_tr <- ga$mse_va <- NA_real_
  for (i in seq_len(nrow(ga))) {
    r <- run_model_elo(lines, ga$k_p[i], ga$hfa[i], ga$reg_p[i], 0, 1, history = FALSE)$history
    err <- lines$model_line - r$implied_line
    ga$mse_tr[i] <- mean(err[tr]^2); ga$mse_va[i] <- mean(err[va]^2)
  }
  bp <- ga[which.min(ga$mse_tr), ]

  # Stage B: form, with the power params fixed. k_f = 0 is the null model.
  gb <- expand.grid(k_f = c(0, 0.005, 0.01, 0.02, 0.03, 0.05, 0.075, 0.10),
                    reg_f = c(0, 0.25, 0.5, 0.75, 1))
  gb$mse_tr <- gb$mse_va <- NA_real_
  hist_by <- list()
  for (i in seq_len(nrow(gb))) {
    r <- run_model_elo(lines, bp$k_p, bp$hfa, bp$reg_p, gb$k_f[i], gb$reg_f[i], history = FALSE)$history
    gb$mse_tr[i] <- mean((surprise[tr] - r$pred_surprise[tr])^2)
    gb$mse_va[i] <- mean((surprise[va] - r$pred_surprise[va])^2)
    hist_by[[i]] <- r$pred_surprise
  }
  bf <- gb[which.min(gb$mse_tr), ]

  # Is form's validation skill distinguishable from zero? Game-level bootstrap
  # of MSE(null) - MSE(form) on the validation seasons.
  ps <- hist_by[[which.min(gb$mse_tr)]]
  dv <- (surprise[va])^2 - (surprise[va] - ps[va])^2
  set.seed(989)
  boot <- replicate(2000, mean(sample(dv, replace = TRUE)))
  form_skill <- list(mse_reduction = mean(dv),
                     ci95 = unname(quantile(boot, c(0.025, 0.975))),
                     skill_pct = 100 * mean(dv) / mean(surprise[va]^2),
                     n_valid = sum(va))

  list(power = bp, form = bf, form_skill = form_skill,
       grid_power = ga, grid_form = gb,
       train_seasons = TRAIN_SEASONS, valid_seasons = range(lines$season[va]),
       tuned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
}

# ---- Current table -----------------------------------------------------------
# The displayed "vs model" numbers are plain descriptive averages, NOT the form
# Elo. Tuning showed form has no predictive skill (2023-26 validation: 0.03%
# of surprise variance, 95% CI spanning zero), so its K is arbitrary and an
# Elo with an arbitrary K shouldn't be presented as if it meant something.
# "Points per game vs our model, capped at +/-21" is honest and readable.
current_table <- function(res) {
  h <- res$history
  last_season <- max(h$season)
  cap <- function(x) pmax(-SURPRISE_CAP, pmin(SURPRISE_CAP, x))
  long <- bind_rows(
    h %>% transmute(team = home_team, season, gameday, home_result,
                    vs_model = cap(home_result - model_line)),
    h %>% transmute(team = away_team, season, gameday, home_result,
                    vs_model = cap(model_line - home_result))) %>%
    filter(season == last_season, !is.na(home_result)) %>% arrange(team, gameday)
  vs <- long %>% group_by(team) %>%
    summarise(games = n(), vs_model_season = mean(vs_model),
              vs_model_last4 = mean(tail(vs_model, 4)), .groups = "drop")
  data.frame(team = names(res$power), power = unname(res$power)) %>%
    mutate(elo = 1500 + ELO_PER_POINT * power,
           rank = rank(-power, ties.method = "first")) %>%
    left_join(vs, by = "team") %>%
    mutate(games = coalesce(games, 0L), season = last_season) %>%
    arrange(rank)
}

# Refresh with SAVED params (no tuning). Used by export_json.R.
model_elo_current <- function(write = TRUE) {
  pp <- p_model_elo("model_elo_params.rds")
  if (!file.exists(pp)) stop("[model_elo] no tuned params; run Rscript scripts/build_model_elo.R first")
  prm <- readRDS(pp)
  lines <- load_model_lines()
  res <- run_model_elo(lines, prm$power$k_p, prm$power$hfa, prm$power$reg_p,
                       prm$form$k_f, prm$form$reg_f)
  cur <- current_table(res)
  if (write) {
    write.csv(res$history, p_model_elo("model_elo_history.csv"), row.names = FALSE)
    write.csv(cur, p_model_elo("model_elo_current.csv"), row.names = FALSE)
  }
  list(current = cur, history = res$history, params = prm)
}

# ---- Main --------------------------------------------------------------------
if (sys.nframe() == 0L) {
  lines <- load_model_lines()
  cat(sprintf("Lines: %d games, seasons %d-%d | played %d | by source: %s\n",
              nrow(lines), min(lines$season), max(lines$season),
              sum(!is.na(lines$home_result)),
              paste(names(table(lines$source)), table(lines$source), collapse = ", ")))

  t0 <- Sys.time()
  prm <- tune_model_elo(lines)
  cat(sprintf("Tuned in %.0fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  saveRDS(prm, p_model_elo("model_elo_params.rds"))

  bp <- prm$power; bf <- prm$form; fs <- prm$form_skill
  cat(sprintf("\nPOWER  K=%.2f  HFA=%.2f  offseason regression=%.1f\n", bp$k_p, bp$hfa, bp$reg_p))
  cat(sprintf("  tracking RMSE vs model line: train %.2f | valid %.2f pts\n",
              sqrt(bp$mse_tr), sqrt(bp$mse_va)))
  cat(sprintf("\nFORM   K=%.3f  offseason regression=%.2f\n", bf$k_f, bf$reg_f))
  cat(sprintf("  valid surprise MSE reduction vs predicting 0: %+.3f (95%% CI %+.3f to %+.3f), skill %.2f%%, n=%d\n",
              fs$mse_reduction, fs$ci95[1], fs$ci95[2], fs$skill_pct, fs$n_valid))

  out <- model_elo_current(write = TRUE)
  h <- out$history; va <- h$season %in% VALID_SEASONS & !is.na(h$home_result)
  cat(sprintf("\nSanity, valid seasons (n=%d) MAE vs actual: model %.3f | power-implied %.3f | market %.3f\n",
              sum(va), mean(abs(h$home_result[va] - h$model_line[va])),
              mean(abs(h$home_result[va] - h$implied_line[va])),
              mean(abs(h$home_result[va] - h$market_line[va]))))
  cat("\nCurrent ratings:\n")
  print(out$current %>% mutate(across(c(power, vs_model_season, vs_model_last4), ~ round(.x, 2)),
                               elo = round(elo)), row.names = FALSE)
}
