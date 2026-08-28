# ============================================
# NFL Predictor: Export Data to JSON for Website
# ============================================
# Loads the production ensemble bundle, predicts on EXPORT_SEASONS, and writes
# JSON files for the React website to consume.
#
# Usage: Rscript export_json.R
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

# Shared inference library (loads tidymodels, glmnet, ranger, prediction helpers)
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

# ---- Configuration ----
# PROJECT_DIR / DATA_DIR / MODELS_DIR come from ensemble_lib.R.
EXPORT_SEASONS <- c(2024, 2025, 2026)
# Bankroll the stake recommendations are sized against (Kelly fraction of this).
# A UNIT is 1% of bankroll — the standard way to talk about bet size independent
# of how much money is actually behind it. With the engine's $5 rounding, a $500
# bankroll makes 1 unit = $5 exactly, so every Kelly stake lands on a whole unit
# (max bet is 5% = 5u). Raise BANKROLL and the unit scales with it.
BET_BANKROLL   <- as.numeric(Sys.getenv("BANKROLL", "500"))
BET_UNIT       <- BET_BANKROLL / 100
WAGER_AMOUNT   <- 100
OUTPUT_DIR     <- file.path(PROJECT_DIR, "website", "public", "data")
BUNDLE_PATH    <- file.path(MODELS_DIR, "ensemble_latest.rds")

# ---- Load ----
cat("Loading feature data...\n")
game_w_features <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))

cat("Loading ensemble bundle...\n")
bundle <- readRDS(BUNDLE_PATH)
cat(sprintf("  Bundle trained %s on %d games (seasons %d-%d)\n",
            bundle$metadata$trained_date,
            bundle$metadata$n_training_games,
            bundle$metadata$training_seasons[1],
            bundle$metadata$training_seasons[2]))

# ---- Helper: per-bet PnL ----
calculate_outcomes <- function(correct, winner, team, odds, wager = WAGER_AMOUNT) {
  case_when(
    correct == 1 & winner == team & odds > 0 ~ round(odds, 2),
    correct == 1 & winner == team & odds < 0 ~ round(100 / abs(odds) * 100, 2),
    TRUE ~ -wager
  )
}

# Run ensemble predictions and merge back to the raw game frame so downstream
# code can use both raw columns (home_points, home_win, ...) and predictions.
make_predictions <- function(data) {
  preds <- predict_with_ensemble(bundle, data) %>%
    select(game_id, nfl_predictor_line, line_diff, pred_win_pct,
           cover_prob_home, xgb_pred, glm_pred, rf_pred)
  data %>% left_join(preds, by = "game_id")
}

# ---- Bet card per game ------------------------------------------------------
# Runs the CANONICAL betting engine (PORTFOLIO_DEFAULTS: min_ev 0.05 on calibrated
# EV, Kelly-sized) over a season's games and returns at most one wager per game —
# the highest-EV side — so the site can show the actual recommended position and
# stake next to each matchup. Settled games also carry the realized P&L.
bets_for_games <- function(preds) {
  ev_cal <- ev_calibration_from_bundle(bundle)
  scored <- preds %>%
    mutate(
      spread_winner      = if_else(home_result - spread_line > 0, home_team, away_team),
      pred_spread_winner = if_else(line_diff > 0, home_team, away_team),
      pred_ml_winner     = if_else(nfl_predictor_line > 0, home_team, away_team),
      won_team           = if_else(home_win == 1, home_team, away_team),
      cover_correct = if_else(is.na(home_result) | (home_result - spread_line) == 0,
                              NA_integer_, as.integer(pred_spread_winner == spread_winner)),
      ml_correct    = if_else(is.na(home_result) | home_result == 0,
                              NA_integer_, as.integer(pred_ml_winner == won_team))
    )
  cands <- tryCatch(build_bet_candidates(scored), error = function(e) tibble())
  if (nrow(cands) == 0) return(tibble(game_id = character()))
  sized <- filter_and_size_bets(cands, BET_BANKROLL, PORTFOLIO_DEFAULTS, ev_cal)
  if (nrow(sized) == 0) return(tibble(game_id = character()))
  sized %>%
    mutate(pnl = pnl_dollars(correct, odds, stake)) %>%
    group_by(game_id) %>% slice_max(ev, n = 1, with_ties = FALSE) %>% ungroup() %>%
    # bet* prefixes avoid colliding with existing game columns on the join
    transmute(game_id,
              betSide   = bet_team,
              betType   = str_remove(bet_type, "^(home|away)_"),
              betOdds   = round(odds),
              betEv     = round(ev, 3),
              betStake  = round(stake),
              betUnits  = round(stake / BET_UNIT, 1),
              betToWin  = round(payout_if_win),
              # NA until the game settles (or on a push)
              betResult = case_when(is.na(correct) ~ NA_character_,
                                    correct == 1 ~ "win", TRUE ~ "loss"),
              betPnl    = if_else(is.na(correct), NA_real_, round(pnl)))
}

# ---- Create output directories ----
dir.create(file.path(OUTPUT_DIR, "seasons"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR, "performance"), recursive = TRUE, showWarnings = FALSE)

# ---- Process each season ----
season_meta <- list()

for (szn in EXPORT_SEASONS) {
  cat(sprintf("\nProcessing season %d...\n", szn))

  # Filter to this season
  season_games <- game_w_features %>%
    filter(season == szn)

  if (nrow(season_games) == 0) {
    cat(sprintf("  No games found for season %d, skipping.\n", szn))
    next
  }

  # Run ensemble + merge predictions back to raw game frame
  season_with_preds <- make_predictions(season_games)

  # What the betting engine would actually wager on, keyed by game.
  season_bets <- bets_for_games(season_with_preds)
  cat(sprintf("  %d EV+ bets across the season\n", nrow(season_bets)))

  # Format each game as a JSON-friendly object
  games_json <- season_with_preds %>%
    left_join(season_bets, by = "game_id") %>%
    mutate(
      # Prediction fields
      pred_winner = if_else(nfl_predictor_line > 0, home_team, away_team),
      pred_win_pct_norm = round(if_else(pred_win_pct > .5, pred_win_pct, 1 - pred_win_pct), 3),
      nfl_predictor_line_fmt = round(nfl_predictor_line, 1),
      home_impl_pts = round((total_line / 2) + (nfl_predictor_line / 2), 1),
      away_impl_pts = round((total_line / 2) - (nfl_predictor_line / 2), 1),
      line_diff = round(nfl_predictor_line - spread_line, 1),
      # Result fields — use corrected home_win/home_result directly
      has_result = !is.na(home_points),
      winning_team = if_else(home_win == 1, home_team, away_team),
      pred_correct = if_else(pred_winner == winning_team, TRUE, FALSE),
      # Vegas favorite: spread_line > 0 means home is favored in this dataset
      vegas_winner = if_else(spread_line > 0, home_team, away_team),
      vegas_correct = if_else(vegas_winner == winning_team, TRUE, FALSE)
    ) %>%
    rowwise() %>%
    mutate(
      game_obj = list(list(
        gameId = game_id,
        season = as.integer(season),
        week = as.integer(week),
        gameday = gameday,
        weekday = weekday,
        gametime = gametime,
        homeTeam = home_team,
        awayTeam = away_team,
        spreadLine = spread_line,
        totalLine = total_line,
        homeMoneyline = if_else(is.na(home_moneyline), NA_real_, home_moneyline),
        awayMoneyline = if_else(is.na(away_moneyline), NA_real_, away_moneyline),
        divGame = as.logical(div_game),
        prediction = list(
          predWinner = pred_winner,
          predWinPct = pred_win_pct_norm,
          nflPredictorLine = nfl_predictor_line_fmt,
          homeImplPts = home_impl_pts,
          awayImplPts = away_impl_pts,
          lineDiff = line_diff
        ),
        result = if (has_result) {
          list(
            homePoints = as.integer(home_points),
            awayPoints = as.integer(away_points),
            homeResult = as.integer(home_result),
            homeWin = as.logical(home_win == 1),
            winningTeam = winning_team,
            predCorrect = pred_correct,
            vegasCorrect = vegas_correct
          )
        } else {
          NULL
        },
        # Recommended position from the canonical betting engine; NULL when no
        # side on this game cleared the EV threshold (most games).
        bet = if (!is.na(betStake)) {
          list(
            side = betSide, type = betType, odds = as.integer(betOdds),
            ev = betEv, stake = as.integer(betStake), units = betUnits,
            toWin = as.integer(betToWin),
            result = if (is.na(betResult)) NULL else betResult,
            pnl = if (is.na(betPnl)) NULL else as.integer(betPnl)
          )
        } else {
          NULL
        }
      ))
    ) %>%
    ungroup()

  season_data <- list(
    season = szn,
    games = games_json$game_obj
  )

  # Write season JSON
  season_file <- file.path(OUTPUT_DIR, "seasons", paste0(szn, ".json"))
  write_json(season_data, season_file, auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat(sprintf("  Wrote %d games to %s\n", nrow(games_json), season_file))

  # ---- Performance stats for completed games ----
  completed <- season_with_preds %>% filter(!is.na(home_points))

  if (nrow(completed) > 0) {
    # Build YTD predictions (simplified — skip FTE since elo_prob1 not in CSV)
    ytd <- completed %>%
      mutate(
        pred_winner = if_else(nfl_predictor_line > 0, home_team, away_team),
        pred_win_pct_norm = round(if_else(pred_win_pct > .5, pred_win_pct, 1 - pred_win_pct), 3),
        nfl_predictor_line_fmt = round(nfl_predictor_line, 1),
        line_diff = nfl_predictor_line - spread_line,
        winning_team = if_else(home_win == 1, home_team, away_team),
        spread_winner = if_else(home_result - spread_line > 0, home_team, away_team),
        pred_spread_adj_winner = if_else(line_diff > 0, home_team, away_team),
        wf_correct_pred = if_else(pred_winner == winning_team, 1, 0),
        # Vegas favorite: spread_line > 0 means home is favored in this dataset
        veg_pred_winner = if_else(spread_line > 0, home_team, away_team),
        veg_correct_pred = if_else(veg_pred_winner == winning_team, 1, 0),
        wf_correct_pred_spr = if_else(pred_spread_adj_winner == spread_winner, 1, 0),
        veg_correct_pred_spr = if_else(veg_pred_winner == spread_winner, 1, 0),
        ml_outcome = calculate_outcomes(wf_correct_pred, pred_winner, home_team, home_moneyline) +
          calculate_outcomes(wf_correct_pred, pred_winner, away_team, away_moneyline),
        spr_outcome = calculate_outcomes(wf_correct_pred_spr, pred_spread_adj_winner, home_team, home_spread_odds) +
          calculate_outcomes(wf_correct_pred_spr, pred_spread_adj_winner, away_team, away_spread_odds),
        margin_mae_model = abs(nfl_predictor_line - home_result),
        margin_mae_vegas = abs(spread_line - home_result),
        home_win_01 = as.integer(home_win == 1),
        brier_model = (pred_win_pct - home_win_01)^2,
        brier_vegas = if_else(!is.na(impl_prob_home), (impl_prob_home - home_win_01)^2, NA_real_)
      )

    # Calculate cumulative stats
    perf_stats <- ytd %>%
      group_by(season, week) %>%
      arrange(season, week) %>%
      summarise(
        gms = n(),
        wk_wf_correct = sum(wf_correct_pred),
        wk_veg_correct = sum(veg_correct_pred),
        wk_wf_accuracy = round(mean(wf_correct_pred), 3),
        wk_veg_accuracy = round(mean(veg_correct_pred), 3),
        wk_wf_correct_spr = sum(wf_correct_pred_spr),
        wk_veg_correct_spr = sum(veg_correct_pred_spr),
        wk_wf_accuracy_spr = round(mean(wf_correct_pred_spr), 3),
        wk_veg_accuracy_spr = round(mean(veg_correct_pred_spr), 3),
        wk_ml_outcome = sum(ml_outcome),
        wk_spr_outcome = sum(spr_outcome),
        wk_ml_roi = round(sum(ml_outcome) / (n() * WAGER_AMOUNT), 3),
        wk_spr_roi = round(sum(spr_outcome) / (n() * WAGER_AMOUNT), 3),
        wk_model_margin_mae = round(mean(margin_mae_model), 3),
        wk_vegas_margin_mae = round(mean(margin_mae_vegas), 3),
        brier_gms = sum(!is.na(brier_vegas)),
        wk_model_brier = round(mean(brier_model, na.rm = TRUE), 4),
        wk_vegas_brier = round(mean(brier_vegas, na.rm = TRUE), 4),
        .groups = 'drop'
      ) %>%
      mutate(
        cml_gms = cumsum(gms),
        cml_wf_correct = cumsum(wk_wf_correct),
        cml_veg_correct = cumsum(wk_veg_correct),
        cml_wf_accuracy = round(cml_wf_correct / cml_gms, 3),
        cml_veg_accuracy = round(cml_veg_correct / cml_gms, 3),
        cml_wf_correct_spr = cumsum(wk_wf_correct_spr),
        cml_veg_correct_spr = cumsum(wk_veg_correct_spr),
        cml_wf_accuracy_spr = round(cml_wf_correct_spr / cml_gms, 3),
        cml_veg_accuracy_spr = round(cml_veg_correct_spr / cml_gms, 3),
        cml_ml_roi = round(cumsum(wk_ml_outcome) / (cml_gms * WAGER_AMOUNT), 3),
        cml_spr_roi = round(cumsum(wk_spr_outcome) / (cml_gms * WAGER_AMOUNT), 3),
        cml_model_margin_mae = round(cumsum(wk_model_margin_mae * gms) / cml_gms, 3),
        cml_vegas_margin_mae = round(cumsum(wk_vegas_margin_mae * gms) / cml_gms, 3),
        cml_brier_gms = cumsum(brier_gms),
        cml_model_brier = round(if_else(cml_brier_gms > 0, cumsum(wk_model_brier * brier_gms) / cml_brier_gms, NA_real_), 4),
        cml_vegas_brier = round(if_else(cml_brier_gms > 0, cumsum(wk_vegas_brier * brier_gms) / cml_brier_gms, NA_real_), 4)
      )

    # Format for JSON
    perf_json <- list(
      season = szn,
      weeklyStats = perf_stats %>%
        rowwise() %>%
        mutate(obj = list(list(
          week = as.integer(week),
          games = as.integer(gms),
          weekly = list(
            modelCorrect = as.integer(wk_wf_correct),
            vegasCorrect = as.integer(wk_veg_correct),
            modelAccuracy = wk_wf_accuracy,
            vegasAccuracy = wk_veg_accuracy,
            modelSpreadCorrect = as.integer(wk_wf_correct_spr),
            vegasSpreadCorrect = as.integer(wk_veg_correct_spr),
            modelSpreadAccuracy = wk_wf_accuracy_spr,
            vegasSpreadAccuracy = wk_veg_accuracy_spr,
            mlROI = wk_ml_roi,
            spreadROI = wk_spr_roi,
            modelMarginMAE = wk_model_margin_mae,
            vegasMarginMAE = wk_vegas_margin_mae,
            modelBrier = wk_model_brier,
            vegasBrier = wk_vegas_brier
          ),
          cumulative = list(
            games = as.integer(cml_gms),
            modelCorrect = as.integer(cml_wf_correct),
            vegasCorrect = as.integer(cml_veg_correct),
            modelAccuracy = cml_wf_accuracy,
            vegasAccuracy = cml_veg_accuracy,
            modelSpreadCorrect = as.integer(cml_wf_correct_spr),
            vegasSpreadCorrect = as.integer(cml_veg_correct_spr),
            modelSpreadAccuracy = cml_wf_accuracy_spr,
            vegasSpreadAccuracy = cml_veg_accuracy_spr,
            mlROI = cml_ml_roi,
            spreadROI = cml_spr_roi,
            modelMarginMAE = cml_model_margin_mae,
            vegasMarginMAE = cml_vegas_margin_mae,
            modelBrier = cml_model_brier,
            vegasBrier = cml_vegas_brier
          )
        ))) %>%
        pull(obj)
    )

    perf_file <- file.path(OUTPUT_DIR, "performance", paste0(szn, ".json"))
    write_json(perf_json, perf_file, auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  Wrote performance stats (%d weeks) to %s\n", nrow(perf_stats), perf_file))

    max_week <- max(completed$week)
  } else {
    # No completed games yet (preseason) — still write an EMPTY performance file.
    # Vite's SPA fallback serves index.html with HTTP 200 for missing files, so a
    # missing JSON reaches res.json() as HTML and error-pages the app.
    perf_file <- file.path(OUTPUT_DIR, "performance", paste0(szn, ".json"))
    write_json(list(season = szn, weeklyStats = list()), perf_file,
               auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  No completed games — wrote empty performance file to %s\n", perf_file))
    max_week <- 0
  }

  season_meta[[as.character(szn)]] <- list(
    maxWeek = max(season_games$week),
    completedWeek = max_week
  )
}

# ---- Write meta.json ----
current_season <- max(EXPORT_SEASONS)
meta <- list(
  availableSeasons = EXPORT_SEASONS,
  currentSeason = current_season,
  currentWeek = season_meta[[as.character(current_season)]]$completedWeek,
  lastUpdated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  weeksPerSeason = lapply(season_meta, function(x) x$maxWeek),
  # staking convention, so the UI can explain the numbers it shows
  bankroll = BET_BANKROLL,
  unitSize = BET_UNIT
)

meta_file <- file.path(OUTPUT_DIR, "meta.json")
write_json(meta, meta_file, auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nWrote meta.json to %s\n", meta_file))

cat("\nDone! JSON files exported to:", OUTPUT_DIR, "\n")
