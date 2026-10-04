# ============================================
# Upcoming-week export — model game predictions + bet card
# ============================================
# Runs the production ensemble on the FIRST INCOMPLETE week of a season and
# writes machine-readable predictions that downstream consumers reuse:
#
#   data/upcoming_predictions.csv   one row per game: market line/total, model
#                                   margin + win prob, market- vs model-implied
#                                   team totals, and the spread/total edge. The
#                                   weekly_fantasy forward model and the website
#                                   games-preview report both read this.
#   data/upcoming_bet_card.csv      EV-positive bets for the week (Kelly-sized).
#
# Usage:
#   Rscript export_upcoming.R                 # auto: latest season, first incomplete week
#   Rscript export_upcoming.R 2026 5          # explicit season + week
#   TARGET_SEASON=2026 TARGET_WEEK=5 Rscript export_upcoming.R
#
# Env: NFL_PROJECT_DIR overrides repo root; BANKROLL overrides bet sizing base.
# ============================================

suppressPackageStartupMessages({ library(tidyverse) })

source(file.path(
  Sys.getenv("NFL_PROJECT_DIR", "~/Projects/R/nfl_predictor"), "ensemble_lib.R"))
source(file.path(PROJECT_DIR, "polymarket_lib.R"))
source(file.path(PROJECT_DIR, "kalshi_lib.R"))

# ---- resolve target season/week --------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
arg_season <- if (length(args) >= 1) suppressWarnings(as.integer(args[1])) else
  suppressWarnings(as.integer(Sys.getenv("TARGET_SEASON", NA)))
arg_week   <- if (length(args) >= 2) suppressWarnings(as.integer(args[2])) else
  suppressWarnings(as.integer(Sys.getenv("TARGET_WEEK", NA)))

game_w_features <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))

target_season <- if (!is.na(arg_season)) arg_season else max(game_w_features$season)

# First incomplete week = earliest week of the season whose last kickoff is today
# or later (matches predict_week.rmd's resolve_target_week). Falls back to the
# first week with any missing result if gameday is unavailable.
resolve_week <- function(data, season) {
  d <- data %>% filter(season == !!season)
  if ("gameday" %in% names(d)) {
    cand <- d %>%
      transmute(week, gameday = as.Date(gameday)) %>%
      group_by(week) %>% summarise(last = max(gameday, na.rm = TRUE), .groups = "drop") %>%
      filter(Sys.Date() <= last) %>% arrange(week) %>% slice(1) %>% pull(week)
    if (length(cand) == 1) return(cand)
  }
  inc <- d %>% filter(is.na(home_result)) %>% summarise(w = suppressWarnings(min(week))) %>% pull(w)
  if (is.finite(inc)) return(inc)
  max(d$week)  # season complete — re-score the final week
}

target_week <- if (!is.na(arg_week)) arg_week else resolve_week(game_w_features, target_season)

cat(sprintf("Upcoming export: season %d, week %d\n", target_season, target_week))

games <- game_w_features %>% filter(season == target_season, week == target_week)
if (nrow(games) == 0)
  stop(sprintf("No games for season %d week %d — run ./run_pipeline.sh features.",
               target_season, target_week))

# ---- predict ----------------------------------------------------------------
# Read-only status refresh; failed or stale feeds remain explicitly unverified.
status_rc <- system2(file.path(R.home("bin"),"Rscript"),
  c(shQuote(file.path(PROJECT_DIR,"capture_qb_availability.R")), target_season, target_week))
if (status_rc != 0) warning("QB availability refresh failed; recommendations fail closed")
history_rc <- system2(file.path(R.home("bin"),"Rscript"),
  c(shQuote(file.path(PROJECT_DIR,"refresh_qb_history.R")),target_season))
if (history_rc != 0) warning("QB history refresh failed; coverage checks will withhold incomplete forecasts")
live_rc <- system2(file.path(R.home("bin"),"Rscript"),
  c(shQuote(file.path(PROJECT_DIR,"refresh_live_features.R")),target_season,target_week))
if (live_rc != 0) stop("Current injury/snap feature refresh failed; refusing to export a stale slate")
game_w_features <- read.csv(file.path(DATA_DIR, "game_w_features.csv"))
games <- game_w_features %>% filter(season == target_season, week == target_week)
bundle <- readRDS(file.path(MODELS_DIR, "ensemble_latest.rds"))
# Persist player identity and exact input values alongside each daily export.
qb_audit <- refresh_qb_features(games,DATA_DIR) %>%
  select(game_id,season,week,starts_with("expected_qb_"),starts_with("qb_features_"),
         matches("^(starter_|new_starter_)"))
readr::write_csv(qb_audit,file.path(DATA_DIR,"upcoming_qb_features.csv"))
preds  <- predict_with_ensemble(bundle, games)

# Implied team totals. spread_line is +ve when the HOME team is favored, so the
# home half of the total is (total + spread)/2. Model swaps the market spread for
# the model's predicted margin (nfl_predictor_line, same +ve-home convention).
out <- preds %>%
  mutate(
    home_implied_market = total_line / 2 + spread_line / 2,
    away_implied_market = total_line / 2 - spread_line / 2,
    home_implied_pred   = total_line / 2 + nfl_predictor_line / 2,
    away_implied_pred   = total_line / 2 - nfl_predictor_line / 2,
    # +ve edge = model expects the home side to outscore the market spread.
    spread_edge_home    = nfl_predictor_line - spread_line,
    pred_margin         = nfl_predictor_line,   # alias for downstream joins
    pred_win_home       = pred_win_pct
  ) %>%
  select(season, week, game_id, home_team, away_team,
         spread_line, total_line, pred_margin, pred_win_home,
         home_implied_market, away_implied_market,
         home_implied_pred, away_implied_pred, spread_edge_home,
         cover_prob_home, home_moneyline, away_moneyline,
         availability_ok, availability_reason, availability_checked_at,
         qb_features_ready, qb_features_reason, starts_with("expected_qb_"))

up_path <- file.path(DATA_DIR, "upcoming_predictions.csv")
readr::write_csv(out, up_path)
cat(sprintf("  wrote %d games -> %s\n", nrow(out), up_path))

# ---- bet card ---------------------------------------------------------------
# Canonical betting engine: PORTFOLIO_DEFAULTS (min_ev = 0.05 on CALIBRATED EV)
# + the bundle's baked EV calibration. This is the true production policy — the
# same one portfolio_sim.R evaluates — so the card reflects the bets the model
# would actually place. (Do NOT hardcode min_ev=0.15 or the stale default cal.)
BANKROLL <- as.numeric(Sys.getenv("BANKROLL", "1000"))  # 1 unit = 1% = $10
PARAMS   <- PORTFOLIO_DEFAULTS

card <- tryCatch({
  # Same rule as the prediction-market libs: a game that has kicked off cannot be
  # bet, and scoring a pre-kickoff model against a settled result reports the
  # model's staleness as edge. This was missed when the market boards were fixed
  # because the stale EV calibration left the card empty, so the bug had nothing
  # to show. With the calibration corrected it immediately recommended four bets
  # on games that had already finished.
  preds_for_card <- tradable_slate(preds, target_season) %>%
    left_join(games %>% select(game_id, home_spread_odds, away_spread_odds),
              by = "game_id")
  generate_bet_card(preds_for_card, BANKROLL, PARAMS, bundle = bundle)
}, error = function(e) { message("bet card skipped: ", conditionMessage(e)); tibble() })

card_path <- file.path(DATA_DIR, "upcoming_bet_card.csv")
readr::write_csv(card, card_path)
cat(sprintf("  wrote %d bets -> %s\n", nrow(card), card_path))

# ---- Polymarket moneyline comparison (read-only) ---------------------------
# This is a separate source from sportsbook recommendations. It compares the
# model to the executable YES ask plus a 1-cent execution allowance, and does
# not place orders or imply that a contract can be filled at the displayed price.
poly <- fetch_polymarket_moneylines(out, target_season, target_week,
                                    ev_cal = ev_calibration_from_bundle(bundle))
poly_path <- file.path(DATA_DIR, "polymarket_nfl_moneyline.csv")
readr::write_csv(poly, poly_path)
cat(sprintf("  wrote %d mapped Polymarket moneyline contracts (%d EV+) -> %s\n",
            nrow(poly), sum(poly$qualifies %||% FALSE), poly_path))

# Kalshi's public NFL spread ladder is a distinct, read-only prediction-market
# source. It supplies executable YES bid/ask contracts around each game's fair
# spread; no account credentials or order placement are involved.
kalshi <- fetch_kalshi_game_spreads(out, target_season, target_week,
                                    sigma = bundle$sigma,
                                    ev_cal = ev_calibration_from_bundle(bundle))
kalshi_path <- file.path(DATA_DIR, "kalshi_nfl_game_spreads.csv")
readr::write_csv(kalshi, kalshi_path)
cat(sprintf("  wrote %d Kalshi NFL game spread references -> %s\n", nrow(kalshi), kalshi_path))
kalshi_win <- fetch_kalshi_win_proxies(out, target_season, target_week, sigma = bundle$sigma)
kalshi_win_path <- file.path(DATA_DIR, "kalshi_nfl_win_proxies.csv")
readr::write_csv(kalshi_win, kalshi_win_path)
cat(sprintf("  wrote %d Kalshi NFL 2+ win probability proxies -> %s\n", nrow(kalshi_win), kalshi_win_path))
