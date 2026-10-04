# ============================================
# Kalshi read-only NFL game-spread feed
# ============================================
# Public Kalshi market data needs no credentials. We use its NFL spread ladder
# rather than treating a futures/combo market as a single-game price. One row per
# game is selected at the most nearly even executable YES ask, which makes the
# displayed contract the clearest market-implied spread reference.

.kalshi_empty <- function() tibble::tibble(
  season = integer(), week = integer(), game_id = character(), event_ticker = character(),
  market_ticker = character(), team = character(), threshold = numeric(), bid = numeric(),
  ask = numeric(), volume = numeric(), liquidity = numeric(), model_margin = numeric(),
  margin_gap = numeric(), model_probability = numeric(), fee_estimate = numeric(),
  ev_raw = numeric(), ev = numeric(), qualifies = logical(), fetched_at = character()
)

.kalshi_code <- c(LA = "LAR", JAX = "JAC")
.kalshi_abbr <- function(team) {
  if (team %in% names(.kalshi_code)) unname(.kalshi_code[[team]]) else team
}

# Kalshi omits optional market fields from some responses. Keep a missing
# liquidity/volume value as NA rather than failing the entire slate export.
.kalshi_numeric_field <- function(markets, field, fallback = NULL) {
  name <- if (field %in% names(markets)) field else fallback
  if (is.null(name) || !name %in% names(markets))
    return(rep(NA_real_, nrow(markets)))
  suppressWarnings(as.numeric(markets[[name]]))
}

.kalshi_win_empty <- function() tibble::tibble(
  season = integer(), week = integer(), game_id = character(), event_ticker = character(),
  market_ticker = character(), team = character(), threshold = numeric(), bid = numeric(),
  ask = numeric(), model_probability = numeric(), fetched_at = character()
)

fetch_kalshi_game_spreads <- function(predictions, season, week, sigma,
                                      ev_cal = EV_CALIBRATION_DEFAULT,
                                      fee_rate = 0.07,
                                      min_ev = PORTFOLIO_DEFAULTS$min_ev) {
  fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  empty <- .kalshi_empty()
  if (nrow(predictions) == 0 || !is.finite(sigma) || sigma <= 0) return(empty)
  # The model probability is pre-kickoff; a started game's book is not. Scoring
  # one against the other reports the model's staleness as an edge.
  predictions <- tradable_slate(predictions, season)
  if (nrow(predictions) == 0) {
    message("  kalshi: no eligible pregame predictions (started or availability unverified)")
    return(empty)
  }
  markets <- tryCatch(
    jsonlite::fromJSON("https://external-api.kalshi.com/trade-api/v2/markets?series_ticker=KXNFLSPREAD&status=open&limit=1000")$markets,
    error = function(e) NULL
  )
  if (!is.data.frame(markets) || nrow(markets) == 0) return(empty)
  markets <- tibble::as_tibble(markets)
  markets <- markets %>%
    mutate(ask = .kalshi_numeric_field(markets, "yes_ask_dollars"),
           bid = .kalshi_numeric_field(markets, "yes_bid_dollars"),
           volume = .kalshi_numeric_field(markets, "volume_fp", "volume"),
           liquidity = .kalshi_numeric_field(markets, "liquidity_dollars", "liquidity")) %>%
    # Kalshi quotes in whole cents, so 0.01 is the real minimum tick. The old
    # bound was `ask > 0`, paired with a `pmax(ask, 1e-6)` divide-guard further
    # down — which does not guard anything: it licenses a denominator of one
    # millionth and therefore an EV of 100,000,000%. Anything quoted below a
    # cent is not a price you can take.
    filter(is.finite(ask), ask >= 0.01, ask < 1)
  if (nrow(markets) == 0) return(empty)

  purrr::map_dfr(seq_len(nrow(predictions)), function(i) {
    game <- predictions[i, ]
    away <- .kalshi_abbr(as.character(game$away_team))
    home <- .kalshi_abbr(as.character(game$home_team))
    # Kalshi's regular-game event ticker contains both team codes, e.g.
    # KXNFLSPREAD-26SEP13CHICAR. This rejects futures and matchup markets.
    event_rows <- markets %>% filter(stringr::str_detect(event_ticker, paste0(away, home)))
    if (nrow(event_rows) == 0) return(empty[0, ])
    event_rows %>%
      mutate(
        market_ticker = ticker,
        threshold = suppressWarnings(as.numeric(stringr::str_match(title, "over ([0-9.]+) points")[, 2])),
        team = dplyr::case_when(
          stringr::str_detect(market_ticker, paste0("-", home)) ~ as.character(game$home_team),
          stringr::str_detect(market_ticker, paste0("-", away)) ~ as.character(game$away_team),
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(team), is.finite(threshold)) %>%
      mutate(distance_from_even = abs(ask - 0.5)) %>%
      arrange(distance_from_even, desc(volume)) %>%
      slice(1) %>%
      transmute(
        season = as.integer(season), week = as.integer(week), game_id = as.character(game$game_id),
        event_ticker, market_ticker = ticker, team, threshold, bid, ask, volume, liquidity,
        model_margin = if_else(team == as.character(game$home_team), as.numeric(game$pred_margin), -as.numeric(game$pred_margin)),
        margin_gap = model_margin - threshold
      ) %>%
      # This is the same calibrated residual-margin uncertainty used for the
      # site's sportsbook cover probabilities, applied to Kalshi's exact rung.
      mutate(
        model_probability = pnorm(margin_gap / sigma),
        # Standard Kalshi quadratic taker-fee estimate, before cent rounding.
        # Actual fee can vary slightly with order size and is surfaced in the UI.
        fee_estimate = fee_rate * ask * (1 - ask),
        # EV PER DOLLAR STAKED, matching the sportsbook card.
        #
        # `model_probability - ask - fee` is the expected profit on ONE CONTRACT,
        # which costs `ask`, not on one dollar. Dividing by `ask` converts to the
        # same basis the sportsbook uses (`p*(dec-1) - (1-p)`), and it matters:
        # on a 46c contract the per-contract figure understates the per-dollar
        # return by more than a factor of ten once the fee is taken out. It also
        # made calibrate_ev() wrong here, since that calibration was fitted on
        # per-dollar sportsbook EV — feeding it a per-contract number applied a
        # slope to the wrong scale, so Kalshi could never clear a threshold the
        # book cleared on identical edge.
        ev_per_contract = model_probability - ask - fee_estimate,
        ev_raw = ev_per_contract / ask,
        ev = calibrate_ev(ev_raw, ev_cal),
        # Out-of-domain EV means the denominator collapsed, not that the edge is
        # real — report it, never qualify it.
        qualifies = ev >= min_ev & ev_raw_in_domain(ev_raw, ev_cal),
        fetched_at = fetched_at
      )
  })
}

# Kalshi's current NFL feed lists game outcomes from "wins by over 1.5" upward,
# rather than a literal moneyline. This is a useful near-win probability but it
# excludes one-point wins, so callers must label it as a 2+-point win proxy.
fetch_kalshi_win_proxies <- function(predictions, season, week, sigma) {
  fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  empty <- .kalshi_win_empty()
  if (nrow(predictions) == 0 || !is.finite(sigma) || sigma <= 0) return(empty)
  predictions <- tradable_slate(predictions, season)
  if (nrow(predictions) == 0) return(empty)
  markets <- tryCatch(
    jsonlite::fromJSON("https://external-api.kalshi.com/trade-api/v2/markets?series_ticker=KXNFLSPREAD&status=open&limit=1000")$markets,
    error = function(e) NULL
  )
  if (!is.data.frame(markets) || nrow(markets) == 0) return(empty)
  markets <- tibble::as_tibble(markets) %>%
    mutate(ask = suppressWarnings(as.numeric(yes_ask_dollars)),
           bid = suppressWarnings(as.numeric(yes_bid_dollars)),
           threshold = suppressWarnings(as.numeric(stringr::str_match(title, "over ([0-9.]+) points")[, 2]))) %>%
    filter(is.finite(ask), ask >= 0.01, ask < 1, threshold == 1.5)
  if (nrow(markets) == 0) return(empty)
  purrr::map_dfr(seq_len(nrow(predictions)), function(i) {
    game <- predictions[i, ]
    away <- .kalshi_abbr(as.character(game$away_team)); home <- .kalshi_abbr(as.character(game$home_team))
    markets %>%
      filter(stringr::str_detect(event_ticker, paste0(away, home))) %>%
      mutate(team = dplyr::case_when(
        stringr::str_detect(ticker, paste0("-", home)) ~ as.character(game$home_team),
        stringr::str_detect(ticker, paste0("-", away)) ~ as.character(game$away_team),
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(team)) %>%
      transmute(season = as.integer(season), week = as.integer(week), game_id = as.character(game$game_id),
                event_ticker, market_ticker = ticker, team, threshold, bid, ask,
                model_probability = pnorm((if_else(team == as.character(game$home_team), as.numeric(game$pred_margin), -as.numeric(game$pred_margin)) - threshold) / sigma),
                fetched_at = fetched_at)
  })
}
