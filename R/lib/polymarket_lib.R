# ============================================
# Polymarket read-only market discovery — NFL moneylines
# ============================================
# This is deliberately an analysis feed, not an execution client. It reads
# public Gamma market metadata and compares the model to the executable YES ask
# with a conservative 1-cent execution allowance.

.poly_team_names <- c(
  ARI = "Arizona Cardinals", ATL = "Atlanta Falcons", BAL = "Baltimore Ravens",
  BUF = "Buffalo Bills", CAR = "Carolina Panthers", CHI = "Chicago Bears",
  CIN = "Cincinnati Bengals", CLE = "Cleveland Browns", DAL = "Dallas Cowboys",
  DEN = "Denver Broncos", DET = "Detroit Lions", GB = "Green Bay Packers",
  HOU = "Houston Texans", IND = "Indianapolis Colts", JAX = "Jacksonville Jaguars",
  KC = "Kansas City Chiefs", LAC = "Los Angeles Chargers", LA = "Los Angeles Rams",
  LV = "Las Vegas Raiders", MIA = "Miami Dolphins", MIN = "Minnesota Vikings",
  NE = "New England Patriots", NO = "New Orleans Saints", NYG = "New York Giants",
  NYJ = "New York Jets", PHI = "Philadelphia Eagles", PIT = "Pittsburgh Steelers",
  SEA = "Seattle Seahawks", SF = "San Francisco 49ers", TB = "Tampa Bay Buccaneers",
  TEN = "Tennessee Titans", WAS = "Washington Commanders"
)

.poly_empty <- function() tibble::tibble(
  season = integer(), week = integer(), game_id = character(), bet_team = character(),
  opponent = character(), question = character(), slug = character(), market_id = character(),
  model_probability = numeric(), model_probability_raw = numeric(), market_p = numeric(), ask = numeric(), execution_price = numeric(), raw_edge = numeric(),
  ev_raw = numeric(), net_ev = numeric(), qualifies = logical(), liquidity = numeric(), volume = numeric(), fetched_at = character()
)

.poly_json <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return(character())
  tryCatch(unlist(jsonlite::fromJSON(x)), error = function(e) character())
}

# Returns mapped, liquid YES contracts for the current slate. `qualifies` flags
# the subset that clears the 5% net-EV threshold used in the opportunities list.
# A contract's expected return per dollar paid is p / effective_price - 1.
# `execution_price` adds a 1-cent buffer for the displayed ask, fee uncertainty,
# and modest slippage; it is not a claim that a user can execute at that exact price.
# Optional shrinkage of the model toward the market before scoring EV.
# DEFAULT 1.0 = OFF: the model's own probability is used as-is.
#
# Why it exists, and why it is off. The 2026 week-1 slate flagged 13 EV+ of 16
# games — no week in 160 historical ones ever had more than a handful — which
# looked like the model being overconfident. Two later measurements undercut
# that reading:
#
#   1. A full walk-forward eval against the REAL ensemble found the probability
#      chain is not miscalibrated: replacing pnorm+Platt with a single logistic
#      moved Brier by +0.00002 (p = 0.52). There is no transformation defect for
#      shrinkage to correct.
#   2. Shrinking toward the market improves Brier only from 0.20986 to 0.20955
#      — p = 0.396, not significant at any lambda tested.
#
# The one piece of evidence that still favours shrinking: regressing outcomes on
# market log-odds with the model's deviation free gives lambda = 0.552, 95% CI
# [0.177, 0.928] — an interval that EXCLUDES 1.0. So the deviation probably does
# deserve some discount; the effect is just too small to prove on 2,638 games.
#
# Left as a knob rather than deleted. Set POLY_MODEL_WEIGHT (or the env var) to
# ~0.55 to re-enable, and re-estimate it as walk-forward data accumulates.
POLY_MODEL_WEIGHT <- suppressWarnings(as.numeric(Sys.getenv("POLY_MODEL_WEIGHT", "1.0")))
if (!is.finite(POLY_MODEL_WEIGHT)) POLY_MODEL_WEIGHT <- 1.0

.poly_logit <- function(p) { p <- pmax(1e-6, pmin(1 - 1e-6, p)); log(p / (1 - p)) }
.poly_inv_logit <- function(x) 1 / (1 + exp(-x))

fetch_polymarket_moneylines <- function(predictions, season, week,
                                        min_liquidity = 1000, execution_buffer = 0.01,
                                        model_weight = POLY_MODEL_WEIGHT,
                                        ev_cal = EV_CALIBRATION_DEFAULT,
                                        min_ev = PORTFOLIO_DEFAULTS$min_ev) {
  fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  empty <- .poly_empty()
  if (nrow(predictions) == 0) return(empty)
  # `model_probability` below is the PRE-KICKOFF number. Scoring it against a
  # live or settled book compares two different moments and always favours the
  # model, so only games that have not started are priced at all.
  predictions <- tradable_slate(predictions, season)
  if (nrow(predictions) == 0) {
    message("  polymarket: no eligible pregame predictions (started or availability unverified)")
    return(empty)
  }

  # WHY THIS IS ADDRESSED BY SLUG AND NOT BY TAG
  # -------------------------------------------
  # The previous implementation queried `tag_id=450`, which is NFL FUTURES and
  # PROPS ("Tush Push banned for 2026?", "Chiefs Week 1 starting QB?"). Those
  # records carry neither `gameId` nor `sportsMarketType`, so the required-column
  # check bailed and this feed wrote an empty CSV on every run for months. Two
  # further faults sat behind it: `limit=1000` silently returns at most 100 rows
  # (Gamma caps page size and needs `offset` to paginate), and the outcome check
  # required outcomes[1] == "yes" — but a game moneyline's outcomes are the two
  # TEAM NAMES, so every real market would have been rejected anyway.
  #
  # Game markets are addressable directly: the slug is
  #   nfl-<away>-<home>-<YYYY-MM-DD>
  # and those abbreviations are nflverse's, lowercased. Gamma accepts repeated
  # `slug=` parameters, so the whole slate is one request rather than a paginated
  # sweep of every active market on the platform.
  sched <- NULL
  for (attempt in 1:3) {
    d <- try(nflreadr::load_schedules(season), silent = TRUE)
    if (!inherits(d, "try-error") && NROW(d) > 0 && all(c("game_id", "gameday") %in% names(d))) { sched <- d; break }
    Sys.sleep(2 * attempt)
  }
  if (is.null(sched)) {
    message("  ! polymarket: schedules unavailable — cannot build game slugs")
    return(empty)
  }

  slate <- predictions %>%
    dplyr::left_join(
      tibble::as_tibble(sched) %>% dplyr::select(game_id, gameday),
      by = "game_id"
    ) %>%
    dplyr::filter(!is.na(gameday)) %>%
    dplyr::mutate(matchup = sprintf("nfl-%s-%s",
                                    tolower(as.character(away_team)),
                                    tolower(as.character(home_team))))
  if (nrow(slate) == 0) return(empty)

  # Polymarket stamps the slug with the UTC date of kickoff, while `gameday` is
  # the local date. Any game starting after 8pm ET is already the next day in
  # UTC, so a Thursday/Sunday-night slug lands a day ahead — that alone lost 4 of
  # 16 week-1 games. Ask for both dates; only one will exist.
  slate <- dplyr::bind_rows(
    dplyr::mutate(slate, slug = sprintf("%s-%s", matchup, as.character(gameday))),
    dplyr::mutate(slate, slug = sprintf("%s-%s", matchup,
                                        as.character(as.Date(gameday) + 1)))
  )

  url <- paste0("https://gamma-api.polymarket.com/markets?",
                paste0("slug=", slate$slug, collapse = "&"))
  markets <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (!is.data.frame(markets) || nrow(markets) == 0) {
    message(sprintf("  ! polymarket: no markets returned for %d slugs", nrow(slate)))
    return(empty)
  }

  markets <- tibble::as_tibble(markets) %>%
    dplyr::mutate(liquidity_num = suppressWarnings(as.numeric(liquidity)),
                  volume_num    = suppressWarnings(as.numeric(volume))) %>%
    dplyr::filter(liquidity_num >= min_liquidity)
  if (nrow(markets) == 0) return(empty)

  rows <- purrr::map_dfr(seq_len(nrow(markets)), function(k) {
    market   <- markets[k, ]
    outcomes <- .poly_json(market$outcomes)
    prices   <- suppressWarnings(as.numeric(.poly_json(market$outcomePrices)))
    if (length(outcomes) != 2 || length(prices) != 2 || any(!is.finite(prices))) return(empty[0, ])
    # `outcomePrices` is the normalised MID — the two sides sum to exactly 1.000,
    # i.e. the spread has been removed. Pricing against it manufactures edge out
    # of nothing (it made 11 of 12 week-1 contracts look EV+). Use the real
    # book: for a two-outcome market the quotes are published for the FIRST
    # token, so outcome 1 pays `bestAsk` and outcome 2 pays `1 - bestBid`.
    best_ask <- suppressWarnings(as.numeric(market$bestAsk))
    best_bid <- suppressWarnings(as.numeric(market$bestBid))
    asks <- if (is.finite(best_ask) && is.finite(best_bid)) c(best_ask, 1 - best_bid) else prices
    game <- slate[slate$slug == market$slug, ]
    if (nrow(game) != 1) return(empty[0, ])

    purrr::map_dfr(seq_along(outcomes), function(j) {
      # Outcomes are team NICKNAMES ("49ers", "Rams"); .poly_team_names holds the
      # full names, of which the nickname is the tail. Resolve by containment
      # against the two teams actually in this game rather than trusting outcome
      # order, so a reordered response cannot silently invert a price.
      cands <- c(as.character(game$home_team), as.character(game$away_team))
      hit <- cands[vapply(cands, function(tm)
        grepl(outcomes[j], .poly_team_names[[tm]] %||% "", fixed = TRUE), logical(1))]
      if (length(hit) != 1) return(empty[0, ])

      p_model <- if (hit == as.character(game$home_team)) as.numeric(game$pred_win_home)
                 else 1 - as.numeric(game$pred_win_home)
      price <- asks[j]
      if (!is.finite(price) || price <= 0 || price >= 1) return(empty[0, ])
      execution_price <- min(0.99, price + execution_buffer)

      tibble::tibble(
        season = as.integer(season), week = as.integer(week),
        game_id = as.character(game$game_id),
        bet_team = hit, opponent = setdiff(cands, hit)[1],
        question = as.character(market$question), slug = as.character(market$slug),
        market_id = as.character(market$id), model_probability = p_model,
        ask = price, execution_price = execution_price,
        raw_edge = p_model - price,
        # net_ev is already profit per dollar staked, but it was reported RAW
        # while the sportsbook card reports a calibrated figure. Left uncalibrated
        # it reads about 1.5x higher for identical edge, which is enough to make
        # Polymarket win every cross-platform comparison on scale alone.
        net_ev = calibrate_ev(p_model / execution_price - 1, ev_cal),
        liquidity = market$liquidity_num, volume = market$volume_num, fetched_at = fetched_at
      )
    })
  })

  if (nrow(rows) == 0) return(empty)
  # Shrink toward the market BEFORE scoring. `ask` carries the vig, so the two
  # sides of a game sum above 1; de-vig first or the shrinkage target is biased
  # high and every contract looks worse than it is.
  # `ask / sum(ask)` de-vigs only if BOTH sides of the game are present. When one
  # is filtered out above (price <= 0 or >= 1, which is what a lopsided book
  # produces) the survivor's market_p becomes exactly 1.0 by construction — a
  # fabricated certainty, not a market view. It then drags model_probability up
  # through the shrinkage below, inflating the very number being scored. The
  # symptom was a row reading `market_p 1` beside `ask 0.001`, which cannot both
  # be true. A game with one usable side cannot be de-vigged, so it is dropped.
  incomplete <- rows %>% dplyr::count(game_id) %>% dplyr::filter(n != 2)
  if (nrow(incomplete) > 0) {
    message(sprintf("  polymarket: %d game(s) had only one usable side — cannot de-vig, dropped",
                    nrow(incomplete)))
    rows <- dplyr::filter(rows, !game_id %in% incomplete$game_id)
  }
  if (nrow(rows) == 0) return(empty)
  rows <- rows %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(market_p = ask / sum(ask)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      model_probability_raw = model_probability,
      model_probability = .poly_inv_logit(
        .poly_logit(market_p) +
          model_weight * (.poly_logit(model_probability_raw) - .poly_logit(market_p))),
      raw_edge = model_probability - ask,
      ev_raw   = model_probability / execution_price - 1,
      net_ev   = calibrate_ev(ev_raw, ev_cal)
    )
  rows %>%
    dplyr::filter(is.finite(net_ev)) %>%
    # An EV outside the calibration's domain is a statement about the PRICE, not
    # about the size of the edge: it takes a near-zero denominator to get there.
    # Such a row is reported (so it stays visible) but never qualifies.
    # Was a hardcoded 0.05 while the sportsbook card and kalshi_lib.R both read
    # PORTFOLIO_DEFAULTS$min_ev — so the three venues silently applied different
    # bars, and re-sweeping the threshold moved two of them and not this one.
    dplyr::mutate(qualifies = net_ev >= min_ev & ev_raw_in_domain(ev_raw, ev_cal)) %>%
    dplyr::arrange(dplyr::desc(qualifies), dplyr::desc(net_ev), dplyr::desc(liquidity)) %>%
    dplyr::distinct(game_id, bet_team, .keep_all = TRUE)
}
