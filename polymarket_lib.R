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
  model_probability = numeric(), ask = numeric(), execution_price = numeric(), raw_edge = numeric(),
  net_ev = numeric(), qualifies = logical(), liquidity = numeric(), volume = numeric(), fetched_at = character()
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
fetch_polymarket_moneylines <- function(predictions, season, week,
                                        min_liquidity = 1000, execution_buffer = 0.01) {
  fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  empty <- .poly_empty()
  if (nrow(predictions) == 0) return(empty)
  markets <- tryCatch(
    jsonlite::fromJSON("https://gamma-api.polymarket.com/markets?limit=1000&active=true&closed=false&tag_id=450"),
    error = function(e) NULL
  )
  if (!is.data.frame(markets) || nrow(markets) == 0) return(empty)
  # Gamma omits all-null optional fields from a response. Without a gameId and
  # sports market type there is no safe way to distinguish a game winner from a
  # team future, so return no recommendation rather than guessing from wording.
  required <- c("question", "gameId", "sportsMarketType", "liquidity", "volume", "bestAsk")
  if (!all(required %in% names(markets))) return(empty)
  markets <- tibble::as_tibble(markets) %>%
    mutate(
      question_lc = tolower(question %||% ""),
      liquidity_num = suppressWarnings(as.numeric(liquidity)),
      volume_num = suppressWarnings(as.numeric(volume)),
      ask_num = suppressWarnings(as.numeric(bestAsk)),
      # Game markets have a canonical game id and a moneyline market type.
      is_game_moneyline = !is.na(gameId) & gameId != "" &
        (tolower(sportsMarketType %||% "") %in% c("moneyline", "winner", "game winner"))
    ) %>%
    filter(is_game_moneyline, liquidity_num >= min_liquidity, is.finite(ask_num), ask_num > 0, ask_num < 1)
  if (nrow(markets) == 0) return(empty)

  candidates <- purrr::map_dfr(seq_len(nrow(predictions)), function(i) {
    game <- predictions[i, ]
    teams <- c(as.character(game$home_team), as.character(game$away_team))
    probs <- c(as.numeric(game$pred_win_home), 1 - as.numeric(game$pred_win_home))
    purrr::map_dfr(seq_along(teams), function(j) {
      team_name <- tolower(.poly_team_names[[teams[j]]] %||% "")
      if (!nzchar(team_name)) return(.poly_empty()[0, ])
      # For standard game markets, the first YES contract asks whether this
      # specific team wins. Ignore non-game player props/futures above.
      hits <- markets %>% filter(stringr::str_detect(question_lc, stringr::fixed(team_name)))
      if (nrow(hits) == 0) return(.poly_empty()[0, ])
      purrr::map_dfr(seq_len(nrow(hits)), function(k) {
        market <- hits[k, ]
        outcomes <- .poly_json(market$outcomes)
        if (length(outcomes) < 2 || tolower(outcomes[1]) != "yes") return(.poly_empty()[0, ])
        execution_price <- min(0.99, market$ask_num + execution_buffer)
        tibble::tibble(
          season = as.integer(season), week = as.integer(week), game_id = as.character(game$game_id),
          bet_team = teams[j], opponent = teams[3 - j], question = market$question,
          slug = market$slug, market_id = as.character(market$id), model_probability = probs[j],
          ask = market$ask_num, execution_price = execution_price,
          raw_edge = probs[j] - market$ask_num, net_ev = probs[j] / execution_price - 1,
          liquidity = market$liquidity_num, volume = market$volume_num, fetched_at = fetched_at
        )
      })
    })
  })
  candidates %>%
    filter(is.finite(net_ev)) %>%
    mutate(qualifies = net_ev >= 0.05) %>%
    arrange(desc(qualifies), desc(net_ev), desc(liquidity)) %>%
    distinct(game_id, bet_team, .keep_all = TRUE)
}
