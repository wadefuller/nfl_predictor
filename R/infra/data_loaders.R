# Upstream data loaders — games (habitatring), PBP, contracts/rosters.
# These are shared by multiple feature modules; build_features.R loads
# them once and passes via `deps` to the registry.
#
# Requires:
#   - team_normalize.R (normalize_team_names + TEAM_REPLACEMENTS)
#   - SEASONS module-level vector (build_features.R defines it)

suppressPackageStartupMessages(library(nflreadr))

# habitatring's games.rds is a RAW http read, not an nflreadr call, so none of
# the nflverse-shaped guards in this file apply to it. A transient blip raises a
# bare connection error and takes the entire run down: this is precisely how the
# 2026-09-02 retrain died ("URL 'http://www.habitatring.com/games.rds': Timeout
# of 60 seconds was reached" -> Execution halted), on a host that served the same
# file in 0.5s when re-tested. One retry would have saved that run. Also assert
# the columns callers immediately filter on, so a truncated or reshaped payload
# names itself here instead of surfacing as "object 'season' not found" further
# downstream.
#
# NOTE: nfl_predictor/build_elo.R reads the same URL and carries its own copy of
# this guard — it does not source this file. Change both together.
GAMES_RDS_URL <- "http://www.habitatring.com/games.rds"

fetch_games_rds <- function(attempts = 3,
                            need = c("season", "game_id", "home_team", "away_team")) {
  for (attempt in seq_len(attempts)) {
    g <- try(readRDS(url(GAMES_RDS_URL)), silent = TRUE)
    if (!inherits(g, "try-error") && NROW(g) > 0 && all(need %in% names(g))) return(g)
    why <- if (inherits(g, "try-error")) {
      trimws(conditionMessage(attr(g, "condition")))
    } else {
      sprintf("%d rows; missing column(s): %s", NROW(g),
              paste(setdiff(need, names(g)), collapse = ", "))
    }
    message(sprintf("  ! games.rds attempt %d/%d unusable (%s); retrying", attempt, attempts, why))
    Sys.sleep(2 * attempt)
  }
  stop(sprintf("games.rds unusable after %d attempts — habitatring.com unreachable, or the schema changed (need: %s)",
               attempts, paste(need, collapse = ", ")))
}

load_game_data <- function(seasons = SEASONS) {
  message("Loading game data from habitatring.com...")
  fetch_games_rds() %>%
    filter(season >= min(seasons)) %>%
    normalize_team_names()
}

load_pbp_data <- function(seasons = SEASONS) {
  pbp_max     <- min(max(seasons), nflreadr::most_recent_season())
  pbp_seasons <- min(seasons):pbp_max
  message(sprintf("Loading PBP for seasons %d-%d (this can take a few minutes)...",
                  min(pbp_seasons), max(pbp_seasons)))
  # ONE SEASON AT A TIME, with retries and a coverage assert. A bulk
  # nflreadr::load_pbp(seasons=...) request has silently returned a subset of
  # seasons elsewhere in this stack while every season loads fine individually
  # (load_ff_opportunity(2011:2025) -> 2012-2023 only) — and a timed-out read
  # returns an EMPTY frame rather than erroring, which then surfaces many steps
  # downstream as a nonsense "column not found" instead of naming the load that
  # actually failed. This feeds every EPA-based feature the model has.
  pbp <- dplyr::bind_rows(lapply(pbp_seasons, function(s) {
    p <- NULL
    for (attempt in 1:3) {
      p <- try(nflreadr::load_pbp(seasons = s), silent = TRUE)
      if (!inherits(p, "try-error") && NROW(p) > 0) break
      message(sprintf("  ! pbp %d attempt %d unusable; retrying", s, attempt))
      p <- NULL; Sys.sleep(2 * attempt)
    }
    if (is.null(p))
      stop(sprintf("load_pbp(%d) returned no usable data after 3 attempts (network, or a timed-out read)", s))
    p
  }))
  got <- sort(unique(as.integer(pbp$season)))
  missing <- setdiff(pbp_seasons, got)
  if (length(missing))
    stop(sprintf("pbp missing season(s) after per-season fetch: %s", paste(missing, collapse = ", ")))
  message(sprintf("  Loaded %s plays.", format(nrow(pbp), big.mark = ",")))

  pbp %>%
    filter(
      !is.na(epa), !is.na(ep), !is.na(posteam), !is.na(down),
      play_type %in% c("pass", "run") | penalty == 1,
      qb_kneel != 1
    ) %>%
    normalize_team_names()
}

load_player_cap_by_season <- function(seasons) {
  con <- expect_frame(nflreadr::load_contracts(),
                      cols = c("gsis_id", "year_signed", "years", "apy", "apy_cap_pct"),
                      label = "load_contracts()") %>%
    filter(!is.na(gsis_id), !is.na(year_signed), !is.na(years))

  map_dfr(seasons, function(s) {
    con %>%
      filter(year_signed <= s, year_signed + years > s) %>%
      group_by(gsis_id) %>%
      slice_max(year_signed, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(gsis_id, season = s, apy, apy_cap_pct)
  })
}

build_team_cap_aggregates <- function(seasons, player_cap) {
  ros_seasons <- intersect(seasons, 2002:min(max(seasons), nflreadr::most_recent_season()))
  # Same bulk-multi-season risk as load_pbp_data: per-season, with retries and
  # a coverage assert, instead of one load_rosters(ros_seasons) call.
  ros <- dplyr::bind_rows(lapply(ros_seasons, function(s) {
    r <- NULL
    for (attempt in 1:3) {
      r <- try(nflreadr::load_rosters(s), silent = TRUE)
      if (!inherits(r, "try-error") && NROW(r) > 0 &&
          all(c("season", "gsis_id", "team") %in% names(r))) break
      message(sprintf("  ! rosters %d attempt %d unusable; retrying", s, attempt))
      r <- NULL; Sys.sleep(2 * attempt)
    }
    if (is.null(r))
      stop(sprintf("load_rosters(%d) returned no usable data after 3 attempts", s))
    # nflverse roster schemas vary by season (for example jersey_number is an
    # integer in some snapshots and character in others). Keep only the stable
    # keys before bind_rows so an unrelated historical column cannot break the
    # entire current-week refresh.
    r %>% select(season, gsis_id, team)
  })) %>%
    distinct(season, gsis_id, team)
  missing <- setdiff(ros_seasons, unique(ros$season))
  if (length(missing))
    stop(sprintf("rosters missing season(s) after per-season fetch: %s", paste(missing, collapse = ", ")))

  player_cap %>%
    inner_join(ros, by = c("season", "gsis_id")) %>%
    group_by(season, team) %>%
    summarise(
      team_total_apy     = sum(apy, na.rm = TRUE),
      top5_apy_threshold = ifelse(n() >= 5,
                                  sort(apy, decreasing = TRUE)[5],
                                  min(apy, na.rm = TRUE)),
      .groups = "drop"
    )
}
