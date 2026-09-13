# ============================================
# Fantasy: Export Market Index + League Data to JSON for Website
# ============================================
# Two data sources, one output tree:
#   1. FantasyCalc public API -> market values per league format (the "index")
#   2. ffscrapr -> live league state for each league in fantasy_leagues.json
#      (MFL and Sleeper both go through the same ff_* verbs)
#
# Values are joined onto rosters so every franchise can be scored on market
# value, and - for salary-cap leagues - on value returned per cap dollar.
#
# Usage: Rscript export_fantasy.R
# ============================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(ffscrapr)
  library(glue)
})

PROJECT_DIR  <- path.expand("~/Projects/R/nfl_predictor")
OUTPUT_DIR   <- file.path(PROJECT_DIR, "website", "public", "data", "fantasy")
SNAPSHOT_DIR <- file.path(PROJECT_DIR, "data", "fantasy_snapshots")
CACHE_DIR    <- file.path(PROJECT_DIR, "data", "fantasy_cache")
CONFIG_PATH  <- file.path(PROJECT_DIR, "fantasy_leagues.json")
source("/Users/wadefuller/Projects/R/shared/espn.R")   # espn_auth(), ff_connect_auth()
source("/Users/wadefuller/Projects/R/shared/contracts.R")  # .contract_fail(); CONTRACTS_WARN=1 downgrades to warnings

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SNAPSHOT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# MFL rate-limits aggressively and bans for a while once you trip it, so every
# league call goes through a disk cache. Re-running the export to iterate on the
# website costs zero API calls until the TTL expires.
# Force a refresh with: FANTASY_TTL_HOURS=0 Rscript export_fantasy.R
CACHE_TTL_HOURS <- suppressWarnings(as.numeric(Sys.getenv("FANTASY_TTL_HOURS", "6")))
if (is.na(CACHE_TTL_HOURS)) CACHE_TTL_HOURS <- 6

cached <- function(key, expr) {
  path <- file.path(CACHE_DIR, paste0(key, ".rds"))
  if (file.exists(path)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "hours"))
    if (age < CACHE_TTL_HOURS) {
      cat(sprintf("  . %s (cached %.1fh)\n", key, age))
      return(readRDS(path))
    }
  }
  val <- force(expr)
  if (!is.null(val)) saveRDS(val, path)
  val
}

# jsonlite renders a NULL field as {} rather than [], so any array-shaped field
# that came back empty reaches the browser as an object and throws the moment a
# component calls .map on it — a blank tab with no server-side error. This is the
# same shape bug that blanked the site once already this cycle, and it recurs
# whenever a platform is missing an endpoint: ESPN has no readable transactions
# feed through ffscrapr, so `transactions` is NULL on every ESPN league.
# Everything the website iterates is forced to an array before it is written.
JSON_ARRAY_FIELDS <- c("franchises", "roster", "transactions", "history",
                       "all_time", "pick_capital")

as_json_arrays <- function(x, fields = JSON_ARRAY_FIELDS) {
  for (f in fields) if (f %in% names(x) && is.null(x[[f]])) x[[f]] <- list()
  x
}

write_json_file <- function(x, ...) {
  path <- file.path(...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(x, path, auto_unbox = TRUE, na = "null", digits = 4)
  cat(sprintf("  wrote %s (%.0f KB)\n",
              sub(paste0(OUTPUT_DIR, "/"), "", path, fixed = TRUE),
              file.size(path) / 1024))
}

# ---- Market formats ---------------------------------------------------------
# numTeams/ppr are matched to how the format is actually played, so the values
# reflect the scarcity of the league you care about rather than a generic index.
FORMATS <- list(
  list(key = "dynasty_sf",   label = "Dynasty Superflex", dynasty = TRUE,  qbs = 2, teams = 10, ppr = 1),
  list(key = "dynasty_1qb",  label = "Dynasty 1QB",       dynasty = TRUE,  qbs = 1, teams = 12, ppr = 1),
  list(key = "redraft_sf",   label = "Redraft Superflex", dynasty = FALSE, qbs = 2, teams = 12, ppr = 1),
  list(key = "redraft_ppr",  label = "Redraft PPR",       dynasty = FALSE, qbs = 1, teams = 12, ppr = 1),
  list(key = "redraft_half", label = "Redraft Half PPR",  dynasty = FALSE, qbs = 1, teams = 12, ppr = 0.5),
  list(key = "redraft_std",  label = "Redraft Standard",  dynasty = FALSE, qbs = 1, teams = 12, ppr = 0)
)

# ---- My season-model rankings ------------------------------------------------
# The market index is a CONSENSUS price. Carrying my model's rank alongside it
# turns the tab into a disagreement view: where the market and my projection part
# company is where a draft edge lives.
#
# Joined on positional rank, NOT overall rank. The two universes aren't
# comparable overall — dynasty formats price rookie PICKs my board doesn't
# contain, and each source ranks a different-sized player pool — but "WR7" means
# the same thing in both. Sign convention matches the vs-ECR tab:
#   my_delta = market pos_rank - my pos_rank   (+ve = I'm higher than the market)
FDB_DIR <- path.expand("~/Projects/R/fantasy_db")
# nflreadr::clean_player_names() only strips a generational suffix when it carries
# a trailing period ("Jr." -> dropped, "Jr" -> kept). FantasyCalc writes them
# without one while nflverse writes them with, so every Jr/II/III player silently
# failed to join (Marvin Harrison Jr was market WR31 and matched nothing). Strip
# suffixes and punctuation ourselves so both sides land on the same key.
clean_nm <- function(x) {
  x %>%
    nflreadr::clean_player_names(lowercase = TRUE) %>%
    str_replace_all("[.'`]", "") %>%
    str_remove("\\s+(jr|sr|ii|iii|iv|v)$") %>%
    str_squish()
}

# Each market format is priced at its own reception value, so pair it with the
# board trained on that scoring rather than comparing across formats.
scoring_for <- function(spec) {
  if (spec$ppr >= 1) "ppr" else if (spec$ppr > 0) "half" else "std"
}

# Value over replacement, recomputed per market format.
#
# Positional rank alone can't be compared across positions — WR7 and QB7 are not
# the same asset — but the market's value IS a single cross-position currency.
# VOR is the model's equivalent currency, so it's the like-for-like column.
#
# The board's own `vor` column is fixed at a 12-team 1QB league, which is wrong
# for most of these formats. Replacement depth scales with roster demand:
#   QB  teams x 1, or x 2 in superflex (a second QB is a starter, so the
#       replacement-level QB sits ~24th rather than ~12th, which is exactly why
#       a 1QB baseline understates every superflex QB)
#   RB  teams x 2.5   (2 starters + flex share)
#   WR  teams x 3     (2 starters + flex share)
#   TE  teams x 1
repl_ranks <- function(spec) list(
  QB = round(spec$teams * if (spec$qbs >= 2) 2 else 1),
  RB = round(spec$teams * 2.5),
  WR = round(spec$teams * 3),
  TE = round(spec$teams * 1)
)

# `board` is the FULL model board (~915 players), not the joined market rows:
# replacement level is a property of the whole player pool, and the market list
# is truncated at 200, which would put "replacement" far too high.
add_vor <- function(df, spec, board) {
  rr <- repl_ranks(spec)
  base <- board %>%
    filter(!is.na(my_pts), pos %in% names(rr)) %>%
    group_by(pos) %>%
    summarise(repl = {
      v <- sort(my_pts, decreasing = TRUE)
      v[min(rr[[first(pos)]], length(v))]
    }, .groups = "drop")
  df %>%
    left_join(base, by = "pos") %>%
    mutate(my_vor = ifelse(is.na(my_pts) | is.na(repl), NA_real_, round(my_pts - repl))) %>%
    # 0-100 on the same footing as the market's own index column, so the two
    # scales can be read side by side instead of points vs trade value.
    mutate(my_index = {
      top <- suppressWarnings(max(my_vor, na.rm = TRUE))
      if (!is.finite(top) || top <= 0) NA_real_ else round(100 * my_vor / top, 1)
    }) %>%
    select(-repl, -my_pts)
}

my_ranks <- function(scoring) {
  suffix <- if (identical(scoring, "ppr")) "" else paste0("_", scoring)
  # Newest forward/season board for this scoring, else the unsuffixed default.
  cands <- list.files(FDB_DIR, full.names = TRUE,
    pattern = sprintf("^season_model_rankings_\\d{4}%s\\.csv$", suffix))
  path <- if (length(cands)) cands[order(basename(cands), decreasing = TRUE)][1]
          else file.path(FDB_DIR, sprintf("season_model_rankings%s.csv", suffix))
  if (!file.exists(path)) {
    cat(sprintf("  ! no %s rankings board — market rows keep NA model ranks\n", scoring))
    return(NULL)
  }
  board <- readr::read_csv(path, show_col_types = FALSE) %>%
    filter(position %in% c("QB", "RB", "WR", "TE")) %>%
    mutate(cn = clean_nm(player_display_name), pos = position) %>%
    # One row per (name, pos): predict_forward's ADP join can fan a player out,
    # and a duplicate here would fan out the market table on join.
    group_by(cn, pos) %>%
    slice_min(pos_rank, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(cn, pos,
              gsis_id = if ("player_id" %in% names(.)) player_id else NA_character_,
              my_pos_rank = as.integer(pos_rank),
              my_overall  = as.integer(overall_rank),
              # rank_target = "ppg": pred_ppr_total holds the per-game rate.
              my_proj     = round(pred_ppr_total, 1),
              my_pts      = pred_ppr_total,   # kept raw for per-format VOR
              my_games    = suppressWarnings(as.numeric(pred_games)),
              my_tier     = as.integer(tier))
  cat(sprintf("  . my %-4s board: %s (%d players)\n", scoring, basename(path), nrow(board)))
  list(board = board, src = basename(path))
}

# nflverse id crosswalk: FantasyCalc hands us sleeper/mfl ids, my board carries
# gsis. Matching on ids first means nicknames and spelling drift can't silently
# drop a player; the cleaned-name join is only the fallback for rows the
# crosswalk doesn't cover.
XWALK_FAIL <- NULL   # reason string when the crosswalk could not be built
XWALK <- local({
  need <- c("gsis_id", "sleeper_id", "mfl_id", "espn_id")
  # The whole build sits inside the handler, not just the fetch. nflreadr does
  # NOT error on a failed download — it returns a 0-column tibble — so it was the
  # transmute below that threw ("object 'gsis_id' not found"), one line OUTSIDE
  # the old tryCatch, and that killed the entire export on 2026-09-04 and 09-05.
  # Assert the columns first so a truncated feed degrades to NULL like a genuine
  # network error does, instead of aborting the script.
  tryCatch({
    x <- nflreadr::load_ff_playerids()
    if (!is.data.frame(x) || nrow(x) == 0)
      stop("load_ff_playerids() returned ",
           if (is.data.frame(x)) "0 rows" else "no data frame", call. = FALSE)
    miss <- setdiff(need, names(x))
    if (length(miss))
      stop("load_ff_playerids() is missing column(s): ",
           paste(miss, collapse = ", "), call. = FALSE)
    x %>%
      transmute(gsis_id, sleeper_id = as.character(sleeper_id),
                mfl_id = as.character(mfl_id), espn_id = as.character(espn_id)) %>%
      filter(!is.na(gsis_id))
  }, error = function(e) {
    XWALK_FAIL <<- conditionMessage(e)
    NULL
  })
})
if (is.null(XWALK))
  cat(sprintf(paste0(
    "  !! WARNING: nflverse id crosswalk unavailable (%s)\n",
    "  !! market joins fall back to cleaned-name matching only, and every ESPN\n",
    "  !! league joins on nothing (espn_id is derived from this crosswalk).\n"),
    XWALK_FAIL %||% "unknown"))

attach_my_ranks <- function(idx, spec, boards) {
  b <- boards[[scoring_for(spec)]]
  if (is.null(b)) {
    return(idx %>% mutate(my_pos_rank = NA_integer_, my_overall = NA_integer_,
                          my_proj = NA_real_, my_games = NA_real_, my_tier = NA_integer_,
                          my_delta = NA_integer_, my_vor = NA_real_,
                          my_index = NA_real_))
  }
  cols <- c("my_pos_rank", "my_overall", "my_proj", "my_pts", "my_games", "my_tier")

  # pass 1 - id join
  by_id <- if (!is.null(XWALK) && any(!is.na(b$board$gsis_id))) {
    idx %>%
      left_join(XWALK %>% distinct(sleeper_id, .keep_all = TRUE) %>% select(sleeper_id, gsis_sl = gsis_id),
                by = "sleeper_id") %>%
      left_join(XWALK %>% distinct(mfl_id, .keep_all = TRUE) %>% select(mfl_id, gsis_mfl = gsis_id),
                by = "mfl_id") %>%
      mutate(gsis_id = coalesce(gsis_sl, gsis_mfl)) %>%
      select(-gsis_sl, -gsis_mfl) %>%
      left_join(b$board %>% filter(!is.na(gsis_id)) %>%
                  distinct(gsis_id, .keep_all = TRUE) %>% select(gsis_id, all_of(cols)),
                by = "gsis_id") %>%
      select(-gsis_id)
  } else {
    # Every my_* column goes to NA here, which makes pass 2 (cleaned names) the
    # ONLY matcher — exactly the drift the id join exists to absorb. Silently
    # this reads as a normal run with a slightly lower match count, so name the
    # reason: without it a dead crosswalk is indistinguishable from a few
    # players legitimately missing from the board.
    cat(sprintf("  !! WARNING %s: id join skipped (%s) — market match is name-only\n",
                spec$key,
                if (is.null(XWALK)) paste0("crosswalk unavailable: ", XWALK_FAIL %||% "unknown")
                else "model board carries no gsis_id"))
    idx %>% mutate(my_pos_rank = NA_integer_, my_overall = NA_integer_, my_games = NA_real_,
                   my_proj = NA_real_, my_pts = NA_real_, my_tier = NA_integer_)
  }
  n_id <- sum(!is.na(by_id$my_pos_rank))

  # pass 2 - cleaned-name fallback for whatever the crosswalk missed
  nm <- b$board %>% select(cn, pos, all_of(cols)) %>% rename_with(~paste0(.x, "_nm"), all_of(cols))
  out <- by_id %>%
    mutate(cn = clean_nm(name)) %>%
    left_join(nm, by = c("cn", "pos")) %>%
    mutate(across(all_of(cols), ~coalesce(.x, get(paste0(cur_column(), "_nm"))))) %>%
    select(-cn, -ends_with("_nm")) %>%
    mutate(my_delta = ifelse(is.na(my_pos_rank), NA_integer_,
                             as.integer(pos_rank - my_pos_rank))) %>%
    add_vor(spec, b$board)
  stopifnot(nrow(out) == nrow(idx))   # join must not fan out
  matched <- sum(!is.na(out$my_pos_rank))
  cat(sprintf("  . %-14s matched %d/%d to my %s board (%d by id, %d by name)\n",
              spec$key, matched, sum(idx$pos != "PICK"), scoring_for(spec),
              n_id, matched - n_id))
  out
}

fetch_fantasycalc <- function(spec) {
  url <- glue(
    "https://api.fantasycalc.com/values/current",
    "?isDynasty={tolower(as.character(spec$dynasty))}",
    "&numQbs={spec$qbs}&numTeams={spec$teams}&ppr={spec$ppr}"
  )
  raw <- fromJSON(url, flatten = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0) stop("empty response for ", spec$key)

  # The API returns a flat frame with player.* prefixes; maybe* fields are
  # optional, so pull them defensively rather than assuming presence.
  col <- function(name, default = NA) if (name %in% names(raw)) raw[[name]] else default

  tibble(
    player_id_fc = col("player.id"),
    name         = col("player.name"),
    pos          = col("player.position"),
    team         = col("player.maybeTeam"),
    age          = suppressWarnings(as.numeric(col("player.maybeAge"))),
    exp          = suppressWarnings(as.numeric(col("player.maybeYoe"))),
    mfl_id       = as.character(col("player.mflId")),
    sleeper_id   = as.character(col("player.sleeperId")),
    value        = suppressWarnings(as.numeric(col("value"))),
    overall_rank = suppressWarnings(as.integer(col("overallRank"))),
    pos_rank     = suppressWarnings(as.integer(col("positionRank"))),
    tier         = suppressWarnings(as.integer(col("maybeTier"))),
    trend30      = suppressWarnings(as.numeric(col("trend30Day"))),
    redraft_value = suppressWarnings(as.numeric(col("redraftValue")))
  ) %>%
    filter(!is.na(value), value > 0)
}

build_market_index <- function(df, spec) {
  top <- max(df$value, na.rm = TRUE)
  # ESPN id is not in the vendor feed (FantasyCalc ships mflId and sleeperId
  # only), so it is derived from the crosswalk: sleeper_id first because its
  # coverage is better, then mfl_id for anyone that misses. Without this an ESPN
  # league joins on nothing and every market value comes back NA.
  df <- if (is.null(XWALK)) {
    df %>% mutate(espn_id = NA_character_)
  } else {
    df %>%
      left_join(XWALK %>% filter(!is.na(sleeper_id), sleeper_id != "") %>%
                  distinct(sleeper_id, .keep_all = TRUE) %>%
                  select(sleeper_id, espn_sl = espn_id), by = "sleeper_id") %>%
      left_join(XWALK %>% filter(!is.na(mfl_id), mfl_id != "") %>%
                  distinct(mfl_id, .keep_all = TRUE) %>%
                  select(mfl_id, espn_mf = espn_id), by = "mfl_id") %>%
      mutate(espn_id = coalesce(espn_sl, espn_mf)) %>%
      select(-espn_sl, -espn_mf)
  }
  df %>%
    mutate(
      # 0-100 index, linear in value so the numbers stay additive: two players
      # summing to 60 really are worth one player at 60.
      index      = round(100 * value / top, 1),
      prev_value = value - coalesce(trend30, 0),
      trend_pct  = ifelse(prev_value > 0, round(100 * trend30 / prev_value, 1), NA_real_),
      is_pick    = pos == "PICK",
      format     = spec$key
    ) %>%
    arrange(desc(value))
}

# Append a dated row per player so we can build our own longitudinal trend
# instead of relying only on the vendor's 30-day number.
append_snapshot <- function(df, spec, stamp) {
  path <- file.path(SNAPSHOT_DIR, paste0(spec$key, ".csv"))
  snap <- df %>%
    transmute(date = stamp, player_id_fc, name, pos, value, overall_rank)
  if (file.exists(path)) {
    # Read date as text so re-runs don't collide with readr's Date guess.
    prior <- suppressMessages(read_csv(path, show_col_types = FALSE,
                                       col_types = cols(date = col_character()))) %>%
      mutate(date = as.character(date)) %>%
      filter(date != stamp)
    snap <- bind_rows(prior, snap)
  }
  write_csv(snap, path)
  invisible(path)
}

cat("== Market values ==\n")
stamp <- format(Sys.Date())
# Load each scoring board once and reuse across formats that share it.
MY_BOARDS <- set_names(map(c("ppr", "half", "std"), my_ranks), c("ppr", "half", "std"))
market <- list()
for (spec in FORMATS) {
  res <- tryCatch({
    idx <- build_market_index(fetch_fantasycalc(spec), spec) %>%
      attach_my_ranks(spec, MY_BOARDS)
    append_snapshot(idx, spec, stamp)
    cat(sprintf("  %-14s %3d players/picks, top=%s\n", spec$key, nrow(idx), idx$name[1]))
    idx
  }, error = function(e) {
    cat(sprintf("  %-14s FAILED: %s\n", spec$key, conditionMessage(e)))
    NULL
  })
  if (!is.null(res)) market[[spec$key]] <- res
}
if (length(market) == 0) stop("No market formats fetched; aborting.")

for (spec in FORMATS) {
  idx <- market[[spec$key]]
  if (is.null(idx)) next
  write_json_file(
    list(
      format    = spec$key,
      label     = spec$label,
      dynasty   = spec$dynasty,
      qbs       = spec$qbs,
      teams     = spec$teams,
      ppr       = spec$ppr,
      generated = stamp,
      players   = idx %>% select(-prev_value, -format)
    ),
    OUTPUT_DIR, "market", paste0(spec$key, ".json")
  )
}

# ---- Movers ----------------------------------------------------------------
# Risers/fallers use percentage move with a value floor, so a deep bench player
# ticking from 20 -> 40 doesn't outrank a real market shift at the top.
movers_for <- function(idx, n = 15, min_value = 300) {
  base <- idx %>%
    filter(!is.na(trend_pct), value >= min_value) %>%
    select(name, pos, team, value, index, tier, overall_rank, pos_rank, trend30, trend_pct)
  list(
    risers  = base %>% arrange(desc(trend_pct)) %>% head(n),
    fallers = base %>% arrange(trend_pct) %>% head(n)
  )
}

write_json_file(
  list(
    generated = stamp,
    formats = map(FORMATS, function(spec) {
      idx <- market[[spec$key]]
      if (is.null(idx)) return(NULL)
      c(list(format = spec$key, label = spec$label), movers_for(idx))
    }) %>% compact()
  ),
  OUTPUT_DIR, "movers.json"
)

# ---- Leagues ---------------------------------------------------------------
config <- fromJSON(CONFIG_PATH, simplifyDataFrame = FALSE)
leagues <- keep(config$leagues, ~ isTRUE(.x$enabled))

# A missing config key yields NULL -> as.numeric(NULL) is numeric(0), which makes
# any downstream `if (n <= 0)` throw "argument is of length zero". Collapse the
# empty case to a scalar 0 so absent lineup slots simply mean "none".
num0 <- function(x) {
  v <- coalesce(suppressWarnings(as.numeric(x)), 0)
  if (length(v) == 0) 0 else v[1]
}

# num0() deliberately collapses to a scalar, which is right for config values
# (lineup slot counts) and catastrophic inside mutate(): it recycles the first
# row's value across the whole column. Use this for columns.
num_col <- function(x) coalesce(suppressWarnings(as.numeric(x)), 0)

# MFL returns "Last, First"; everything else (and every UI) wants "First Last".
format_name <- function(x) {
  ifelse(str_detect(x, ","),
         str_trim(paste(str_trim(str_extract(x, "(?<=,).*")),
                        str_trim(str_extract(x, "^[^,]+")))),
         x)
}

# Value of the lineup a franchise can actually start, which is what dynasty
# depth charts are really worth. FLEX takes the best remaining RB/WR/TE,
# SUPERFLEX the best remaining QB/RB/WR/TE.
starter_value <- function(players, lineup) {
  if (is.null(lineup) || nrow(players) == 0) return(NA_real_)
  pool <- players %>% filter(!is.na(value), pos %in% c("QB", "RB", "WR", "TE")) %>% arrange(desc(value))
  used <- rep(FALSE, nrow(pool))
  take <- function(eligible, n) {
    n <- num0(n)                       # tolerate NULL/NA slot counts
    if (!is.finite(n) || n <= 0) return(0)
    idx <- which(!used & pool$pos %in% eligible)
    idx <- head(idx, n)
    used[idx] <<- TRUE
    sum(pool$value[idx])
  }
  total <- 0
  for (p in c("QB", "RB", "WR", "TE")) total <- total + take(p, num0(lineup[[p]]))
  total <- total + take(c("RB", "WR", "TE"), num0(lineup$FLEX))
  total <- total + take(c("QB", "RB", "WR", "TE"), num0(lineup$SUPERFLEX))
  round(total)
}

# ---- Depth-aware power rankings ---------------------------------------------
#
# The `starter_value` column above answers "how good is the best lineup you can
# field", which silently assumes everyone plays every week. Nobody does: the
# board projects 7-10 games for a typical starter, so a top-heavy roster bleeds
# once absences land and a deep one barely notices.
#
# This simulates it. Each week every player is available with probability
# games/17, the best legal lineup is filled from whoever is up, and the season
# is summed. Averaged over many seasons that yields:
#   expected  mean season - the headline, depth already priced in
#   floor     10th percentile - how bad a bad-luck year gets
#   stress    season with the two best starters gone all year
#   spare     startable bodies beyond what the lineup demands
WEEKS <- 17L
N_SIMS <- 300L

# Best legal lineup from (pos, points) vectors. Same slot order as
# starter_value(): fixed positions, then FLEX, then SUPERFLEX.
fill_lineup <- function(pos, pts, lineup) {
  if (length(pts) == 0) return(0)
  ord  <- order(pts, decreasing = TRUE)
  pos  <- pos[ord]; pts <- pts[ord]
  used <- logical(length(pts))
  take <- function(eligible, n) {
    n <- num0(n)
    if (!is.finite(n) || n <= 0) return(0)
    idx <- head(which(!used & pos %in% eligible), n)
    used[idx] <<- TRUE
    sum(pts[idx])
  }
  tot <- 0
  for (p in c("QB", "RB", "WR", "TE")) tot <- tot + take(p, lineup[[p]])
  tot <- tot + take(c("RB", "WR", "TE"), lineup$FLEX)
  tot <- tot + take(c("QB", "RB", "WR", "TE"), lineup$SUPERFLEX)
  tot
}

# "Startable" = would start somewhere in this league, so it scales with size.
startable_cut <- function(spec) list(
  QB = round(spec$teams * if (spec$qbs >= 2) 2.4 else 1.4),
  RB = round(spec$teams * 3),
  WR = round(spec$teams * 3.5),
  TE = round(spec$teams * 1.4)
)

power_rankings <- function(roster, lineup, spec, seed = 42L) {
  # roster_status is MFL-only; Sleeper exports have no such column, and taxi
  # players can't be started so they must not count toward weekly lineups.
  act <- roster %>% filter(!is.na(my_proj), pos %in% c("QB", "RB", "WR", "TE"))
  if ("roster_status" %in% names(act)) {
    act <- act %>% filter(is.na(roster_status) |
                            !toupper(roster_status) %in% c("TAXI_SQUAD", "TAXI", "IR", "INJURED_RESERVE"))
  }
  if (nrow(act) == 0) return(NULL)
  set.seed(seed)
  cut <- startable_cut(spec)
  need <- list(QB = num0(lineup$QB) + num0(lineup$SUPERFLEX),
               RB = num0(lineup$RB) + 1, WR = num0(lineup$WR) + 1, TE = num0(lineup$TE))

  act %>%
    group_by(franchise_id, franchise_name) %>%
    group_modify(function(g, ...) {
      games <- pmax(1, pmin(WEEKS, ifelse(is.na(g$my_games), WEEKS, g$my_games)))
      per   <- g$my_proj / games          # points per game actually played
      avail <- games / WEEKS
      healthy <- fill_lineup(g$pos, per, lineup) * WEEKS
      sims <- vapply(seq_len(N_SIMS), function(i) {
        sum(vapply(seq_len(WEEKS), function(w) {
          up <- runif(nrow(g)) < avail
          if (!any(up)) 0 else fill_lineup(g$pos[up], per[up], lineup)
        }, numeric(1)))
      }, numeric(1))
      top2 <- order(per, decreasing = TRUE)[seq_len(min(2, nrow(g)))]
      keep <- setdiff(seq_len(nrow(g)), top2)
      stress <- if (length(keep)) fill_lineup(g$pos[keep], per[keep], lineup) * WEEKS else 0
      depth <- vapply(c("QB", "RB", "WR", "TE"), function(p) {
        sum(g$pos == p & !is.na(g$my_pos_rank) & g$my_pos_rank <= cut[[p]])
      }, numeric(1))
      thin <- names(need)[vapply(names(need), function(p) depth[[p]] < num0(need[[p]]), logical(1))]
      tibble(
        pw_healthy  = round(healthy),
        pw_expected = round(mean(sims)),
        pw_floor    = round(as.numeric(quantile(sims, 0.10))),
        pw_stress   = round(stress),
        pw_stress_ratio = if (healthy > 0) round(stress / healthy, 3) else NA_real_,
        pw_depth_ratio  = if (healthy > 0) round(mean(sims) / healthy, 3) else NA_real_,
        pw_spare    = sum(pmax(0, unlist(depth) - unlist(need)[names(depth)])),
        pw_thin     = if (length(thin)) paste(thin, collapse = ", ") else NA_character_,
        pw_dep_QB = depth[["QB"]], pw_dep_RB = depth[["RB"]],
        pw_dep_WR = depth[["WR"]], pw_dep_TE = depth[["TE"]]
      )
    }) %>%
    ungroup() %>%
    mutate(pw_rank = rank(-pw_expected, ties.method = "min"))
}

pull_league <- function(cfg) {
  cat(sprintf("\n== League: %s (%s %s) ==\n", cfg$label, cfg$platform, cfg$league_id))
  # ff_connect_auth, not ff_connect: the generic constructor drops espn_s2/swid,
  # and a private ESPN league returns the same 404 as a nonexistent one.
  conn <- ff_connect_auth(cfg$platform, cfg$league_id, cfg$season)

  soft <- function(expr, what) tryCatch(expr, error = function(e) {
    cat(sprintf("  ! %s unavailable: %s\n", what, conditionMessage(e))); NULL
  })

  ck <- function(what) paste(cfg$key, cfg$season, what, sep = "_")
  rosters   <- cached(ck("rosters"),   soft(ff_rosters(conn), "rosters"))
  standings <- cached(ck("standings"), soft(ff_standings(conn), "standings"))
  txns      <- cached(ck("txns"),      soft(ff_transactions(conn), "transactions"))
  franch    <- cached(ck("franchises"),soft(ff_franchises(conn), "franchises"))
  picks     <- cached(ck("picks"),     soft(ff_draftpicks(conn), "draft picks"))

  if (is.null(rosters) || nrow(rosters) == 0) {
    cat("  ! no rosters; skipping league\n"); return(NULL)
  }

  # Join market values on the platform's own player id.
  fmt <- cfg$market_format %||% "dynasty_sf"
  idx <- market[[fmt]]
  if (is.null(idx)) idx <- market[[1]]
  # Each platform reports rosters under its OWN player id, so the market index
  # has to be keyed to match. FantasyCalc ships mflId and sleeperId but no ESPN
  # id, so espn_id is derived from the crosswalk further up.
  join_col <- switch(cfg$platform, sleeper = "sleeper_id", espn = "espn_id", "mfl_id")
  vals <- idx %>%
    filter(!is.na(.data[[join_col]]), .data[[join_col]] != "") %>%
    distinct(.data[[join_col]], .keep_all = TRUE) %>%
    transmute(player_id = as.character(.data[[join_col]]),
              value, index, tier, overall_rank, pos_rank, trend30, trend_pct,
              market_age = age,
              # Projected points + availability drive the depth-aware power model.
              my_proj, my_games, my_vor, my_pos_rank)

  # Platforms disagree on which roster columns exist. MFL and Sleeper both
  # report age and MFL reports salary; ESPN reports neither, so both are probed
  # rather than assumed — an unguarded reference fails the whole league with
  # "object 'age' not found" well after the data has been fetched.
  has_salary <- "salary" %in% names(rosters)
  has_age    <- "age"    %in% names(rosters)
  roster <- rosters %>%
    # franchise_id to character to match the franchises table, which is cast the
    # same way further down. MFL and Sleeper already hand back strings, but ESPN
    # returns integers, so the website's strict `p.franchise_id === fid` filter
    # matched nothing and every ESPN roster rendered as an empty table under a
    # correctly-populated header.
    mutate(franchise_id = as.character(franchise_id),
           player_id = as.character(player_id),
           age    = if (has_age)    suppressWarnings(as.numeric(age)) else NA_real_,
           salary = if (has_salary) num_col(salary) else NA_real_) %>%
    left_join(vals, by = "player_id") %>%
    mutate(
      # MFL leaves age blank for most rookies; the market feed has it.
      age = coalesce(suppressWarnings(as.numeric(age)), market_age),
      player_name = format_name(player_name),
      # Cap efficiency: market value bought per salary dollar. Undefined for
      # $0 players (they'd divide to Inf), which is the honest answer.
      value_per_dollar = ifelse(!is.na(salary) & salary > 0, round(value / salary, 1), NA_real_)
    ) %>%
    arrange(franchise_name, desc(value))

  # Per-franchise salaryCapAmount is blank on MFL when the cap is league-wide;
  # ff_league carries the real number.
  cap <- NA_real_
  if (!is.null(franch) && "salaryCapAmount" %in% names(franch)) {
    cap <- suppressWarnings(as.numeric(franch$salaryCapAmount[1]))
  }
  # ff_league()$salary_cap is a 0/1 "uses salaries" flag, not the amount - read
  # the amount off MFL's league endpoint directly.
  if (is.na(cap) && identical(cfg$platform, "mfl")) {
    cap <- cached(ck("cap"), soft({
      lg <- mfl_getendpoint(conn, "league")$content$league
      suppressWarnings(as.numeric(unlist(lg$salaryCapAmount)[1]))
    }, "league salary cap")) %||% NA_real_
  }
  if (length(cap) != 1 || is.na(cap) || cap <= 1) cap <- NA_real_

  franchises <- roster %>%
    group_by(franchise_id, franchise_name) %>%
    summarise(
      n_players     = n(),
      total_value   = round(sum(value, na.rm = TRUE)),
      top5_value    = round(sum(head(sort(value, decreasing = TRUE), 5), na.rm = TRUE)),
      total_salary  = if (has_salary) sum(salary, na.rm = TRUE) else NA_real_,
      # Value-weighted so a franchise's age reflects the players it actually
      # relies on, not the back of its taxi squad.
      avg_age       = round(weighted.mean(age[!is.na(age) & !is.na(value)],
                                          pmax(value[!is.na(age) & !is.na(value)], 1)), 1),
      matched       = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    mutate(
      value_per_dollar = ifelse(!is.na(total_salary) & total_salary > 0,
                                round(total_value / total_salary, 1), NA_real_),
      cap_space = if (!is.na(cap)) round(cap - total_salary, 1) else NA_real_
    )

  # Positional value split + startable-lineup value, per franchise.
  pos_split <- roster %>%
    filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
    group_by(franchise_id, pos) %>%
    summarise(v = round(sum(value, na.rm = TRUE)),
              s = if (has_salary) sum(salary, na.rm = TRUE) else NA_real_,
              .groups = "drop") %>%
    pivot_wider(names_from = pos, values_from = c(v, s), values_fill = 0)

  starters <- roster %>%
    group_by(franchise_id) %>%
    group_modify(~ tibble(starter_value = starter_value(.x, cfg$lineup))) %>%
    ungroup()

  # Depth-aware power model. Two things are deliberately taken from the league
  # itself rather than the market-comparison format:
  #   teams    a 14-team keeper must not be scored against a 12-team board
  #   scoring  UNHFFL is half-PPR while its market format (dynasty_sf) is full
  #            PPR; using the wrong board mis-ranks every pass-catcher
  lg_spec <- list(teams = n_distinct(roster$franchise_id),
                  qbs   = if (num0(cfg$lineup$SUPERFLEX) > 0 || num0(cfg$lineup$QB) >= 2) 2 else 1)
  fmt_spec <- detect(FORMATS, ~ identical(.x$key, fmt)) %||% list(ppr = 1)
  lg_scoring <- cfg$scoring %||% scoring_for(fmt_spec)
  roster_pw <- roster
  if (!identical(lg_scoring, scoring_for(fmt_spec))) {
    lb <- MY_BOARDS[[lg_scoring]]
    if (!is.null(lb)) {
      roster_pw <- roster %>%
        select(-any_of(c("my_proj", "my_games", "my_pos_rank"))) %>%
        mutate(cn = clean_nm(player_name)) %>%
        left_join(lb$board %>% select(cn, pos, my_proj, my_games, my_pos_rank),
                  by = c("cn", "pos")) %>%
        select(-cn)
      cat(sprintf("  . power board: %s scoring (league overrides %s market format)\n",
                  lg_scoring, fmt))
    }
  }
  power <- tryCatch(power_rankings(roster_pw, cfg$lineup, lg_spec),
                    error = function(e) { cat("  ! power model:", conditionMessage(e), "\n"); NULL })
  if (!is.null(power)) {
    cat(sprintf("  . power model: %d franchises, top expected %s\n",
                nrow(power), format(max(power$pw_expected), big.mark = ",")))
  }

  franchises <- franchises %>%
    left_join(pos_split, by = "franchise_id") %>%
    left_join(starters, by = "franchise_id") %>%
    { if (is.null(power)) . else left_join(., power %>% select(-franchise_name), by = "franchise_id") } %>%
    arrange(desc(total_value)) %>%
    mutate(value_rank = row_number())

  if (!is.null(standings)) {
    standings <- standings %>% mutate(franchise_id = as.character(franchise_id))
    franchises <- franchises %>%
      mutate(franchise_id = as.character(franchise_id)) %>%
      left_join(standings %>% select(-any_of("franchise_name")), by = "franchise_id")
  }

  # Future draft pick capital, valued off the same market index.
  pick_capital <- NULL
  if (!is.null(picks) && nrow(picks) > 0) {
    pick_capital <- picks %>%
      mutate(franchise_id = as.character(franchise_id)) %>%
      count(franchise_id, season, round, name = "n_picks")
  }

  # History: standings for prior seasons, for all-time records.
  history <- NULL
  n_hist <- cfg$history_seasons %||% 0
  if (n_hist > 0) {
    seasons <- (cfg$season - n_hist):(cfg$season - 1)
    history <- map(seasons, function(s) {
      # Completed seasons never change, so cache them effectively forever.
      hp <- file.path(CACHE_DIR, paste0(cfg$key, "_", s, "_standings.rds"))
      if (file.exists(hp)) return(readRDS(hp))
      tryCatch({
        c2 <- ff_connect(platform = cfg$platform, league_id = cfg$league_id,
                         season = s)
        st <- ff_standings(c2) %>% mutate(season = s, franchise_id = as.character(franchise_id))
        cat(sprintf("  history %d: %d franchises\n", s, nrow(st)))
        saveRDS(st, hp)
        st
      }, error = function(e) {
        cat(sprintf("  history %d: unavailable (%s)\n", s, conditionMessage(e))); NULL
      })
    }) %>% compact() %>% bind_rows()
    if (nrow(history) == 0) history <- NULL
  }

  all_time <- NULL
  if (!is.null(history)) {
    # Group by franchise_id, not name: MFL owners rename their team most years,
    # so grouping by name splits one franchise into several phantom rows.
    finishes <- history %>%
      group_by(season) %>%
      mutate(finish = rank(-num0(h2h_wins), ties.method = "min")) %>%
      ungroup()

    current_names <- roster %>% distinct(franchise_id, franchise_name)

    all_time <- finishes %>%
      group_by(franchise_id) %>%
      summarise(
        seasons     = n_distinct(season),
        wins        = sum(num0(h2h_wins)),
        losses      = sum(num0(h2h_losses)),
        ties        = sum(num0(h2h_ties)),
        points_for  = round(sum(num0(points_for)), 1),
        best_finish = min(finish, na.rm = TRUE),
        # Fall back to the most recent historical name if the franchise is gone.
        last_name   = dplyr::last(franchise_name[order(season)]),
        .groups = "drop"
      ) %>%
      left_join(current_names, by = "franchise_id") %>%
      mutate(franchise_name = coalesce(franchise_name, last_name)) %>%
      select(-last_name) %>%
      mutate(win_pct = round(wins / pmax(wins + losses + ties, 1), 3)) %>%
      arrange(desc(win_pct))
  }

  if (!is.null(txns)) {
    txns <- txns %>%
      mutate(timestamp = as.character(timestamp),
             player_name = format_name(player_name)) %>%
      arrange(desc(timestamp)) %>%
      head(400)
  }

  # Market coverage, checked rather than only printed. Two different numbers,
  # because they fail for two different reasons:
  #   rate      share of the roster carrying a market value. Bounded above by the
  #             index size, so a 299-man keeper roster priced against a 199-row
  #             redraft board tops out near 66% however healthy the join is.
  #   covered   share of the ACHIEVABLE ceiling. This is the one that detects id
  #             drift (mfl_id vs sleeper_id, an id space changing under us),
  #             which collapses it toward zero. Observed healthy range across the
  #             live leagues is 75-99% — rosters also hold IDP/K/DST that the
  #             market never lists — so the floor sits well below that.
  n_val   <- sum(!is.na(roster$value))
  rate    <- if (nrow(roster) > 0) n_val / nrow(roster) else 0
  covered <- n_val / max(min(nrow(roster), nrow(vals)), 1)
  cat(sprintf("  %d players (%d matched to market, %.0f%%), %d franchises\n",
              nrow(roster), n_val, 100 * rate, nrow(franchises)))
  if (covered < 0.60)
    .contract_fail(sprintf(
      "[contract] %s matched %d of the %d players the %s index could possibly cover (%.0f%%). Almost always %s drift between the platform roster and FantasyCalc.",
      cfg$key, n_val, min(nrow(roster), nrow(vals)), fmt, 100 * covered, join_col))
  # A low `rate` has two very different causes, and only one is a problem.
  #
  #   join degraded    covered is ALSO low — the board could have valued these
  #                    players and did not. Worth a warning.
  #   board is smaller covered is high — every player the board could value WAS
  #                    valued, the roster simply runs deeper than the index.
  #                    That is what a 299-man keeper league priced against a
  #                    199-row redraft board looks like, and it is a deliberate
  #                    configuration (unhffl, confirmed 2026-09-06), not a fault.
  #
  # Warning on `rate` alone would cry wolf on that league every single run, which
  # is how a real alert gets trained into background noise.
  if (rate < 0.70 && covered < 0.95) {
    cat(sprintf("  !! WARNING %s: only %.0f%% of the roster carries a market value and the join covered just %.0f%% of what the %s index could reach — check %s in %s.\n",
                cfg$key, 100 * rate, 100 * covered, fmt, join_col, basename(CONFIG_PATH)))
  } else if (rate < 0.70) {
    cat(sprintf("  . %s: %d of %d rostered players unvalued — the %s index lists %d. Join is healthy (%.0f%% of reachable); the board is simply shallower than the roster.\n",
                cfg$key, nrow(roster) - n_val, nrow(roster), fmt, nrow(vals), 100 * covered))
  }

  list(
    key         = cfg$key,
    label       = cfg$label,
    platform    = cfg$platform,
    league_id   = cfg$league_id,
    season      = cfg$season,
    market_format = fmt,
    salary_cap  = cap,
    has_salary  = has_salary,
    generated   = stamp,
    franchises  = franchises,
    roster      = roster,
    transactions = txns,
    history     = history,
    all_time    = all_time,
    pick_capital = pick_capital
  )
}

# ONLY_LEAGUE=<key> refreshes a single league and leaves the rest exactly as they
# are — the normal case after one league drafts. The others still contribute their
# meta entry (read back from their existing export) so the league selector keeps
# listing them; filtering the loop itself would silently drop them from the site.
ONLY_LEAGUE <- Sys.getenv("ONLY_LEAGUE", "")
if (nzchar(ONLY_LEAGUE) && !any(map_chr(leagues, "key") == ONLY_LEAGUE))
  stop("ONLY_LEAGUE=", ONLY_LEAGUE, " matches no enabled league in ",
       basename(CONFIG_PATH))

league_meta <- list()
for (cfg in leagues) {
  prior_path <- file.path(OUTPUT_DIR, "leagues", paste0(cfg$key, ".json"))

  if (nzchar(ONLY_LEAGUE) && !identical(cfg$key, ONLY_LEAGUE)) {
    if (file.exists(prior_path)) {
      pj <- tryCatch(fromJSON(prior_path, simplifyDataFrame = FALSE),
                     error = function(e) NULL)
      cat(sprintf("  skip %s — ONLY_LEAGUE=%s (existing export untouched)\n",
                  cfg$key, ONLY_LEAGUE))
      league_meta[[length(league_meta) + 1]] <- list(
        key = cfg$key, label = cfg$label, platform = cfg$platform,
        season = cfg$season,
        has_salary = if (!is.null(pj$has_salary)) pj$has_salary else TRUE,
        market_format = if (!is.null(pj$market_format)) pj$market_format else cfg$market_format
      )
    }
    next
  }

  out <- tryCatch(pull_league(cfg), error = function(e) {
    cat(sprintf("  ! league %s failed: %s\n", cfg$key, conditionMessage(e))); NULL
  })
  if (is.null(out)) {
    # A transient API failure shouldn't blank out a league the site is serving.
    if (file.exists(prior_path)) {
      cat(sprintf("  keeping previous export for %s\n", cfg$key))
      league_meta[[length(league_meta) + 1]] <- list(
        key = cfg$key, label = cfg$label, platform = cfg$platform,
        season = cfg$season, has_salary = TRUE, stale = TRUE
      )
    }
    next
  }
  write_json_file(as_json_arrays(out), OUTPUT_DIR, "leagues", paste0(cfg$key, ".json"))
  league_meta[[length(league_meta) + 1]] <- list(
    key = out$key, label = out$label, platform = out$platform,
    season = out$season, has_salary = out$has_salary,
    # so the league selector can show the valuation basis without fetching each
    # league's full export
    market_format = out$market_format
  )
}

write_json_file(
  list(
    generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
    formats = map(FORMATS, ~ list(key = .x$key, label = .x$label, dynasty = .x$dynasty)),
    leagues = league_meta
  ),
  OUTPUT_DIR, "meta.json"
)

cat("\nDone.\n")
