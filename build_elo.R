# ============================================
# NFL Predictor: QB-Adjusted Elo Builder
# ============================================
# Walks all games chronologically (2000-current), maintains:
#   - team_elo[team]     (rolling team strength)
#   - qb_elo[player_id]  (rolling QB strength, regressed each offseason)
#
# Per-game prediction uses ADJUSTED Elo:
#   home_total = team_elo[home] + (qb_elo[home_qb] - MEAN) * QB_WEIGHT + HOME_ADV
#   away_total = team_elo[away] + (qb_elo[away_qb] - MEAN) * QB_WEIGHT
#   elo_diff   = home_total - away_total
#   elo_prob_home = 1 / (1 + 10^(-elo_diff/400))
#
# Update rule (538-style MoV-scaled, split between team + QB):
#   residual = actual - expected
#   team_elo[home] +=  (1 - QB_SHARE) * K_TEAM * mov_mult * residual    (- for away)
#   qb_elo[home_qb] += QB_SHARE       * K_QB  * mov_mult * residual    (- for away)
# K_QB is the QB-specific learning rate (default 20 = matches team rate).
# QB_SHARE apportions credit/blame between unit (team) and individual (QB).
#
# Offseason regression toward MEAN_ELO:
#   team_elo *= REGRESSION_TEAM + MEAN_ELO * (1 - REGRESSION_TEAM)
#   qb_elo   *= REGRESSION_QB   + MEAN_ELO * (1 - REGRESSION_QB)
#
# Hyperparams (env-var overridable via ELO_PARAMS_JSON for sweeps):
#   K_TEAM, K_QB, HOME_ADV, REGRESSION_TEAM, REGRESSION_QB,
#   QB_SHARE, QB_WEIGHT, MEAN_ELO
#
# Output: data/elo_per_game.csv with one row per game.
# Usage:  Rscript build_elo.R
# Runtime: ~30s (after PBP cache is warm — we only need games + QB IDs, not PBP)
# ============================================

suppressPackageStartupMessages({
  library(nflreadr)
  library(jsonlite)
})

# DATA_DIR + PROJECT_DIR from ensemble_lib.R (also loads tidyverse).
source("~/Projects/R/nfl_predictor/ensemble_lib.R")

OUT_PATH <- file.path(DATA_DIR, "elo_per_game.csv")

# ---- Hyperparam config (env-var overridable) -------------------------------

parse_elo_config <- function() {
  # Team-only Elo (QB_SHARE=0, QB_WEIGHT=0) chosen 2026-05-21 after sweep.
  # Best MAE (9.875 vs 9.890 baseline) and Brier (0.21175 vs 0.21215). QB
  # adjustments did not add signal — elo_qb_heavy was the worst config on both
  # metrics. Override via ELO_PARAMS_JSON for future experiments.
  defaults <- list(
    K_TEAM          = 20,
    K_QB            = 20,
    HOME_ADV        = 55,
    REGRESSION_TEAM = 0.75,
    REGRESSION_QB   = 0.85,
    QB_SHARE        = 0.00,
    QB_WEIGHT       = 0.00,
    MEAN_ELO        = 1505,
    INIT_TEAM       = 1500,
    INIT_QB         = 1500
  )
  override_json <- Sys.getenv("ELO_PARAMS_JSON", unset = "")
  if (nzchar(override_json)) {
    overrides <- jsonlite::fromJSON(override_json)
    for (nm in names(overrides)) defaults[[nm]] <- as.numeric(overrides[[nm]])
  }
  defaults
}

TEAM_REPLACEMENTS <- c("JAC" = "JAX", "SD" = "LAC", "OAK" = "LV", "STL" = "LA")

# Main entry point. Wrapped in a function so sourcing this script loads the
# function defs WITHOUT triggering the chronological walk (~30s) or writing
# the CSV. Called at the bottom only when invoked via Rscript.
build_elo <- function(cfg = parse_elo_config(),
                       out_path = OUT_PATH) {
  message(sprintf(
    "Elo hyperparams: K_TEAM=%g K_QB=%g HOME_ADV=%g REGRESS_T=%g REGRESS_QB=%g QB_SHARE=%g QB_WEIGHT=%g",
    cfg$K_TEAM, cfg$K_QB, cfg$HOME_ADV,
    cfg$REGRESSION_TEAM, cfg$REGRESSION_QB,
    cfg$QB_SHARE, cfg$QB_WEIGHT
  ))

  # Same raw-http hazard, same fix as load_game_data() in R/infra/data_loaders.R
  # (which this file does not source — keep the two in step). A single 60s
  # timeout here used to abort the whole retrain.
  fetch_games_rds_elo <- function(attempts = 3,
                                  need = c("season", "game_id", "home_team", "away_team")) {
    for (attempt in seq_len(attempts)) {
      g <- try(readRDS(url("http://www.habitatring.com/games.rds")), silent = TRUE)
      if (!inherits(g, "try-error") && NROW(g) > 0 && all(need %in% names(g))) return(g)
      why <- if (inherits(g, "try-error")) trimws(conditionMessage(attr(g, "condition")))
             else sprintf("%d rows; missing column(s): %s", NROW(g),
                          paste(setdiff(need, names(g)), collapse = ", "))
      message(sprintf("  ! games.rds attempt %d/%d unusable (%s); retrying", attempt, attempts, why))
      Sys.sleep(2 * attempt)
    }
    stop(sprintf("games.rds unusable after %d attempts — habitatring.com unreachable, or the schema changed", attempts))
  }

  message("Loading game data from habitatring.com...")
  games <- fetch_games_rds_elo() %>%
    filter(season >= 2000) %>%
    mutate(across(contains("team"), ~stringr::str_replace_all(., TEAM_REPLACEMENTS))) %>%
    arrange(gameday, gametime, game_id)

  message(sprintf("Loaded %d games (%d-%d).",
                  nrow(games), min(games$season), max(games$season)))

  all_teams <- unique(c(games$home_team, games$away_team))
  team_elo  <- setNames(rep(cfg$INIT_TEAM, length(all_teams)), all_teams)
  qb_elo    <- new.env(parent = emptyenv())

  get_qb_elo <- function(qb_id) {
    if (is.na(qb_id) || qb_id == "" || is.null(qb_id)) return(cfg$INIT_QB)
    v <- qb_elo[[qb_id]]
    if (is.null(v)) cfg$INIT_QB else v
  }
  set_qb_elo <- function(qb_id, val) {
    if (is.na(qb_id) || qb_id == "" || is.null(qb_id)) return(invisible())
    qb_elo[[qb_id]] <- val
  }

  n <- nrow(games)
  out <- list(
    game_id           = character(n),
    home_team_elo_pre = numeric(n), away_team_elo_pre = numeric(n),
    home_qb_elo_pre   = numeric(n), away_qb_elo_pre   = numeric(n),
    home_elo_adj      = numeric(n), away_elo_adj      = numeric(n),
    elo_diff          = numeric(n), elo_prob_home     = numeric(n)
  )

  prev_season <- NA_integer_

  message("Walking games chronologically...")
  for (i in seq_len(n)) {
    g <- games[i, ]
    if (!is.na(prev_season) && g$season != prev_season) {
      team_elo <- cfg$REGRESSION_TEAM * team_elo +
        (1 - cfg$REGRESSION_TEAM) * cfg$MEAN_ELO
      for (qb in ls(qb_elo)) {
        qb_elo[[qb]] <- cfg$REGRESSION_QB * qb_elo[[qb]] +
          (1 - cfg$REGRESSION_QB) * cfg$MEAN_ELO
      }
    }
    prev_season <- g$season

    home <- g$home_team; away <- g$away_team
    home_qb <- g$home_qb_id; away_qb <- g$away_qb_id

    h_team <- team_elo[[home]]; a_team <- team_elo[[away]]
    h_qb   <- get_qb_elo(home_qb); a_qb <- get_qb_elo(away_qb)

    h_adj <- h_team + (h_qb - cfg$MEAN_ELO) * cfg$QB_WEIGHT + cfg$HOME_ADV
    a_adj <- a_team + (a_qb - cfg$MEAN_ELO) * cfg$QB_WEIGHT
    diff  <- h_adj - a_adj
    prob  <- 1 / (1 + 10^(-diff / 400))

    out$game_id[i] <- g$game_id
    out$home_team_elo_pre[i] <- h_team; out$away_team_elo_pre[i] <- a_team
    out$home_qb_elo_pre[i]   <- h_qb;   out$away_qb_elo_pre[i]   <- a_qb
    out$home_elo_adj[i] <- h_adj; out$away_elo_adj[i] <- a_adj
    out$elo_diff[i] <- diff; out$elo_prob_home[i] <- prob

    hs <- g$home_score; as_ <- g$away_score
    if (!is.na(hs) && !is.na(as_)) {
      margin   <- hs - as_
      actual   <- if (margin > 0) 1 else if (margin < 0) 0 else 0.5
      mov_mult <- log(abs(margin) + 1) * 2.2 / (abs(diff) * 0.001 + 2.2)
      residual <- actual - prob

      team_delta <- (1 - cfg$QB_SHARE) * cfg$K_TEAM * mov_mult * residual
      qb_delta   <-      cfg$QB_SHARE  * cfg$K_QB   * mov_mult * residual

      team_elo[[home]] <- h_team + team_delta
      team_elo[[away]] <- a_team - team_delta
      if (!is.na(home_qb) && nzchar(home_qb)) set_qb_elo(home_qb, h_qb + qb_delta)
      if (!is.na(away_qb) && nzchar(away_qb)) set_qb_elo(away_qb, a_qb - qb_delta)
    }
  }

  elo_df <- as_tibble(out)
  message(sprintf("Built Elo for %d games. %d distinct QBs tracked.",
                  nrow(elo_df), length(ls(qb_elo))))

  # Quick sanity check: top-10 current team Elos
  final <- sort(team_elo, decreasing = TRUE)
  message("\nTop 10 current team Elos:")
  for (i in seq_len(min(10, length(final)))) {
    message(sprintf("  %-4s  %.1f", names(final)[i], final[i]))
  }
  qb_vals <- unlist(as.list(qb_elo))
  if (length(qb_vals) > 0) {
    message(sprintf("\nQB Elo: n=%d, mean=%.1f, sd=%.1f, range=[%.1f, %.1f]",
                    length(qb_vals), mean(qb_vals), sd(qb_vals),
                    min(qb_vals), max(qb_vals)))
    top_qbs <- sort(qb_vals, decreasing = TRUE)[1:min(10, length(qb_vals))]
    message("Top 10 current QB Elos (player_id):")
    for (i in seq_along(top_qbs)) {
      message(sprintf("  %-12s  %.1f", names(top_qbs)[i], top_qbs[i]))
    }
  }

  message(sprintf("\nWriting %d rows to %s", nrow(elo_df), out_path))
  write_csv(elo_df, out_path)
  message("Done.")
  invisible(elo_df)
}

# Only run when invoked as a script — sourcing just loads function defs.
if (sys.nframe() == 0L) {
  build_elo()
}
