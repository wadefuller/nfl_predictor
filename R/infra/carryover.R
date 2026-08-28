# Team-state carryover with per-class exponential decay. Applies after all
# pre-pivot team modules have joined into the combined per-team frame, so
# the carryover blending sees the union of all rolling/cumulative features.
#
# Decay (per feature class, configurable via CARRYOVER_HL_JSON env var):
#   α(season_age) = exp(-season_age / half_life_games)
#   blended       = α * prior_season_end + (1 - α) * within_season_value
# `half_life = Inf` for ALL classes ⇒ pure forward-fill (back-compat sentinel).
suppressPackageStartupMessages(library(jsonlite))

CARRYOVER_CLASS_PATTERNS <- list(
  team_epa     = "_r(4|8|12|16)$",
  season_agg   = "_(4gm_trend|season_avg)$",
  pythag       = "^pythag_",
  qb_starter   = "^starter_",
  games_played = "^games_played$"
)

classify_carryover_col <- function(col) {
  # qb_starter (^starter_) takes precedence over team_epa (_r4$ etc.) because
  # starter_qb_epa_r4 also ends in _r4 but is a player-level feature.
  if (grepl(CARRYOVER_CLASS_PATTERNS$qb_starter, col))   return("qb_starter")
  if (grepl(CARRYOVER_CLASS_PATTERNS$team_epa, col))     return("team_epa")
  if (grepl(CARRYOVER_CLASS_PATTERNS$season_agg, col))   return("season_agg")
  if (grepl(CARRYOVER_CLASS_PATTERNS$pythag, col))       return("pythag")
  if (grepl(CARRYOVER_CLASS_PATTERNS$games_played, col)) return("games_played")
  NA_character_
}

# Default to Inf (no decay) — chosen 2026-05-21 after a 2x2 ablation showed
# carryover HURTS pure prediction metrics. Override via CARRYOVER_HL_JSON.
parse_carryover_config <- function() {
  defaults <- list(
    team_epa     = Inf,
    season_agg   = Inf,
    pythag       = Inf,
    qb_starter   = Inf,
    games_played = Inf
  )
  env_json <- Sys.getenv("CARRYOVER_HL_JSON", "")
  if (!nzchar(env_json)) return(defaults)
  parsed <- jsonlite::fromJSON(env_json, simplifyVector = TRUE)
  parsed <- lapply(parsed, function(x) {
    if (is.character(x) && tolower(x) %in% c("inf", "infinity")) Inf
    else as.numeric(x)
  })
  utils::modifyList(defaults, parsed)
}

apply_team_carryover_decay <- function(per_team_df,
                                        half_lives = parse_carryover_config()) {
  fill_patterns <- paste(c(
    "_r(4|8|12|16)$", "_4gm_trend$", "_season_avg$",
    "^pythag_", "^starter_qb_epa_r", "^starter_pass_epa_r",
    "^starter_career_games$", "^starter_games_team_season$",
    "^games_played$"
  ), collapse = "|")
  fill_cols <- grep(fill_patterns, names(per_team_df), value = TRUE)

  col_class <- vapply(fill_cols, classify_carryover_col, character(1))
  if (any(is.na(col_class))) {
    stop("Unclassified carryover columns: ",
         paste(fill_cols[is.na(col_class)], collapse = ", "))
  }
  col_hl <- vapply(col_class, function(cls) half_lives[[cls]], numeric(1))

  message(sprintf("Carryover half-lives: team_epa=%s season_agg=%s pythag=%s qb_starter=%s games_played=%s",
                  half_lives$team_epa, half_lives$season_agg, half_lives$pythag,
                  half_lives$qb_starter, half_lives$games_played))

  # Back-compat sentinel: all-Inf ⇒ exact original forward-fill behavior.
  if (all(is.infinite(col_hl))) {
    return(
      per_team_df %>%
        arrange(team, season, week) %>%
        group_by(team) %>%
        fill(all_of(fill_cols), .direction = "down") %>%
        ungroup()
    )
  }

  # Prior-season-end value per (team, season): last non-NA, shifted season+1
  last_non_na <- function(x) { v <- x[!is.na(x)]; if (length(v) == 0) NA_real_ else dplyr::last(v) }
  prior <- per_team_df %>%
    arrange(team, season, week) %>%
    group_by(team, season) %>%
    summarise(across(all_of(fill_cols), last_non_na), .groups = "drop") %>%
    mutate(season = season + 1L) %>%
    rename_with(~ paste0(.x, "__prior"), all_of(fill_cols))

  out <- per_team_df %>%
    arrange(team, season, week) %>%
    group_by(team, season) %>%
    mutate(.season_age = row_number() - 1L) %>%
    ungroup() %>%
    left_join(prior, by = c("team", "season"))

  for (col in fill_cols) {
    prior_col <- paste0(col, "__prior")
    hl        <- half_lives[[ col_class[[col]] ]]
    within    <- out[[col]]
    prior_v   <- out[[prior_col]]
    if (is.infinite(hl)) {
      out[[col]] <- dplyr::if_else(is.na(within), prior_v, within)
    } else {
      alpha <- exp(-out$.season_age / hl)
      out[[col]] <- dplyr::case_when(
        is.na(within) & is.na(prior_v) ~ NA_real_,
        is.na(within)                  ~ prior_v,
        is.na(prior_v)                 ~ within,
        TRUE                           ~ alpha * prior_v + (1 - alpha) * within
      )
    }
  }

  out %>%
    arrange(team, season, week) %>%
    group_by(team) %>%
    fill(all_of(fill_cols), .direction = "down") %>%
    ungroup() %>%
    select(-.season_age, -all_of(paste0(fill_cols, "__prior")))
}
