# Generic rolling-window helper. Used by EPA modules to compute r4/r12/r16
# trailing means per team (chronological, excluding the current game).
suppressPackageStartupMessages(library(zoo))

roll_epa <- function(df, cols) {
  df %>%
    group_by(team) %>%
    arrange(team, season, week) %>%
    mutate(
      across(
        all_of(cols),
        list(
          r4  = ~rollapply(., list(-(4:1)),  mean, align = "right", fill = NA, partial = TRUE),
          r12 = ~rollapply(., list(-(12:1)), mean, align = "right", fill = NA, partial = TRUE),
          r16 = ~rollapply(., list(-(16:1)), mean, align = "right", fill = NA, partial = TRUE)
        )
      )
    ) %>%
    ungroup()
}
