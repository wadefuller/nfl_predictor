# Per-game weather + venue features from PBP. Domes normalized to baseline
# indoor conditions (temp=70, wind=0) so the model can distinguish them via
# is_dome alone. Game-level (not per-team).

extract_weather_venue <- function(pbp_full) {
  pbp_full %>%
    group_by(game_id) %>%
    summarise(
      temp_raw    = first(c(temp[!is.na(temp)], NA_real_)),
      wind_raw    = first(c(wind[!is.na(wind)], NA_real_)),
      roof        = first(c(as.character(roof)[!is.na(roof)], NA_character_)),
      surface     = first(c(as.character(surface)[!is.na(surface)], NA_character_)),
      .groups     = "drop"
    ) %>%
    mutate(
      is_dome    = as.integer(roof %in% c("dome", "closed")),
      is_grass   = as.integer(str_detect(coalesce(surface, ""), "grass")),
      temp       = if_else(is_dome == 1L, 70, as.numeric(temp_raw)),
      # Cap wind at 40 mph (above is nflfastR data error; games would be
      # canceled at hurricane speeds). Affects 2 historical games.
      wind       = pmin(if_else(is_dome == 1L, 0, as.numeric(wind_raw)), 40),
      high_wind  = as.integer(wind >= 15),
      cold_game  = as.integer(temp <= 32)
    ) %>%
    select(game_id, temp, wind, is_dome, is_grass, high_wind, cold_game)
}

WEATHER_VENUE_MODULE <- feature_module(
  name        = "weather_venue",
  description = "Per-game temp, wind, dome flag, grass flag, derived high_wind/cold_game",
  level       = "game",
  needs       = c("pbp"),
  build       = function(seasons, deps) extract_weather_venue(deps$pbp)
)
