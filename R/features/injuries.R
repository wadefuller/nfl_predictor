# Per-position injury depth from weekly injury reports, cap%-weighted via
# load_contracts. Position groupings: QB, OL, WR/TE, RB/FB, front7,
# secondary, ST. Aggregate + per-pos features. Pre-2010 contract data is
# sparse — features will be NA there. Positional star thresholds come from the
# roster for that week, not just players who happened to appear on an injury
# report. The QB-starter flag uses the pregame QB proxy, not salary rank.

extract_injury_features <- function(seasons, player_cap, team_cap_agg,
                                    expected_qb, roster_weekly = NULL,
                                    inj_data = NULL) {
  inj_max     <- min(max(seasons), nflreadr::most_recent_season())
  inj_seasons <- intersect(seasons, 2009:inj_max)
  if (length(inj_seasons) == 0) return(tibble())

  pos_group <- function(p) {
    case_when(
      p == "QB" ~ "qb",
      p %in% c("T","G","C","OL","LT","RT","LG","RG","OT","OG") ~ "ol",
      p %in% c("WR","TE") ~ "wrte",
      p %in% c("RB","FB","HB") ~ "rbfb",
      p %in% c("LB","DE","DT","EDGE","NT","ILB","OLB","MLB","DL") ~ "front7",
      p %in% c("CB","S","FS","SS","DB") ~ "sec",
      p %in% c("K","P","LS") ~ "st",
      TRUE ~ "other"
    )
  }

  if (is.null(roster_weekly)) {
    roster_weekly <- dplyr::bind_rows(lapply(inj_seasons, function(s) {
      expect_frame(nflreadr::load_rosters_weekly(s),
                   cols = c("season", "week", "team", "gsis_id", "position", "game_type"),
                   label = sprintf("load_rosters_weekly(%d)", s)) %>%
        select(season, week, team, gsis_id, position, game_type)
    }))
  }
  roster_weekly <- roster_weekly %>%
    filter(game_type == "REG", !is.na(team), !is.na(week), !is.na(gsis_id)) %>%
    normalize_team_names() %>%
    distinct(season, week, team, gsis_id, .keep_all = TRUE)

  inj <- expect_frame(if (is.null(inj_data)) nflreadr::load_injuries(inj_seasons) else inj_data,
                      cols = c("game_type", "team", "week", "gsis_id",
                              "report_status", "position", "season"),
                      label = sprintf("load_injuries(%d-%d)", min(inj_seasons), max(inj_seasons))) %>%
    filter(game_type == "REG", !is.na(team), !is.na(week)) %>%
    normalize_team_names() %>%
    distinct(season, week, team, gsis_id, .keep_all = TRUE) %>%
    left_join(player_cap,   by = c("season", "gsis_id")) %>%
    left_join(team_cap_agg, by = c("season", "team")) %>%
    left_join(expected_qb %>% distinct(season, week, team, expected_id),
              by = c("season", "week", "team")) %>%
    mutate(
      is_out  = !is.na(report_status) & report_status == "Out",
      apy     = coalesce(apy, 0),
      pos_grp = pos_group(position)
    )

  pos_thresh <- roster_weekly %>%
    left_join(player_cap, by = c("season", "gsis_id")) %>%
    mutate(pos_grp = pos_group(position)) %>%
    filter(apy > 0) %>%
    group_by(season, week, team, pos_grp) %>%
    summarise(top2_apy = if (n() >= 2) sort(apy, decreasing = TRUE)[2] else max(apy, 0),
              .groups = "drop")

  inj <- inj %>% left_join(pos_thresh, by = c("season", "week", "team", "pos_grp"))

  agg <- inj %>%
    group_by(season, week, team) %>%
    summarise(
      inj_out_cap_pct     = sum(apy * is_out) / first(team_total_apy),
      inj_qb_out_cap      = sum(apy * is_out * (position == "QB")),
      inj_qb_starter_out  = as.integer(any(is_out & position == "QB" &
        !is.na(expected_id) & gsis_id == expected_id, na.rm = TRUE)),
      .groups = "drop"
    )

  per_pos <- inj %>%
    filter(pos_grp != "other") %>%
    group_by(season, week, team, pos_grp) %>%
    summarise(
      num_out  = sum(is_out),
      cap_pct  = sum(apy * is_out) / first(team_total_apy),
      star_out = as.integer(any(is_out & !is.na(top2_apy) &
        apy >= top2_apy & apy > 0)),
      .groups  = "drop"
    ) %>%
    pivot_wider(names_from = pos_grp,
                values_from = c(num_out, cap_pct, star_out),
                names_glue = "inj_{pos_grp}_{.value}",
                values_fill = list(num_out = 0L, cap_pct = 0, star_out = 0L))

  agg %>%
    left_join(per_pos, by = c("season", "week", "team")) %>%
    mutate(across(starts_with("inj_"), ~coalesce(., 0)))
}

INJURIES_MODULE <- feature_module(
  name        = "injuries",
  description = "Per-position injury depth (QB/OL/WR-TE/RB-FB/front7/secondary/ST) cap%-weighted",
  level       = "team_week_post_pivot",
  needs       = c("player_cap", "team_cap", "pbp", "game_split"),
  build       = function(seasons, deps) {
    actual <- identify_starters(deps$pbp)
    expected <- historical_pregame_qb_ids(deps$game_split, actual)
    extract_injury_features(seasons, deps$player_cap, deps$team_cap, expected)
  }
)
