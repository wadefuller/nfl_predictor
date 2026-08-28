# Per-position injury depth from weekly injury reports, cap%-weighted via
# load_contracts. Position groupings: QB, OL, WR/TE, RB/FB, front7,
# secondary, ST. Aggregate + per-pos features. Pre-2010 contract data is
# sparse — features will be NA there; model handles via XGB native NA.

extract_injury_features <- function(seasons, player_cap, team_cap_agg) {
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

  inj <- nflreadr::load_injuries(inj_seasons) %>%
    filter(game_type == "REG", !is.na(team), !is.na(week)) %>%
    normalize_team_names() %>%
    left_join(player_cap,   by = c("season", "gsis_id")) %>%
    left_join(team_cap_agg, by = c("season", "team")) %>%
    mutate(
      is_out  = !is.na(report_status) & report_status == "Out",
      apy     = coalesce(apy, 0),
      pos_grp = pos_group(position)
    )

  pos_thresh <- inj %>%
    filter(apy > 0) %>%
    group_by(season, team, pos_grp) %>%
    summarise(top2_apy = if (n() >= 2) sort(apy, decreasing = TRUE)[2] else max(apy, 0),
              .groups = "drop")

  inj <- inj %>% left_join(pos_thresh, by = c("season", "team", "pos_grp"))

  agg <- inj %>%
    group_by(season, week, team) %>%
    summarise(
      inj_out_cap_pct     = sum(apy * is_out) / first(team_total_apy),
      inj_qb_out_cap      = sum(apy * is_out * (position == "QB")),
      inj_qb_starter_out  = as.integer(any(is_out & position == "QB" & apy >= first(top5_apy_threshold))),
      .groups = "drop"
    )

  per_pos <- inj %>%
    filter(pos_grp != "other") %>%
    group_by(season, week, team, pos_grp) %>%
    summarise(
      num_out  = sum(is_out),
      cap_pct  = sum(apy * is_out) / first(team_total_apy),
      star_out = as.integer(any(is_out & apy >= first(top2_apy) & apy > 0)),
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
  needs       = c("player_cap", "team_cap"),
  build       = function(seasons, deps) {
    extract_injury_features(seasons, deps$player_cap, deps$team_cap)
  }
)
