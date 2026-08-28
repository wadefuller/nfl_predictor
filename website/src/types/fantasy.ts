// Types for the Fantasy section: market values from FantasyCalc + live league
// state from ffscrapr, both written by export_fantasy.R.

export interface MarketPlayer {
  player_id_fc: number;
  name: string;
  pos: 'QB' | 'RB' | 'WR' | 'TE' | 'PICK';
  team: string | null;
  age: number | null;
  exp: number | null;
  mfl_id: string | null;
  sleeper_id: string | null;
  value: number;
  overall_rank: number;
  pos_rank: number;
  tier: number | null;
  trend30: number | null;
  redraft_value: number | null;
  index: number;
  trend_pct: number | null;
  is_pick: boolean;
  /** My season model, joined on positional rank. Null for picks and unmatched names. */
  my_pos_rank: number | null;
  my_overall: number | null;
  my_proj: number | null;
  my_tier: number | null;
  /** market pos_rank − my pos_rank. +ve = my model is higher on them than the market. */
  my_delta: number | null;
  /** Projected points over replacement, at THIS format's replacement depth. */
  my_vor: number | null;
  /** my_vor rescaled 0-100, directly comparable to the market's own `index`. */
  my_index: number | null;
}

export interface MarketFormat {
  format: string;
  label: string;
  dynasty: boolean;
  qbs: number;
  teams: number;
  ppr: number;
  generated: string;
  players: MarketPlayer[];
}

export interface Mover {
  name: string;
  pos: string;
  team: string | null;
  value: number;
  index: number;
  tier: number | null;
  overall_rank: number;
  pos_rank: number;
  trend30: number;
  trend_pct: number;
}

export interface MoversData {
  generated: string;
  formats: { format: string; label: string; risers: Mover[]; fallers: Mover[] }[];
}

export interface RosterPlayer {
  franchise_id: string;
  franchise_name: string;
  player_id: string;
  player_name: string;
  pos: string;
  team: string | null;
  age: number | null;
  salary: number | null;
  contract_years: string | null;
  roster_status: string | null;
  draft_year: string | null;
  draft_round: string | null;
  value: number | null;
  index: number | null;
  tier: number | null;
  overall_rank: number | null;
  pos_rank: number | null;
  trend30: number | null;
  trend_pct: number | null;
  value_per_dollar: number | null;
}

export interface Franchise {
  franchise_id: string;
  franchise_name: string;
  n_players: number;
  total_value: number;
  top5_value: number;
  total_salary: number | null;
  avg_age: number | null;
  matched: number;
  value_per_dollar: number | null;
  cap_space: number | null;
  starter_value: number | null;
  value_rank: number;
  // positional value / salary splits (v_QB, s_QB, ...)
  v_QB?: number; v_RB?: number; v_WR?: number; v_TE?: number;
  s_QB?: number; s_RB?: number; s_WR?: number; s_TE?: number;
  h2h_wins?: number; h2h_losses?: number; h2h_ties?: number;
  points_for?: number; points_against?: number;
  // Depth-aware power model (availability-simulated, league's own scoring).
  // Absent when the projection board couldn't be matched for a league.
  pw_rank?: number;
  pw_healthy?: number;      // best lineup every week, nobody ever out
  pw_expected?: number;     // mean simulated season - the headline
  pw_floor?: number;        // 10th-percentile season
  pw_stress?: number;       // season with the two best starters gone
  pw_stress_ratio?: number; // stress / healthy - shallower drop-off is better
  pw_depth_ratio?: number;  // expected / healthy - how little availability costs
  pw_spare?: number;        // startable bodies beyond the lineup requirement
  pw_thin?: string | null;  // positions short of a full starting group
  pw_dep_QB?: number; pw_dep_RB?: number; pw_dep_WR?: number; pw_dep_TE?: number;
}

export interface Transaction {
  timestamp: string;
  type: string;
  type_desc: string | null;
  franchise_id: string;
  franchise_name: string;
  player_id: string | null;
  player_name: string | null;
  pos: string | null;
  team: string | null;
  trade_partner: string | null;
  comments: string | null;
}

export interface HistoryRow {
  season: number;
  franchise_id: string;
  franchise_name: string;
  h2h_wins: number;
  h2h_losses: number;
  h2h_ties: number;
  points_for: number;
  points_against: number;
}

export interface AllTimeRow {
  franchise_name: string;
  seasons: number;
  wins: number;
  losses: number;
  ties: number;
  points_for: number;
  win_pct: number;
}

export interface LeagueData {
  key: string;
  label: string;
  platform: string;
  league_id: string;
  season: number;
  market_format: string;
  salary_cap: number | null;
  has_salary: boolean;
  generated: string;
  franchises: Franchise[];
  roster: RosterPlayer[];
  transactions: Transaction[] | null;
  history: HistoryRow[] | null;
  all_time: AllTimeRow[] | null;
}

export interface FantasyMeta {
  generated: string;
  formats: { key: string; label: string; dynasty: boolean }[];
  leagues: { key: string; label: string; platform: string; season: number; has_salary: boolean; stale?: boolean; market_format?: string }[];
}
