export interface Game {
  gameId: string;
  season: number;
  week: number;
  gameday: string;
  weekday: string;
  gametime: string;
  homeTeam: string;
  awayTeam: string;
  spreadLine: number;
  totalLine: number;
  homeMoneyline: number | null;
  awayMoneyline: number | null;
  divGame: boolean;
  prediction: GamePrediction;
  result: GameResult | null;
  /** Recommended position from the canonical betting engine; absent when no side
   *  on this game cleared the EV threshold. */
  bet?: GameBet | null;
}

export interface GameBet {
  side: string;
  type: 'spread' | 'ml' | string;
  odds: number;
  ev: number;
  stake: number;
  /** stake expressed in units (1 unit = 1% of bankroll) */
  units?: number;
  toWin: number;
  result?: 'win' | 'loss' | null;
  pnl?: number | null;
}

export interface GamePrediction {
  predWinner: string;
  predWinPct: number;
  nflPredictorLine: number;
  homeImplPts: number;
  awayImplPts: number;
  lineDiff: number;
}

export interface GameResult {
  homePoints: number;
  awayPoints: number;
  homeResult: number;
  homeWin: boolean;
  winningTeam: string;
  predCorrect: boolean;
  vegasCorrect: boolean;
}

export interface SeasonData {
  season: number;
  games: Game[];
}

export interface WeeklyPerformance {
  week: number;
  games: number;
  weekly: PerformanceMetrics;
  cumulative: PerformanceMetrics;
}

export interface PerformanceMetrics {
  modelCorrect: number;
  vegasCorrect: number;
  modelAccuracy: number;
  vegasAccuracy: number;
  modelSpreadCorrect: number;
  vegasSpreadCorrect: number;
  modelSpreadAccuracy: number;
  vegasSpreadAccuracy: number;
  mlROI: number;
  spreadROI: number;
}

export interface SeasonPerformance {
  season: number;
  weeklyStats: WeeklyPerformance[];
}

export interface MetaData {
  availableSeasons: number[];
  currentSeason: number;
  currentWeek: number;
  lastUpdated: string;
  weeksPerSeason: Record<string, number>;
  bankroll?: number;
  unitSize?: number;
}
