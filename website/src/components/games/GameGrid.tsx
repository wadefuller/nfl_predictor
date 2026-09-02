import type { Game } from '../../types';
import { getWeekLabel } from '../../data/weekLabels';
import { GameCard } from './GameCard';

interface GameGridProps {
  games: Game[];
  week: number;
  /** Read-only Polymarket winner contracts keyed by the canonical game id. */
  polymarket?: any[];
  kalshi?: any[];
  kalshiWin?: any[];
}

export function GameGrid({ games, week, polymarket = [], kalshi = [], kalshiWin = [] }: GameGridProps) {
  if (games.length === 0) {
    return (
      <div className="text-center py-16 text-text-muted">
        <p className="text-lg">No games found for {getWeekLabel(week)}</p>
        <p className="text-sm mt-1">Try selecting a different week</p>
      </div>
    );
  }

  // One flat, denser grid — each card carries its own date, so per-day section
  // headers (which cost a full row of vertical space each and left ragged gaps
  // when a day had 1-2 games) are no longer needed.
  const sorted = [...games].sort(
    (a, b) => (a.gameday + a.gametime).localeCompare(b.gameday + b.gametime)
  );

  return (
    <div className="space-y-3">
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-2.5">
        {sorted.map(game => (
          <GameCard key={game.gameId} game={game}
            polymarket={polymarket.filter(m => m.game_id === game.gameId)}
            kalshi={kalshi.filter(m => m.game_id === game.gameId)}
            kalshiWin={kalshiWin.filter(m => m.game_id === game.gameId)} />
        ))}
      </div>
      <details className="rounded-lg border border-border-light bg-bg-card/30 px-3 py-2 text-[11px] text-text-muted">
        <summary className="cursor-pointer select-none font-medium text-text-secondary">How to read Kalshi on these cards</summary>
        <div className="mt-2 leading-relaxed">
          <span className="font-medium text-[#f0b44d]">KC -2.5 · 50¢ ask · 54% model · +2.1% EV</span> means a YES contract costs 50¢ and pays $1 if Kansas City wins by more than 2.5 points; otherwise it pays $0. The card selects the contract priced nearest 50¢, then uses the model’s calibrated margin uncertainty to estimate the chance of clearing that exact threshold. EV is the model probability minus the executable ask and estimated Kalshi fee, then passed through the same historical EV calibration used for sportsbook recommendations. The fee estimate can vary slightly with order size because of cent rounding. Open the row for rules and order book.
        </div>
      </details>
    </div>
  );
}
