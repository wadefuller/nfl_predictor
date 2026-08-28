import type { Game } from '../../types';
import { getWeekLabel } from '../../data/weekLabels';
import { GameCard } from './GameCard';

interface GameGridProps {
  games: Game[];
  week: number;
}

export function GameGrid({ games, week }: GameGridProps) {
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
    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-2.5">
      {sorted.map(game => <GameCard key={game.gameId} game={game} />)}
    </div>
  );
}
