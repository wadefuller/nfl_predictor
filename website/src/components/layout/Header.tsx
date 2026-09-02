import { getWeekLabel, getWeeksForSeason } from '../../data/weekLabels';

interface HeaderProps {
  selectedSeason: number;
  selectedWeek: number;
  availableSeasons: number[];
  maxWeek: number;
  onSeasonChange: (season: number) => void;
  onWeekChange: (week: number) => void;
  // Fantasy views aren't scoped to an NFL week, so they hide these controls
  // rather than showing selectors that change nothing.
  showControls?: boolean;
  product: 'predictor' | 'fantasy';
  onProductChange: (product: 'predictor' | 'fantasy') => void;
}

export function Header({
  selectedSeason,
  selectedWeek,
  availableSeasons,
  maxWeek,
  onSeasonChange,
  onWeekChange,
  showControls = true,
  product,
  onProductChange,
}: HeaderProps) {
  const weeks = getWeeksForSeason(maxWeek);

  return (
    <header className="sticky top-0 z-10 border-b border-border bg-bg-page/95 backdrop-blur-sm">
      <div className="mx-auto flex max-w-7xl items-center justify-between px-4 py-3">
        <div className="flex items-center gap-3 min-w-0">
          <h1 className="shrink-0 text-lg font-bold text-text-primary tracking-tight">NFL Predictor</h1>
          <div className="flex rounded-lg border border-border-light bg-bg-card/70 p-0.5" aria-label="Product area">
            {([['predictor', 'Predictor'], ['fantasy', 'Fantasy HQ']] as const).map(([key, label]) => (
              <button key={key} type="button" onClick={() => onProductChange(key)}
                className={`rounded-md px-2 py-1 text-[11px] font-semibold transition-colors ${
                  product === key ? 'bg-bg-card-hover text-text-primary shadow-sm' : 'text-text-muted hover:text-text-secondary'
                }`}>{label}</button>
            ))}
          </div>
          {product === 'predictor' && <span className="hidden lg:inline text-xs text-text-muted font-medium bg-bg-card px-2 py-0.5 rounded-full border border-border">Ensemble Model</span>}
        </div>

        <div className={`items-center gap-2 ${showControls ? 'flex' : 'hidden'}`}>
          {/* Season selector */}
          <select
            value={selectedSeason}
            onChange={e => onSeasonChange(Number(e.target.value))}
            className="rounded-md border border-border bg-bg-card px-2 py-1.5 text-sm text-text-primary focus:border-accent focus:outline-none"
          >
            {availableSeasons.map(s => (
              <option key={s} value={s}>
                {s}
              </option>
            ))}
          </select>

          {/* Week navigation */}
          <div className="flex items-center gap-1">
            <button
              onClick={() => onWeekChange(Math.max(1, selectedWeek - 1))}
              disabled={selectedWeek <= 1}
              className="rounded-md border border-border bg-bg-card px-2 py-1.5 text-sm text-text-secondary hover:text-text-primary disabled:opacity-30 disabled:cursor-not-allowed"
            >
              &larr;
            </button>

            <select
              value={selectedWeek}
              onChange={e => onWeekChange(Number(e.target.value))}
              className="rounded-md border border-border bg-bg-card px-2 py-1.5 text-sm text-text-primary focus:border-accent focus:outline-none min-w-[140px]"
            >
              {weeks.map(w => (
                <option key={w} value={w}>
                  {getWeekLabel(w)}
                </option>
              ))}
            </select>

            <button
              onClick={() => onWeekChange(Math.min(maxWeek, selectedWeek + 1))}
              disabled={selectedWeek >= maxWeek}
              className="rounded-md border border-border bg-bg-card px-2 py-1.5 text-sm text-text-secondary hover:text-text-primary disabled:opacity-30 disabled:cursor-not-allowed"
            >
              &rarr;
            </button>
          </div>
        </div>
      </div>
    </header>
  );
}
