import { arr } from '../ui/primitives';
import { useEffect, useState } from 'react';
import { dataUrl } from '../../lib/dataUrl';

// Season archive — the whole run of weeks with their headlines, so the season
// reads back like a stack of back issues rather than a dropdown.
// Backed by a single `league_archive.json` the builder maintains, so opening the
// archive costs one fetch instead of one per week.

interface ArchiveWeek {
  season: number;
  week: number;
  tag: string;
  league_id: string;
  label: string;
  headline: string;
  n_facts: number;
  n_threads: number;
}

export function LeagueArchive({ season, week, onNavigate }:
  { season: number; week: number; onNavigate?: (s: number, w: number) => void }) {
  const [weeks, setWeeks] = useState<ArchiveWeek[] | null>(null);

  useEffect(() => {
    fetch(dataUrl('/data/weekly/league_archive.json'))
      .then(r => (r.ok ? r.json() : null))
      .then(j => setWeeks(arr(j?.weeks)))
      .catch(() => setWeeks([]));
  }, []);

  if (!weeks || weeks.length === 0) return null;

  const seasons = [...new Set(weeks.map(w => w.season))].sort((a, b) => b - a);

  return (
    <div className="data-surface rounded-2xl border border-border bg-bg-card/40 p-3 space-y-3">
      <div className="flex items-baseline justify-between">
        <span className="text-[10px] uppercase tracking-[0.14em] text-text-muted">Archive</span>
        <span className="text-[10px] text-text-muted">{weeks.length} issues</span>
      </div>

      {seasons.map(s => (
        <div key={s} className="space-y-1">
          {seasons.length > 1 && (
            <div className="text-[11px] font-semibold text-text-secondary">{s}</div>
          )}
          <ul className="space-y-0.5">
            {weeks.filter(w => w.season === s).map(w => {
              const active = w.season === season && w.week === week;
              return (
                <li key={w.tag}>
                  <button
                    onClick={() => onNavigate?.(w.season, w.week)}
                    className={`w-full text-left rounded-lg px-2 py-1.5 flex items-baseline gap-2.5 transition-colors ${
                      active ? 'bg-accent/10 ring-1 ring-accent/40' : 'hover:bg-bg-card-hover'
                    }`}
                  >
                    <span className={`shrink-0 tabular-nums text-[11px] font-semibold ${
                      active ? 'text-accent' : 'text-text-muted'}`}>
                      W{String(w.week).padStart(2, '0')}
                    </span>
                    <span className={`text-xs leading-snug ${
                      active ? 'text-text-primary' : 'text-text-secondary'}`}>
                      {w.headline || <span className="text-text-muted italic">no headline</span>}
                    </span>
                  </button>
                </li>
              );
            })}
          </ul>
        </div>
      ))}
    </div>
  );
}
