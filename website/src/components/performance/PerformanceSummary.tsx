import type { SeasonPerformance } from '../../types';
import { StatTile, CompareBar, num, PageHero } from '../ui/primitives';

interface PerformanceSummaryProps {
  performance: SeasonPerformance | null;
  week: number;
  season: number;
}

export function PerformanceSummary({ performance, week, season }: PerformanceSummaryProps) {
  if (!performance || performance.weeklyStats.length === 0) return null;

  const weekStats = performance.weeklyStats
    .filter(w => w.week <= week)
    .sort((a, b) => b.week - a.week);
  if (weekStats.length === 0) return null;

  const latest = weekStats[0].cumulative;
  const totalGames = weekStats.reduce((sum, w) => sum + w.games, 0);
  const modelRecord = `${latest.modelCorrect}-${totalGames - latest.modelCorrect}`;
  const vegasRecord = `${latest.vegasCorrect}-${totalGames - latest.vegasCorrect}`;
  const edge = latest.modelAccuracy - latest.vegasAccuracy;

  return (
    <PageHero kicker="Season Performance" title={season}
      right={<span className="text-[11px] text-text-muted">{totalGames} games · thru week {weekStats[0].week}</span>}>
      <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-5 gap-2">
        <StatTile label="Model SU" value={`${(latest.modelAccuracy * 100).toFixed(1)}%`} sub={modelRecord} accent="#58a6ff" />
        <StatTile label="Vegas SU" value={`${(latest.vegasAccuracy * 100).toFixed(1)}%`} sub={vegasRecord} />
        <StatTile label="Edge vs Vegas" value={`${edge >= 0 ? '+' : ''}${(edge * 100).toFixed(1)}%`}
          accent={edge >= 0 ? '#3fb950' : '#f85149'} />
        <StatTile label="ML ROI" value={`${latest.mlROI >= 0 ? '+' : ''}${(latest.mlROI * 100).toFixed(1)}%`}
          accent={latest.mlROI >= 0 ? '#3fb950' : '#f85149'} />
        <StatTile label="Spread ROI" value={`${latest.spreadROI >= 0 ? '+' : ''}${(latest.spreadROI * 100).toFixed(1)}%`}
          accent={latest.spreadROI >= 0 ? '#3fb950' : '#f85149'} />
      </div>

      <div className="rounded-xl border border-border-light bg-bg-card/40 p-3">
        <div className="text-[10px] uppercase tracking-wider text-text-muted mb-2">Straight-up accuracy</div>
        <CompareBar aLabel="Model" aVal={latest.modelAccuracy} bLabel="Vegas" bVal={latest.vegasAccuracy} />
        <div className="text-[10px] text-text-muted mt-2">
          Spread: model {num(latest.modelSpreadAccuracy * 100, 1)}% · vegas {num(latest.vegasSpreadAccuracy * 100, 1)}%
        </div>
      </div>
    </PageHero>
  );
}
