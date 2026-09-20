import { Legend, LineChart } from '../ui/LineChart';

type DistributionRow = {
  x?: number;
  threshold?: number;
  actual?: number;
  incumbent: number;
  candidate: number;
};

type SegmentRow = {
  position?: string;
  season?: number;
  n: number;
  pinball_gain: number;
  mae_gain: number;
};

export type ExperimentDiagnosticsData = {
  comparison?: {
    candidate?: { label?: string; hypothesis?: string };
    incumbent?: { label?: string };
    seeds?: number[] | number;
    n_player_weeks?: number;
    n_lineup_player_weeks?: number;
    season_wins?: number;
    season_total?: number;
  };
  headline?: Record<string, number>;
  distributions?: {
    score?: DistributionRow[] | DistributionRow;
    signed_error?: DistributionRow[] | DistributionRow;
    absolute_error_cdf?: DistributionRow[] | DistributionRow;
  };
  interval_coverage?: Array<{ nominal: number; incumbent: number; candidate: number }>;
  by_position?: SegmentRow[] | SegmentRow;
  by_season?: SegmentRow[] | SegmentRow;
};

const MODEL = '#3fb950';
const BASELINE = '#8b949e';
const ACTUAL = '#58a6ff';

const rows = <T,>(value: T[] | T | null | undefined): T[] =>
  Array.isArray(value) ? value : value == null ? [] : [value];
const pct = (value: number, digits = 1) => `${(Number(value) * 100).toFixed(digits)}%`;
const signed = (value: number, digits = 4) => `${value > 0 ? '+' : ''}${Number(value).toFixed(digits)}`;
const clamp = (value: number, lo: number, hi: number) => Math.min(hi, Math.max(lo, value));

function ChartPanel({ title, subtitle, children, legend }: {
  title: string;
  subtitle: string;
  children: React.ReactNode;
  legend: { name: string; color: string; bold?: boolean }[];
}) {
  return (
    <div className="min-w-0 rounded-xl border border-border-light bg-bg-page/20 p-3 sm:p-4">
      <div className="flex flex-col gap-2 sm:flex-row sm:items-start sm:justify-between">
        <div>
          <h4 className="text-xs font-semibold text-text-primary">{title}</h4>
          <p className="mt-0.5 text-[10px] leading-relaxed text-text-muted">{subtitle}</p>
        </div>
        <Legend items={legend} />
      </div>
      <div className="mt-2">{children}</div>
    </div>
  );
}

function GainBars({ values, labelKey }: { values: SegmentRow[]; labelKey: 'position' | 'season' }) {
  const max = Math.max(0.0001, ...values.map(r => Math.abs(Number(r.pinball_gain ?? 0))));
  return (
    <div className="mt-3 space-y-2.5">
      {values.map((row, index) => {
        const value = Number(row.pinball_gain ?? 0);
        const width = Math.max(1.5, Math.abs(value) / max * 48);
        const label = String(row[labelKey] ?? '—');
        return (
          <div key={`${label}-${index}`} className="grid grid-cols-[3.2rem_minmax(0,1fr)_4.6rem] items-center gap-2">
            <span className="text-[10px] font-semibold tabular-nums text-text-secondary">{label}</span>
            <div className="relative h-3" title={`${label}: ${signed(value)} mean pinball gain`}>
              <div className="absolute inset-x-0 top-[5px] h-px bg-border-light" />
              <div className="absolute bottom-0 top-0 left-1/2 w-px bg-text-muted/70" />
              <div
                className={`absolute top-[2px] h-2 rounded-sm ${value >= 0 ? 'bg-win' : 'bg-loss'}`}
                style={value >= 0
                  ? { left: '50%', width: `${width}%` }
                  : { right: '50%', width: `${width}%` }}
              />
            </div>
            <span className={`text-right text-[10px] font-semibold tabular-nums ${value >= 0 ? 'text-win' : 'text-loss'}`}>
              {signed(value)}
            </span>
          </div>
        );
      })}
    </div>
  );
}

function CoverageRows({ values }: { values: Array<{ nominal: number; incumbent: number; candidate: number }> }) {
  const scale = (value: number) => clamp((value - 0.4) / 0.6 * 100, 0, 100);
  return (
    <div className="mt-3 space-y-4">
      {values.map(row => (
        <div key={row.nominal}>
          <div className="flex items-center justify-between gap-3 text-[10px]">
            <span className="font-semibold text-text-secondary">{pct(row.nominal, 0)} interval</span>
            <span className="tabular-nums text-text-muted">
              baseline {pct(row.incumbent)} · <span className="font-semibold text-win">candidate {pct(row.candidate)}</span>
            </span>
          </div>
          <div className="relative mt-2 h-4" aria-label={`${pct(row.nominal, 0)} interval coverage: baseline ${pct(row.incumbent)}, candidate ${pct(row.candidate)}`}>
            <div className="absolute inset-x-0 top-[7px] h-1 rounded-full bg-border-light" />
            <div className="absolute top-[2px] h-3 w-px bg-text-muted" style={{ left: `${scale(row.nominal)}%` }} title={`Target ${pct(row.nominal, 0)}`} />
            <div className="absolute top-[5px] h-2 w-2 -translate-x-1/2 rounded-full bg-text-secondary" style={{ left: `${scale(row.incumbent)}%` }} title={`Baseline ${pct(row.incumbent)}`} />
            <div className="absolute top-[4px] h-2.5 w-2.5 -translate-x-1/2 rounded-full border-2 border-bg-card bg-win" style={{ left: `${scale(row.candidate)}%` }} title={`Candidate ${pct(row.candidate)}`} />
          </div>
        </div>
      ))}
      <div className="flex justify-between text-[8px] tabular-nums text-text-muted"><span>40%</span><span>target marker</span><span>100%</span></div>
    </div>
  );
}

export function ExperimentDiagnosticsView({ data }: { data: ExperimentDiagnosticsData }) {
  const comparison = data.comparison ?? {};
  const headline = data.headline ?? {};
  const score = rows<DistributionRow>(data.distributions?.score);
  const error = rows<DistributionRow>(data.distributions?.signed_error);
  const cdf = rows<DistributionRow>(data.distributions?.absolute_error_cdf);
  const positions = rows<SegmentRow>(data.by_position);
  const seasons = rows<SegmentRow>(data.by_season);
  const coverage = rows<{ nominal: number; incumbent: number; candidate: number }>(data.interval_coverage);
  const candidate = comparison.candidate?.label ?? 'Candidate';
  const incumbent = comparison.incumbent?.label ?? 'Incumbent';
  const seasonWins = Number(comparison.season_wins ?? 0);
  const seasonTotal = Number(comparison.season_total ?? 0);
  const lineupN = Number(comparison.n_lineup_player_weeks ?? 0);

  return (
    <div className="space-y-3">
      <div className="rounded-xl border border-win/25 bg-win/[0.035] p-3 sm:p-4">
        <div className="flex flex-col gap-3 lg:flex-row lg:items-start lg:justify-between">
          <div className="max-w-3xl">
            <div className="text-[9px] font-bold uppercase tracking-[0.16em] text-win">Latest confirmed feature experiment</div>
            <h4 className="mt-1 text-base font-semibold text-text-primary">{candidate} vs {incumbent}</h4>
            <p className="mt-1 text-[11px] leading-relaxed text-text-secondary">{comparison.candidate?.hypothesis}</p>
            <p className="mt-1 text-[9px] leading-relaxed text-text-muted">Charts use {lineupN.toLocaleString()} paired, lineup-likely player-weeks. Headline gains use the full {Number(comparison.n_player_weeks ?? 0).toLocaleString()} player-week confirmation archive across {seasonTotal} seasons and {rows<number>(comparison.seeds).length} seeds.</p>
          </div>
          <span className="w-fit shrink-0 rounded-full border border-win/35 bg-win/10 px-2.5 py-1 text-[9px] font-bold uppercase tracking-wider text-win">{seasonWins}/{seasonTotal} seasons improved</span>
        </div>
      </div>

      <div className="grid grid-cols-2 gap-2 lg:grid-cols-4">
        <div className="stat-tile rounded-xl border border-border-light bg-bg-page/25 p-3">
          <div className="text-[9px] uppercase tracking-wider text-text-muted">Pinball gain</div>
          <div className="mt-1 text-lg font-bold tabular-nums text-win">{signed(Number(headline.pinball_gain ?? 0))}</div>
          <div className="text-[9px] text-text-muted">lower distribution loss</div>
        </div>
        <div className="stat-tile rounded-xl border border-border-light bg-bg-page/25 p-3">
          <div className="text-[9px] uppercase tracking-wider text-text-muted">MAE gain</div>
          <div className="mt-1 text-lg font-bold tabular-nums text-win">{signed(Number(headline.mae_gain ?? 0), 3)}</div>
          <div className="text-[9px] text-text-muted">points per player-week</div>
        </div>
        <div className="stat-tile rounded-xl border border-border-light bg-bg-page/25 p-3">
          <div className="text-[9px] uppercase tracking-wider text-text-muted">Lower abs. error</div>
          <div className="mt-1 text-lg font-bold tabular-nums text-text-primary">{pct(Number(headline.candidate_abs_error_win_share ?? 0))}</div>
          <div className="text-[9px] text-text-muted">of lineup-likely cases</div>
        </div>
        <div className="stat-tile rounded-xl border border-border-light bg-bg-page/25 p-3">
          <div className="text-[9px] uppercase tracking-wider text-text-muted">Candidate bias</div>
          <div className="mt-1 text-lg font-bold tabular-nums text-text-primary">{signed(Number(headline.bias_candidate ?? 0), 3)}</div>
          <div className="text-[9px] text-text-muted">projection minus actual</div>
        </div>
      </div>

      <div className="grid gap-3 xl:grid-cols-2">
        <ChartPanel title="Forecast distribution" subtitle="How frequently each model lands in each projected-points band; actual outcomes provide context."
          legend={[{ name: candidate, color: MODEL, bold: true }, { name: incumbent, color: BASELINE }, { name: 'Actual', color: ACTUAL }]}>
          <LineChart x={score.map(r => Number(r.x))} height={205} maxAspect={2.9} endLabels={false}
            yFmt={v => pct(v, 0)}
            series={[
              { name: candidate, color: MODEL, data: score.map(r => Number(r.candidate)), width: 2.5 },
              { name: incumbent, color: BASELINE, data: score.map(r => Number(r.incumbent)), width: 1.5, dash: true, dot: false },
              { name: 'Actual', color: ACTUAL, data: score.map(r => Number(r.actual)), width: 1.5, dot: false },
            ]} />
          <div className="text-center text-[9px] text-text-muted">Half-PPR points · share of player-weeks</div>
        </ChartPanel>

        <ChartPanel title="Signed error distribution" subtitle="Projection minus actual. A tighter shape around zero means fewer and smaller misses."
          legend={[{ name: candidate, color: MODEL, bold: true }, { name: incumbent, color: BASELINE }]}>
          <LineChart x={error.map(r => Number(r.x))} height={205} maxAspect={2.9} endLabels={false}
            yFmt={v => pct(v, 0)}
            series={[
              { name: candidate, color: MODEL, data: error.map(r => Number(r.candidate)), width: 2.5 },
              { name: incumbent, color: BASELINE, data: error.map(r => Number(r.incumbent)), width: 1.5, dash: true, dot: false },
            ]} />
          <div className="flex justify-between text-[9px] text-text-muted"><span>projected too low</span><span>0 = centered</span><span>projected too high</span></div>
        </ChartPanel>

        <ChartPanel title="Error containment" subtitle="Share of lineup-likely predictions with absolute error at or below each threshold. Higher is better."
          legend={[{ name: candidate, color: MODEL, bold: true }, { name: incumbent, color: BASELINE }]}>
          <LineChart x={cdf.map(r => Number(r.threshold))} height={205} maxAspect={2.9} endLabels={false} yDomain={[0, 1]}
            yFmt={v => pct(v, 0)}
            series={[
              { name: candidate, color: MODEL, data: cdf.map(r => Number(r.candidate)), width: 2.5 },
              { name: incumbent, color: BASELINE, data: cdf.map(r => Number(r.incumbent)), width: 1.5, dash: true, dot: false },
            ]} />
          <div className="text-center text-[9px] text-text-muted">Absolute error threshold · half-PPR points</div>
        </ChartPanel>

        <div className="rounded-xl border border-border-light bg-bg-page/20 p-3 sm:p-4">
          <div className="grid gap-5 sm:grid-cols-2 xl:grid-cols-1 2xl:grid-cols-2">
            <div>
              <h4 className="text-xs font-semibold text-text-primary">Gain by position</h4>
              <p className="mt-0.5 text-[10px] text-text-muted">Mean pinball improvement; right of center favors the candidate.</p>
              <GainBars values={positions} labelKey="position" />
            </div>
            <div>
              <h4 className="text-xs font-semibold text-text-primary">Interval coverage</h4>
              <p className="mt-0.5 text-[10px] text-text-muted">Observed share inside the stated 50% and 90% ranges.</p>
              <CoverageRows values={coverage} />
            </div>
          </div>
        </div>
      </div>

      <div className="rounded-xl border border-border-light bg-bg-page/20 p-3 sm:p-4">
        <div className="flex flex-col gap-1 sm:flex-row sm:items-end sm:justify-between">
          <div><h4 className="text-xs font-semibold text-text-primary">Season consistency</h4><p className="mt-0.5 text-[10px] text-text-muted">The candidate reduced mean pinball loss in every historical outer fold.</p></div>
          <span className="text-[9px] text-text-muted">Positive = candidate better</span>
        </div>
        <GainBars values={seasons} labelKey="season" />
      </div>
    </div>
  );
}
