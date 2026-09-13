import { useEffect, useMemo, useState } from 'react';
import type { SeasonPerformance } from '../../types';
import { StatTile, num, PageHero, Skeleton, SubNav, arr } from '../ui/primitives';
import { RankingsReport } from '../rankings/RankingsReport';
import { LineChart, Legend, type Series } from '../ui/LineChart';
import { dataUrl } from '../../lib/dataUrl';

interface SeasonReportProps {
  performance: SeasonPerformance | null;
  season: number;
  week: number;
  onNavigate?: (season: number, week: number) => void;
  /** Active sub-tab, driven by the URL. Unknown/absent falls back to 'model'. */
  group?: string | null;
  onGroupChange?: (g: string) => void;
}

const MODEL_BLUE = '#58a6ff', VEGAS_GRAY = '#8b949e';
const WIN = '#3fb950', LOSS = '#f85149', GOLD = '#d29922', PURPLE = '#a371f7';

function Panel({ title, subtitle, legend, children }:
  { title: string; subtitle?: string; legend?: React.ReactNode; children: React.ReactNode }) {
  return (
    <div className="surface-panel rounded-xl border border-border-light bg-bg-card/40 p-3 space-y-2">
      <div className="flex items-start justify-between gap-2">
        <div>
          <div className="text-xs font-semibold text-text-primary">{title}</div>
          {subtitle && <div className="text-[11px] text-text-muted">{subtitle}</div>}
        </div>
        {legend}
      </div>
      {children}
    </div>
  );
}

function Section({ title, subtitle, accent, children, right }:
  { title: string; subtitle?: string; accent: string; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <section className="report-section rounded-2xl border border-border bg-bg-card/60 overflow-hidden">
      <div className="report-section__header flex items-center justify-between border-b border-border-light px-4 py-3" style={{ boxShadow: `inset 3px 0 0 ${accent}` }}>
        <div>
          <h3 className="text-sm font-semibold text-text-primary tracking-tight">{title}</h3>
          {subtitle && <p className="text-[11px] text-text-muted mt-0.5">{subtitle}</p>}
        </div>
        {right}
      </div>
      <div className="report-section__body p-4 space-y-4">{children}</div>
    </section>
  );
}

// distinct palette for non-highlighted franchises
const TEAM_PALETTE = ['#e06c75', '#56b6c2', '#c678dd', '#98c379', '#e5c07b', '#61afef', '#d19a66', '#7f848e', '#be5046', '#3fb6a8'];

function ModelSection({ performance }: { performance: SeasonPerformance }) {
  const ws = useMemo(() => [...performance.weeklyStats].sort((a, b) => a.week - b.week), [performance]);
  const x = ws.map(w => w.week);
  const c = (sel: (m: any) => number) => ws.map(w => { const v = sel(w.cumulative); return v == null || isNaN(v) ? null : v; });

  const last = ws[ws.length - 1]?.cumulative;
  const pct = (v: number) => `${(v * 100).toFixed(0)}%`;
  const roiPct = (v: number) => `${v >= 0 ? '+' : ''}${(v * 100).toFixed(0)}%`;

  const suSeries: Series[] = [
    { name: 'Model', color: MODEL_BLUE, data: c(m => m.modelAccuracy) },
    { name: 'Vegas', color: VEGAS_GRAY, data: c(m => m.vegasAccuracy), width: 1.5, dash: true },
  ];
  const atsSeries: Series[] = [
    { name: 'Model', color: MODEL_BLUE, data: c(m => m.modelSpreadAccuracy) },
    { name: 'Vegas', color: VEGAS_GRAY, data: c(m => m.vegasSpreadAccuracy), width: 1.5, dash: true },
  ];
  const roiSeries: Series[] = [
    { name: 'Moneyline', color: WIN, data: c(m => m.mlROI) },
    { name: 'Spread', color: GOLD, data: c(m => m.spreadROI) },
    { name: 'Combined', color: PURPLE, data: c(m => (m.mlROI + m.spreadROI) / 2) },
  ];

  return (
    <Section title="Model Performance" accent={MODEL_BLUE}
      subtitle="cumulative accuracy & ROI across the season — spread, moneyline, combined">
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-2">
        <StatTile label="Moneyline (SU)" value={last ? pct(last.modelAccuracy) : '–'} sub={last ? `vegas ${pct(last.vegasAccuracy)}` : ''} accent={MODEL_BLUE} />
        <StatTile label="Spread (ATS)" value={last ? pct(last.modelSpreadAccuracy) : '–'} sub={last ? `vegas ${pct(last.vegasSpreadAccuracy)}` : ''} accent={MODEL_BLUE} />
        <StatTile label="ML ROI" value={last ? roiPct(last.mlROI) : '–'} sub="flat · every game" accent={last && last.mlROI >= 0 ? WIN : LOSS} />
        <StatTile label="Spread ROI" value={last ? roiPct(last.spreadROI) : '–'} sub="flat · every game" accent={last && last.spreadROI >= 0 ? WIN : LOSS} />
      </div>
      <div className="grid lg:grid-cols-2 gap-3">
        <Panel title="Moneyline accuracy" subtitle="straight-up winners" legend={<Legend items={[{ name: 'Model', color: MODEL_BLUE, bold: true }, { name: 'Vegas', color: VEGAS_GRAY }]} />}>
          <LineChart x={x} series={suSeries} yDomain={[0.4, 0.8]} baseline={0.5} yFmt={v => pct(v)} xPrefix="W" />
        </Panel>
        <Panel title="Spread accuracy" subtitle="against the spread" legend={<Legend items={[{ name: 'Model', color: MODEL_BLUE, bold: true }, { name: 'Vegas', color: VEGAS_GRAY }]} />}>
          <LineChart x={x} series={atsSeries} yDomain={[0.4, 0.65]} baseline={0.5} yFmt={v => pct(v)} xPrefix="W" />
        </Panel>
        <Panel title="Return on investment" subtitle="flat $1 on every game's pick (not the strategy)" legend={<Legend items={[{ name: 'Moneyline', color: WIN }, { name: 'Spread', color: GOLD }, { name: 'Combined', color: PURPLE, bold: true }]} />}>
          <LineChart x={x} series={roiSeries} baseline={0} yFmt={v => `${(v * 100).toFixed(0)}%`} xPrefix="W" />
        </Panel>
        <Panel title="Calibration" subtitle="prediction error vs Vegas" legend={<Legend items={[{ name: 'Model MAE', color: MODEL_BLUE, bold: true }, { name: 'Vegas MAE', color: VEGAS_GRAY }]} />}>
          <LineChart x={x} yFmt={v => v.toFixed(1)} xPrefix="W"
            series={[{ name: 'Model', color: MODEL_BLUE, data: c(m => m.modelMarginMAE) },
                     { name: 'Vegas', color: VEGAS_GRAY, data: c(m => m.vegasMarginMAE), width: 1.5, dash: true }]} />
        </Panel>
      </div>
    </Section>
  );
}

function BettingSection({ season }: { season: number }) {
  const [data, setData] = useState<any>(null);
  const [loaded, setLoaded] = useState(false);
  useEffect(() => {
    fetch(dataUrl(`/data/season/${season}_betting.json`)).then(r => r.ok ? r.json() : null)
      .then(d => { setData(d); setLoaded(true); }).catch(() => setLoaded(true));
  }, [season]);

  const weeks = data?.weeks ?? [];
  const t = data?.totals ?? {};
  const x = weeks.map((w: any) => w.week);
  const start = data?.start_bankroll ?? 10000;
  const roiPct = (v: number) => v == null ? '–' : `${v >= 0 ? '+' : ''}${(v * 100).toFixed(1)}%`;

  return (
    <Section title="Betting Strategy" accent={WIN}
      subtitle="the REAL strategy — only EV+ bets, Kelly-sized (not flat / not every game)">
      {!loaded ? <Skeleton h={180} />
        : weeks.length === 0 ? (
          <div className="text-center py-10 text-text-muted text-sm">
            <p>No betting data for {season}.</p>
            <p className="mt-1 text-xs">Run <code className="text-accent">weekly_run/export_season_betting.R {season}</code>.</p>
          </div>
        ) : (
          <>
            <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-2">
              <StatTile label="Bets" value={t.bets ?? 0} sub={`${t.wins ?? 0}-${t.losses ?? 0}${t.pushes ? `-${t.pushes}` : ''}`} />
              <StatTile label="Staked" value={`$${(t.staked ?? 0).toLocaleString()}`} />
              <StatTile label="P&L" value={`${(t.pnl ?? 0) >= 0 ? '+' : ''}$${Math.round(t.pnl ?? 0).toLocaleString()}`} accent={(t.pnl ?? 0) >= 0 ? WIN : LOSS} />
              <StatTile label="ROI on staked" value={roiPct(t.roi)} accent={(t.roi ?? 0) >= 0 ? WIN : LOSS} />
              <StatTile label="Hit rate" value={t.bets ? `${Math.round(100 * (t.wins ?? 0) / ((t.wins ?? 0) + (t.losses ?? 0))) }%` : '–'} />
              <StatTile label="Bankroll" value={`$${Math.round(t.final_bankroll ?? start).toLocaleString()}`} sub={`from $${start.toLocaleString()}`} accent={(t.final_bankroll ?? start) >= start ? WIN : LOSS} />
            </div>
            <Panel title="Bankroll" subtitle={`$${start.toLocaleString()} base, flat-staked per week`}
              legend={<Legend items={[{ name: 'Bankroll', color: WIN, bold: true }]} />}>
              {/* axis scale follows the bankroll — a $500 roll formatted in "k"
                  collapsed every tick to $0k/$1k */}
              <LineChart x={x} baseline={start} xPrefix="W"
                yFmt={v => (start >= 10000 ? `$${(v / 1000).toFixed(1)}k` : `$${Math.round(v)}`)}
                series={[{ name: 'Bankroll', color: WIN, data: weeks.map((w: any) => w.bankroll) }]} />
            </Panel>
            <Panel title="Cumulative ROI on staked" subtitle="settled, EV-filtered + Kelly-sized">
              <LineChart x={x} baseline={0} yFmt={v => `${(v * 100).toFixed(0)}%`} xPrefix="W"
                series={[{ name: 'ROI', color: GOLD, data: weeks.map((w: any) => w.roi) }]} />
            </Panel>
          </>
        )}
    </Section>
  );
}

function FantasySection({ season }: { season: number }) {
  const [data, setData] = useState<any>(null);
  const [loaded, setLoaded] = useState(false);
  const [metric, setMetric] = useState<'cum_pf' | 'cum_wins'>('cum_pf');

  useEffect(() => {
    fetch(dataUrl(`/data/season/${season}.json`)).then(r => r.ok ? r.json() : null)
      .then(d => { setData(d); setLoaded(true); }).catch(() => setLoaded(true));
  }, [season]);

  const leagues = data?.leagues ?? [];

  return (
    <Section title="Fantasy Teams" accent="#f778ba"
      subtitle="every team's season trajectory + standings"
      right={
        <div className="flex gap-1">
          {([['cum_pf', 'Points For'], ['cum_wins', 'Wins']] as const).map(([k, label]) => (
            <button key={k} onClick={() => setMetric(k)}
              className={`rounded-md px-2 py-0.5 text-[11px] font-medium border transition-colors ${metric === k ? 'border-accent text-accent bg-accent/10' : 'border-border text-text-muted hover:text-text-secondary'}`}>{label}</button>
          ))}
        </div>}>
      {!loaded ? <Skeleton h={180} />
        : leagues.length === 0 ? (
          <div className="text-center py-10 text-text-muted text-sm">
            <p>No season fantasy data for {season}.</p>
            <p className="mt-1 text-xs">Run <code className="text-accent">weekly_run/export_season_fantasy.R {season}</code>.</p>
          </div>
        ) : leagues.map((lg: any, li: number) => {
          const franchises = lg.franchises ?? [];
          const weeks = lg.weeks ?? [];
          const colorFor = (f: any, i: number) => f.is_mine ? MODEL_BLUE : TEAM_PALETTE[i % TEAM_PALETTE.length];
          const series: Series[] = franchises.map((f: any, i: number) => {
            const byWeek = new Map(arr(f.weekly).map((w: any) => [w.week, w[metric]]));
            return {
              name: f.franchise, color: colorFor(f, i),
              data: weeks.map((w: number) => (byWeek.has(w) ? (byWeek.get(w) as number) : null)),
              width: f.is_mine ? 2.6 : 1.2, dot: f.is_mine,
            };
          });
          const yFmt = metric === 'cum_pf' ? (v: number) => `${Math.round(v)}` : (v: number) => `${Math.round(v)}`;
          return (
            <div key={li} className="space-y-3">
              <div className="flex items-center justify-between">
                <span className="text-xs font-semibold text-text-primary">{lg.label}</span>
                {lg.my_franchise && <span className="text-[11px] text-text-muted">my team: <span style={{ color: MODEL_BLUE }} className="font-semibold">{lg.my_franchise}</span></span>}
              </div>
              <div className="rounded-xl border border-border-light bg-bg-card/40 p-3">
                <LineChart x={weeks} series={series} height={240} yFmt={yFmt} xPrefix="W" />
              </div>
              {/* standings */}
              <div className="overflow-x-auto">
                <table className="w-full text-xs">
                  <thead><tr className="text-text-muted text-left border-b border-border-light">
                    {['#', 'Team', 'W-L', 'Points For'].map(h => <th key={h} className="py-1.5 pr-3 font-medium">{h}</th>)}
                  </tr></thead>
                  <tbody>
                    {franchises.map((f: any, i: number) => (
                      <tr key={i} className={`border-b border-border-light/50 ${f.is_mine ? 'bg-accent/5' : ''}`}>
                        <td className="py-1.5 pr-3 text-text-muted tabular-nums">{i + 1}</td>
                        <td className="py-1.5 pr-3">
                          <span className="inline-flex items-center gap-2">
                            <span className="inline-block rounded-full" style={{ width: 8, height: 8, background: colorFor(f, i) }} />
                            <span className={f.is_mine ? 'text-text-primary font-semibold' : 'text-text-secondary'}>{f.franchise}</span>
                          </span>
                        </td>
                        <td className="py-1.5 pr-3 tabular-nums text-text-secondary">{f.wins}-{f.losses}{f.ties ? `-${f.ties}` : ''}</td>
                        <td className="py-1.5 pr-3 tabular-nums text-text-primary">{num(f.points_for, 1)}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          );
        })}
    </Section>
  );
}

type SeasonGroup = 'model' | 'betting' | 'fantasy' | 'ecr';
const SEASON_GROUPS: { key: SeasonGroup; label: string; accent: string }[] = [
  { key: 'model',   label: 'Model',      accent: MODEL_BLUE },
  { key: 'betting', label: 'Betting',    accent: WIN },
  { key: 'fantasy', label: 'Fantasy',    accent: '#f778ba' },
  { key: 'ecr',     label: 'vs ECR',     accent: GOLD },
];

/** Sub-tab keys this view accepts in a permalink. */
export const SEASON_GROUP_KEYS: string[] = SEASON_GROUPS.map(g => g.key);

function SeasonEmptyState({ season, onNavigate }: { season: number; onNavigate?: (season: number, week: number) => void }) {
  const priorSeason = season - 1;
  return (
    <section className="relative overflow-hidden rounded-2xl border border-border bg-bg-card/60 p-6 sm:p-8">
      <div className="pointer-events-none absolute inset-0" style={{ background: 'radial-gradient(440px 220px at 100% 0%, rgba(88,166,255,0.11), transparent 70%)' }} />
      <div className="relative max-w-lg">
        <div className="inline-flex rounded-full border border-accent/30 bg-accent/10 px-2 py-1 text-[9px] font-bold uppercase tracking-[0.14em] text-accent">Season tracking</div>
        <h3 className="mt-3 text-lg font-semibold tracking-tight text-text-primary">Performance history will populate after the first graded slate.</h3>
        <p className="mt-2 text-sm leading-relaxed text-text-secondary">The weekly board is live now. Model accuracy, calibration, and strategy return appear here once completed games have been reconciled.</p>
        {onNavigate && (
          <button type="button" onClick={() => onNavigate(priorSeason, 1)}
            className="mt-5 rounded-lg border border-border bg-bg-card-hover px-3 py-2 text-xs font-semibold text-text-primary transition-colors hover:border-accent/60 hover:bg-accent/10 focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
            Review {priorSeason} performance →
          </button>
        )}
      </div>
    </section>
  );
}

export function SeasonReport({ performance, season, week, onNavigate, group: groupProp, onGroupChange }: SeasonReportProps) {
  const group: SeasonGroup =
    SEASON_GROUPS.some(g => g.key === groupProp) ? (groupProp as SeasonGroup) : 'model';
  const setGroup = (g: SeasonGroup) => onGroupChange?.(g);
  return (
    <div className="space-y-4">
      <PageHero kicker="Season Report" title={season}
        right={<span className="text-[11px] text-text-muted">model accuracy · betting · fantasy · rankings</span>} />
      <SubNav items={SEASON_GROUPS} value={group} onChange={setGroup} />
      {group === 'model' && (performance && performance.weeklyStats.length > 0
        ? <ModelSection performance={performance} />
        : <SeasonEmptyState season={season} onNavigate={onNavigate} />)}
      {group === 'betting' && <BettingSection season={season} />}
      {group === 'fantasy' && <FantasySection season={season} />}
      {/* Rankings-vs-ECR is a season-long view, so it lives here rather than as
          its own top-level tab. It brings its own hero, which we suppress. */}
      {group === 'ecr' && <RankingsReport season={season} week={week} onNavigate={onNavigate} embedded />}
    </div>
  );
}
