import { useMemo, useState } from 'react';
import { useLeague } from '../../hooks/useFantasyData';
import type { Franchise, LeagueData, Transaction } from '../../types/fantasy';
import { PageHero, Skeleton, StatTile, TeamChip, clamp, hexToRgba, arr } from '../ui/primitives';
import { PosChip, Segmented, TierBadge, TrendPill, fmtMoney, fmtValue } from './fantasyUi';

const VIEWS = [
  ['power', 'Power'], ['positions', 'Positions'], ['rosters', 'Rosters'],
  ['transactions', 'Activity'], ['history', 'History'],
] as const;

const POS = ['QB', 'RB', 'WR', 'TE'] as const;

// Sequential magnitude ramp for heatmap cells: one hue, monotonic lightness.
const HEAT = ['#0d2847', '#0d419d', '#1158c7', '#1f6feb', '#388bfd', '#58a6ff'];
const heatColor = (t: number) => HEAT[Math.min(HEAT.length - 1, Math.max(0, Math.floor(t * HEAT.length)))];

export function LeagueReport({ leagueKey }: { leagueKey: string }) {
  const { data, loading } = useLeague(leagueKey);
  const [view, setView] = useState<(typeof VIEWS)[number][0]>('power');

  if (loading && !data) return <div className="space-y-3"><Skeleton h={120} /><Skeleton h={420} /></div>;
  if (!data) {
    return (
      <div className="rounded-2xl border border-border bg-bg-card p-6 text-center text-text-secondary">
        <p className="font-medium text-text-primary">No league data yet</p>
        <p className="text-sm mt-1">
          Add the league to <code className="text-accent">fantasy_leagues.json</code> and run{' '}
          <code className="text-accent">Rscript export_fantasy.R</code>.
        </p>
      </div>
    );
  }

  const totalValue = data.franchises.reduce((s, f) => s + f.total_value, 0);
  const totalSalary = data.franchises.reduce((s, f) => s + (f.total_salary ?? 0), 0);
  const overCap = data.salary_cap
    ? data.franchises.filter(f => (f.total_salary ?? 0) > data.salary_cap!).length
    : 0;

  return (
    <div className="space-y-4">
      <PageHero
        kicker={`${data.platform.toUpperCase()} · league ${data.league_id}`}
        title={data.label}
        badge={
          <span className="flex items-center gap-1.5">
            <span className="rounded-full border border-border bg-bg-card-hover px-2 py-0.5 text-[11px] text-text-secondary">
              {data.season} · {data.franchises.length} teams
              {data.salary_cap ? ` · ${fmtMoney(data.salary_cap)} cap` : ''}
            </span>
            {/* Which valuation basis the player values come from — dynasty leagues
                are priced on long-term value, redraft/keeper on season-long. */}
            {data.market_format && (() => {
              const dyn = data.market_format.startsWith('dynasty');
              return (
                <span className={`rounded-full border px-2 py-0.5 text-[10px] font-semibold uppercase tracking-wide ${
                  dyn ? 'border-accent/40 bg-accent/10 text-accent' : 'border-win/40 bg-win/10 text-win'}`}
                  title={`values from the ${data.market_format} market index`}>
                  {dyn ? 'dynasty value' : 'season-long value'}
                </span>
              );
            })()}
          </span>
        }
        right={<span className="text-[11px] text-text-muted">updated {data.generated}</span>}
      >
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-2">
          <StatTile label="Market value in play" value={fmtValue(totalValue)} sub={`${data.roster.length} rostered`} />
          <StatTile label="Most valuable roster" value={data.franchises[0]?.franchise_name ?? '–'}
            sub={fmtValue(data.franchises[0]?.total_value)} />
          {data.has_salary && (
            <StatTile label="Salary committed" value={fmtMoney(totalSalary)}
              sub={data.salary_cap ? `of ${fmtMoney(data.salary_cap * data.franchises.length)} total cap` : undefined} />
          )}
          {data.salary_cap ? (
            <StatTile label="Teams over cap" value={overCap} accent={overCap > 0 ? '#f85149' : undefined}
              sub={`${fmtMoney(data.salary_cap)} each`} />
          ) : (
            <StatTile label="Value per team" value={fmtValue(totalValue / Math.max(data.franchises.length, 1))} />
          )}
        </div>
      </PageHero>

      <Segmented value={view} options={VIEWS} onChange={setView} />

      {view === 'power' && <PowerTable data={data} />}
      {view === 'positions' && <PositionMatrix data={data} />}
      {view === 'rosters' && <Rosters data={data} />}
      {view === 'transactions' && <Activity txns={data.transactions ?? []} />}
      {view === 'history' && <History data={data} />}
    </div>
  );
}

// Bar scaled to the league max, labeled with the actual value — the bar carries
// the comparison, the number carries the magnitude.
function ValueBar({ v, max }: { v: number; max: number }) {
  return (
    <span className="inline-flex items-center gap-2">
      <span className="relative inline-block h-2 w-16 rounded-full overflow-hidden" style={{ background: 'rgba(255,255,255,0.07)' }}>
        <span className="absolute left-0 top-0 bottom-0 rounded-full"
          style={{ width: `${clamp((v / max) * 100)}%`, background: '#388bfd' }} />
      </span>
      <span className="tabular-nums text-[11px] font-semibold text-text-secondary">{fmtValue(v)}</span>
    </span>
  );
}

// Power ranking by market value of assets held — the dynasty-relevant ordering,
// which in preseason says more than a 0-0 record does.
function PowerTable({ data }: { data: LeagueData }) {
  const max = Math.max(...data.franchises.map(f => f.total_value), 1);
  const maxStart = Math.max(...data.franchises.map(f => f.starter_value ?? 0), 1);
  const hasPower = data.franchises.some(f => f.pw_expected != null);
  const [mode, setMode] = useState<'projected' | 'market'>(hasPower ? 'projected' : 'market');

  // Projected view: rank by the simulated season, which already prices depth.
  const rows = mode === 'projected' && hasPower
    ? [...data.franchises].sort((a, b) => (b.pw_expected ?? 0) - (a.pw_expected ?? 0))
    : data.franchises;
  const maxExp = Math.max(...data.franchises.map(f => f.pw_expected ?? 0), 1);
  const minExp = Math.min(...data.franchises.filter(f => f.pw_expected != null).map(f => f.pw_expected!), 0);

  return (
    <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
      {hasPower && (
        <div className="flex items-center gap-2 px-3 py-2 border-b border-border-light">
          {([['projected', 'Projected season'], ['market', 'Market value']] as const).map(([k, label]) => (
            <button key={k} onClick={() => setMode(k)}
              className={`px-2.5 py-1 rounded-lg text-xs font-medium transition-colors ${
                mode === k ? 'bg-accent/15 text-accent' : 'text-text-muted hover:text-text-secondary'}`}>
              {label}
            </button>
          ))}
          <span className="ml-auto text-[11px] text-text-muted">
            {mode === 'projected'
              ? 'Simulated 17-week seasons — depth priced in'
              : 'Dynasty trade value'}
          </span>
        </div>
      )}
      <div className="overflow-x-auto">
        <table className="w-full text-sm min-w-[900px]">
          <thead className="text-[11px] uppercase tracking-wider text-text-muted border-b border-border">
            {mode === 'projected' && hasPower ? (
              <tr className="text-left">
                <th className="px-3 py-2 font-medium w-10">#</th>
                <th className="px-3 py-2 font-medium">Franchise</th>
                <th className="px-3 py-2 font-medium w-44" title="Mean simulated season, best legal lineup each week from whoever is available">Expected season</th>
                <th className="px-3 py-2 font-medium w-20 text-right" title="10th-percentile season — a bad-luck year">Floor</th>
                <th className="px-3 py-2 font-medium w-20 text-right" title="Season with the two best starters out all year">Stress</th>
                <th className="px-3 py-2 font-medium w-16 text-right" title="Stress ÷ healthy ceiling. Higher = shallower drop-off behind the stars">Resil.</th>
                <th className="px-3 py-2 font-medium w-16 text-right" title="Startable players beyond what the starting lineup requires">Spare</th>
                <th className="px-3 py-2 font-medium w-28">Thin at</th>
                <th className="px-3 py-2 font-medium w-14 text-right">Plyrs</th>
              </tr>
            ) : (
              <tr className="text-left">
                <th className="px-3 py-2 font-medium w-10">#</th>
                <th className="px-3 py-2 font-medium">Franchise</th>
                <th className="px-3 py-2 font-medium w-44">Total value</th>
                <th className="px-3 py-2 font-medium w-40">Starters</th>
                {data.has_salary && <th className="px-3 py-2 font-medium w-20 text-right">Salary</th>}
                {data.salary_cap ? <th className="px-3 py-2 font-medium w-20 text-right">Cap space</th> : null}
                <th className="px-3 py-2 font-medium w-20 text-right" title="Market value per salary dollar">Val/$</th>
                <th className="px-3 py-2 font-medium w-16 text-right">Age</th>
                <th className="px-3 py-2 font-medium w-14 text-right">Plyrs</th>
              </tr>
            )}
          </thead>
          <tbody>
            {rows.map((f, i) => {
              const over = data.salary_cap !== null && (f.total_salary ?? 0) > data.salary_cap;
              if (mode === 'projected' && hasPower) {
                // Scale the bar across the league's own spread so differences read.
                const span = Math.max(1, maxExp - minExp * 0.92);
                const pct = Math.max(4, (((f.pw_expected ?? 0) - minExp * 0.92) / span) * 100);
                return (
                  <tr key={f.franchise_id} className="border-b border-border-light/60 hover:bg-bg-card-hover">
                    <td className="px-3 py-2 tabular-nums text-text-muted">{i + 1}</td>
                    <td className="px-3 py-2 font-medium text-text-primary">{f.franchise_name}</td>
                    <td className="px-3 py-2">
                      <div className="flex items-center gap-2">
                        <div className="flex-1 h-1.5 rounded-full bg-bg-elevated overflow-hidden">
                          <div className="h-full rounded-full bg-accent" style={{ width: `${pct}%` }} />
                        </div>
                        <span className="tabular-nums text-text-secondary w-12 text-right">
                          {f.pw_expected?.toLocaleString()}
                        </span>
                      </div>
                    </td>
                    <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{f.pw_floor?.toLocaleString() ?? '–'}</td>
                    <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{f.pw_stress?.toLocaleString() ?? '–'}</td>
                    <td className="px-3 py-2 text-right tabular-nums font-medium"
                      style={{ color: (f.pw_stress_ratio ?? 0) >= 0.87 ? '#3fb950' : (f.pw_stress_ratio ?? 1) < 0.83 ? '#f85149' : undefined }}>
                      {f.pw_stress_ratio != null ? f.pw_stress_ratio.toFixed(2) : '–'}
                    </td>
                    <td className="px-3 py-2 text-right tabular-nums font-medium"
                      style={{ color: (f.pw_spare ?? 0) === 0 ? '#d29922' : undefined }}>
                      {f.pw_spare ?? '–'}
                    </td>
                    <td className="px-3 py-2 text-xs" style={{ color: f.pw_thin ? '#f85149' : undefined }}>
                      {f.pw_thin ?? '—'}
                    </td>
                    <td className="px-3 py-2 text-right tabular-nums text-text-muted">{f.n_players}</td>
                  </tr>
                );
              }
              return (
                <tr key={f.franchise_id} className="border-b border-border-light/60 hover:bg-bg-card-hover">
                  <td className="px-3 py-2 tabular-nums text-text-muted">{f.value_rank}</td>
                  <td className="px-3 py-2 font-medium text-text-primary">{f.franchise_name}</td>
                  <td className="px-3 py-2"><ValueBar v={f.total_value} max={max} /></td>
                  <td className="px-3 py-2"><ValueBar v={f.starter_value ?? 0} max={maxStart} /></td>
                  {data.has_salary && (
                    <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{fmtMoney(f.total_salary)}</td>
                  )}
                  {data.salary_cap ? (
                    <td className="px-3 py-2 text-right tabular-nums font-medium"
                      style={{ color: over ? '#f85149' : '#3fb950' }}>
                      {f.cap_space === null ? '–' : `${f.cap_space < 0 ? '−' : ''}$${Math.abs(f.cap_space).toFixed(0)}`}
                    </td>
                  ) : null}
                  <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{f.value_per_dollar?.toFixed(0) ?? '–'}</td>
                  <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{f.avg_age?.toFixed(1) ?? '–'}</td>
                  <td className="px-3 py-2 text-right tabular-nums text-text-muted">{f.n_players}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
      <div className="px-3 py-2 text-[11px] text-text-muted border-t border-border-light">
        {mode === 'projected' && hasPower
          ? 'Every player is available each week with probability = projected games ÷ 17; the best legal lineup is filled from whoever is up, over 300 simulated seasons. Resil. = season with your two best starters gone, as a share of the healthy ceiling. Spare = startable bodies beyond the lineup requirement.'
          : 'Starters = best startable lineup by market value. Val/$ = market value per salary dollar (higher is better cap efficiency).'}
      </div>
    </div>
  );
}

// Positional strength heatmap. Cells are shaded by percentile *within* a
// position, so a column reads as "who is strong at RB" rather than being
// dominated by the fact that WRs cost more than TEs league-wide.
function PositionMatrix({ data }: { data: LeagueData }) {
  const rows = useMemo(() => {
    const vals = (p: string) => data.franchises.map(f => (f[`v_${p}` as keyof Franchise] as number) ?? 0);
    const ranges: Record<string, [number, number]> = {};
    for (const p of POS) {
      const v = vals(p);
      ranges[p] = [Math.min(...v), Math.max(...v)];
    }
    return data.franchises.map(f => ({
      f,
      cells: POS.map(p => {
        const v = (f[`v_${p}` as keyof Franchise] as number) ?? 0;
        const [lo, hi] = ranges[p];
        return { pos: p, v, t: hi > lo ? (v - lo) / (hi - lo) : 0.5 };
      }),
    }));
  }, [data]);

  return (
    <div className="space-y-3">
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1.5 rounded-xl border border-border-light bg-bg-card/50 px-3 py-2">
        <span className="text-[10px] uppercase tracking-wider text-text-muted">Weakest</span>
        <span className="flex gap-0.5">
          {HEAT.map(c => <span key={c} className="h-2.5 w-6 first:rounded-l-sm last:rounded-r-sm" style={{ background: c }} />)}
        </span>
        <span className="text-[10px] uppercase tracking-wider text-text-muted">Strongest</span>
        <span className="text-[11px] text-text-muted ml-auto">shaded within each position column</span>
      </div>

      <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full text-sm min-w-[640px]">
            <thead className="text-[11px] uppercase tracking-wider text-text-muted border-b border-border">
              <tr className="text-left">
                <th className="px-3 py-2 font-medium">Franchise</th>
                {POS.map(p => <th key={p} className="px-3 py-2 font-medium text-right w-28">{p}</th>)}
                <th className="px-3 py-2 font-medium text-right w-24">Total</th>
              </tr>
            </thead>
            <tbody>
              {rows.map(({ f, cells }) => (
                <tr key={f.franchise_id} className="border-b border-border-light/60">
                  <td className="px-3 py-2 font-medium text-text-primary">{f.franchise_name}</td>
                  {cells.map(c => (
                    <td key={c.pos} className="px-1.5 py-1.5">
                      <div
                        className="rounded-md px-2 py-1.5 text-right tabular-nums text-xs font-semibold text-text-primary"
                        title={`${f.franchise_name} · ${c.pos} · ${fmtValue(c.v)}`}
                        style={{
                          background: hexToRgba(heatColor(c.t), 0.55),
                          boxShadow: `inset 0 0 0 1px ${hexToRgba(heatColor(c.t), 0.8)}`,
                        }}
                      >
                        {fmtValue(c.v)}
                      </div>
                    </td>
                  ))}
                  <td className="px-3 py-2 text-right tabular-nums font-semibold text-text-primary">{fmtValue(f.total_value)}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

function Rosters({ data }: { data: LeagueData }) {
  const [fid, setFid] = useState(data.franchises[0]?.franchise_id ?? '');
  const players = data.roster.filter(p => p.franchise_id === fid);
  const f = data.franchises.find(x => x.franchise_id === fid);

  return (
    <div className="space-y-3">
      <div className="flex flex-wrap gap-1">
        {data.franchises.map(x => (
          <button
            key={x.franchise_id}
            onClick={() => setFid(x.franchise_id)}
            className={`rounded-lg px-2.5 py-1.5 text-xs font-medium border transition-colors ${
              fid === x.franchise_id
                ? 'border-accent/50 bg-accent/10 text-text-primary'
                : 'border-border-light bg-bg-card/60 text-text-muted hover:text-text-secondary'
            }`}
          >
            {x.franchise_name}
          </button>
        ))}
      </div>

      {f && (
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-2">
          <StatTile label="Roster value" value={fmtValue(f.total_value)} sub={`rank ${f.value_rank} of ${data.franchises.length}`} />
          <StatTile label="Starter value" value={fmtValue(f.starter_value)} />
          {data.has_salary && <StatTile label="Salary" value={fmtMoney(f.total_salary)}
            sub={data.salary_cap ? `${f.cap_space! < 0 ? 'over' : 'under'} by $${Math.abs(f.cap_space!).toFixed(0)}` : undefined}
            accent={data.salary_cap && f.cap_space! < 0 ? '#f85149' : undefined} />}
          <StatTile label="Value-weighted age" value={f.avg_age?.toFixed(1) ?? '–'} />
        </div>
      )}

      <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full text-sm min-w-[860px]">
            <thead className="text-[11px] uppercase tracking-wider text-text-muted border-b border-border">
              <tr className="text-left">
                <th className="px-3 py-2 font-medium">Player</th>
                <th className="px-3 py-2 font-medium w-16">Pos</th>
                <th className="px-3 py-2 font-medium w-20">Team</th>
                <th className="px-3 py-2 font-medium w-16">Age</th>
                <th className="px-3 py-2 font-medium w-16">Tier</th>
                {data.has_salary && <th className="px-3 py-2 font-medium w-20 text-right">Salary</th>}
                <th className="px-3 py-2 font-medium w-24 text-right">Value</th>
                {data.has_salary && <th className="px-3 py-2 font-medium w-20 text-right">Val/$</th>}
                <th className="px-3 py-2 font-medium w-24">30d</th>
              </tr>
            </thead>
            <tbody>
              {players.map(p => (
                <tr key={p.player_id} className="border-b border-border-light/60 hover:bg-bg-card-hover">
                  <td className="px-3 py-2 font-medium text-text-primary">
                    {p.player_name}
                    {p.roster_status && p.roster_status !== 'ROSTER' && (
                      <span className="ml-2 rounded px-1 py-0.5 text-[10px] text-text-muted bg-white/5">{p.roster_status}</span>
                    )}
                  </td>
                  <td className="px-3 py-2"><PosChip pos={p.pos} /></td>
                  <td className="px-3 py-2">{p.team ? <TeamChip team={p.team} /> : null}</td>
                  <td className="px-3 py-2 tabular-nums text-text-secondary">{p.age?.toFixed(1) ?? '–'}</td>
                  <td className="px-3 py-2"><TierBadge tier={p.tier} /></td>
                  {data.has_salary && <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{fmtMoney(p.salary)}</td>}
                  <td className="px-3 py-2 text-right tabular-nums font-semibold text-text-primary">{fmtValue(p.value)}</td>
                  {data.has_salary && <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{p.value_per_dollar?.toFixed(0) ?? '–'}</td>}
                  <td className="px-3 py-2"><TrendPill pct={p.trend_pct} compact /></td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

function Activity({ txns }: { txns: Transaction[] }) {
  const [type, setType] = useState('ALL');
  const types = useMemo(
    () => ['ALL', ...Array.from(new Set(txns.map(t => t.type))).sort()],
    [txns]
  );
  const rows = type === 'ALL' ? txns : txns.filter(t => t.type === type);

  if (txns.length === 0) {
    return <div className="rounded-2xl border border-border bg-bg-card px-3 py-8 text-center text-sm text-text-muted">No transactions available.</div>;
  }

  return (
    <div className="space-y-3">
      <Segmented value={type} options={types.map(t => [t, t === 'ALL' ? 'All' : t] as const)} onChange={setType} />
      <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full text-sm min-w-[680px]">
          <tbody>
            {rows.slice(0, 200).map((t, i) => (
              <tr key={i} className="border-b border-border-light/60 last:border-0 hover:bg-bg-card-hover">
                <td className="px-3 py-2 w-36 text-[11px] tabular-nums text-text-muted">{t.timestamp}</td>
                <td className="px-3 py-2 w-24">
                  <span className="rounded px-1.5 py-0.5 text-[10px] font-semibold text-text-secondary bg-white/5">{t.type}</span>
                </td>
                <td className="px-3 py-2 text-text-secondary">{t.franchise_name}</td>
                <td className="px-3 py-2 font-medium text-text-primary">
                  {t.player_name ?? <span className="text-text-muted">{t.comments ?? '–'}</span>}
                </td>
                <td className="px-3 py-2 w-14">{t.pos ? <PosChip pos={t.pos} /> : null}</td>
                <td className="px-3 py-2 w-16">{t.team ? <TeamChip team={t.team} /> : null}</td>
                <td className="px-3 py-2 w-24 text-[11px] text-text-muted">{t.type_desc ?? ''}</td>
              </tr>
            ))}
          </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

function History({ data }: { data: LeagueData }) {
  const seasons = useMemo(
    () => Array.from(new Set(arr(data.history).map(h => h.season))).sort((a, b) => b - a),
    [data.history]
  );

  if (!data.all_time || data.all_time.length === 0) {
    return <div className="rounded-2xl border border-border bg-bg-card px-3 py-8 text-center text-sm text-text-muted">No history available.</div>;
  }

  const maxWinPct = Math.max(...data.all_time.map(a => a.win_pct), 0.001);

  return (
    <div className="space-y-3">
      <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
        <div className="px-3 py-2.5 border-b border-border">
          <div className="text-sm font-semibold text-text-primary">All-time record</div>
          <div className="text-[11px] text-text-muted">
            {seasons.length ? `${Math.min(...seasons)}–${Math.max(...seasons)}` : ''} · regular season
          </div>
        </div>
        <div className="overflow-x-auto">
          <table className="w-full text-sm min-w-[620px]">
          <thead className="text-[11px] uppercase tracking-wider text-text-muted border-b border-border">
            <tr className="text-left">
              <th className="px-3 py-2 font-medium w-10">#</th>
              <th className="px-3 py-2 font-medium">Franchise</th>
              <th className="px-3 py-2 font-medium w-16 text-right">Sns</th>
              <th className="px-3 py-2 font-medium w-24 text-right">Record</th>
              <th className="px-3 py-2 font-medium w-44">Win %</th>
              <th className="px-3 py-2 font-medium w-28 text-right">Points for</th>
            </tr>
          </thead>
          <tbody>
            {data.all_time.map((a, i) => (
              <tr key={a.franchise_name} className="border-b border-border-light/60 hover:bg-bg-card-hover">
                <td className="px-3 py-2 tabular-nums text-text-muted">{i + 1}</td>
                <td className="px-3 py-2 font-medium text-text-primary">{a.franchise_name}</td>
                <td className="px-3 py-2 text-right tabular-nums text-text-muted">{a.seasons}</td>
                <td className="px-3 py-2 text-right tabular-nums text-text-secondary">
                  {a.wins}–{a.losses}{a.ties ? `–${a.ties}` : ''}
                </td>
                {/* bar scaled to the league leader, label is the true win rate */}
                <td className="px-3 py-2">
                  <span className="inline-flex items-center gap-2">
                    <span className="relative inline-block h-2 w-20 rounded-full overflow-hidden" style={{ background: 'rgba(255,255,255,0.07)' }}>
                      <span className="absolute left-0 top-0 bottom-0 rounded-full"
                        style={{ width: `${clamp((a.win_pct / maxWinPct) * 100)}%`, background: '#388bfd' }} />
                    </span>
                    <span className="tabular-nums text-[11px] font-semibold text-text-secondary">
                      {(a.win_pct * 100).toFixed(1)}%
                    </span>
                  </span>
                </td>
                <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{a.points_for.toLocaleString()}</td>
              </tr>
            ))}
          </tbody>
          </table>
        </div>
      </div>

      {seasons.map(s => {
        const rows = arr(data.history)
          .filter(h => h.season === s)
          .sort((a, b) => b.h2h_wins - a.h2h_wins || b.points_for - a.points_for);
        return (
          <div key={s} className="rounded-2xl border border-border bg-bg-card overflow-hidden">
            <div className="px-3 py-2 border-b border-border text-sm font-semibold text-text-primary">{s}</div>
            <div className="overflow-x-auto">
              <table className="w-full text-sm min-w-[460px]">
              <tbody>
                {rows.map((h, i) => (
                  <tr key={h.franchise_id} className="border-b border-border-light/60 last:border-0">
                    <td className="px-3 py-1.5 w-10 tabular-nums text-text-muted">{i + 1}</td>
                    <td className="px-3 py-1.5 text-text-primary">{h.franchise_name}</td>
                    <td className="px-3 py-1.5 w-24 text-right tabular-nums text-text-secondary">
                      {h.h2h_wins}–{h.h2h_losses}{h.h2h_ties ? `–${h.h2h_ties}` : ''}
                    </td>
                    <td className="px-3 py-1.5 w-28 text-right tabular-nums text-text-muted">{h.points_for?.toFixed(1)}</td>
                  </tr>
                ))}
              </tbody>
              </table>
            </div>
          </div>
        );
      })}
    </div>
  );
}
