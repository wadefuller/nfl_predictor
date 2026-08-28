import { useEffect, useMemo, useState } from 'react';
import { StatTile, TeamChip, num, obj, PageHero, Skeleton, DeltaBar, arr } from '../ui/primitives';
import { getWeekLabel } from '../../data/weekLabels';
import { dataUrl } from '../../lib/dataUrl';

interface Snapshot { season: number; week: number; tag: string; scoring?: string }
interface IndexFile { snapshots: Snapshot[]; latest?: string; scoring?: string; formats?: string[] }

const FMT_LABEL: Record<string, string> = { ppr: 'Full PPR', half: 'Half PPR', std: 'Standard' };

interface RankingsReportProps {
  season: number; week: number; onNavigate?: (s: number, w: number) => void;
  /** true when rendered inside Season Report — suppresses the duplicate page hero */
  embedded?: boolean;
}

const MODEL_BLUE = '#58a6ff', ECR_ORANGE = '#d29922', WIN = '#3fb950';

async function getJSON<T>(url: string): Promise<T | null> {
  try { const r = await fetch(url); return r.ok ? ((await r.json()) as T) : null; } catch { return null; }
}

function FreshnessTag({ updated }: { updated?: string | null }) {
  if (!updated) return null;
  const parsed = new Date(updated.replace(/([+-]\d{2})(\d{2})$/, '$1:$2'));
  const label = Number.isNaN(parsed.getTime())
    ? updated
    : parsed.toLocaleString([], { month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit' });
  return <span title={`Rankings snapshot generated ${updated}`} className="rounded-full border border-border-light bg-bg-card-hover px-1.5 py-0.5 text-[9px] font-medium tabular-nums text-text-muted">Data as of {label}</span>;
}

function Section({ title, subtitle, accent, right, children }:
  { title: string; subtitle?: string; accent: string; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <section className="rounded-2xl border border-border bg-bg-card/60 overflow-hidden">
      <div className="flex items-center justify-between border-b border-border-light px-4 py-3" style={{ boxShadow: `inset 3px 0 0 ${accent}` }}>
        <div>
          <h3 className="text-sm font-semibold text-text-primary tracking-tight">{title}</h3>
          {subtitle && <p className="text-[11px] text-text-muted mt-0.5">{subtitle}</p>}
        </div>
        {right}
      </div>
      <div className="p-4 space-y-3">{children}</div>
    </section>
  );
}

// Signed rank error vs Pts Finish: + = finished worse than ranked (overrated),
// − = finished better (underrated). Green when this source beat the other on the player.
function ErrCell({ err, otherErr }: { err: number | null; otherErr: number | null }) {
  if (err == null) return <td className="px-2.5 py-1.5 tabular-nums text-right text-text-muted">–</td>;
  const closer = otherErr == null || Math.abs(err) < Math.abs(otherErr);
  const tied = otherErr != null && Math.abs(err) === Math.abs(otherErr);
  return (
    <td className={`px-2.5 py-1.5 tabular-nums text-right ${closer ? 'text-win font-medium' : tied ? 'text-text-secondary' : 'text-text-muted'}`}>
      {err > 0 ? `+${err}` : err}
    </td>
  );
}

function DisagreementList({ title, rows, tone }: { title: string; rows: any[]; tone: string }) {
  return (
    <div>
      <div className="text-[10px] uppercase tracking-wider mb-1.5" style={{ color: tone }}>{title}</div>
      <div className="grid grid-cols-[minmax(0,1fr)_2.5rem_2.5rem_2rem_3.25rem] items-center gap-1 border-b border-border-light/60 pb-1 text-[8px] font-medium uppercase tracking-wider text-text-muted">
        <span>Player</span><span className="text-right">Model</span><span className="text-right">ECR</span><span className="text-right">Δ</span><span className="text-right">PPG</span>
      </div>
      <div className="divide-y divide-border-light/40">
        {arr(rows).slice(0, 8).map((r: any, i: number) => (
          <div key={i} className="grid grid-cols-[minmax(0,1fr)_2.5rem_2.5rem_2rem_3.25rem] items-center gap-1 py-1 text-xs tabular-nums">
            <span className="min-w-0 truncate text-text-secondary"><span className="font-sans text-text-muted">{r.position}</span> <span className="font-sans">{r.player}</span></span>
            <span className="text-right text-text-muted">{r.my_pos_rank}</span>
            <span className="text-right text-text-muted">{r.ecr_pos_rank}</span>
            <span className="text-right font-medium" style={{ color: tone }}>{r.delta > 0 ? '+' : ''}{r.delta}</span>
            <span className="text-right text-text-secondary">{r.games >= 2 && r.actual_ppg != null ? `${num(r.actual_ppg)}pg` : '–'}</span>
          </div>
        ))}
      </div>
    </div>
  );
}

function PerformanceDiagnostics({ acc, players }: { acc: any[]; players: any[] }) {
  const positions = ['QB', 'RB', 'WR', 'TE'];
  const groups = positions.map(position => obj(acc.find((r: any) => r.group === position)));
  const scored = groups.filter(g => g.my_acc != null && g.ecr_acc != null);
  const modelPositionWins = scored.filter(g => g.my_acc > g.ecr_acc).length;
  const evaluated = arr(players).filter((p: any) => p.my_err != null && p.ecr_err != null);
  const meanAbs = (key: string) => evaluated.length
    ? evaluated.reduce((sum: number, p: any) => sum + Math.abs(p[key]), 0) / evaluated.length : null;
  const modelMae = meanAbs('my_err');
  const ecrMae = meanAbs('ecr_err');
  const modelCloser = evaluated.filter((p: any) => Math.abs(p.my_err) < Math.abs(p.ecr_err)).length;
  const ecrCloser = evaluated.filter((p: any) => Math.abs(p.ecr_err) < Math.abs(p.my_err)).length;
  const tied = evaluated.length - modelCloser - ecrCloser;
  const width = (v: number | null | undefined) => `${Math.max(0, Math.min(1, v ?? 0)) * 100}%`;

  return (
    <Section title="Performance diagnostics" accent={WIN}
      subtitle="Two ways to test the rankings: overall ordering and each player’s positional-rank miss">
      <div className="grid gap-3 lg:grid-cols-[minmax(0,1.45fr)_minmax(15rem,0.8fr)]">
        <div className="rounded-xl border border-border-light bg-bg-card/30 px-3 py-2.5">
          <div className="mb-2 flex items-center justify-between text-[10px] uppercase tracking-wider text-text-muted">
            <span>Rank correlation by position</span>
            <span><span style={{ color: MODEL_BLUE }}>Model</span> · <span style={{ color: ECR_ORANGE }}>ECR</span></span>
          </div>
          <div className="space-y-2">
            {positions.map((position, i) => {
              const g = groups[i];
              const diff = (g.my_acc ?? 0) - (g.ecr_acc ?? 0);
              return (
                <div key={position} className="grid grid-cols-[1.75rem_minmax(0,1fr)_3.25rem] items-center gap-2">
                  <span className="text-[10px] font-semibold text-text-secondary">{position}</span>
                  <div className="space-y-1">
                    <div className="h-1.5 overflow-hidden rounded-full bg-bg-card-hover"><div className="h-full rounded-full" style={{ width: width(g.my_acc), background: MODEL_BLUE }} /></div>
                    <div className="h-1.5 overflow-hidden rounded-full bg-bg-card-hover"><div className="h-full rounded-full" style={{ width: width(g.ecr_acc), background: ECR_ORANGE }} /></div>
                  </div>
                  <span className={`text-right text-[10px] font-medium tabular-nums ${diff > 0 ? 'text-win' : diff < 0 ? 'text-loss' : 'text-text-muted'}`}>{diff >= 0 ? '+' : ''}{num(diff, 3)}</span>
                </div>
              );
            })}
          </div>
        </div>
        <div className="grid grid-cols-2 gap-2">
          <div className="rounded-xl border border-border-light bg-bg-card/30 px-3 py-2.5">
            <div className="text-[9px] uppercase tracking-wider text-text-muted">Position wins</div>
            <div className="mt-1 text-lg font-bold tabular-nums text-text-primary">{modelPositionWins}<span className="text-sm font-medium text-text-muted"> / {scored.length}</span></div>
            <div className="text-[10px] leading-snug text-text-muted">positions where the model’s rank correlation beat ECR</div>
          </div>
          <div className="rounded-xl border border-border-light bg-bg-card/30 px-3 py-2.5">
            <div className="text-[9px] uppercase tracking-wider text-text-muted">Average rank miss</div>
            <div className="mt-1 text-lg font-bold tabular-nums" style={{ color: modelMae != null && ecrMae != null && modelMae <= ecrMae ? WIN : ECR_ORANGE }}>{modelMae != null ? num(modelMae, 1) : '–'}<span className="text-sm font-medium text-text-muted"> vs {ecrMae != null ? num(ecrMae, 1) : '–'}</span></div>
            <div className="text-[10px] leading-snug text-text-muted">absolute positional ranks missed · lower is better</div>
          </div>
          <div className="col-span-2 rounded-xl border border-border-light bg-bg-card/30 px-3 py-2.5">
            <div className="flex items-baseline justify-between gap-2"><span className="text-[9px] uppercase tracking-wider text-text-muted">Closer player calls</span><span className="text-[10px] tabular-nums text-text-muted">n={evaluated.length}</span></div>
            <div className="mt-1.5 grid grid-cols-3 gap-2 text-center text-[10px] tabular-nums">
              <span><b className="text-win">{modelCloser}</b><span className="block text-text-muted">model closer</span></span>
              <span><b className="text-text-secondary">{tied}</b><span className="block text-text-muted">ties</span></span>
              <span><b style={{ color: ECR_ORANGE }}>{ecrCloser}</b><span className="block text-text-muted">ECR closer</span></span>
            </div>
          </div>
        </div>
      </div>
    </Section>
  );
}

export function RankingsReport({ season, week, embedded }: RankingsReportProps) {
  const [index, setIndex] = useState<IndexFile | null>(null);
  const [snap, setSnap] = useState<any>(null);
  const [loaded, setLoaded] = useState(false);
  const [pos, setPos] = useState('ALL');
  const [sort, setSort] = useState<{ key: string; dir: 1 | -1 }>({ key: 'my_overall', dir: 1 });
  // Scoring format. Snapshots for different formats coexist in the index, so the
  // resolver MUST filter on it — otherwise a half-PPR board can be served under a
  // full-PPR heading. Defaults to whichever format was published most recently.
  const [fmt, setFmt] = useState<string | null>(null);

  useEffect(() => {
    getJSON<IndexFile>(dataUrl('/data/rankings/index.json')).then(idx => {
      setIndex(idx);
      setFmt(f => f ?? idx?.scoring ?? 'ppr');
    });
  }, []);

  // A completed season only has the formats that were actually published for
  // it (2024/25 are full-PPR). Never let the current season's default format
  // resolve to a different year's snapshot.
  const seasonFormats = useMemo(() => Array.from(new Set(
    arr(index?.snapshots).filter(s => s.season === season).map(s => s.scoring ?? 'ppr'),
  )), [index, season]);
  const activeFmt = seasonFormats.includes(fmt ?? '')
    ? fmt!
    : seasonFormats.includes('ppr') ? 'ppr' : seasonFormats[0] ?? fmt ?? 'ppr';
  const formats = seasonFormats.length ? seasonFormats : index?.formats ?? [];

  useEffect(() => {
    if (seasonFormats.length && fmt !== activeFmt) setFmt(activeFmt);
  }, [fmt, activeFmt, seasonFormats.length]);

  // Resolve the snapshot for the header's season/week: exact week, else the latest
  // snapshot at or before it in that season, else the season's newest, else global latest.
  const resolvedTag = useMemo(() => {
    const snaps = arr(index?.snapshots).filter(s => (s.scoring ?? 'ppr') === activeFmt);
    if (!snaps.length) return null;
    const inSeason = snaps.filter(s => s.season === season);
    if (inSeason.length) {
      // The vs-ECR tab in a Season Report is a completed-season scorecard.
      // Show its final snapshot, not whatever week is selected elsewhere in
      // the app (which may be the current season's Week 1).
      if (embedded) return [...inSeason].sort((a, b) => b.week - a.week)[0].tag;
      const exact = inSeason.find(s => s.week === week);
      if (exact) return exact.tag;
      const atOrBefore = inSeason.filter(s => s.week <= week).sort((a, b) => b.week - a.week)[0];
      return (atOrBefore ?? inSeason.sort((a, b) => b.week - a.week)[0]).tag;
    }
    return index?.latest ?? snaps[0].tag;
  }, [index, season, week, activeFmt]);

  useEffect(() => {
    if (!resolvedTag) { setSnap(null); setLoaded(true); return; }
    setLoaded(false);
    setSnap(null);
    getJSON(dataUrl(`/data/rankings/${resolvedTag}.json`)).then(d => { setSnap(d); setLoaded(true); });
  }, [resolvedTag]);

  if (index && (index.snapshots?.length ?? 0) === 0)
    return (
      <div className="text-center py-16 text-text-muted text-sm">
        <p>No rankings snapshots yet.</p>
        <p className="mt-1 text-xs">Run <code className="text-accent">weekly_run/export_rankings_ecr.R &lt;season&gt;</code>.</p>
      </div>
    );

  const acc = snap?.accuracy ?? [];
  const all = obj(acc.find ? acc.find((a: any) => a.group === 'ALL') : acc);
  const hasActuals = snap?.has_actuals;
  const leader = hasActuals && all.my_acc != null && all.ecr_acc != null
    ? (all.my_acc >= all.ecr_acc ? 'model' : 'ecr') : null;

  const players = useMemo(() => {
    const rows = arr(snap?.players).filter((p: any) => pos === 'ALL' || p.position === pos);
    const { key, dir } = sort;
    const str = key === 'player' || key === 'team' || key === 'position';
    return [...rows].sort((a: any, b: any) => {
      const av = a[key], bv = b[key];
      if (av == null && bv == null) return 0;
      if (av == null) return 1;            // nulls always last
      if (bv == null) return -1;
      return str ? dir * String(av).localeCompare(String(bv)) : dir * (av - bv);
    });
  }, [snap, pos, sort]);
  const viewingWk = snap?.week;

  const sortBy = (key: string, defDir: 1 | -1 = 1) =>
    setSort(s => (s.key === key ? { key, dir: (s.dir * -1) as 1 | -1 } : { key, dir: defDir }));

  // Summary block renders inside a full page hero standalone, or a plain panel
  // when embedded in Season Report (which already supplies the page hero).
  const summary = (<>
        {!loaded ? <Skeleton h={80} />
          : !hasActuals ? (
            <div className="text-[11px] text-text-muted">No games played yet — showing the ranking disagreement only. Accuracy scoring appears once results come in.</div>
          ) : (
            <>
              <div className="grid grid-cols-2 sm:grid-cols-4 gap-2">
                <StatTile label="My rank accuracy" value={num(all.my_acc, 3)} sub={`ρ vs actual PPG · n=${all.n}`} accent={leader === 'model' ? WIN : MODEL_BLUE} />
                <StatTile label="ECR rank accuracy" value={num(all.ecr_acc, 3)} sub="FantasyPros" accent={leader === 'ecr' ? WIN : ECR_ORANGE} />
                <StatTile label="Leader" value={leader === 'model' ? 'My model' : leader === 'ecr' ? 'ECR' : '–'}
                  sub={all.my_acc != null ? `${all.my_acc >= all.ecr_acc ? '+' : ''}${num(all.my_acc - all.ecr_acc, 3)}` : ''}
                  accent={leader === 'model' ? WIN : ECR_ORANGE} />
                <StatTile label="Top-12 hit (all)" value={all.my_hit12 != null ? `${Math.round(all.my_hit12 * 100)}%` : '–'}
                  sub={all.ecr_hit12 != null ? `ecr ${Math.round(all.ecr_hit12 * 100)}%` : ''} accent={MODEL_BLUE} />
              </div>
              {/* RB/WR carry through T36 because those are the deeper weekly
                  starter pools; QB/TE retain their useful T12/T24 view. */}
              <div className="grid grid-cols-2 sm:grid-cols-4 gap-2">
                {['QB', 'RB', 'WR', 'TE'].map(p => {
                  const g = obj(acc.find ? acc.find((a: any) => a.group === p) : null);
                  const cutoffs = p === 'RB' || p === 'WR'
                    ? [{ label: 'T12', mine: 'my_hit12', ecr: 'ecr_hit12' }, { label: 'T24', mine: 'my_hit24', ecr: 'ecr_hit24' }, { label: 'T36', mine: 'my_hit36', ecr: 'ecr_hit36' }]
                    : [{ label: 'T12', mine: 'my_hit12', ecr: 'ecr_hit12' }, { label: 'T24', mine: 'my_hit24', ecr: 'ecr_hit24' }];
                  const cell = (mine: number | null, ecr: number | null) => {
                    if (mine == null) return <div className="text-sm font-bold text-text-muted">–</div>;
                    const win = ecr != null && mine > ecr, loss = ecr != null && mine < ecr;
                    return (
                      <div>
                        <div className="text-sm font-bold tabular-nums" style={{ color: win ? WIN : loss ? ECR_ORANGE : 'var(--color-text-primary)' }}>
                          {Math.round(mine * 100)}%
                        </div>
                        <div className="text-[10px] text-text-muted tabular-nums">ecr {ecr != null ? `${Math.round(ecr * 100)}%` : '–'}</div>
                      </div>
                    );
                  };
                  return (
                    <div key={p} className="rounded-xl border border-border bg-gradient-to-b from-bg-card to-bg-card/30 px-3 py-2">
                      <div className="text-[10px] uppercase tracking-wider text-text-muted mb-1">{p} hit rate</div>
                      <div className={`grid gap-2 ${cutoffs.length === 3 ? 'grid-cols-3' : 'grid-cols-2'}`}>
                        {cutoffs.map(c => <div key={c.label}><div className="text-[9px] uppercase text-text-muted">{c.label}</div>{cell(g[c.mine], g[c.ecr])}</div>)}
                      </div>
                    </div>
                  );
                })}
              </div>
            </>
          )}
  </>);

  return (
    <div className="space-y-4">
      {embedded ? (
        <div className="rounded-2xl border border-border-light bg-bg-card/40 p-4 space-y-3">
          <div className="flex items-center justify-between">
            <div className="flex flex-wrap items-center gap-2">
              <span className="text-[11px] text-text-muted">{FMT_LABEL[activeFmt] ?? activeFmt} · {hasActuals ? `final performance through ${getWeekLabel(viewingWk)}` : 'preseason rankings'}</span>
              <FreshnessTag updated={snap?.updated} />
            </div>
            {formats.length > 1 && (
              <div className="flex gap-1">
                {formats.map(f => (
                  <button key={f} onClick={() => setFmt(f)}
                    className={`px-2 py-0.5 rounded-md text-[11px] border transition-colors ${
                      activeFmt === f ? 'border-accent text-accent bg-accent/10' : 'border-border text-text-muted hover:text-text-secondary'}`}>
                    {FMT_LABEL[f] ?? f}
                  </button>
                ))}
              </div>
            )}
          </div>
          {summary}
        </div>
      ) : (
        <PageHero kicker="Rankings vs ECR"
          title={`${season}${viewingWk != null && viewingWk >= 1 ? ` · through ${getWeekLabel(viewingWk)}` : ' · Preseason'}`}
          right={<div className="flex items-center gap-2">
            <FreshnessTag updated={snap?.updated} />
            {formats.length > 1 && (
              <div className="flex gap-1">
                {formats.map(f => (
                  <button key={f} onClick={() => setFmt(f)}
                    className={`px-2 py-0.5 rounded-md text-[11px] border transition-colors ${
                      activeFmt === f ? 'border-accent text-accent bg-accent/10' : 'border-border text-text-muted hover:text-text-secondary'}`}>
                    {FMT_LABEL[f] ?? f}
                  </button>
                ))}
              </div>
            )}
            <span className="text-[11px] text-text-muted">my season model vs FantasyPros ECR · {FMT_LABEL[activeFmt] ?? activeFmt}</span>
          </div>}>
          {summary}
        </PageHero>
      )}

      {hasActuals && <PerformanceDiagnostics acc={acc} players={arr(snap?.players)} />}

      {/* biggest disagreements */}
      <Section title="Biggest disagreements" accent={MODEL_BLUE}
        subtitle="where my model and ECR most diverge (Δ = ECR pos-rank − my pos-rank)">
        <div className="grid sm:grid-cols-2 gap-x-6 gap-y-3">
          <DisagreementList title="My model higher (values)" rows={snap?.values} tone={MODEL_BLUE} />
          <DisagreementList title="My model lower (fades)" rows={snap?.fades} tone={ECR_ORANGE} />
        </div>
      </Section>

      {/* full comparison */}
      <Section title="Full board" accent={ECR_ORANGE} subtitle="my season model rank vs FantasyPros ECR"
        right={
          <div className="flex items-center gap-2">
            <span className="hidden text-[10px] text-text-muted sm:inline">{players.length} players · scroll to explore</span>
            <div className="flex gap-1">
              {['ALL', 'QB', 'RB', 'WR', 'TE'].map(p => (
                <button key={p} onClick={() => setPos(p)}
                  className={`rounded-md px-2 py-0.5 text-[11px] font-medium border transition-colors ${pos === p ? 'border-accent text-accent bg-accent/10' : 'border-border text-text-muted hover:text-text-secondary'}`}>{p}</button>
              ))}
            </div>
          </div>}>
        <div className="max-h-[42rem] overflow-auto rounded-xl border border-border-light bg-bg-card/30 overscroll-contain">
          <table className="min-w-[76rem] w-full text-xs tabular-nums">
            <thead className="sticky top-0 z-10 bg-bg-card shadow-[0_1px_0_var(--color-border-light)]"><tr className="text-text-muted">
              {([
                ['Pos#', 'my_pos_rank', 'left'], ['Player', 'player', 'left'], ['Tm', 'team', 'left'],
                ['My Rank', 'my_pos_rank', 'right'], ['ECR', 'ecr_pos_rank', 'right'], ['Δ', 'delta', 'left'], ['Proj', 'proj', 'right'], ['VOR', 'vor', 'right'],
                ...(hasActuals ? [['Pts Finish', 'actual_finish', 'right'], ['PPG Finish', 'actual_ppg_finish', 'right'], ['My Err', 'my_err', 'right'], ['ECR Err', 'ecr_err', 'right'], ['PPG (gms)', 'actual_ppg', 'right'], ['Pts', 'actual_pts', 'right']] : []),
              ] as [string, string, string][]).map(([label, key, align]) => (
                <th key={label} onClick={() => sortBy(key, key === 'proj' || key === 'vor' || key === 'actual_ppg' || key === 'actual_pts' || key === 'delta' ? -1 : 1)}
                  className={`px-2.5 py-2 font-medium cursor-pointer select-none hover:text-text-primary whitespace-nowrap ${align === 'right' ? 'text-right' : 'text-left'}`}>
                  {label}{sort.key === key ? (sort.dir === 1 ? ' ▲' : ' ▼') : ''}
                </th>
              ))}
            </tr></thead>
            <tbody>
              {players.map((p: any, i: number) => (
                <tr key={i} className="border-b border-border-light/50 hover:bg-bg-card-hover">
                  <td className="px-2.5 py-1.5 text-text-muted">{p.position}{p.my_pos_rank}</td>
                  <td className="px-2.5 py-1.5 text-text-primary whitespace-nowrap">{p.player}</td>
                  <td className="px-2.5 py-1.5"><TeamChip team={p.team ?? ''} /></td>
                  <td className="px-2.5 py-1.5 text-right" style={{ color: MODEL_BLUE }}>{p.my_pos_rank}</td>
                  <td className="px-2.5 py-1.5 text-right" style={{ color: ECR_ORANGE }}>{p.ecr_pos_rank}</td>
                  <td className="px-2.5 py-1.5 whitespace-nowrap"><DeltaBar v={p.delta} /></td>
                  <td className="px-2.5 py-1.5 text-right text-text-secondary">{num(p.proj)}</td>
                  {/* VOR is the cross-position currency: pos ranks say WHO I prefer
                      within a position, VOR says how much the disagreement is worth. */}
                  <td className="px-2.5 py-1.5 text-right text-text-primary font-medium">
                    {p.vor != null ? Math.round(p.vor) : <span className="text-text-muted font-normal">–</span>}
                  </td>
                  {hasActuals && (
                    <>
                      <td className="px-2.5 py-1.5 text-right text-text-primary font-medium">
                        {p.actual_finish != null ? `${p.position}${p.actual_finish}` : '–'}
                      </td>
                      <td className="px-2.5 py-1.5 text-right text-text-secondary font-medium">
                        {p.actual_ppg_finish != null ? `${p.position}${p.actual_ppg_finish}` : '–'}
                      </td>
                      <ErrCell err={p.my_err} otherErr={p.ecr_err} />
                      <ErrCell err={p.ecr_err} otherErr={p.my_err} />
                      <td className="px-2.5 py-1.5 text-right text-text-secondary whitespace-nowrap">
                        {p.games >= 1 ? `${num(p.actual_ppg)} (${p.games})` : '–'}
                      </td>
                      <td className="px-2.5 py-1.5 text-right text-text-primary">
                        {p.actual_pts != null ? num(p.actual_pts) : '–'}
                      </td>
                    </>
                  )}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </Section>
    </div>
  );
}
