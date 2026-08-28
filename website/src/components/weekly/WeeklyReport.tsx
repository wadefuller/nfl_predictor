import { createContext, useContext, useEffect, useMemo, useState } from 'react';
import { getTeamColor } from '../../data/teamColors';
import { teamNames } from '../../data/teamMeta';
import { WinProbabilityBar } from '../games/WinProbabilityBar';
import { num, clamp, hexToRgba, obj, StatTile, TeamChip, EdgeBadge, RangeBar, Skeleton, readableTeamColor, SubNav, arr } from '../ui/primitives';
import { TopBets, type TopBetRow } from '../games/TopBets';
import { SectionBoundary } from '../ui/SectionBoundary';
import { LeagueReportSection } from './LeagueReportSection';
import { dataUrl } from '../../lib/dataUrl';

// ---- types -----------------------------------------------------------------
interface WeekEntry {
  tag: string; season: number; week: number; phase: string;
  reports: string[]; updated: string;
}
interface IndexFile { weeks: WeekEntry[]; latest?: string }

const SECTION_ACCENT: Record<string, string> = {
  games_preview: '#58a6ff', games_result: '#58a6ff', bet_card: '#d29922', lineups: '#3fb950', waivers: '#a371f7',
  projections: '#58a6ff', model_scorecard: '#3fb950', fantasy_recap: '#f778ba',
  weekly_accuracy: '#d29922', league_report: '#f778ba', dst_stream: '#56b6c2', fantasy_ranks: '#3fb950',
};

const ReportFreshnessContext = createContext<string | null>(null);

function FreshnessTag() {
  const updated = useContext(ReportFreshnessContext);
  if (!updated) return null;
  const parsed = new Date(updated.replace(/([+-]\d{2})(\d{2})$/, '$1:$2'));
  const label = Number.isNaN(parsed.getTime())
    ? updated
    : parsed.toLocaleString([], { month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit' });
  return <span title={`Report generated ${updated}`} className="rounded-full border border-border-light bg-bg-card-hover px-1.5 py-0.5 text-[9px] font-medium tabular-nums text-text-muted">Data as of {label}</span>;
}

async function getJSON<T>(url: string): Promise<T | null> {
  try { const r = await fetch(url); return r.ok ? ((await r.json()) as T) : null; } catch { return null; }
}

// ---- section + report-specific primitives ----------------------------------
function Section({ id, title, subtitle, accent = 'var(--color-accent)', right, children }:
  { id: string; title: string; subtitle?: string; accent?: string; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <section id={id} className="scroll-mt-28 rounded-2xl border border-border bg-bg-card/60 overflow-hidden">
      <div className="flex items-center justify-between border-b border-border-light px-4 py-3"
        style={{ boxShadow: `inset 3px 0 0 ${accent}` }}>
        <div>
          <h3 className="text-sm font-semibold text-text-primary tracking-tight">{title}</h3>
          {subtitle && <p className="text-[11px] text-text-muted mt-0.5">{subtitle}</p>}
        </div>
        <div className="flex items-center gap-2">
          <FreshnessTag />
          {right}
        </div>
      </div>
      <div className="p-4 space-y-3">{children}</div>
    </section>
  );
}

const CLASS_STYLE: Record<string, string> = {
  boom: 'text-win border-win/40 bg-win/10', bust: 'text-loss border-loss/40 bg-loss/10',
  met: 'text-text-secondary border-border bg-bg-card-hover',
};
const ClassPill = ({ c }: { c: string }) => (
  <span className={`rounded-full border px-2 py-0.5 text-[9px] font-bold uppercase tracking-widest ${CLASS_STYLE[c] ?? CLASS_STYLE.met}`}>{c}</span>
);

function PlayerRow({ p, max }: { p: any; max: number }) {
  const c = readableTeamColor(p.team || '');
  return (
    <div className="grid grid-cols-[2.2rem_1fr_2.4rem] items-center gap-2">
      <span className="text-[10px] font-semibold text-text-muted">{p.position}</span>
      <div className="min-w-0">
        <div className="flex items-baseline justify-between gap-2">
          <span className="truncate text-xs text-text-secondary">{p.player}</span>
          <span className="text-xs font-semibold text-text-primary tabular-nums">{num(p.proj)}</span>
        </div>
        <div className="mt-1"><RangeBar floor={p.floor} proj={p.proj} ceiling={p.ceiling} max={max} color={c} /></div>
      </div>
      <span className="text-[10px] text-text-muted tabular-nums text-right">{num(p.floor, 0)}–{num(p.ceiling, 0)}</span>
    </div>
  );
}

// ---- report renderers ------------------------------------------------------
function GamesPreview({ d }: { d: any }) {
  const games = d.games ?? [];
  return (
    <Section id="games_preview" title="Games Preview" accent={SECTION_ACCENT.games_preview}
      subtitle="model vs market implied totals · key projections · sorted by edge"
      right={<span className="text-[11px] text-text-muted">{games.length} games</span>}>
      <div className="grid lg:grid-cols-2 gap-3">
        {games.map((g: any, i: number) => {
          const ah = getTeamColor(g.away_team), hh = getTeamColor(g.home_team);
          const max = 32;
          const TeamSide = ({ team, mkt, pred, players }: any) => {
            const c = readableTeamColor(team);
            const delta = pred - mkt;
            return (
              <div className="space-y-2">
                <div className="flex items-center justify-between">
                  <TeamChip team={team} />
                  <span className="text-[10px] text-text-muted">{teamNames[team] ?? ''}</span>
                </div>
                <div className="flex items-end gap-2">
                  <span className="text-2xl font-bold tabular-nums leading-none" style={{ color: c }}>{num(pred)}</span>
                  <span className="text-[10px] text-text-muted mb-0.5">model · mkt {num(mkt)}
                    {Math.abs(delta) >= 0.1 && <span className={delta > 0 ? 'text-win' : 'text-loss'}> ({delta > 0 ? '+' : ''}{num(delta)})</span>}
                  </span>
                </div>
                <div className="space-y-1.5 pt-1">
                  {arr(players).map((p: any, j: number) => <PlayerRow key={j} p={{ ...p, team }} max={max} />)}
                </div>
              </div>
            );
          };
          return (
            <div key={i} className="rounded-xl border border-border-light overflow-hidden"
              style={{ background: `linear-gradient(135deg, ${hexToRgba(ah, 0.07)}, ${hexToRgba(hh, 0.07)})` }}>
              <div className="flex items-center justify-between px-3 py-2 border-b border-border-light bg-bg-page/30">
                <span className="text-sm font-bold text-text-primary">{g.away_team} <span className="text-text-muted font-normal">@</span> {g.home_team}</span>
                <div className="flex items-center gap-2 text-[10px] text-text-muted">
                  <span>spread {num(g.spread_line)}</span><span>·</span><span>O/U {num(g.total_line)}</span>
                  <EdgeBadge v={g.spread_edge_home} />
                </div>
              </div>
              <div className="px-3 py-2">
                <WinProbabilityBar awayTeam={g.away_team} homeTeam={g.home_team}
                  awayWinPct={1 - (g.pred_win_home ?? 0.5)} homeWinPct={g.pred_win_home ?? 0.5} />
              </div>
              <div className="grid grid-cols-2 gap-4 px-3 pb-3">
                <TeamSide team={g.away_team} mkt={g.away_implied_market} pred={g.away_implied_pred} players={g.away_players} />
                <TeamSide team={g.home_team} mkt={g.home_implied_market} pred={g.home_implied_pred} players={g.home_players} />
              </div>
            </div>
          );
        })}
      </div>
    </Section>
  );
}

// Post-week twin of GamesPreview. Same source-colour system as the game cards
// (blue = my model, grey = market) and the same proj→actual idiom as the fantasy
// sections, so Results reads like the rest of the site rather than a new dialect.
/** The model's side on one market, plus whether it cashed. */
function PickBadge({ kind, pick, line, correct }:
  { kind: 'ml' | 'ats'; pick?: string | null; line?: number | null; correct?: number | null }) {
  const tone = correct === 1
    ? 'text-win border-win/40 bg-win/10'
    : correct === 0 ? 'text-loss border-loss/40 bg-loss/10'
      : 'text-text-muted border-border-light';
  // A push, or a week with no OOS prediction to attribute a side to.
  if (!pick) return <span className={`rounded border px-1 py-px uppercase ${tone}`}>{kind}</span>;
  // +3.5 needs its sign; -3.5 already carries one.
  const num = line == null ? '' : ` ${line > 0 ? '+' : ''}${line}`;
  return (
    <span className={`inline-flex items-center gap-1 rounded border px-1.5 py-px ${tone}`}>
      <span className="uppercase opacity-60">{kind}</span>
      <span className="tabular-nums">{pick}{num}</span>
      <span aria-hidden="true">{correct === 1 ? '✓' : correct === 0 ? '✗' : '–'}</span>
      <span className="sr-only">{correct === 1 ? 'won' : correct === 0 ? 'lost' : 'push'}</span>
    </span>
  );
}

function GamesResult({ d }: { d: any }) {
  const games = d.games ?? [];
  const MODEL = '#58a6ff', VEGAS = '#8b949e';
  const pct = (v: number | null) => (v == null ? '–' : `${Math.round(v * 100)}%`);

  const PlayerLine = ({ p }: { p: any }) => {
    const tone = p.classed === 'boom' ? '#3fb950' : p.classed === 'bust' ? '#f85149' : 'var(--color-text-secondary)';
    return (
      <div className="py-0.5">
        <div className="flex items-baseline justify-between gap-2 text-[11px]">
          <span className="truncate text-text-secondary">
            <span className="text-text-muted">{p.position}</span> {p.player}
          </span>
          <span className="tabular-nums whitespace-nowrap text-text-muted">
            {num(p.proj)}<span className="mx-0.5">→</span>
            <span className="font-semibold" style={{ color: tone }}>{num(p.actual)}</span>
            <span className="ml-1" style={{ color: tone }}>
              ({p.diff > 0 ? '+' : ''}{num(p.diff)})
            </span>
          </span>
        </div>
        {/* What actually happened, so a miss reads as "3 catches" rather than
            just a number that came up short. */}
        {p.stat && (
          <div className="text-[10px] leading-tight text-text-muted/70 tabular-nums">{p.stat}</div>
        )}
      </div>
    );
  };

  return (
    <Section id="games_result" title="Games" accent={SECTION_ACCENT.games_preview}
      subtitle="final scores · model vs market probability · projected → actual half-PPR points, with the box-score line behind each"
      right={<span className="text-[11px] text-text-muted">{games.length} games · sorted by edge</span>}>
      <div className="grid lg:grid-cols-2 gap-3">
        {games.map((g: any, i: number) => {
          const homeWon = g.home_points > g.away_points;
          const bigEdge = Math.abs(g.edge ?? 0) >= 0.05;
          return (
            <div key={i} className="rounded-xl border border-border-light overflow-hidden">
              {/* score line */}
              <div className="flex items-center justify-between px-3 py-2 border-b border-border-light bg-bg-page/30">
                <span className="text-sm font-bold">
                  <span className={homeWon ? 'text-text-muted' : 'text-text-primary'}>
                    {g.away_team} {g.away_points}
                  </span>
                  <span className="text-text-muted font-normal mx-1.5">·</span>
                  <span className={homeWon ? 'text-text-primary' : 'text-text-muted'}>
                    {g.home_team} {g.home_points}
                  </span>
                </span>
                {/* Name the side the model took, not just whether it hit — a
                    bare green "ATS" tells you it won without saying on whom. */}
                <div className="flex items-center gap-1.5 text-[9px] font-semibold tracking-wide">
                  <PickBadge kind="ml" pick={g.ml_pick} correct={g.ml_correct} />
                  <PickBadge kind="ats" pick={g.ats_pick} line={g.ats_line} correct={g.cover_correct} />
                </div>
              </div>

              {/* model vs market probability, stated for the home team */}
              <div className="px-3 py-2 border-b border-border-light">
                <div className="flex items-center justify-between text-[9px] uppercase tracking-wide text-text-muted mb-1">
                  <span>{g.home_team} win probability</span>
                  {bigEdge && <span style={{ color: '#d29922' }}>edge {g.edge > 0 ? '+' : ''}{Math.round(g.edge * 100)}</span>}
                </div>
                <div className="relative h-4 rounded bg-bg-card-hover overflow-hidden">
                  <div className="absolute inset-y-0 left-0" style={{ width: `${(g.model_win_home ?? 0) * 100}%`, background: `${MODEL}66` }} />
                  {g.vegas_win_home != null && (
                    <div className="absolute inset-y-0 border-l-2 border-dashed"
                      style={{ left: `${g.vegas_win_home * 100}%`, borderColor: VEGAS }} />
                  )}
                </div>
                <div className="flex justify-between text-[10px] mt-0.5 tabular-nums">
                  <span style={{ color: MODEL }}>model {pct(g.model_win_home)}</span>
                  <span className="text-text-muted">vegas {pct(g.vegas_win_home)}</span>
                </div>
              </div>

              {/* projected → actual, per side. Stacked on phones — two columns
                  inside one card leaves ~145px a side, which truncates names to
                  "QB …" and defeats the point of naming the players at all. */}
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-3 gap-y-4 px-3 py-2">
                {[[g.away_team, g.away_players], [g.home_team, g.home_players]].map(([team, rows]: any, j) => (
                  <div key={j} className="space-y-0.5 min-w-0">
                    {/* Name the columns — the bare "19.6 → 8.8 (-10.8)" idiom is
                        only obvious once you already know what it means. */}
                    <div className="flex items-baseline justify-between gap-2 border-b border-border-light/60 pb-1 mb-1">
                      <span className="text-[10px] font-semibold text-text-muted">{team}</span>
                      <span className="text-[9px] uppercase tracking-wide text-text-muted whitespace-nowrap">
                        proj <span className="mx-0.5">→</span> actual <span className="normal-case">(diff)</span>
                      </span>
                    </div>
                    {arr(rows).length === 0
                      ? <div className="text-[10px] text-text-muted">no projections</div>
                      : rows.map((p: any, k: number) => <PlayerLine key={k} p={p} />)}
                  </div>
                ))}
              </div>
            </div>
          );
        })}
      </div>
    </Section>
  );
}

// Post-week fantasy board: MY projected positional rank vs the rank the player
// actually finished at this week. Points say how much he scored; rank says
// whether he was startable — which is the call the lineup actually turned on.
function FantasyRanks({ d }: { d: any }) {
  const [pos, setPos] = useState('ALL');
  const [sort, setSort] = useState<{ key: string; dir: 1 | -1 }>({ key: 'proj_rank', dir: 1 });
  const sortBy = (key: string, defDir: 1 | -1 = 1) =>
    setSort(s => (s.key === key ? { key, dir: (s.dir * -1) as 1 | -1 } : { key, dir: defDir }));

  const rows = useMemo(() => {
    const all = arr(d.players).filter((p: any) => pos === 'ALL' || p.position === pos);
    const { key, dir } = sort;
    const str = key === 'player' || key === 'team' || key === 'position';
    return [...all].sort((a: any, b: any) => {
      const av = a[key], bv = b[key];
      if (av == null && bv == null) return 0;
      if (av == null) return 1;
      if (bv == null) return -1;
      return str ? dir * String(av).localeCompare(String(bv)) : dir * (av - bv);
    });
  }, [d, pos, sort]);

  // − = finished BETTER than projected (rank moved up); + = we ranked him too high
  const diffColor = (v: number) =>
    v <= -10 ? '#3fb950' : v >= 10 ? '#f85149' : 'var(--color-text-muted)';

  const cols: [string, string, 1 | -1][] = [
    ['Player', 'player', 1], ['Tm', 'team', 1],
    ['Proj', 'proj', -1], ['My rank', 'proj_rank', 1],
    ['Actual', 'actual', -1], ['Finished', 'actual_rank', 1],
    ['Δ rank', 'rank_diff', 1], ['Δ pts', 'pts_diff', -1],
  ];

  return (
    <Section id="fantasy_ranks" title="Projected vs actual rank" accent={SECTION_ACCENT.projections}
      subtitle="my positional ranking against where they actually finished this week"
      right={
        <div className="flex gap-1">
          {['ALL', 'QB', 'RB', 'WR', 'TE'].map(p => (
            <button key={p} onClick={() => setPos(p)}
              className={`rounded-md px-2 py-0.5 text-[11px] font-medium border transition-colors ${
                pos === p ? 'border-accent text-accent bg-accent/10' : 'border-border text-text-muted hover:text-text-secondary'}`}>
              {p}
            </button>
          ))}
        </div>}>
      {/* aggregate performance for the selected slice — tiles follow the position
          filter, so the summary always describes the board underneath it */}
      {(() => {
        const mAll: any[] = Array.isArray(d.metrics) ? d.metrics : d.metrics ? [d.metrics] : [];
        const m = obj(mAll.find((x: any) => x.group === pos) ?? mAll.find((x: any) => x.group === 'ALL'));
        if (m.n == null) return null;
        const corr = m.rank_corr ?? 0;
        return (
          <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-2">
            <StatTile label="Rank accuracy" value={num(m.rank_corr, 2)}
              sub={`ρ · ${m.n} players`} accent={corr >= 0.5 ? '#3fb950' : corr >= 0.3 ? '#d29922' : '#f85149'} />
            <StatTile label="Top-12 hit" value={m.hit12 != null ? `${Math.round(m.hit12 * 100)}%` : '–'}
              sub="of projected top 12" accent="#58a6ff" />
            <StatTile label="Median rank miss" value={m.med_rank_err ?? '–'} sub="positions off" />
            <StatTile label="Points MAE" value={num(m.mae_pts, 2)} sub="half-PPR" />
            <StatTile label="Booms" value={m.booms ?? 0} accent="#3fb950" />
            <StatTile label="Busts" value={m.busts ?? 0} accent="#f85149" />
          </div>
        );
      })()}
      <div className="overflow-x-auto">
        <table className="w-full text-xs">
          <thead>
            <tr className="text-text-muted border-b border-border-light">
              {cols.map(([label, key, dd], i) => (
                <th key={key} onClick={() => sortBy(key, dd)}
                  className={`py-1.5 pr-3 font-medium cursor-pointer select-none hover:text-text-primary whitespace-nowrap ${i === 0 ? 'text-left' : 'text-right'}`}>
                  {label}{sort.key === key ? (sort.dir === 1 ? ' ▲' : ' ▼') : ''}
                </th>
              ))}
              <th className="py-1.5 font-medium text-right">Result</th>
            </tr>
          </thead>
          <tbody>
            {rows.slice(0, 60).map((p: any, i: number) => (
              <tr key={i} className="border-b border-border-light/50 hover:bg-bg-card-hover">
                <td className="py-1.5 pr-3 whitespace-nowrap">
                  <span className="text-text-muted">{p.position}</span>{' '}
                  <span className="text-text-primary">{p.player}</span>
                </td>
                <td className="py-1.5 pr-3 text-right"><TeamChip team={p.team ?? ''} /></td>
                <td className="py-1.5 pr-3 tabular-nums text-right text-text-secondary">{num(p.proj)}</td>
                <td className="py-1.5 pr-3 tabular-nums text-right font-medium" style={{ color: '#58a6ff' }}>
                  {p.position}{p.proj_rank}
                </td>
                <td className="py-1.5 pr-3 tabular-nums text-right text-text-primary">{num(p.actual)}</td>
                <td className="py-1.5 pr-3 tabular-nums text-right font-medium text-text-primary">
                  {p.position}{p.actual_rank}
                </td>
                <td className="py-1.5 pr-3 tabular-nums text-right font-semibold" style={{ color: diffColor(p.rank_diff) }}>
                  {p.rank_diff > 0 ? `+${p.rank_diff}` : p.rank_diff}
                </td>
                <td className="py-1.5 pr-3 tabular-nums text-right text-text-muted">
                  {p.pts_diff > 0 ? `+${num(p.pts_diff)}` : num(p.pts_diff)}
                </td>
                <td className="py-1.5 text-right"><ClassPill c={p.classed} /></td>
              </tr>
            ))}
          </tbody>
        </table>
        <p className="text-[10px] text-text-muted mt-1.5">
          Δ rank = finished − projected. <span style={{ color: '#3fb950' }}>Negative</span> means he beat
          his projected rank; <span style={{ color: '#f85149' }}>positive</span> means we ranked him too high.
          Showing {Math.min(rows.length, 60)} of {rows.length}.
        </p>
      </div>
    </Section>
  );
}

function BetCard({ d }: { d: any }) {
  // Same component the unplayed-week view uses, so a week with its own bet_card
  // reads identically to one falling back to the season JSON — one design for
  // the whole tab. The card is the richer source: it carries EVERY qualifying
  // bet, where the season JSON keeps only the highest-EV one per game.
  const rows: TopBetRow[] = arr(d.bets).map((b: any, i: number) => {
    const [away, home] = String(b.game ?? '').split('@').map((x: string) => x.trim());
    const parts = String(b.bet ?? '').trim().split(/\s+/);
    const side = parts[0] ?? '';
    const kind = (parts[1] ?? '').toLowerCase();
    return {
      key: `${b.game}-${b.bet}-${i}`, away: away ?? '', home: home ?? '',
      side, type: kind.startsWith('ml') ? 'ml' : 'spread',
      odds: b.odds, ev: b.ev, stake: b.stake,
      toWin: Math.round(b.payout_if_win ?? 0),
      result: b.result ?? null, pnl: b.pnl ?? null,
    } as TopBetRow;
  });
  if (rows.length === 0) {
    return (
      <Section id="bet_card" title="Bet Card" accent={SECTION_ACCENT.bet_card}
        subtitle="EV-positive plays, Kelly-sized">
        <p className="text-xs text-text-muted py-2">No bets cleared the EV threshold this week.</p>
      </Section>
    );
  }
  return <div id="bet_card"><TopBets bets={rows} limit={rows.length} freshness={d._freshness} /></div>;
}

function Lineups({ d }: { d: any }) {
  const rows = arr(d.lineups);
  const meta = arr(d.leagues);
  // League cards stay compact as more connected leagues are added. State is
  // keyed by league name so opening one bench never expands every other card.
  const [openBenches, setOpenBenches] = useState<Record<string, boolean>>({});
  const byLeague = useMemo(() => {
    const m: Record<string, any[]> = {};
    rows.forEach((r: any) => { (m[r.league] ??= []).push(r); });
    return m;
  }, [d]);

  // One dense row per player. No range bars — at ~40 players across four
  // leagues the bars were most of the vertical space and none of the decision.
  const Row = ({ p, dim }: { p: any; dim?: boolean }) => {
    const rescaled = p.scoring_mult != null && Math.abs(p.scoring_mult - 1) > 0.02;
    return (
      <div className={`grid grid-cols-[1.9rem_2.4rem_1fr_auto] items-baseline gap-x-2 px-1 py-[3px] rounded
                       ${dim ? 'text-text-muted' : 'text-text-secondary'} hover:bg-bg-card-hover`}>
        <span className={`text-[9px] font-bold text-center rounded py-px ${
          dim ? 'bg-bg-card-hover text-text-muted' : 'bg-win/10 text-win'}`}>{p.position}</span>
        <span className="text-[10px] font-semibold tabular-nums text-text-muted">{p.team}</span>
        <span className="truncate text-[11.5px]">
          <span className={dim ? '' : 'text-text-primary'}>{p.player}</span>
          <span className="text-text-muted text-[10px]"> vs {p.opponent}</span>
        </span>
        <span className="flex items-baseline gap-1.5 tabular-nums whitespace-nowrap">
          <span className="text-[10px] text-text-muted">{num(p.floor, 0)}–{num(p.ceiling, 0)}</span>
          <span className={`text-[11.5px] font-semibold ${dim ? '' : 'text-text-primary'}`}>{num(p.proj)}</span>
          {rescaled && (
            <span className="text-[9px] text-accent" title={`league scoring ×${p.scoring_mult} — half-PPR ${num(p.half_ppr)}`}>
              ×{Number(p.scoring_mult).toFixed(2)}
            </span>
          )}
        </span>
      </div>
    );
  };

  return (
    <Section id="lineups" title="My Lineups" accent={SECTION_ACCENT.lineups}
      subtitle="optimal start/sit — ranked on each league's own scoring">
      {Object.keys(byLeague).length === 0 && <p className="text-xs text-text-muted">No connected leagues.</p>}
      <div className="grid xl:grid-cols-2 gap-x-5 gap-y-4">
        {Object.entries(byLeague).map(([league, lr]) => {
          const starters = lr.filter(r => r.status === 'START');
          const bench = lr.filter(r => r.status !== 'START').sort((a, b) => (b.proj ?? 0) - (a.proj ?? 0));
          const benchOpen = openBenches[league] === true;
          const benchId = `bench-${league.replace(/[^a-z0-9]+/gi, '-').toLowerCase()}`;
          const total = starters.reduce((a, s) => a + (s.proj || 0), 0);
          const half = starters.reduce((a, s) => a + (s.half_ppr ?? s.proj ?? 0), 0);
          const m = meta.find((x: any) => x.league === league);
          const custom = Math.abs(total - half) > 0.5;
          return (
            <div key={league} className="rounded-xl border border-border-light bg-bg-card/40 overflow-hidden">
              <div className="flex items-baseline justify-between gap-2 border-b border-border-light px-2.5 py-1.5">
                <div className="min-w-0">
                  <div className="truncate text-xs font-semibold text-text-primary">{league}</div>
                  <div className="text-[10px] text-text-muted truncate">
                    {m?.franchise ? `${m.franchise} · ` : ''}{starters.length} starters · {bench.length} bench
                    {m && m.scoring_exact === false && <span className="text-loss"> · half-PPR assumed</span>}
                  </div>
                </div>
                <div className="text-right whitespace-nowrap">
                  <div className="text-sm font-bold tabular-nums text-text-primary">{num(total)}</div>
                  {custom && <div className="text-[9px] text-text-muted tabular-nums">half-PPR {num(half)}</div>}
                </div>
              </div>
              <div className="px-1.5 py-1.5">
                {starters.map((s, i) => <Row key={i} p={s} />)}
                {bench.length > 0 && (
                  <>
                    <button
                      type="button"
                      aria-expanded={benchOpen}
                      aria-controls={benchId}
                      onClick={() => setOpenBenches(current => ({ ...current, [league]: !benchOpen }))}
                      className="mt-1.5 flex w-full items-center justify-between border-t border-border-light px-1 pt-1.5 pb-0.5 text-[9px] uppercase tracking-wider text-text-muted hover:text-text-secondary focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent rounded"
                    >
                      <span>Bench · {bench.length}</span>
                      <span aria-hidden="true" className="text-xs leading-none">{benchOpen ? '−' : '+'}</span>
                    </button>
                    {benchOpen && (
                      <div id={benchId} className="pt-0.5">
                        {bench.map((b, i) => <Row key={i} p={b} dim />)}
                      </div>
                    )}
                  </>
                )}
              </div>
            </div>
          );
        })}
      </div>
      {arr(d.skipped).length > 0 && (
        <p className="text-[11px] text-text-muted border-t border-border-light pt-2">
          Skipped: {arr(d.skipped).map((s: any) => `${s.label} (${s.reason})`).join('; ')}
        </p>
      )}
    </Section>
  );
}

function Waivers({ d }: { d: any }) {
  const w = arr(d.waivers);
  const byTeam = useMemo(() => {
    const groups: Record<string, any[]> = {};
    w.forEach((r: any) => {
      const key = `${r.league ?? 'League'}::${r.franchise ?? 'My team'}`;
      (groups[key] ??= []).push(r);
    });
    return groups;
  }, [w]);
  return (
    <Section id="waivers" title="Waiver Board" accent={SECTION_ACCENT.waivers} subtitle="weekly upgrades, with season-long context">
      {w.length === 0 ? <p className="text-xs text-text-muted">No meaningful upgrades on the wire.</p> : (
        <div className="space-y-3">
          {Object.entries(byTeam).map(([key, recs]) => {
            const first = recs[0];
            return (
              <div key={key} className="overflow-hidden rounded-xl border border-border-light bg-bg-card/30">
                <div className="border-b border-border-light bg-bg-card-hover/50 px-3 py-2">
                  <div className="text-xs font-semibold text-text-primary">For {first.franchise ?? 'My team'}</div>
                  <div className="text-[10px] text-text-muted">{first.league ?? 'League'}</div>
                </div>
                <div className="divide-y divide-border-light/60">
                  {recs.map((r: any, i: number) => (
                    <div key={i} className="grid grid-cols-[2rem_minmax(0,1fr)_auto] items-center gap-2 px-3 py-2.5">
                      <span className="rounded bg-bg-card-hover text-text-muted text-[9px] font-bold px-1.5 py-0.5 text-center">{r.position}</span>
                      <div className="min-w-0 grid grid-cols-[minmax(0,1fr)_1rem_minmax(0,1fr)] items-center gap-1.5">
                        <div className="min-w-0 text-xs">
                          <div className="truncate text-win font-semibold">+ {r.suggested_add}</div>
                          <div className="text-[10px] tabular-nums text-text-muted">Wk {num(r.add_projected_ppg)} · Season {num(r.add_season_total, 0)}</div>
                        </div>
                        <span className="text-center text-text-muted">→</span>
                        <div className="min-w-0 text-right text-xs">
                          <div className="truncate text-loss">− {r.suggested_drop}</div>
                          <div className="text-[10px] tabular-nums text-text-muted">Wk {num(r.drop_projected_ppg)} · Season {num(r.drop_season_total, 0)}</div>
                        </div>
                      </div>
                      <div className="text-right whitespace-nowrap">
                        <div className="rounded-full border border-win/40 bg-win/10 px-2 py-0.5 text-[11px] font-semibold tabular-nums text-win">+{num(r.net_gain)} wk</div>
                        {r.season_net_gain != null && <div className="mt-1 text-[10px] tabular-nums text-text-muted">{r.season_net_gain >= 0 ? '+' : ''}{num(r.season_net_gain, 0)} season</div>}
                      </div>
                    </div>
                  ))}
                </div>
              </div>
            );
          })}
        </div>
      )}
    </Section>
  );
}

function Projections({ d }: { d: any }) {
  const positions = ['QB', 'RB', 'WR', 'TE'];
  const byPosition = useMemo(() => Object.fromEntries(positions.map(pos => [
    pos, (d.players ?? []).filter((p: any) => p.position === pos).slice(0, 50),
  ])), [d.players]);
  return (
    <Section id="projections" title="Projections" accent={SECTION_ACCENT.projections} subtitle="top 50 at each position · scroll a column for more">
      <div className="grid gap-3 md:grid-cols-2 2xl:grid-cols-4">
        {positions.map(pos => (
          <div key={pos} className="overflow-hidden rounded-lg border border-border-light bg-bg-card/30">
            <div className="flex items-center justify-between border-b border-border-light bg-bg-card-hover/50 px-2.5 py-1.5">
              <span className="text-xs font-semibold text-text-primary">{pos}</span>
              <span className="text-[9px] uppercase tracking-wider text-text-muted">{byPosition[pos].length} · proj · range · availability</span>
            </div>
            <div className="max-h-[36rem] overflow-y-auto overscroll-contain">
              {byPosition[pos].map((p: any, i: number) => (
                <div key={i} className="grid grid-cols-[1.4rem_minmax(0,1fr)_2.8rem] items-center gap-1.5 border-b border-border-light/50 px-2.5 py-1.5 last:border-b-0 hover:bg-bg-card-hover">
                  <span className="text-[10px] font-medium tabular-nums text-center text-text-muted">{p.pos_rank ?? i + 1}</span>
                  <div className="min-w-0">
                    <div className="truncate text-[11px] leading-tight text-text-primary">
                      <span className="mr-1 inline-flex items-center gap-1 font-semibold text-text-secondary"><span className="inline-block h-1.5 w-1.5 rounded-full" style={{ backgroundColor: readableTeamColor(p.team ?? '') }} />{p.team}</span>
                      <span className="font-medium">{p.player}</span>
                    </div>
                    <div className="mt-0.5 truncate text-[9px] tabular-nums text-text-muted">{p.opponent} · {num(p.floor, 0)}–{num(p.ceiling, 0)} · Avail {Math.round((p.p_active ?? 1) * 100)}%</div>
                  </div>
                  <span className="text-right text-[12px] font-semibold tabular-nums text-text-primary">{num(p.proj)}</span>
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    </Section>
  );
}

function ModelScorecard({ d }: { d: any }) {
  const m = obj(d.metrics); const b = obj(d.bet_summary);
  // Settled bets, ordered by EV — the same ranking the pre-week Top Bets card
  // uses, so a pick can be followed from recommendation through to result.
  const bets: any[] = (Array.isArray(d.bets) ? d.bets : d.bets ? [d.bets] : [])
    .slice().sort((x: any, y: any) => (y.ev ?? 0) - (x.ev ?? 0));
  const fmtU = (u: number) => `${Number.isInteger(u) ? u : u.toFixed(1)}u`;
  const RES: Record<string, string> = { WIN: '#3fb950', LOSS: '#f85149', PUSH: '#8b949e' };

  return (
    <Section id="model_scorecard" title="Model Scorecard" accent={SECTION_ACCENT.model_scorecard} subtitle="game predictions vs actuals + bet P&L">
      <div className="grid grid-cols-3 sm:grid-cols-6 gap-2">
        <StatTile label="Games" value={m.games ?? '–'} />
        <StatTile label="Margin MAE" value={num(m.mae, 2)} />
        <StatTile label="ATS" value={`${num((m.cover_acc ?? 0) * 100, 0)}%`} accent={m.cover_acc >= 0.5 ? '#3fb950' : undefined} />
        <StatTile label="Moneyline" value={`${num((m.ml_acc ?? 0) * 100, 0)}%`} />
        <StatTile label="Brier" value={num(m.brier, 3)} />
        <StatTile label="Bet P&L" value={`${(b.pnl ?? 0) >= 0 ? '+' : '−'}$${Math.abs(b.pnl ?? 0)}`}
          accent={(b.pnl ?? 0) >= 0 ? '#3fb950' : '#f85149'}
          sub={`${b.n_bets ?? 0} bets · ${b.wins ?? 0}-${b.losses ?? 0}${b.pushes ? `-${b.pushes}` : ''}`} />
      </div>

      {/* HOW THE PICKS DID — mirrors the pre-week Top Bets card (same EV ordering,
          same odds/units columns) so you can follow a recommendation to its result. */}
      {bets.length > 0 && (
        <div className="rounded-xl border border-border-light overflow-hidden"
          style={{ boxShadow: `inset 3px 0 0 ${SECTION_ACCENT.bet_card}` }}>
          <div className="flex items-center justify-between px-3 py-2 border-b border-border-light">
            <span className="text-xs font-semibold text-text-primary">How the bets did</span>
            <span className="text-[10px] text-text-muted tabular-nums">
              staked ${b.staked ?? 0}
              {b.roi != null && <> · ROI <span style={{ color: (b.roi ?? 0) >= 0 ? '#3fb950' : '#f85149' }}>
                {(b.roi ?? 0) >= 0 ? '+' : ''}{num((b.roi ?? 0) * 100, 1)}%</span></>}
            </span>
          </div>
          <div className="divide-y divide-border-light">
            {bets.map((r: any, i: number) => (
              <div key={i} className="flex items-center gap-3 px-3 py-1.5">
                <span className="w-4 text-[11px] tabular-nums text-text-muted">{i + 1}</span>
                <div className="min-w-0 flex-1">
                  <div className="text-xs font-semibold text-text-primary">
                    {r.bet} <span className="tabular-nums text-text-muted font-normal">
                      {r.odds > 0 ? `+${r.odds}` : r.odds}</span>
                  </div>
                  <div className="text-[10px] text-text-muted">{r.game}</div>
                </div>
                <span className="w-12 text-right text-xs tabular-nums" style={{ color: SECTION_ACCENT.bet_card }}>
                  {num((r.ev ?? 0) * 100, 1)}%
                </span>
                <span className="w-16 text-right text-xs tabular-nums">
                  <span className="font-bold text-text-primary">{fmtU(r.units ?? r.stake / 5)}</span>
                  <span className="text-[10px] text-text-muted"> ${r.stake}</span>
                </span>
                <span className="w-9 text-right text-[10px] font-bold uppercase tracking-wide"
                  style={{ color: RES[r.result] ?? '#8b949e' }}>{r.result}</span>
                <span className="w-12 text-right text-xs font-bold tabular-nums"
                  style={{ color: (r.pnl ?? 0) > 0 ? '#3fb950' : (r.pnl ?? 0) < 0 ? '#f85149' : '#8b949e' }}>
                  {(r.pnl ?? 0) >= 0 ? '+' : '−'}${Math.abs(r.pnl ?? 0)}
                </span>
              </div>
            ))}
          </div>
        </div>
      )}

      <div className="overflow-x-auto">
        <table className="w-full text-xs">
          <thead>
            <tr className="text-text-muted border-b border-border-light">
              <th className="py-1.5 pr-3 font-medium text-left">Game</th>
              <th className="py-1.5 pr-3 font-medium text-right">Pred</th>
              <th className="py-1.5 pr-3 font-medium text-right">Actual</th>
              <th className="py-1.5 pr-3 font-medium text-right">Err</th>
              {/* home win probability, model vs de-vigged market, and the gap */}
              <th className="py-1.5 pr-3 font-medium text-right" style={{ color: '#58a6ff' }}>Model</th>
              <th className="py-1.5 pr-3 font-medium text-right">Implied</th>
              <th className="py-1.5 pr-3 font-medium text-right">Edge</th>
              <th className="py-1.5 pr-3 font-medium text-right">ATS</th>
              <th className="py-1.5 pr-3 font-medium text-right">ML</th>
            </tr>
          </thead>
          <tbody>
            {arr(d.games).map((r: any, i: number) => {
              const e = r.edge_pct;
              return (
                <tr key={i} className="border-b border-border-light/50 hover:bg-bg-card-hover">
                  <td className="py-1.5 pr-3 text-text-secondary whitespace-nowrap">{r.game}</td>
                  <td className="py-1.5 pr-3 tabular-nums text-right text-text-secondary">{num(r.pred_margin)}</td>
                  <td className="py-1.5 pr-3 tabular-nums text-right text-text-primary">{r.actual_margin}</td>
                  <td className="py-1.5 pr-3 tabular-nums text-right text-text-muted">{num(r.abs_err)}</td>
                  <td className="py-1.5 pr-3 tabular-nums text-right font-medium" style={{ color: '#58a6ff' }}>
                    {r.pred_win_pct != null ? `${Math.round(r.pred_win_pct * 100)}%` : '–'}
                  </td>
                  <td className="py-1.5 pr-3 tabular-nums text-right text-text-muted">
                    {r.vegas_win_pct != null ? `${Math.round(r.vegas_win_pct * 100)}%` : '–'}
                  </td>
                  <td className="py-1.5 pr-3 tabular-nums text-right"
                    style={{ color: e == null ? 'var(--color-text-muted)' : Math.abs(e) >= 0.05 ? '#d29922' : 'var(--color-text-muted)' }}>
                    {e == null ? '–' : `${e > 0 ? '+' : ''}${Math.round(e * 100)}`}
                  </td>
                  <td className="py-1.5 pr-3 text-right">{mark(r.cover_correct)}</td>
                  <td className="py-1.5 pr-3 text-right">{mark(r.ml_correct)}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
        <p className="text-[10px] text-text-muted mt-1.5">
          Win probabilities are for the <span className="text-text-secondary">home</span> team (second listed).
          Implied is de-vigged from the moneylines; Edge is model − implied, in points of probability.
        </p>
      </div>
    </Section>
  );
}
const mark = (v: any) => v == null
  ? <span className="text-text-muted">P</span>
  : v ? <span className="text-win font-bold">✓</span> : <span className="text-loss font-bold">✗</span>;

function FantasyRecap({ d }: { d: any }) {
  const leagues = Array.isArray(d.leagues) ? d.leagues : d.leagues ? [d.leagues] : [];
  return (
    <Section id="fantasy_recap" title="Fantasy Recap" accent={SECTION_ACCENT.fantasy_recap} subtitle="results · boom / bust · pickups">
      {leagues.length === 0 && <p className="text-xs text-text-muted">No connected leagues.</p>}
      <div className="space-y-4">
        {leagues.map((lg: any, i: number) => {
          const r = lg.record; const won = r?.result === 'W';
          const players = lg.players ?? [];
          return (
            <div key={i} className="rounded-xl border border-border-light overflow-hidden">
              <div className="flex items-center justify-between px-3 py-2.5"
                style={{ background: r ? (won ? hexToRgba('#3fb950', 0.12) : hexToRgba('#f85149', 0.1)) : undefined }}>
                <div>
                  <div className="text-xs font-semibold text-text-primary">{lg.franchise}</div>
                  <div className="text-[10px] text-text-muted">{lg.label}</div>
                </div>
                {r && (
                  <div className="text-right">
                    <span className={`text-lg font-bold ${won ? 'text-win' : 'text-loss'}`}>{r.result}</span>
                    <div className="text-xs tabular-nums text-text-secondary">{num(r.franchise_score)} – {num(r.opponent_score)}</div>
                  </div>
                )}
              </div>
              <div className="grid sm:grid-cols-2 gap-x-6 gap-y-1 px-3 py-2">
                {players.slice(0, 12).map((p: any, j: number) => (
                  <div key={j} className="flex items-center justify-between gap-2 text-xs py-0.5">
                    <span className="truncate text-text-secondary"><span className="text-text-muted">{p.position}</span> {p.player}</span>
                    <span className="flex items-center gap-2">
                      <span className="text-text-muted tabular-nums">{num(p.proj)}→<span className="text-text-primary font-semibold">{num(p.actual)}</span></span>
                      <ClassPill c={p.class} />
                    </span>
                  </div>
                ))}
              </div>
              {arr(lg.free_agents).length > 0 && (
                <div className="border-t border-border-light px-3 py-2">
                  <div className="text-[10px] uppercase tracking-wider text-text-muted mb-1.5">Free-agent targets</div>
                  <div className="flex flex-wrap gap-1.5">
                    {lg.free_agents.slice(0, 8).map((f: any, j: number) => (
                      <span key={j} className="inline-flex items-center gap-1.5 rounded-md border border-win/30 bg-win/5 px-2 py-0.5 text-[11px]">
                        <span className="text-win font-medium">{f.player}</span>
                        <span className="text-text-muted">{f.position}</span>
                        <span className="text-text-secondary tabular-nums">{num(f.proj)}</span>
                      </span>
                    ))}
                  </div>
                </div>
              )}
            </div>
          );
        })}
      </div>
    </Section>
  );
}

function CoverageBar({ label, value, target }: { label: string; value: number; target: number }) {
  const pct = clamp(value * 100);
  const good = Math.abs(value - target) <= 0.05;
  return (
    <div>
      <div className="flex justify-between text-[10px] text-text-muted mb-1"><span>{label}</span><span className="tabular-nums">{Math.round(pct)}% <span className="text-text-muted">/ {Math.round(target * 100)}%</span></span></div>
      <div className="relative h-2 rounded-full bg-bg-card-hover overflow-hidden">
        <div className="absolute h-full rounded-full" style={{ width: `${pct}%`, background: good ? '#3fb950' : '#d29922' }} />
        <div className="absolute top-0 h-full w-0.5 bg-text-secondary" style={{ left: `${target * 100}%` }} />
      </div>
    </div>
  );
}

function WeeklyAccuracy({ d }: { d: any }) {
  const cc = obj(d.class_counts);
  const rows = d.metrics ?? [];
  const all = rows.find((r: any) => r.position === 'ALL') ?? {};
  // ECR baseline (season-long consensus scored on this week), keyed by position.
  const ecrRows: any[] = Array.isArray(d.ecr) ? d.ecr : d.ecr ? [d.ecr] : [];
  const ecrBy: Record<string, any> = Object.fromEntries(ecrRows.map((r: any) => [r.position, r]));
  const ecrAll = ecrBy['ALL'];
  const modelWins = ecrAll && ecrAll.model >= ecrAll.ecr;
  const ecrKind = d.ecr_kind === 'weekly' ? 'weekly' : 'season';
  const SurpriseList = ({ title, rows, color }: any) => (
    <div>
      <div className="text-[10px] uppercase tracking-wider mb-1.5" style={{ color }}>{title}</div>
      <div className="space-y-1">
        {arr(rows).slice(0, 6).map((r: any, i: number) => (
          <div key={i} className="flex items-center justify-between text-xs">
            <span className="truncate text-text-secondary"><span className="text-text-muted">{r.position}</span> {r.player}</span>
            <span className="tabular-nums text-text-muted">{num(r.proj)}→<span className="text-text-primary">{num(r.actual)}</span>
              <span style={{ color }}> ({r.surprise > 0 ? '+' : ''}{num(r.surprise)})</span></span>
          </div>
        ))}
      </div>
    </div>
  );
  return (
    <Section id="weekly_accuracy" title="Weekly-Model Accuracy" accent={SECTION_ACCENT.weekly_accuracy} subtitle="projections vs actual half-PPR">
      <div className="grid grid-cols-2 sm:grid-cols-4 lg:grid-cols-5 gap-2">
        <StatTile label="MAE" value={num(all.mae, 2)} />
        <StatTile label="Spearman" value={num(all.spearman, 2)} accent="#58a6ff" />
        {ecrAll && (
          <StatTile label={`Rank acc vs ${ecrKind} ECR`} value={`${num(ecrAll.model, 2)} / ${num(ecrAll.ecr, 2)}`}
            sub={`model vs ${ecrKind} ECR${modelWins ? ' · model +' + num(ecrAll.model - ecrAll.ecr, 2) : ''}`}
            accent={modelWins ? '#3fb950' : '#d29922'} />
        )}
        <StatTile label="Booms" value={cc.boom ?? 0} accent="#3fb950" sub={`${cc.met ?? 0} met`} />
        <StatTile label="Busts" value={cc.bust ?? 0} accent="#f85149" />
      </div>
      <div className="grid sm:grid-cols-2 gap-3">
        {(all.ceiling_cov != null) && <CoverageBar label="Ceiling coverage" value={all.ceiling_cov} target={0.9} />}
        {(all.floor_cov != null) && <CoverageBar label="Floor coverage" value={all.floor_cov} target={0.9} />}
      </div>
      <div className="grid sm:grid-cols-2 gap-x-6 gap-y-3 pt-1">
        <SurpriseList title="Biggest beats" rows={d.beats} color="#3fb950" />
        <SurpriseList title="Biggest misses" rows={d.misses} color="#f85149" />
      </div>
      <div className="overflow-x-auto pt-1">
        <table className="w-full text-xs">
          <thead><tr className="text-text-muted border-b border-border-light">
            {['Pos', 'N', 'MAE', 'Model ρ', 'ECR ρ', 'Ceil cov', 'Floor cov'].map((h, i) => (
              <th key={h} className={`py-1 pr-3 font-medium ${i === 0 ? 'text-left' : 'text-right'}`}>{h}</th>
            ))}
          </tr></thead>
          <tbody>
            {rows.filter((r: any) => r.position !== 'ALL').map((r: any, i: number) => {
              const e = ecrBy[r.position];
              const win = e && e.model >= e.ecr;
              return (
                <tr key={i} className="border-b border-border-light/50">
                  <td className="py-1 pr-3 text-text-primary font-medium">{r.position}</td>
                  <td className="py-1 pr-3 tabular-nums text-right text-text-muted">{r.n}</td>
                  <td className="py-1 pr-3 tabular-nums text-right text-text-secondary">{num(r.mae, 2)}</td>
                  <td className="py-1 pr-3 tabular-nums text-right font-medium" style={{ color: e ? (win ? '#3fb950' : 'var(--color-text-secondary)') : 'var(--color-text-secondary)' }}>{num(e ? e.model : r.spearman, 2)}</td>
                  <td className="py-1 pr-3 tabular-nums text-right" style={{ color: e && !win ? '#d29922' : 'var(--color-text-muted)' }}>{e ? num(e.ecr, 2) : '–'}</td>
                  <td className="py-1 pr-3 tabular-nums text-right text-text-secondary">{num((r.ceiling_cov ?? 0) * 100, 0)}%</td>
                  <td className="py-1 pr-3 tabular-nums text-right text-text-secondary">{num((r.floor_cov ?? 0) * 100, 0)}%</td>
                </tr>
              );
            })}
          </tbody>
        </table>
        {ecrAll && <div className="text-[10px] text-text-muted mt-1.5">ρ = Spearman rank-accuracy vs actual half-PPR, on players both sources rank. ECR = {ecrKind === 'weekly' ? "FantasyPros WEEKLY consensus captured pre-kickoff." : "season-long FantasyPros consensus (true weekly ECR isn't archived for past weeks, so this proxies with the season board)."}</div>}
      </div>
    </Section>
  );
}

// League-wide weekly recap — every matchup, ranked storylines, next-week watch.
function LeagueReport({ d, onNav }: { d: any; onNav?: (s: number, w: number) => void }) {
  return (
    <Section id="league_report" title="League Report" accent={SECTION_ACCENT.league_report}
      subtitle="all matchups · storylines · what to watch next week">
      <LeagueReportSection data={d} onNavigate={onNav} />
    </Section>
  );
}

function DstStream({ d }: { d: any }) {
  const board = d.board ?? [];
  const leagues: any[] = Array.isArray(d.leagues) ? d.leagues : d.leagues ? [d.leagues] : [];
  const dstLeagues = leagues.filter(lg => lg.uses_dst !== false);
  const best = board[0];
  // lower opponent implied total = better streaming matchup; scale bar to the board
  const lo = board.length ? board[0].opp_implied : 0;
  const hi = board.length ? board[board.length - 1].opp_implied : 1;
  const frac = (v: number) => clamp(((hi - v) / Math.max(0.1, hi - lo)) * 100);
  return (
    <Section id="dst_stream" title="DST Streaming" accent={SECTION_ACCENT.dst_stream}
      subtitle="start the available defense facing the lowest implied team total"
      right={best && <span className="text-[11px] text-text-muted">top pick <span className="font-semibold" style={{ color: readableTeamColor(best.team) }}>{best.team}</span></span>}>
      {!d.has_lines ? (
        <p className="text-xs text-text-muted py-2">No betting lines posted yet for this week — check back closer to kickoff.</p>
      ) : (
        <>
          {dstLeagues.map((lg, i) => (
            <div key={i} className="rounded-lg border border-border-light p-2.5">
              <div className="flex items-center justify-between mb-1.5">
                <span className="text-xs font-medium text-text-primary">{lg.label}</span>
                <span className="text-[10px] text-text-muted">{lg.n_taken} rostered{lg.my_dst ? ` · your DST: ${lg.my_dst}` : ''}</span>
              </div>
              {arr(lg.top_available).length > 0 && (
                <>
                  <div className="mb-1.5 flex flex-wrap items-baseline justify-between gap-x-3 gap-y-0.5 text-[10px]">
                    <span className="font-medium text-text-secondary">Available waiver-wire streamers <span className="font-normal text-text-muted">· first is the top option</span></span>
                    <span className="text-text-muted">Opponent implied points · lower is better</span>
                  </div>
                  <div className="flex flex-wrap gap-1.5">
                  {lg.top_available.map((a: any, j: number) => (
                    <span key={j} className={`inline-flex items-center gap-1.5 rounded-md border px-2 py-0.5 text-[11px] ${j === 0 ? 'border-win/40 bg-win/10' : 'border-border-light bg-bg-card-hover'}`}>
                      {j === 0 && <span className="text-[8px] font-bold uppercase tracking-wide text-win">Best</span>}
                      <span className="font-semibold" style={{ color: readableTeamColor(a.team) }}>{a.team}</span>
                      <span className="text-text-muted">{a.is_home ? 'vs' : '@'} {a.opp}</span>
                      <span className="tabular-nums text-text-secondary">{num(a.opp_implied)} pts</span>
                    </span>
                  ))}
                </div>
                </>
              )}
            </div>
          ))}
          <div className="space-y-1 pt-1">
            {board.slice(0, 12).map((r: any, i: number) => (
              <div key={i} className="grid grid-cols-[1.5rem_3rem_1fr] items-center gap-2">
                <span className="text-[11px] tabular-nums text-text-muted text-center">{r.rank}</span>
                <TeamChip team={r.team} />
                <div className="min-w-0">
                  <div className="flex items-baseline justify-between gap-2">
                    <span className="text-[11px] text-text-muted">{r.is_home ? 'vs' : '@'} {r.opp} · O/U {num(r.total)}</span>
                    <span className="text-xs font-semibold tabular-nums text-text-primary">{num(r.opp_implied)} <span className="text-text-muted font-normal">opp pts</span></span>
                  </div>
                  <div className="mt-1 h-1.5 rounded-full bg-bg-card-hover overflow-hidden">
                    <div className="h-full rounded-full" style={{ width: `${frac(r.opp_implied)}%`, background: readableTeamColor(r.team) }} />
                  </div>
                </div>
              </div>
            ))}
          </div>
          <p className="text-[10px] text-text-muted pt-1">{d.rationale}</p>
        </>
      )}
    </Section>
  );
}

const RENDERERS: Record<string, (p: { d: any; onNav?: (s: number, w: number) => void }) => React.ReactNode> = {
  games_preview: GamesPreview, games_result: GamesResult, bet_card: BetCard, lineups: Lineups, waivers: Waivers, fantasy_ranks: FantasyRanks,
  projections: Projections, model_scorecard: ModelScorecard, fantasy_recap: FantasyRecap,
  weekly_accuracy: WeeklyAccuracy, league_report: LeagueReport, dst_stream: DstStream,
};
const PRE_ORDER = ['games_preview', 'bet_card', 'lineups', 'waivers', 'dst_stream', 'projections'];
const POST_ORDER = ['games_result', 'fantasy_ranks', 'league_report', 'model_scorecard', 'fantasy_recap', 'weekly_accuracy'];

// Sections grouped by the QUESTION being asked, so the page shows one coherent
// group at a time instead of a ten-section scroll mixing model, fantasy and league.
type GroupKey = 'games' | 'lineups' | 'fantasy' | 'results';
const GROUPS: { key: GroupKey; label: string; accent: string; sections: string[] }[] = [
  { key: 'games',   label: 'Games & Bets', accent: '#58a6ff', sections: ['games_result', 'games_preview', 'bet_card'] },
  // Lineups get their own tab rather than sitting fifth in the fantasy stack:
  // it's the one section that is about MY teams and gets acted on every week.
  { key: 'lineups', label: 'My Lineups',   accent: '#3fb950', sections: ['lineups'] },
  { key: 'fantasy', label: 'Fantasy',      accent: '#3fb950', sections: ['fantasy_ranks', 'dst_stream', 'waivers', 'projections'] },
  { key: 'results', label: 'Results',      accent: '#f778ba', sections: ['model_scorecard', 'fantasy_recap', 'weekly_accuracy', 'league_report'] },
];

/** Sub-tab keys this view accepts in a permalink. */
export const WEEKLY_GROUP_KEYS: string[] = GROUPS.map(g => g.key);

// ---- main ------------------------------------------------------------------
interface WeeklyReportProps {
  season: number;
  week: number;
  onNavigate?: (season: number, week: number) => void;
  /** Active sub-tab, driven by the URL. Unknown/absent falls back to 'games'. */
  group?: string | null;
  onGroupChange?: (g: string) => void;
  /** Canonical game cards, available for EVERY week from seasons/<yr>.json.
   *  These are now the standard pre-kickoff view — see `ready` below. */
  gamesFallback?: React.ReactNode;
  /** EV-ordered top bets built from the season JSON. Only used when the week
   *  has no bet_card of its own, which is the richer source. */
  betsFallback?: React.ReactNode;
}

export function WeeklyReport({ season, week, onNavigate, gamesFallback, betsFallback, group: groupProp, onGroupChange }: WeeklyReportProps) {
  const [index, setIndex] = useState<IndexFile | null>(null);
  const [data, setData] = useState<Record<string, any>>({});
  const [loading, setLoading] = useState(true);
  const group: GroupKey =
    GROUPS.some(g => g.key === groupProp) ? (groupProp as GroupKey) : 'games';
  const setGroup = (g: GroupKey) => onGroupChange?.(g);

  useEffect(() => {
    getJSON<IndexFile>(dataUrl('/data/weekly/index.json')).then(idx => {
      setIndex(idx);
    });
  }, []);

  // The report shown is driven by the header's season/week picker.
  const wantTag = `${season}-wk${String(week).padStart(2, '0')}`;
  const entry = useMemo(() => index?.weeks.find(w => w.tag === wantTag), [index, wantTag]);

  useEffect(() => {
    if (!entry) { setData({}); setLoading(false); return; }
    setLoading(true);
    let cancelled = false;
    Promise.all(entry.reports.map(async r => [r, await getJSON(dataUrl(`/data/weekly/${entry.tag}/${r}.json`))] as const))
      .then(pairs => { if (!cancelled) { setData(Object.fromEntries(pairs.filter(([, v]) => v))); setLoading(false); } });
    return () => { cancelled = true; };
  }, [entry]);

  const present = entry?.reports ?? [];
  const all = [...PRE_ORDER, ...POST_ORDER].filter(r => present.includes(r) && data[r]);
  // Once a week is played, the results view supersedes the pre-kickoff preview —
  // showing both would be the same games asked two different questions.
  const played = all.includes('games_result');
  // Before kickoff the canonical game cards ARE the games view: they carry the
  // model-vs-market probability bar, the spread comparison and the recommended
  // position. `games_preview` is an older parallel rendering of the same 16
  // games (verified identical predictions), so it is superseded rather than
  // stacked alongside — one design for every week.
  const supersededByCards = !played && !!gamesFallback;
  const ready = all.filter(r => !(r === 'games_preview' && (played || supersededByCards)));
  // bet_card lists EVERY qualifying bet; the season JSON keeps only the
  // highest-EV one per game. Prefer the card, and drop the fallback bets when it
  // exists so the page can't show 10 bets under a header that says 13.
  const showBetsFallback = !ready.includes('bet_card') && !!betsFallback;

  // Groups that actually have something to show. "Games" always qualifies because
  // the fallback game cards exist for every week.
  const groups = GROUPS
    .map(g => ({ ...g, ready: g.sections.filter(s => ready.includes(s)) }))
    .filter(g => g.ready.length > 0 || (g.key === 'games' && gamesFallback));
  const active = groups.find(g => g.key === group) ?? groups[0];

  return (
    <ReportFreshnessContext.Provider value={entry?.updated ?? null}>
    <div className="space-y-4">
      {groups.length > 1 && (
        <SubNav items={groups.map(g => ({ key: g.key, label: g.label, accent: g.accent }))}
          value={active?.key ?? 'games'} onChange={setGroup} />
      )}

      {loading ? (
        <div className="space-y-4"><Skeleton h={280} /><Skeleton h={200} /></div>
      ) : (
        <div className="space-y-4">
          {active?.key === 'games' && showBetsFallback && betsFallback}
          {active?.ready.map(r => {
            const C = RENDERERS[r];
            if (!C) return null;
            // Each report is isolated: a malformed field costs that card, not the page.
            return (
              <SectionBoundary key={r} name={r.replace(/_/g, ' ')}>
                <C d={{ ...data[r], _freshness: entry?.updated }} onNav={onNavigate} />
              </SectionBoundary>
            );
          })}
          {/* canonical game cards — the standard view for any unplayed week */}
          {active?.key === 'games' && !played && gamesFallback}
        </div>
      )}
    </div>
    </ReportFreshnessContext.Provider>
  );
}
