import { useEffect, useMemo, useState } from 'react';
import { getTeamColor } from '../../data/teamColors';
import { teamNames } from '../../data/teamMeta';
import { WinProbabilityBar } from '../games/WinProbabilityBar';
import { num, clamp, hexToRgba, obj, StatTile, TeamChip, EdgeBadge, RangeBar, Skeleton, readableTeamColor, SubNav, arr } from '../ui/primitives';
import { TopBets, type TopBetRow } from '../games/TopBets';
import { SectionBoundary } from '../ui/SectionBoundary';
import { LeagueReportSection } from './LeagueReportSection';
import { TransactionDesk } from '../fantasy/TransactionDesk';
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

function formatFreshness(updated: string | null | undefined) {
  if (!updated) return null;
  const parsed = new Date(updated.replace(/([+-]\d{2})(\d{2})$/, '$1:$2'));
  return Number.isNaN(parsed.getTime())
    ? updated
    : parsed.toLocaleString([], { month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit' });
}

async function getJSON<T>(url: string): Promise<T | null> {
  try { const r = await fetch(url); return r.ok ? ((await r.json()) as T) : null; } catch { return null; }
}

// GitHub Pages serves the static app but cannot run Pages Functions. Keep the
// public Cloudflare deployment as the read-only live-data origin so the same
// scoring panel works on both hosts. Local and Cloudflare builds stay same-origin.
const LIVE_API_ORIGIN = typeof window !== 'undefined' && window.location.hostname.endsWith('github.io')
  ? 'https://nfl-predictor-mf4-cjl.pages.dev'
  : '';
const liveApiUrl = (params: URLSearchParams) => `${LIVE_API_ORIGIN}/api/live?${params}`;

// ---- section + report-specific primitives ----------------------------------
function Section({ id, title, subtitle, accent = 'var(--color-accent)', right, children }:
  { id: string; title: string; subtitle?: string; accent?: string; right?: React.ReactNode; children: React.ReactNode }) {
  return (
    <section id={id} className="report-section scroll-mt-28 rounded-2xl border border-border bg-bg-card/60 overflow-hidden">
      <div className="report-section__header flex items-center justify-between border-b border-border-light px-4 py-3"
        style={{ boxShadow: `inset 3px 0 0 ${accent}` }}>
        <div>
          <h3 className="text-sm font-semibold text-text-primary tracking-tight">{title}</h3>
          {subtitle && <p className="text-[11px] text-text-muted mt-0.5">{subtitle}</p>}
        </div>
        {right && <div className="flex items-center gap-2">{right}</div>}
      </div>
      <div className="report-section__body p-4 space-y-3">{children}</div>
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

function PolymarketBets({ d }: { d: any }) {
  const allMarkets = arr(d.markets);
  const markets = allMarkets.filter((m: any) => m.qualifies);
  const dollars = (v: any) => `$${Math.round(Number(v ?? 0)).toLocaleString()}`;
  const probability = (v: any) => `${Math.round(Number(v ?? 0) * 100)}%`;
  return (
    <Section id="polymarket_bets" title="Polymarket opportunities" accent="#a371f7"
      subtitle="NFL winner contracts · model compared with executable YES ask">
      {markets.length === 0 ? (
        <div className="rounded-lg border border-border-light bg-bg-card/30 px-3 py-2.5 text-xs text-text-muted">
          No mapped, liquid NFL moneyline contract clears the model’s 5% net-EV threshold right now.
          <span className="block mt-1 text-[10px]">Contracts must be a mapped game winner with at least {dollars(d.min_liquidity)} liquidity; a 1¢ execution allowance is included before ranking.</span>
        </div>
      ) : (
        <div className="overflow-hidden rounded-xl border border-border-light">
          <div className="hidden grid-cols-[minmax(0,1fr)_4.5rem_4.5rem_4.5rem_5.5rem] gap-3 border-b border-border-light bg-bg-card-hover/50 px-3 py-1.5 text-[8px] font-medium uppercase tracking-wider text-text-muted sm:grid">
            <span>Contract</span><span className="text-right">Model</span><span className="text-right">YES ask</span><span className="text-right">Net EV</span><span className="text-right">Liquidity</span>
          </div>
          <div className="divide-y divide-border-light">
            {markets.map((m: any) => (
              <a key={`${m.market_id}-${m.bet_team}`} href={`https://polymarket.com/event/${m.slug}`} target="_blank" rel="noreferrer"
                className="grid grid-cols-[minmax(0,1fr)_auto] gap-x-3 px-3 py-2.5 hover:bg-bg-card-hover sm:grid-cols-[minmax(0,1fr)_4.5rem_4.5rem_4.5rem_5.5rem] sm:items-center">
                <div className="min-w-0">
                  <div className="truncate text-xs font-semibold text-text-primary">Buy {m.bet_team} YES</div>
                  <div className="truncate text-[10px] text-text-muted">{m.question} · {m.opponent} opponent</div>
                </div>
                <div className="text-right text-xs font-semibold tabular-nums text-text-primary">{probability(m.model_probability)}<span className="block text-[9px] font-normal text-text-muted sm:hidden">model</span></div>
                <div className="hidden text-right text-xs tabular-nums text-text-secondary sm:block">{probability(m.ask)}<span className="block text-[9px] text-text-muted">+{Math.round(Number(d.execution_buffer ?? 0) * 100)}¢ allowance</span></div>
                <div className="hidden text-right text-xs font-semibold tabular-nums text-win sm:block">+{num(Number(m.net_ev) * 100, 1)}%</div>
                <div className="hidden text-right text-xs tabular-nums text-text-secondary sm:block">{dollars(m.liquidity)}</div>
                <div className="col-span-2 mt-1 flex items-center gap-2 text-[10px] tabular-nums sm:hidden"><span>YES ask {probability(m.ask)}</span><span className="font-semibold text-win">+{num(Number(m.net_ev) * 100, 1)}% net EV</span><span>{dollars(m.liquidity)} liquidity</span></div>
              </a>
            ))}
          </div>
        </div>
      )}
      <p className="text-[10px] leading-relaxed text-text-muted">Read-only analysis—not an execution tool. Net EV uses the model probability divided by the displayed YES ask plus a 1¢ allowance for fees and slippage. Open a contract to inspect its rules and order book.</p>
    </Section>
  );
}

function KalshiLines({ d }: { d: any }) {
  const lines = arr(d.lines).filter((m: any) => m.qualifies).sort((a: any, b: any) => Number(b.ev) - Number(a.ev));
  const pct = (v: any) => `${Math.round(Number(v ?? 0) * 100)}%`;
  return (
    <Section id="kalshi_lines" title="Kalshi spread opportunities" accent="#f0a000"
      subtitle="Exact spread contracts · model probability, executable ask, and fee-aware EV">
      {lines.length === 0 ? (
        <p className="rounded-lg border border-border-light bg-bg-card/30 px-3 py-2.5 text-xs text-text-muted">No Kalshi spread rung clears the 5% calibrated-EV threshold right now.</p>
      ) : (
        <div className="overflow-hidden rounded-xl border border-border-light">
          <div className="hidden grid-cols-[minmax(0,1fr)_4rem_4rem_4rem_4rem] gap-3 border-b border-border-light bg-bg-card-hover/50 px-3 py-1.5 text-[8px] font-medium uppercase tracking-wider text-text-muted sm:grid">
            <span>Contract</span><span className="text-right">Model</span><span className="text-right">YES ask</span><span className="text-right">Fee est.</span><span className="text-right">Net EV</span>
          </div>
          <div className="divide-y divide-border-light">
            {lines.map((m: any) => (
              <a key={m.market_ticker} href={`https://kalshi.com/markets/${m.event_ticker}`} target="_blank" rel="noreferrer"
                className="grid grid-cols-[minmax(0,1fr)_auto] gap-x-3 px-3 py-2.5 hover:bg-bg-card-hover sm:grid-cols-[minmax(0,1fr)_4rem_4rem_4rem_4rem] sm:items-center">
                <div><div className="text-xs font-semibold text-text-primary">{m.team} by more than {Number(m.threshold).toFixed(1)}</div><div className="text-[10px] text-text-muted">{m.game_id?.replace(/^\d{4}_\d{2}_/, '').replace('_', ' @ ')}</div></div>
                <div className="text-right text-xs font-semibold tabular-nums text-text-primary">{pct(m.model_probability)}<span className="block text-[9px] font-normal text-text-muted sm:hidden">model</span></div>
                <div className="hidden text-right text-xs tabular-nums text-text-secondary sm:block">{Math.round(Number(m.ask) * 100)}¢</div>
                <div className="hidden text-right text-xs tabular-nums text-text-secondary sm:block">{(Number(m.fee_estimate) * 100).toFixed(1)}¢</div>
                <div className="hidden text-right text-xs font-semibold tabular-nums text-win sm:block">+{num(Number(m.ev) * 100, 1)}%</div>
                <div className="col-span-2 mt-1 flex gap-2 text-[10px] tabular-nums text-text-muted sm:hidden"><span>{Math.round(Number(m.ask) * 100)}¢ ask</span><span>{(Number(m.fee_estimate) * 100).toFixed(1)}¢ fee est.</span><span className="font-semibold text-win">+{num(Number(m.ev) * 100, 1)}% EV</span></div>
              </a>
            ))}
          </div>
        </div>
      )}
      <p className="text-[10px] leading-relaxed text-text-muted">Model probability uses the ensemble’s calibrated margin uncertainty at this exact Kalshi threshold. EV subtracts the executable YES ask and estimated quadratic fee, then uses the same historical EV calibration as the sportsbook bet card. Actual fees can vary slightly with order size and cent rounding.</p>
    </Section>
  );
}

/**
 * A single venue-neutral market board. It replaces two visually identical
 * provider reports with one ranked answer to the question users actually ask:
 * where is the best live price this week?
 */
function MarketBoard({ polymarket, kalshi }: { polymarket: any[]; kalshi: any[] }) {
  const moneyline = polymarket.filter(m => m.qualifies).map(m => ({
    key: `pm-${m.market_id}-${m.bet_team}`,
    venue: 'Polymarket', color: '#c9a7ff',
    pick: `${m.bet_team} YES`, matchup: m.question ?? `${m.bet_team} vs ${m.opponent}`,
    model: Number(m.model_probability), price: `${Math.round(Number(m.ask) * 100)}¢`,
    ev: Number(m.net_ev), detail: `$${Math.round(Number(m.liquidity ?? 0)).toLocaleString()} liquidity`,
    href: `https://polymarket.com/event/${m.slug}`,
  }));
  const spreads = kalshi.filter(m => m.qualifies).map(m => ({
    key: `kalshi-${m.market_ticker}`,
    venue: 'Kalshi', color: '#f0b44d',
    pick: `${m.team} by ${Number(m.threshold).toFixed(1)}+`,
    matchup: String(m.game_id ?? '').replace(/^\d{4}_\d{2}_/, '').replace('_', ' @ '),
    model: Number(m.model_probability), price: `${Math.round(Number(m.ask) * 100)}¢`,
    ev: Number(m.ev), detail: `${(Number(m.fee_estimate ?? 0) * 100).toFixed(1)}¢ fee estimate`,
    href: `https://kalshi.com/markets/${m.event_ticker}`,
  }));
  const opportunities = [...moneyline, ...spreads].sort((a, b) => b.ev - a.ev);
  if (opportunities.length === 0) return null;
  return (
    <Section id="market_board" title="Market board" accent="#c9a7ff"
      subtitle="Venue-neutral opportunities, ranked by estimated return after execution costs"
      right={<span className="text-[10px] tabular-nums text-text-muted">{opportunities.length} live opportunities</span>}>
      <div className="overflow-hidden rounded-xl border border-border-light">
        <div className="hidden grid-cols-[5.8rem_minmax(0,1fr)_4.5rem_4rem_4.5rem] items-center gap-3 border-b border-border-light bg-bg-card-hover/50 px-3 py-1.5 text-[8px] font-medium uppercase tracking-wider text-text-muted sm:grid">
          <span>Venue</span><span>Position</span><span className="text-right">Model</span><span className="text-right">Price</span><span className="text-right">Net EV</span>
        </div>
        <div className="divide-y divide-border-light">
          {opportunities.map(item => (
            <a key={item.key} href={item.href} target="_blank" rel="noreferrer"
              className="group grid grid-cols-[minmax(0,1fr)_auto] gap-x-3 px-3 py-2.5 transition-colors hover:bg-bg-card-hover sm:grid-cols-[5.8rem_minmax(0,1fr)_4.5rem_4rem_4.5rem] sm:items-center">
              <span className="hidden w-fit rounded-full border px-2 py-0.5 text-[9px] font-semibold sm:inline-flex" style={{ borderColor: `${item.color}66`, background: `${item.color}14`, color: item.color }}>{item.venue}</span>
              <div className="min-w-0">
                <div className="flex min-w-0 items-center gap-1.5">
                  <span className="inline-flex rounded-full border px-1.5 py-px text-[8px] font-semibold sm:hidden" style={{ borderColor: `${item.color}66`, color: item.color }}>{item.venue}</span>
                  <span className="truncate text-xs font-semibold text-text-primary group-hover:text-white">{item.pick}</span>
                </div>
                <div className="mt-0.5 truncate text-[10px] text-text-muted">{item.matchup} · {item.detail}</div>
              </div>
              <div className="text-right text-xs font-semibold tabular-nums text-text-primary">{Math.round(item.model * 100)}%<span className="block text-[9px] font-normal text-text-muted sm:hidden">model</span></div>
              <div className="hidden text-right text-xs tabular-nums text-text-secondary sm:block">{item.price}</div>
              <div className="hidden text-right text-xs font-semibold tabular-nums text-win sm:block">+{num(item.ev * 100, 1)}%</div>
              <div className="col-span-2 mt-1 flex gap-2 text-[10px] tabular-nums sm:hidden"><span>{item.price} ask</span><span className="font-semibold text-win">+{num(item.ev * 100, 1)}% net EV</span></div>
            </a>
          ))}
        </div>
      </div>
      <p className="text-[10px] leading-relaxed text-text-muted">Prices are read-only market snapshots. Polymarket EV includes the displayed YES ask and execution allowance; Kalshi EV includes estimated contract fees. Open a venue to review its current order book and rules.</p>
    </Section>
  );
}

/**
 * One live read for the whole page.
 *
 * Live Sunday and the Watching Guide both need the same three things — league
 * scores, NFL game state, and the lineup actually set on each platform — and
 * until now they were free to describe DIFFERENT lineups: one the real one, the
 * other the optimizer's recommendation. Hoisting the fetch to their common
 * parent makes that divergence impossible, and halves the polling besides.
 */
function useLiveData(d: any) {
  const leagues = useMemo(() => arr(d.leagues), [d.leagues]);
  const [nonce, setNonce] = useState(0);
  const [live, setLive] = useState<{ loading: boolean; checked?: string; scores: any[]; games: Record<string, any>; stats: Record<string, any>; error?: string }>({
    loading: true, scores: [], games: {}, stats: {},
  });

  useEffect(() => {
    let cancelled = false;
    const load = async () => {
      setLive(v => ({ ...v, loading: true, error: undefined }));
      try {
        // Sleeper reports the league's own live fantasy totals, which means
        // custom scoring needs no browser-side reimplementation.
        const sleeper = leagues.filter((l: any) => l.platform === 'sleeper' && l.league_id && l.franchise_id);
        const sleeperScores = await Promise.all(sleeper.map(async (l: any) => { try {
          const response = await fetch(liveApiUrl(new URLSearchParams({
            source: 'sleeper', league: String(l.league_id), week: String(d.week),
          })));
          if (!response.ok) throw new Error(`Sleeper ${response.status}`);
          const rows = await response.json();
          const mine = arr(rows).find((r: any) => String(r.roster_id) === String(l.franchise_id));
          const opponent = mine ? arr(rows).find((r: any) => r.matchup_id === mine.matchup_id && String(r.roster_id) !== String(mine.roster_id)) : null;
          // players_points is in the LEAGUE's own scoring, which is what the
          // roster rows should show — a custom format would otherwise be
          // rendered in half-PPR and quietly disagree with the league total.
          // `starters` is who is ACTUALLY in the lineup on the platform, which is
          // not necessarily who the optimizer recommended. The header score comes
          // from the real lineup, so the rows have to as well — otherwise the card
          // shows a player who is benched and omits one whose points are already
          // in the total.
          return mine ? { league: l.league, source: 'Sleeper', mine: mine.points ?? 0, opponent: opponent?.points ?? 0,
                          opponentName: l.opponent, playerPoints: mine.players_points ?? {},
                          opponentPlayerPoints: opponent?.players_points ?? {},
                          starters: (mine.starters ?? []).filter((x: any) => x && x !== '0'),
                          opponentStarters: (opponent?.starters ?? []).filter((x: any) => x && x !== '0') } : null;
        } catch { return null; } }));
        const mfl = leagues.filter((l: any) => l.platform === 'mfl' && l.league_id && l.franchise_id);
        const mflScores = await Promise.all(mfl.map(async (l: any) => { try {
          const response = await fetch(liveApiUrl(new URLSearchParams({
            source: 'mfl', league: String(l.league_id), week: String(d.week), season: String(d.season),
          })));
          if (!response.ok) throw new Error(`MFL ${response.status}`);
          const payload = await response.json();
          const matchupsRaw = payload?.liveScoring?.matchup ?? payload?.liveScoring?.matchups;
          const matchups = Array.isArray(matchupsRaw) ? matchupsRaw : matchupsRaw ? [matchupsRaw] : [];
          const target = Number(l.franchise_id);
          for (const matchup of matchups) {
            const franchisesRaw = matchup?.franchise ?? matchup?.franchises;
            const franchises = Array.isArray(franchisesRaw) ? franchisesRaw : franchisesRaw ? [franchisesRaw] : [];
            const mine = franchises.find((f: any) => Number(f.id ?? f.franchise_id) === target);
            if (!mine) continue;
            const opponent = franchises.find((f: any) => Number(f.id ?? f.franchise_id) !== target);
            const points = (f: any) => Number(f?.score ?? f?.points ?? f?.liveScore ?? 0);
            return { league: l.league, source: 'MFL', mine: points(mine), opponent: points(opponent), opponentName: l.opponent };
          }
          return null;
        } catch { return null; } }));
        // The public ESPN scoreboard is deliberately used only for NFL game
        // state; it complements (rather than replaces) league-native scoring.
        // One league-wide stat call serves every roster on the page. Isolated in
        // its own catch: stat lines are an enhancement, and losing them must not
        // take the live SCORES down with them. The status check alone is not
        // enough — a dev server's SPA fallback answers an unknown /api path with
        // 200 and index.html, so .ok is true and .json() throws on the markup.
        let stats: Record<string, any> = {};
        try {
          const statsResponse = await fetch(liveApiUrl(new URLSearchParams({
            source: 'sleeper_stats', season: String(d.season), week: String(d.week),
          })));
          const parsed = statsResponse.ok ? await statsResponse.json() : null;
          if (parsed && typeof parsed === 'object' && !Array.isArray(parsed)) stats = parsed;
        } catch { /* stat lines simply stay empty */ }
        // Scope game state to the report being viewed. ESPN's unqualified
        // scoreboard always returns the current NFL week; pairing that with a
        // week-specific Sleeper stat feed made last week's box scores appear
        // beside this week's not-yet-started players.
        const boardParams = new URLSearchParams({
          limit: '100', dates: String(d.season), seasontype: '2', week: String(d.week),
        });
        const boardResponse = await fetch(`https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?${boardParams}`);
        const boardPayload = boardResponse.ok ? await boardResponse.json() : null;
        // Fail closed if the provider ever ignores the week filter. Empty game
        // state is preferable to confidently attaching another week's stats.
        const board = Number(boardPayload?.week?.number) === Number(d.week)
          ? boardPayload
          : { events: [] };
        // ESPN's abbreviations are not ours for two clubs: it writes LAR and WSH
        // where the rest of this stack writes LA and WAS. Keyed raw, every Rams
        // and Commanders starter matches no game — so they show no clock and,
        // under the pregame rule below, a permanent dash even while playing.
        const ESPN_TEAM_FIX: Record<string, string> = { LAR: 'LA', WSH: 'WAS' };
        const games: Record<string, any> = {};
        arr(board.events).forEach((event: any) => {
          const competition = event.competitions?.[0];
          const status = event.status ?? competition?.status;
          const competitors = competition?.competitors ?? [];
          const kickoff = event.date
            ? new Date(event.date).toLocaleString([], { weekday: 'short', hour: 'numeric', minute: '2-digit' })
            : 'Time TBD';
          competitors.forEach((c: any) => {
            const raw = c.team?.abbreviation;
            const team = raw ? (ESPN_TEAM_FIX[raw] ?? raw) : raw;
            if (team) games[team] = {
              event: event.name, score: c.score ?? '0', state: status?.type?.state,
              detail: status?.type?.detail ?? status?.type?.shortDetail ?? 'Scheduled',
              kickoff,
              // Raw clock, so a live win probability can weight how much of the
              // game is still to come rather than treating a player mid-game as
              // either finished or unplayed.
              period: Number(status?.period) || 0,
              clock: String(status?.displayClock ?? ''),
              // A compact label for the roster rows. ESPN's own `detail` reads
              // "Sun, September 13th at 1:00 PM EDT", which is far too long for a
              // right-hand column repeated once per starter.
              short: (() => {
                const st = status?.type?.state;
                if (st === 'post') return 'Final';
                if (st === 'in') {
                  const period = status?.period;
                  const clock = status?.displayClock;
                  return period && clock ? `Q${period} ${clock}` : (status?.type?.shortDetail ?? 'Live');
                }
                return kickoff;
              })(),
            };
          });
        });
        if (!cancelled) setLive({ loading: false, checked: new Date().toLocaleTimeString([], { hour: 'numeric', minute: '2-digit' }),
          scores: [...sleeperScores, ...mflScores].filter(Boolean), games, stats });
      } catch {
        if (!cancelled) setLive(v => ({ ...v, loading: false, error: 'Live sources are temporarily unavailable.' }));
      }
    };
    load();
    // A single page polls at a conservative cadence. The page remains useful
    // pregame, while in-progress matchups receive a fresh read each minute.
    const timer = window.setInterval(load, 60_000);
    return () => { cancelled = true; window.clearInterval(timer); };
  }, [d.week, leagues, nonce]);
  return { live, leagues, refresh: () => setNonce((n: number) => n + 1) };
}

/**
 * League -> the roster rows for the lineup ACTUALLY set on the platform.
 *
 * Resolves the platform's `starters` id list against every roster row the export
 * carries, bench included — the player the optimizer benched may be the one you
 * started, and he still has to resolve to a name. Leagues whose platform exposes
 * no starter list (MFL, ESPN) are absent from the map, and callers fall back to
 * the recommendation and say so rather than passing it off as live.
 */
function useActualStarters(d: any, live: any) {
  return useMemo(() => {
    const index = new Map<string, Map<string, any>>();
    arr(d.lineups).forEach((r: any) => {
      const key = r.sleeper_id || (r.position === 'DEF' ? r.team : null);
      if (!key) return;
      const m = index.get(r.league) ?? new Map();
      m.set(String(key), r); index.set(r.league, m);
    });
    const out = new Map<string, any[]>();
    arr(live?.scores).forEach((score: any) => {
      const ids: string[] = arr(score?.starters);
      if (!ids.length) return;
      const idx = index.get(score.league);
      const rows = ids.map(id => idx?.get(String(id))).filter(Boolean);
      // All-or-nothing: a partly resolved lineup would silently drop starters.
      if (rows.length === ids.length) out.set(score.league, rows);
    });
    return out;
  }, [d.lineups, live?.scores]);
}

function LiveSunday({ d, live, leagues, refresh, actualStarters }: { d: any; live: any; leagues: any[]; refresh: () => void; actualStarters: Map<string, any[]> }) {

  // Starters, grouped by league, each carrying the live game state of their NFL
  // team. The panel used to end in a wall of 32 team chips — one per club, most
  // of them reading "0 · Sun, September 13th at 1:00 PM EDT" — which is a lot of
  // room for information nobody is watching for. The same game state is more
  // useful attached to the player it affects.
  const byLeague = useMemo(() => {
    const starters = arr(d.lineups).filter((r: any) => r.status === 'START');
    const groups = new Map<string, any[]>();
    starters.forEach((r: any) => {
      const g = groups.get(r.league) ?? [];
      g.push(r); groups.set(r.league, g);
    });
    return groups;
  }, [d.lineups]);

  // Sleeper keys team defenses by team abbreviation rather than a numeric id, so
  // DEF rows join on `team`. Everything else joins on the sleeper_id the R export
  // now carries for every player, whatever platform the league runs on.
  const statKey = (r: any) => r.sleeper_id || (r.position === 'DEF' ? r.team : null);

  /**
   * Did the weekly model actually produce a distribution for this player?
   *
   * IDP slots (DL/LB/DB) are outside the model entirely: the export carries them
   * with proj 0 and null floor/ceiling. Counting them as a genuine zero drags the
   * projected total down and, because the opponent's optimized lineup contains no
   * IDP, biases the whole matchup — switching No Fun League to its real lineup
   * dropped the projection 94.6 to 83.1 and the win chance 38% to 16%, none of
   * which was real. They are excluded from the projection and the win
   * probability, and their points still count in the live score above, which is
   * the platform's own number.
   */
  // The null check is load-bearing: Number(null) is 0, not NaN, so a bare
  // Number.isFinite(Number(r.floor)) accepts a player the model never touched and
  // the whole filter silently does nothing.
  const isProjectable = (r: any) =>
    r?.floor != null && r?.ceiling != null &&
    Number.isFinite(Number(r.floor)) && Number.isFinite(Number(r.ceiling));

  /** Standard normal CDF (Abramowitz & Stegun 7.1.26). No Math.erf in JS. */
  const normCdf = (z: number) => {
    const sign = z < 0 ? -1 : 1;
    const x = Math.abs(z) / Math.SQRT2;
    const t = 1 / (1 + 0.3275911 * x);
    const y = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-x * x);
    return 0.5 * (1 + sign * y);
  };

  /**
   * Share of regulation still to play, from the game clock. 0 once final.
   *
   * A player mid-game is neither settled nor unplayed, and treating him as
   * either is what makes a naive live win probability lurch. This weights his
   * remaining projection by the time left.
   *
   * The assumption — that fantasy production accrues evenly across the clock —
   * is a simplification. It is wrong in the specific ways you would expect (a
   * trailing team throws more late), but it is far closer than the alternative
   * of ignoring the clock entirely.
   */
  const shareRemaining = (g: any) => {
    if (!g || g.state === 'post') return 0;
    if (g.state !== 'in') return 1;
    const period = Number(g.period) || 1;
    if (period > 4) return 0;                       // overtime — treat as settled
    const [mm, ss] = String(g.clock || '0:00').split(':').map(Number);
    const leftInQuarter = (Number.isFinite(mm) ? mm : 0) + (Number.isFinite(ss) ? ss : 0) / 60;
    const minutesLeft = (4 - period) * 15 + Math.min(15, Math.max(0, leftInQuarter));
    return Math.min(1, Math.max(0, minutesLeft / 60));
  };

  /**
   * Live win probability, recomputed from what has actually happened.
   *
   * Mirrors the R model this card's static number comes from — combine each
   * side's player spreads in quadrature, then the normal CDF of the margin — but
   * replaces a projection with a RESULT the moment one exists:
   *
   *   finished   mean = actual points,             sd = 0   (settled, no variance left)
   *   in play    mean = actual + share x proj,     sd = player_sd x sqrt(share)
   *   not begun  mean = proj,                      sd = player_sd
   *
   * Variance scales with the square root of time remaining, not linearly: points
   * accumulate as a sum of independent contributions, so their variance grows
   * with elapsed time and the standard deviation with its root.
   */
  const liveWinProb = (mine: any[], opp: any[], scoreRow: any) => {
    const side = (players: any[], pointsMap: any) => {
      let mean = 0, variance = 0, settled = 0;
      players.filter(isProjectable).forEach((r: any) => {
        const g = live.games[r.team];
        const share = shareRemaining(g);
        const sd = Math.max(0, ((Number(r.ceiling) - Number(r.floor)) || 0) / 3.29);
        const k = statKey(r);
        const nativePts = k != null ? pointsMap?.[k] : undefined;
        const feedPts = k != null ? live.stats?.[k]?.pts_half_ppr : undefined;
        const actual = Number(nativePts != null ? nativePts : feedPts);
        const hasActual = Number.isFinite(actual);
        if (share === 0 && g) {                       // game over: the result stands
          mean += hasActual ? actual : Number(r.proj) || 0;
          settled += 1;
        } else if (share < 1 && hasActual) {          // in play
          mean += actual + share * (Number(r.proj) || 0);
          variance += (sd * Math.sqrt(share)) ** 2;
        } else {                                      // not started
          mean += Number(r.proj) || 0;
          variance += sd * sd;
        }
      });
      return { mean, variance, settled };
    };
    const a = side(mine, scoreRow?.playerPoints);
    const b = side(opp, scoreRow?.opponentPlayerPoints);
    const marginSd = Math.sqrt(a.variance + b.variance);
    if (!(marginSd > 0)) return { p: a.mean > b.mean ? 1 : a.mean < b.mean ? 0 : 0.5,
                                  mine: a.mean, opp: b.mean, settled: a.settled + b.settled };
    return { p: normCdf((a.mean - b.mean) / marginSd), mine: a.mean, opp: b.mean,
             settled: a.settled + b.settled };
  };

  /** "62 pass yd · 1 TD" — only the categories the player actually has. */
  const statBlurb = (st: any) => {
    if (!st) return null;
    const parts: string[] = [];
    const n = (v: any) => Math.round(Number(v) || 0);
    if (n(st.pass_yd) || n(st.pass_td)) parts.push(`${n(st.pass_yd)} pass yd${n(st.pass_td) ? ` · ${n(st.pass_td)} TD` : ''}`);
    if (n(st.rush_yd) || n(st.rush_td)) parts.push(`${n(st.rush_yd)} rush yd${n(st.rush_td) ? ` · ${n(st.rush_td)} TD` : ''}`);
    if (n(st.rec) || n(st.rec_yd) || n(st.rec_td)) parts.push(`${n(st.rec)} rec, ${n(st.rec_yd)} yd${n(st.rec_td) ? ` · ${n(st.rec_td)} TD` : ''}`);
    if (n(st.pass_int)) parts.push(`${n(st.pass_int)} INT`);
    if (n(st.fum_lost)) parts.push(`${n(st.fum_lost)} FUM`);
    return parts.length ? parts.join('  ·  ') : null;
  };

  return (
    <Section id="live_sunday" title="Live Sunday" accent={SECTION_ACCENT.lineups}
      subtitle="your starters, live — league scoring with NFL game state from ESPN"
      right={<button type="button" onClick={refresh} disabled={live.loading}
        className="rounded border border-border-light bg-bg-card-hover px-1.5 py-0.5 text-[9px] font-medium text-text-secondary hover:text-text-primary disabled:opacity-50">
        {live.loading ? 'Refreshing…' : 'Refresh'}
      </button>}>
      {live.error && <p className="text-xs text-loss">{live.error}</p>}
      {!live.error && <>
        <div className="grid gap-2 lg:grid-cols-2">
          {Array.from(byLeague.entries()).map(([league, recommended]) => {
            const score = live.scores.find((x: any) => x.league === league);
            // Prefer the lineup that is actually set. MFL and ESPN expose no
            // starter list here, so those fall back to the recommendation and are
            // labelled as such rather than being passed off as live.
            const resolved = actualStarters.get(league);
            const usingActual = Boolean(resolved?.length);
            const players = usingActual ? resolved! : recommended;
            // Whoever is playing right now goes to the top; then the not-yet-started;
            // finished last. Within a bucket, the biggest score leads.
            const rank = (r: any) => {
              const st = live.games[r.team]?.state;
              return st === 'in' ? 0 : st === 'post' ? 2 : 1;
            };
            const pts = (r: any) => {
              // The points feed is week-scoped, but it is only valid for this
              // row once the same selected-week game has actually begun. This
              // prevents cached or mismatched stats from leaking into pregame
              // rows.
              const state = live.games[r.team]?.state;
              if (state !== 'in' && state !== 'post') return NaN;
              const k = statKey(r);
              const native = score?.playerPoints?.[k as string];
              // League-native where the platform reports it; otherwise Sleeper's
              // half-PPR, which is exactly right for the MFL league (it really is
              // half-PPR) and an approximation for any custom format.
              return native != null ? Number(native) : Number(live.stats?.[k as string]?.pts_half_ppr ?? NaN);
            };
            const sorted = [...players].sort((a, b) => rank(a) - rank(b) || (pts(b) || 0) - (pts(a) || 0));
            const anyLive = players.some((r: any) => live.games[r.team]?.state === 'in');
            // Projected totals and model win chance, matching the My Lineups card:
            // my side is the sum of the optimized starters, the opponent's comes
            // precomputed from their own optimized lineup. Shown alongside the LIVE
            // score so the two are read together — the live number alone says
            // nothing about whether you are ahead of where you should be.
            const meta = leagues.find((x: any) => x.league === league);
            const modelled = players.filter(isProjectable);
            const projMine = modelled.reduce((acc: number, r: any) => acc + (r.proj || 0), 0);
            const projOpp = Number(meta?.opponent_proj);
            const pregamePct = Number(meta?.win_probability);
            const oppPlayers = arr(d.opponent_lineups).filter((r: any) => r.league === league);
            // Recompute from results where they exist. Falls back to the R figure
            // when the opponent's lineup is missing, so a league without one is
            // not silently dropped to a coin flip.
            // Uses `players` — the actual lineup where the platform reports one.
            // The opponent stays on their optimized lineup, which is the best
            // available: their bench is not in this export, so their real starters
            // cannot all be resolved. Their FINISHED players are still scored from
            // live points, so the asymmetry only affects who is yet to play.
            const lw = oppPlayers.length ? liveWinProb(players, oppPlayers, score) : null;
            const winPct = lw ? lw.p : pregamePct;
            const projMineLive = lw ? lw.mine : projMine;
            const projOppLive = lw ? lw.opp : projOpp;
            const hasProj = Number.isFinite(projOppLive) && projMineLive > 0;
            const hasOriginal = Number.isFinite(projOpp) && projMine > 0;
            return (
              <div key={league} className="overflow-hidden rounded-lg border border-border-light bg-bg-card/30">
                <div className="flex items-baseline justify-between gap-2 border-b border-border-light px-2.5 py-1.5">
                  <span className="min-w-0 truncate text-[11px] font-medium text-text-primary">
                    {league}
                    {anyLive && <span className="ml-1.5 inline-block h-1.5 w-1.5 rounded-full bg-win align-middle" />}
                  </span>
                  <span className="max-w-[45%] truncate text-right text-[9px] text-text-muted">
                    {score?.opponentName ? `vs ${score.opponentName}` : 'opponent unavailable'}
                  </span>
                </div>
                <div className="border-b border-border-light bg-bg-card-hover/20 tabular-nums">
                  <div className="flex items-center gap-2 px-2.5 py-1.5 text-[9px]"
                    title={lw
                      ? `Win probability moved from ${Math.round(pregamePct * 100)}% pregame to ${Math.round(winPct * 100)}% live. ${lw.settled} starters have finished; their results are fixed and the remaining games are projected.`
                      : 'Original pregame win probability.'}>
                    <span className="shrink-0 whitespace-nowrap text-text-muted">
                      Now <span className="font-semibold text-text-primary">{score ? `${num(score.mine)}–${num(score.opponent)}` : '—'}</span>
                    </span>
                    <span className="h-1.5 min-w-[2.75rem] max-w-24 flex-1 overflow-hidden rounded-full bg-bg-card-hover">
                      <span className="block h-full rounded-full"
                        style={{ width: `${Number.isFinite(winPct) ? clamp(winPct * 100) : 0}%`,
                                 background: Number.isFinite(winPct) && winPct >= 0.5 ? 'var(--color-win)' : 'var(--color-loss)' }} />
                    </span>
                    <span className="shrink-0 whitespace-nowrap font-semibold"
                      style={{ color: Number.isFinite(winPct) && winPct >= 0.5 ? 'var(--color-win)' : 'var(--color-loss)' }}>
                      {Number.isFinite(winPct) ? `${Math.round(winPct * 100)}% win` : '—'}
                    </span>
                    <span className="shrink-0 whitespace-nowrap text-[8px] text-text-muted">
                      from {Number.isFinite(pregamePct) ? `${Math.round(pregamePct * 100)}%` : '—'}
                    </span>
                  </div>
                  <div className="grid grid-cols-2 divide-x divide-border border-t border-border-light text-center text-[9px]">
                    <span className="min-w-0 whitespace-nowrap px-2 py-1" title="Expected final score using results so far and projections for remaining games">
                      <span className="text-text-muted">Expected </span>
                      <span className="font-medium text-text-secondary">{hasProj ? `${num(projMineLive, 0)}–${num(projOppLive, 0)}` : '—'}</span>
                    </span>
                    <span className="min-w-0 whitespace-nowrap px-2 py-1" title="Original pregame projected score">
                      <span className="text-text-muted">Original </span>
                      <span className="font-medium text-text-secondary">{hasOriginal ? `${num(projMine, 0)}–${num(projOpp, 0)}` : '—'}</span>
                    </span>
                  </div>
                  </div>
                <div className="divide-y divide-border-light/60">
                  {sorted.map((r: any, i: number) => {
                    const g = live.games[r.team];
                    const isLive = g?.state === 'in';
                    const isDone = g?.state === 'post';
                    // A player whose game has not kicked off has not scored zero —
                    // he has not played. Sleeper reports 0 for both, so the state
                    // has to come from the game, not the feed. Once the ball is in
                    // the air a 0.0 is real information and is shown as such.
                    const played = isLive || isDone;
                    const st = played ? live.stats?.[statKey(r) as string] : null;
                    const blurb = statBlurb(st);
                    const p = pts(r);
                    return (
                      <div key={i} className="grid grid-cols-[1.9rem_minmax(0,1fr)_auto] items-center gap-2 px-2.5 py-1">
                        <span className="rounded bg-bg-card-hover py-0.5 text-center text-[8.5px] font-bold text-text-muted">{r.position}</span>
                        <div className="min-w-0">
                          <div className="truncate text-[11.5px] text-text-primary">
                            {r.player}
                            <span className="ml-1 text-[9px] text-text-muted">{r.team}</span>
                          </div>
                          <div className="truncate text-[9.5px]">
                            {blurb
                              ? <span className="text-text-secondary">{blurb}</span>
                              : <span className="text-text-muted">{isLive ? 'no stats yet' : isDone ? 'did not record' : `vs ${r.opponent ?? '—'}`}</span>}
                          </div>
                        </div>
                        <div className="shrink-0 text-right">
                          <div className="text-[11.5px] font-semibold tabular-nums"
                            style={{ color: played && Number.isFinite(p) ? 'var(--color-text-primary)' : 'var(--color-text-muted)' }}>
                            {played && Number.isFinite(p) ? num(p) : '–'}
                            <span className="ml-1 text-[9px] font-normal text-text-muted">/ {num(r.proj)}</span>
                          </div>
                          <div className={`text-[8.5px] tabular-nums ${isLive ? 'text-win' : 'text-text-muted'}`}>
                            {g ? (isDone ? `Final ${g.score}` : g.short) : '—'}
                          </div>
                        </div>
                      </div>
                    );
                  })}
                </div>
              </div>
            );
          })}
        </div>
        <p className="text-[9px] text-text-muted">Refreshes every 60 seconds{live.checked ? ` · checked ${live.checked}` : ''}. Sleeper totals use each league’s actual scoring; ESPN supplies game clock and score.</p>
      </>}
    </Section>
  );
}

function WatchingGuide({ d, live, actualStarters }: { d: any; live: any; actualStarters: Map<string, any[]> }) {
  // Organised by GAME, not by player.
  //
  // The previous shape was two flat columns — everyone you are rooting for, then
  // everyone you are rooting against — which is the wrong unit for the job. You
  // do not watch a player, you watch a game, and the question in front of a game
  // is "what is my stake in this one". A player-ranked list split the two sides
  // of a single broadcast across two columns and forty rows.
  //
  // Only STARTED players count. A benched player is context for a lineup
  // decision, not a reason to care about a game in progress.
  const games = useMemo(() => {
    const byGame = new Map<string, any>();

    // Who each team plays, learned from every row that knows. IDP starters come
    // through with a null opponent — the weekly model does not cover DL/LB/DB, so
    // it never resolves one for them — and requiring the field outright silently
    // dropped them from the guide even though they are in the lineup. Any other
    // row from the same game supplies the answer.
    const opponentOf = new Map<string, string>();
    [...arr(d.lineups), ...arr(d.opponent_lineups)].forEach((r: any) => {
      if (r?.team && r?.opponent) {
        opponentOf.set(r.team, r.opponent);
        if (!opponentOf.has(r.opponent)) opponentOf.set(r.opponent, r.team);
      }
    });

    const touch = (row: any, side: 'mine' | 'against') => {
      if (!row?.player || !row?.team) return;
      const opponent = row.opponent || opponentOf.get(row.team);
      if (!opponent) return;
      row = { ...row, opponent };
      // One key per broadcast, whichever side a player is on.
      const key = [row.team, row.opponent].sort().join('@');
      const g = byGame.get(key) ?? { key, teams: [row.team, row.opponent].sort(), mine: [], against: [] };
      g[side].push(row);
      byGame.set(key, g);
    };
    // The lineup actually set, where the platform reports one; the optimizer's
    // recommendation otherwise. Same resolution Live Sunday uses, so the two
    // panels cannot name different starters for the same league.
    const recommended = arr(d.lineups).filter((r: any) => r.status === 'START');
    const byLeague = new Map<string, any[]>();
    recommended.forEach((r: any) => {
      byLeague.set(r.league, [...(byLeague.get(r.league) ?? []), r]);
    });
    Array.from(byLeague.keys()).forEach(lg => {
      const live = actualStarters.get(lg);
      if (live?.length) byLeague.set(lg, live);
    });
    Array.from(byLeague.values()).flat().forEach((r: any) => touch(r, 'mine'));
    arr(d.opponent_lineups).forEach((r: any) => touch(r, 'against'));

    return Array.from(byGame.values()).filter((g: any) =>
      // A guide is for the remainder of the slate. ESPN attaches the same NFL
      // status to both teams, but checking either side makes this robust when a
      // provider has only populated one team entry.
      !g.teams.some((team: string) => live?.games?.[team]?.state === 'post')
    ).map((g: any) => {
      // Net projected points at stake: what your rosters stand to gain from this
      // game minus what your opponents' do. A game with equal representation is
      // a genuine wash, and the card says so rather than implying a side.
      const sum = (rows: any[]) => rows.reduce((a, r) => a + (Number(r.proj) || 0), 0);
      const forPts = sum(g.mine), againstPts = sum(g.against);
      // A player can legitimately appear on BOTH sides: yours in one league,
      // your opponent's in another. He nets to nothing, and the card marks him
      // rather than leaving the same name printed twice with no explanation.
      const ids = (rows: any[]) => new Set(rows.map((r: any) => `${r.team}::${r.player}`));
      const mineIds = ids(g.mine), againstIds = ids(g.against);
      const conflicted = new Set([...mineIds].filter(k => againstIds.has(k)));
      const game = g.teams.map((team: string) => live?.games?.[team]).find(Boolean);
      return { ...g, forPts, againstPts, net: forPts - againstPts, conflicted,
               kickoff: game?.kickoff ?? 'Time TBD',
               involved: g.mine.length + g.against.length };
    }).sort((a: any, b: any) => Math.abs(b.net) - Math.abs(a.net) || b.involved - a.involved);
  }, [d, live?.games, actualStarters]);

  // One person rostered in three leagues is one row with a x3, not three rows.
  const collapse = (rows: any[]) => {
    const m = new Map<string, any>();
    rows.forEach(r => {
      const k = `${r.team}::${r.player}`;
      const cur = m.get(k) ?? { ...r, count: 0, leagues: [] as string[] };
      cur.count += 1; cur.leagues.push(r.league);
      m.set(k, cur);
    });
    return Array.from(m.values())
      .sort((a, b) => b.count - a.count || (Number(b.proj) || 0) - (Number(a.proj) || 0));
  };

  const Side = ({ rows, tone, conflicted }: { rows: any[]; tone: 'for' | 'against'; conflicted: Set<string> }) => {
    const list = collapse(rows);
    if (!list.length) return <div className="px-2 py-1.5 text-[9.5px] text-text-muted">none</div>;
    return (
      <div className="divide-y divide-border-light/40">
        {list.map((r, i) => {
        const split = conflicted.has(`${r.team}::${r.player}`);
        return (
          <div key={i} className={`flex items-center gap-1.5 px-2 py-1 ${split ? 'opacity-55' : ''}`}
            title={split
              ? `Also started against you — ${r.leagues.join(' · ')}. His points cancel out.`
              : r.leagues.join(' · ')}>
            <span className="w-7 shrink-0 rounded bg-bg-card-hover py-0.5 text-center text-[8.5px] font-bold text-text-muted">{r.position}</span>
            <span className="min-w-0 flex-1 truncate text-[11px] text-text-primary">
              {r.player}
              <span className="ml-1 text-[9px] text-text-muted">{r.team}</span>
              {split && <span className="ml-1 text-[9px] text-text-muted">⇄</span>}
            </span>
            {r.count > 1 && (
              <span className="shrink-0 text-[9px] font-semibold tabular-nums"
                style={{ color: tone === 'for' ? 'var(--color-win)' : 'var(--color-loss)' }}>×{r.count}</span>
            )}
            <span className="w-8 shrink-0 text-right text-[10px] tabular-nums text-text-secondary">{num(r.proj)}</span>
          </div>
        );
        })}
      </div>
    );
  };

  return (
    <Section id="watching_guide" title="Watching Guide" accent={SECTION_ACCENT.lineups}
      subtitle="your stake in remaining games · started players across every league">
      {games.length === 0 ? <p className="text-xs text-text-muted">No remaining games to watch.</p> : (
        <div className="grid gap-2 md:grid-cols-2 xl:grid-cols-3">
          {games.map((g: any) => {
            const lean = g.net > 0.5 ? 'for' : g.net < -0.5 ? 'against' : 'wash';
            const color = lean === 'for' ? 'var(--color-win)'
                        : lean === 'against' ? 'var(--color-loss)' : 'var(--color-text-muted)';
            return (
              <div key={g.key} className="overflow-hidden rounded-lg border border-border-light bg-bg-card/30">
                <div className="flex items-center justify-between gap-2 border-b border-border-light px-2.5 py-1.5"
                  style={{ boxShadow: `inset 2px 0 0 ${color}` }}>
                  <div className="min-w-0">
                    <div className="truncate text-[11.5px] font-semibold text-text-primary">
                      {g.teams[0]} <span className="font-normal text-text-muted">v</span> {g.teams[1]}
                    </div>
                    <div className="mt-0.5 text-[9px] font-medium text-text-muted">{g.kickoff}</div>
                  </div>
                  <span className="shrink-0 text-[10px] font-semibold tabular-nums" style={{ color }}>
                    {lean === 'wash' ? 'even' : `${g.net > 0 ? '+' : '−'}${num(Math.abs(g.net))} net`}
                  </span>
                </div>
                {/* minmax(0,1fr), not grid-cols-2. A `1fr` track is
                    minmax(auto, 1fr), and that auto floor stops a column
                    shrinking below its longest player name — on a phone the name
                    pushed the column past the card and printed over the other
                    side's total. */}
                <div className="grid grid-cols-[minmax(0,1fr)_minmax(0,1fr)] divide-x divide-border-light">
                  <div className="min-w-0">
                    <div className="flex items-baseline justify-between px-2 py-0.5">
                      <span className="text-[8.5px] font-semibold uppercase tracking-wider text-win">Yours</span>
                      {g.mine.length > 0 && <span className="text-[9px] tabular-nums text-text-muted">{num(g.forPts)}</span>}
                    </div>
                    <Side rows={g.mine} tone="for" conflicted={g.conflicted} />
                  </div>
                  <div className="min-w-0">
                    <div className="flex items-baseline justify-between px-2 py-0.5">
                      <span className="text-[8.5px] font-semibold uppercase tracking-wider text-loss">Against</span>
                      {g.against.length > 0 && <span className="text-[9px] tabular-nums text-text-muted">{num(g.againstPts)}</span>}
                    </div>
                    <Side rows={g.against} tone="against" conflicted={g.conflicted} />
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}
    </Section>
  );
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
          const hasMatchup = Boolean(m?.opponent) && Number.isFinite(Number(m?.opponent_proj));
          const winPct = Number(m?.win_probability);
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
              {hasMatchup && (
                <div className="flex items-center justify-between gap-2 border-b border-border-light bg-bg-card-hover/30 px-2.5 py-1 text-[10px]">
                  <span className="min-w-0 truncate text-text-secondary">
                    <span className="text-text-muted">This week vs </span>
                    <span className="font-medium text-text-primary">{m.opponent}</span>
                    <span className="text-text-muted"> · model {num(total)}–{num(m.opponent_proj)}</span>
                  </span>
                  {Number.isFinite(winPct) && (
                    <span
                      className={`shrink-0 rounded-full px-1.5 py-0.5 font-semibold tabular-nums ${
                        winPct >= 0.5 ? 'bg-win/10 text-win' : 'bg-loss/10 text-loss'
                      }`}
                      title="Model win chance based on both optimized projected lineups and their player ranges. It is not a betting-market probability."
                    >
                      {Math.round(winPct * 100)}% win
                    </span>
                  )}
                </div>
              )}
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

function MyLineups({ d }: { d: any }) {
  // One fetch, shared. Both panels describe the same lineup by construction.
  const { live, leagues, refresh } = useLiveData(d);
  const actualStarters = useActualStarters(d, live);
  return <>
    <LiveSunday d={d} live={live} leagues={leagues} refresh={refresh} actualStarters={actualStarters} />
    <WatchingGuide d={d} live={live} actualStarters={actualStarters} />
    <Lineups d={d} />
  </>;
}

/** A short, cross-league priority list ahead of the detailed roster reports. */
function RosterActionQueue({ lineups, waivers, dst, onOpenLineups }: {
  lineups: any; waivers: any; dst: any; onOpenLineups: () => void;
}) {
  const starters = arr(lineups?.lineups).filter((row: any) => row.status === 'START');
  const leagues = new Set(starters.map((row: any) => row.league));
  const waiverMoves = arr(waivers?.waivers)
    .filter((row: any) => row.suggested_add && row.suggested_drop)
    .sort((a: any, b: any) => (Number(b.net_gain) || 0) - (Number(a.net_gain) || 0))
    .slice(0, 2);
  const stream = arr(dst?.board)[0];
  const priorities = [
    ...waiverMoves.map((move: any) => ({
      kind: 'Wire', tone: 'text-win', title: `Add ${move.suggested_add}`,
      detail: `Drop ${move.suggested_drop} · ${move.league ?? 'League'}`,
      value: Number(move.net_gain) ? `+${num(move.net_gain)} this week` : 'Review upgrade', href: '#waivers',
    })),
    ...(stream ? [{
      kind: 'Stream', tone: 'text-[#56b6c2]', title: `${stream.team} DST`,
      detail: `${stream.is_home ? 'vs' : '@'} ${stream.opp} · lowest opponent total`,
      value: `${num(stream.opp_implied)} implied pts`, href: '#dst_stream',
    }] : []),
  ].slice(0, 3);

  if (!starters.length && !priorities.length) return null;
  return (
    <section className="report-section overflow-hidden rounded-2xl border border-border bg-bg-card/60">
      <div className="report-section__header flex flex-wrap items-center justify-between gap-3 border-b border-border-light px-4 py-3" style={{ boxShadow: 'inset 3px 0 0 #3fb950' }}>
        <div>
          <h3 className="text-sm font-semibold tracking-tight text-text-primary">Roster action queue</h3>
          <p className="mt-0.5 text-[11px] text-text-muted">The highest-impact work across your leagues, ranked before the full boards.</p>
        </div>
        <button type="button" onClick={onOpenLineups}
          className="rounded-lg border border-border-light bg-bg-card/60 px-2.5 py-1.5 text-[11px] font-medium text-text-secondary transition-colors hover:border-accent/60 hover:bg-accent/10 hover:text-text-primary focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
          Review lineups →
        </button>
      </div>
      <div className="report-section__body grid gap-2.5 p-4 md:grid-cols-[minmax(11rem,0.72fr)_minmax(0,1.55fr)]">
        <div className="rounded-xl border border-border-light bg-bg-page/20 px-3 py-2.5">
          <div className="text-[9px] font-semibold uppercase tracking-wider text-text-muted">Lineup coverage</div>
          <div className="mt-1 text-lg font-bold tabular-nums text-text-primary">{starters.length}<span className="ml-1 text-xs font-medium text-text-muted">starts</span></div>
          <p className="mt-1 text-[10px] leading-relaxed text-text-muted">Recommended starters across {leagues.size} connected league{leagues.size === 1 ? '' : 's'}, using each league&rsquo;s scoring.</p>
        </div>
        <div className="grid gap-2 sm:grid-cols-2 xl:grid-cols-3">
          {priorities.map((item, index) => (
            <a key={`${item.kind}-${index}`} href={item.href}
              className="group rounded-xl border border-border-light bg-bg-card/25 px-3 py-2.5 transition-colors hover:border-border hover:bg-bg-card-hover focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
              <div className="flex items-center justify-between gap-2"><span className={`text-[9px] font-semibold uppercase tracking-wider ${item.tone}`}>{item.kind}</span><span className={`text-[10px] font-semibold tabular-nums ${item.tone}`}>{item.value}</span></div>
              <div className="mt-1 truncate text-xs font-semibold text-text-primary group-hover:text-white">{item.title}</div>
              <div className="mt-0.5 truncate text-[10px] text-text-muted">{item.detail}</div>
            </a>
          ))}
          {!priorities.length && <div className="rounded-xl border border-border-light bg-bg-card/25 px-3 py-2.5 text-[11px] text-text-muted">No wire or stream recommendation is available yet.</div>}
        </div>
      </div>
    </section>
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
  const games = arr(d.games);
  const gameResults = arr(d.game_results);
  const resultByGame = new Map(gameResults.map((g: any) => [
    `${g.away_team} @ ${g.home_team}`, g,
  ]));
  // Settled bets, ordered by EV — the same ranking the pre-week Top Bets card
  // uses, so a pick can be followed from recommendation through to result.
  const bets: any[] = (Array.isArray(d.bets) ? d.bets : d.bets ? [d.bets] : [])
    .slice().sort((x: any, y: any) => (y.ev ?? 0) - (x.ev ?? 0));
  const fmtU = (u: number) => `${Number.isInteger(u) ? u : u.toFixed(1)}u`;
  const RES: Record<string, string> = { WIN: '#3fb950', LOSS: '#f85149', PUSH: '#8b949e' };
  const atsScored = games.filter((g: any) => g.cover_correct != null);
  const atsWins = atsScored.filter((g: any) => Number(g.cover_correct) === 1).length;
  const atsPushes = games.length - atsScored.length;
  const mlScored = games.filter((g: any) => g.ml_correct != null);
  const mlWins = mlScored.filter((g: any) => Number(g.ml_correct) === 1).length;
  const withinThree = games.filter((g: any) => Number(g.abs_err) <= 3).length;
  const withinSeven = games.filter((g: any) => Number(g.abs_err) <= 7).length;
  const marketBrierRows = games.filter((g: any) => g.vegas_win_pct != null && g.actual_margin != null);
  const marketBrier = marketBrierRows.length
    ? marketBrierRows.reduce((sum: number, g: any) => {
        const homeWin = Number(g.actual_margin) > 0 ? 1 : 0;
        return sum + Math.pow(Number(g.vegas_win_pct) - homeWin, 2);
      }, 0) / marketBrierRows.length
    : null;
  const brierDelta = marketBrier != null && m.brier != null ? marketBrier - Number(m.brier) : null;
  const strongEdges = games.filter((g: any) => Math.abs(Number(g.edge_pct ?? 0)) >= 0.05);
  const strongMl = strongEdges.filter((g: any) => g.ml_correct != null);
  const strongAts = strongEdges.filter((g: any) => g.cover_correct != null);
  const closest = [...games].sort((a: any, z: any) => Number(a.abs_err) - Number(z.abs_err)).slice(0, 3);
  const misses = [...games].sort((a: any, z: any) => Number(z.abs_err) - Number(a.abs_err)).slice(0, 3);
  const marginCall = (game: string, margin: any) => {
    const [away, home] = String(game).split(' @ ');
    const value = Number(margin);
    if (!Number.isFinite(value)) return '–';
    if (value === 0) return 'Pick’em';
    return `${value > 0 ? home : away} by ${num(Math.abs(value), 1)}`;
  };
  const finalScore = (game: string) => {
    const r: any = resultByGame.get(game);
    return r ? `${r.away_team} ${r.away_points} · ${r.home_team} ${r.home_points}` : null;
  };
  const pickLabel = (pick: any, line?: any) => {
    if (!pick) return '–';
    if (line == null) return String(pick);
    const value = Number(line);
    return `${pick} ${value > 0 ? '+' : ''}${num(value, 1)}`;
  };

  return (
    <Section id="model_scorecard" title="Model Scorecard" accent={SECTION_ACCENT.model_scorecard} subtitle="final performance, market benchmark, and every settled call">
      <div className="grid grid-cols-3 sm:grid-cols-6 gap-2">
        <StatTile label="Final games" value={m.games ?? '–'} sub="complete slate" />
        <StatTile label="Margin error" value={`${num(m.mae, 2)} pts`} sub="average absolute miss" />
        <StatTile label="Against spread" value={`${num((m.cover_acc ?? 0) * 100, 0)}%`}
          sub={`${atsWins}-${atsScored.length - atsWins}${atsPushes ? `-${atsPushes}` : ''}`} accent={m.cover_acc >= 0.5 ? '#3fb950' : '#f85149'} />
        <StatTile label="Moneyline" value={`${num((m.ml_acc ?? 0) * 100, 0)}%`}
          sub={`${mlWins}-${mlScored.length - mlWins}`} accent={m.ml_acc >= 0.6 ? '#3fb950' : undefined} />
        <StatTile label="Calibration" value={num(m.brier, 3)}
          sub={marketBrier == null ? 'Brier · lower is better' : `market ${num(marketBrier, 3)} · lower wins`}
          accent={brierDelta != null && brierDelta > 0 ? '#3fb950' : '#d29922'} />
        {(b.n_bets ?? 0) > 0 ? (
          <StatTile label="Settled P&L" value={`${(b.pnl ?? 0) >= 0 ? '+' : '−'}$${Math.abs(b.pnl ?? 0)}`}
            accent={(b.pnl ?? 0) >= 0 ? '#3fb950' : '#f85149'}
            sub={`${b.n_bets} bets · ${b.wins ?? 0}-${b.losses ?? 0}${b.pushes ? `-${b.pushes}` : ''}`} />
        ) : (
          <StatTile label="Settled bets" value="No bets" sub="none cleared the EV threshold" />
        )}
      </div>

      <div className="grid gap-3 lg:grid-cols-[0.9fr_1.1fr]">
        <div className="rounded-xl border border-border-light bg-bg-card/30 p-3">
          <div className="text-[10px] font-semibold uppercase tracking-wider text-text-muted">Week in context</div>
          <div className="mt-2 divide-y divide-border-light/60">
            <div className="flex items-center justify-between gap-3 py-2 first:pt-0">
              <div><div className="text-xs font-medium text-text-primary">Margin precision</div><div className="text-[10px] text-text-muted">How often the score gap landed near the call</div></div>
              <div className="shrink-0 text-right text-xs font-semibold tabular-nums text-text-primary">{withinThree}/{games.length} <span className="font-normal text-text-muted">within 3</span><div className="text-[10px] font-normal text-text-muted">{withinSeven}/{games.length} within 7</div></div>
            </div>
            <div className="flex items-center justify-between gap-3 py-2">
              <div><div className="text-xs font-medium text-text-primary">Strongest probability edges</div><div className="text-[10px] text-text-muted">Games at least five points away from market</div></div>
              <div className="shrink-0 text-right text-xs font-semibold tabular-nums text-text-primary">{strongEdges.length} games<div className="text-[10px] font-normal text-text-muted">ML {strongMl.filter((g: any) => Number(g.ml_correct) === 1).length}-{strongMl.filter((g: any) => Number(g.ml_correct) === 0).length} · ATS {strongAts.filter((g: any) => Number(g.cover_correct) === 1).length}-{strongAts.filter((g: any) => Number(g.cover_correct) === 0).length}</div></div>
            </div>
            <div className="flex items-center justify-between gap-3 py-2 last:pb-0">
              <div><div className="text-xs font-medium text-text-primary">Probability calibration</div><div className="text-[10px] text-text-muted">Brier score compared with the closing market</div></div>
              <div className="shrink-0 text-right text-xs font-semibold tabular-nums" style={{ color: brierDelta != null && brierDelta > 0 ? '#3fb950' : '#d29922' }}>{brierDelta == null ? '–' : `${brierDelta > 0 ? 'Model better by ' : 'Market better by '}${num(Math.abs(brierDelta), 3)}`}</div>
            </div>
          </div>
        </div>

        <div className="grid gap-3 sm:grid-cols-2">
          <div className="rounded-xl border border-border-light bg-bg-card/30 p-3">
            <div className="text-[10px] font-semibold uppercase tracking-wider text-win">Closest margin calls</div>
            <div className="mt-2 divide-y divide-border-light/60">
              {closest.map((r: any) => (
                <div key={r.game} className="flex items-center justify-between gap-3 py-1.5 first:pt-0 last:pb-0">
                  <div className="min-w-0"><div className="truncate text-xs font-medium text-text-primary">{r.game}</div><div className="truncate text-[10px] text-text-muted">{marginCall(r.game, r.pred_margin)} → {marginCall(r.game, r.actual_margin)}</div></div>
                  <span className="shrink-0 text-xs font-semibold tabular-nums text-win">{num(r.abs_err, 1)} pts</span>
                </div>
              ))}
            </div>
          </div>
          <div className="rounded-xl border border-border-light bg-bg-card/30 p-3">
            <div className="text-[10px] font-semibold uppercase tracking-wider text-loss">Biggest margin misses</div>
            <div className="mt-2 divide-y divide-border-light/60">
              {misses.map((r: any) => (
                <div key={r.game} className="flex items-center justify-between gap-3 py-1.5 first:pt-0 last:pb-0">
                  <div className="min-w-0"><div className="truncate text-xs font-medium text-text-primary">{r.game}</div><div className="truncate text-[10px] text-text-muted">{marginCall(r.game, r.pred_margin)} → {marginCall(r.game, r.actual_margin)}</div></div>
                  <span className="shrink-0 text-xs font-semibold tabular-nums text-loss">{num(r.abs_err, 1)} pts</span>
                </div>
              ))}
            </div>
          </div>
        </div>
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
              <th className="py-1.5 pr-3 font-medium text-left">Game · final</th>
              <th className="py-1.5 pr-3 font-medium text-left">Model margin</th>
              <th className="py-1.5 pr-3 font-medium text-left">Final margin</th>
              <th className="py-1.5 pr-3 font-medium text-right">Miss</th>
              <th className="py-1.5 pr-3 font-medium text-right" style={{ color: '#58a6ff' }}>Win prob</th>
              <th className="py-1.5 pr-3 font-medium text-right">Edge</th>
              <th className="py-1.5 pr-3 font-medium text-right">ATS</th>
              <th className="py-1.5 pr-3 font-medium text-right">ML</th>
            </tr>
          </thead>
          <tbody>
            {games.map((r: any, i: number) => {
              const e = r.edge_pct;
              const result: any = resultByGame.get(r.game);
              return (
                <tr key={i} className="border-b border-border-light/50 hover:bg-bg-card-hover">
                  <td className="py-2 pr-3 text-text-secondary whitespace-nowrap"><span className="font-medium text-text-primary">{r.game}</span>{finalScore(r.game) && <span className="block text-[10px] tabular-nums text-text-muted">{finalScore(r.game)}</span>}</td>
                  <td className="py-2 pr-3 tabular-nums text-text-secondary whitespace-nowrap">{marginCall(r.game, r.pred_margin)}</td>
                  <td className="py-2 pr-3 tabular-nums font-medium text-text-primary whitespace-nowrap">{marginCall(r.game, r.actual_margin)}</td>
                  <td className="py-2 pr-3 tabular-nums text-right text-text-muted">{num(r.abs_err, 1)}</td>
                  <td className="py-2 pr-3 tabular-nums text-right font-medium whitespace-nowrap" style={{ color: '#58a6ff' }}>
                    {r.pred_win_pct != null ? `${Math.round(r.pred_win_pct * 100)}%` : '–'}
                    <span className="block text-[10px] font-normal text-text-muted">mkt {r.vegas_win_pct != null ? `${Math.round(r.vegas_win_pct * 100)}%` : '–'}</span>
                  </td>
                  <td className="py-2 pr-3 tabular-nums text-right"
                    style={{ color: e == null ? 'var(--color-text-muted)' : Math.abs(e) >= 0.05 ? '#d29922' : 'var(--color-text-muted)' }}>
                    {e == null ? '–' : `${e > 0 ? '+' : ''}${Math.round(e * 100)} pts`}
                  </td>
                  <td className="py-2 pr-3 text-right whitespace-nowrap">
                    <span className="mr-1 text-[10px] tabular-nums text-text-muted">{pickLabel(result?.ats_pick, result?.ats_line)}</span>{mark(r.cover_correct)}
                  </td>
                  <td className="py-2 pr-3 text-right whitespace-nowrap">
                    <span className="mr-1 text-[10px] text-text-muted">{pickLabel(result?.ml_pick)}</span>{mark(r.ml_correct)}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
        <p className="text-[10px] text-text-muted mt-1.5">
          Win probability is for the <span className="text-text-secondary">home</span> team (second listed); “mkt” is the de-vigged closing probability.
          Edge is model minus market. ATS records exclude pushes.
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

function CalibrationRow({ row, maxPoints }: { row: any; maxPoints: number }) {
  const projected = Number(row.projected ?? 0);
  const actual = Number(row.actual ?? 0);
  const p = clamp((projected / maxPoints) * 100);
  const a = clamp((actual / maxPoints) * 100);
  const left = Math.min(p, a);
  const width = Math.abs(p - a);
  const bias = Number(row.bias ?? actual - projected);
  return (
    <div className="grid grid-cols-[3.3rem_minmax(0,1fr)_4.4rem] items-center gap-3 py-2">
      <div>
        <div className="text-[11px] font-semibold tabular-nums text-text-primary">{row.bucket}</div>
        <div className="text-[9px] tabular-nums text-text-muted">n={row.n}</div>
      </div>
      <div className="relative h-6">
        <div className="absolute inset-x-0 top-[11px] h-px bg-border-light" />
        <div className="absolute top-[10px] h-[3px] rounded-full bg-text-muted/50" style={{ left: `${left}%`, width: `${Math.max(width, 0.8)}%` }} />
        <div className="absolute top-[7px] h-2.5 w-2.5 -translate-x-1/2 rounded-full border-2 border-bg-card bg-accent" style={{ left: `${p}%` }} title={`Projected ${num(projected, 1)}`} />
        <div className="absolute top-[7px] h-2.5 w-2.5 -translate-x-1/2 rounded-full border-2 border-bg-card bg-win" style={{ left: `${a}%` }} title={`Actual ${num(actual, 1)}`} />
      </div>
      <div className={`text-right text-[11px] font-semibold tabular-nums ${Math.abs(bias) <= 1 ? 'text-text-secondary' : bias > 0 ? 'text-win' : 'text-loss'}`}>
        {bias > 0 ? '+' : ''}{num(bias, 1)}<span className="ml-1 text-[9px] font-normal text-text-muted">pts</span>
      </div>
    </div>
  );
}

function WeeklyAccuracy({ d }: { d: any }) {
  const cc = obj(d.class_counts);
  const rows = arr(d.metrics);
  const all = rows.find((r: any) => r.position === 'ALL') ?? {};
  const cohort = obj(d.cohort);
  const calibration = arr(d.calibration).filter((r: any) => Number(r.n ?? 0) > 0);
  const evaluated = Number(cohort.evaluated ?? all.n ?? 0);
  const projected = Number(cohort.projected ?? evaluated);
  const excluded = Number(cohort.no_opportunity ?? Math.max(0, projected - evaluated));
  const activationMisses = Number(cohort.high_confidence_no_opportunity ?? 0);
  const bias = Number(all.bias ?? 0);
  const rmse = Number(all.rmse ?? 0);
  // ECR baseline (season-long consensus scored on this week), keyed by position.
  const ecrRows: any[] = Array.isArray(d.ecr) ? d.ecr : d.ecr ? [d.ecr] : [];
  const ecrBy: Record<string, any> = Object.fromEntries(ecrRows.map((r: any) => [r.position, r]));
  const ecrAll = ecrBy['ALL'];
  const modelWins = ecrAll && ecrAll.model >= ecrAll.ecr;
  const ecrKind = d.ecr_kind === 'weekly' ? 'weekly' : 'season';
  const ecrDelta = ecrAll ? Number(ecrAll.model) - Number(ecrAll.ecr) : null;
  const ceilingGap = Number(all.ceiling_cov ?? 0) - 0.9;
  const floorGap = Number(all.floor_cov ?? 0) - 0.9;
  const worstPosition = rows.filter((r: any) => r.position !== 'ALL')
    .sort((a: any, b: any) => Number(b.mae ?? 0) - Number(a.mae ?? 0))[0];
  const maxPoints = Math.max(25, ...calibration.flatMap((r: any) => [Number(r.projected ?? 0), Number(r.actual ?? 0)])) * 1.08;
  const intervalHealthy = Math.abs(ceilingGap) <= 0.05 && Math.abs(floorGap) <= 0.05;
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
    <Section id="weekly_accuracy" title="Scoring Model Lab" accent={SECTION_ACCENT.weekly_accuracy}
      subtitle={`Week ${d.week} postmortem · half-PPR · point, rank, and interval calibration`}
      right={<span className="rounded-full border border-[#d29922]/40 bg-[#d29922]/10 px-2 py-1 text-[9px] font-bold uppercase tracking-wider text-[#d29922]">Completed audit</span>}>
      <div className="rounded-xl border border-border-light bg-bg-page/30 p-3 sm:p-4">
        <div className="flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between">
          <div>
            <div className="text-[9px] font-bold uppercase tracking-[0.16em] text-[#d29922]">Evaluation cohort</div>
            <div className="mt-1 text-xl font-semibold tracking-tight text-text-primary">{evaluated} opportunity-bearing players</div>
            <p className="mt-1 max-w-2xl text-[11px] leading-relaxed text-text-muted">
              {excluded} of {projected} projections were separated into the participation audit because the player logged no pass attempt, carry, target, or fantasy point. They no longer distort scoring error.
            </p>
          </div>
          <div className="flex shrink-0 gap-2 text-center">
            <div className="min-w-[4.8rem] rounded-lg border border-border-light bg-bg-card/60 px-2 py-2">
              <div className="text-lg font-bold tabular-nums text-text-primary">{evaluated}</div>
              <div className="text-[8px] uppercase tracking-wider text-text-muted">evaluated</div>
            </div>
            <div className="min-w-[4.8rem] rounded-lg border border-border-light bg-bg-card/60 px-2 py-2">
              <div className="text-lg font-bold tabular-nums text-[#d29922]">{excluded}</div>
              <div className="text-[8px] uppercase tracking-wider text-text-muted">participation</div>
            </div>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-2 gap-2 sm:grid-cols-3 lg:grid-cols-6">
        <StatTile label="MAE" value={num(all.mae, 2)} sub="avg absolute miss" />
        <StatTile label="RMSE" value={rmse ? num(rmse, 2) : '—'} sub="penalizes big misses" />
        <StatTile label="Mean bias" value={`${bias > 0 ? '+' : ''}${num(bias, 2)}`} sub={Math.abs(bias) <= 1 ? 'well centered' : bias > 0 ? 'model too low' : 'model too high'} accent={Math.abs(bias) <= 1 ? '#3fb950' : '#d29922'} />
        <StatTile label="Rank ρ" value={num(all.spearman, 2)} sub="ordering quality" accent="#58a6ff" />
        <StatTile label="Booms" value={cc.boom ?? 0} accent="#3fb950" sub={`${cc.met ?? 0} within range`} />
        <StatTile label="Busts" value={cc.bust ?? 0} accent="#f85149" sub="fantasy-relevant" />
      </div>

      <div className="grid gap-3 lg:grid-cols-[minmax(0,1.25fr)_minmax(17rem,.75fr)]">
        <div className="rounded-xl border border-border-light bg-bg-page/20 p-3 sm:p-4">
          <div className="flex items-start justify-between gap-3">
            <div>
              <h4 className="text-xs font-semibold text-text-primary">Point calibration by projection tier</h4>
              <p className="mt-0.5 text-[10px] text-text-muted">Blue = average projection · green = average result</p>
            </div>
            <span className="shrink-0 text-[9px] text-text-muted">bias = actual − projected</span>
          </div>
          <div className="mt-2 divide-y divide-border-light/60">
            {calibration.length ? calibration.map((r: any, i: number) => <CalibrationRow key={i} row={r} maxPoints={maxPoints} />) : (
              <div className="py-6 text-center text-xs text-text-muted">Calibration buckets will appear after the next grading run.</div>
            )}
          </div>
        </div>

        <div className="rounded-xl border border-border-light bg-bg-page/20 p-3 sm:p-4">
          <h4 className="text-xs font-semibold text-text-primary">Calibration readout</h4>
          <div className="mt-3 space-y-3">
            <div className="rounded-lg border border-border-light bg-bg-card/35 p-3">
              <div className="flex items-center justify-between gap-2">
                <span className="text-[10px] font-bold uppercase tracking-wider text-text-muted">Point level</span>
                <span className={`text-[10px] font-semibold ${Math.abs(bias) <= 1 ? 'text-win' : 'text-[#d29922]'}`}>{Math.abs(bias) <= 1 ? 'Centered' : 'Adjust'}</span>
              </div>
              <p className="mt-1 text-[11px] leading-relaxed text-text-secondary">The model finished {Math.abs(bias) <= 1 ? 'within one point of neutral bias' : `${num(Math.abs(bias), 1)} points ${bias > 0 ? 'too low' : 'too high'}`} across the scored cohort.</p>
            </div>
            <div className="rounded-lg border border-border-light bg-bg-card/35 p-3">
              <div className="flex items-center justify-between gap-2">
                <span className="text-[10px] font-bold uppercase tracking-wider text-text-muted">Intervals</span>
                <span className={`text-[10px] font-semibold ${intervalHealthy ? 'text-win' : 'text-[#d29922]'}`}>{intervalHealthy ? 'On target' : 'Watch'}</span>
              </div>
              <p className="mt-1 text-[11px] leading-relaxed text-text-secondary">Ceiling {num(Number(all.ceiling_cov ?? 0) * 100, 0)}% · floor {num(Number(all.floor_cov ?? 0) * 100, 0)}%, against a 90% target on each side.</p>
            </div>
            <div className="rounded-lg border border-border-light bg-bg-card/35 p-3">
              <div className="flex items-center justify-between gap-2">
                <span className="text-[10px] font-bold uppercase tracking-wider text-text-muted">Ranking benchmark</span>
                <span className={`text-[10px] font-semibold ${modelWins ? 'text-win' : 'text-[#d29922]'}`}>{ecrAll ? (modelWins ? 'Model leads' : 'ECR leads') : 'Pending'}</span>
              </div>
              <p className="mt-1 text-[11px] leading-relaxed text-text-secondary">{ecrAll ? `Model ρ ${num(ecrAll.model, 2)} vs ${ecrKind} ECR ${num(ecrAll.ecr, 2)} (${ecrDelta != null && ecrDelta >= 0 ? '+' : ''}${num(ecrDelta, 2)}).` : 'A common-player ECR comparison was not available.'}</p>
            </div>
          </div>
        </div>
      </div>

      <div className="grid gap-3 rounded-xl border border-border-light bg-bg-page/20 p-3 sm:grid-cols-2 sm:p-4">
        <div>
          <div className="mb-3 flex items-center justify-between"><h4 className="text-xs font-semibold text-text-primary">Prediction intervals</h4><span className="text-[9px] text-text-muted">target 90%</span></div>
          <div className="space-y-3">
            {(all.ceiling_cov != null) && <CoverageBar label="Below projected ceiling" value={all.ceiling_cov} target={0.9} />}
            {(all.floor_cov != null) && <CoverageBar label="Above projected floor" value={all.floor_cov} target={0.9} />}
          </div>
        </div>
        <div className="rounded-lg border border-border-light bg-bg-card/35 p-3">
          <div className="text-[9px] font-bold uppercase tracking-wider text-[#d29922]">Recommended next move</div>
          <div className="mt-1 text-sm font-semibold text-text-primary">{activationMisses > 0 ? 'Repair participation before tuning points' : worstPosition ? `Focus the next feature test on ${worstPosition.position}` : 'Hold parameters and collect another week'}</div>
          <p className="mt-1 text-[11px] leading-relaxed text-text-secondary">
            {activationMisses > 0
              ? `${activationMisses} players received at least 75% active probability but logged no scoring opportunity. Calibrate the availability/depth-chart stage separately before changing the scoring model.`
              : worstPosition
                ? `${worstPosition.position} had the largest MAE at ${num(worstPosition.mae, 2)}. Treat Week ${d.week} as a diagnostic signal, not enough evidence for a production retune on its own.`
                : 'One completed week is a diagnostic sample, not enough evidence for a production retune on its own.'}
          </p>
        </div>
      </div>

      <div className="grid sm:grid-cols-2 gap-x-6 gap-y-3 pt-1">
        <SurpriseList title="Largest under-projections" rows={d.beats} color="#3fb950" />
        <SurpriseList title="Largest over-projections" rows={d.misses} color="#f85149" />
      </div>
      <div className="overflow-x-auto pt-1">
        <table className="w-full text-xs">
          <thead><tr className="text-text-muted border-b border-border-light">
            {['Pos', 'N', 'MAE', 'RMSE', 'Bias', 'Model ρ', 'ECR ρ', 'Ceil', 'Floor'].map((h, i) => (
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
                  <td className="py-1 pr-3 tabular-nums text-right text-text-secondary">{num(r.rmse, 2)}</td>
                  <td className={`py-1 pr-3 tabular-nums text-right ${Math.abs(Number(r.bias ?? 0)) > 1 ? 'text-[#d29922]' : 'text-text-secondary'}`}>{Number(r.bias ?? 0) > 0 ? '+' : ''}{num(r.bias, 2)}</td>
                  <td className="py-1 pr-3 tabular-nums text-right font-medium" style={{ color: e ? (win ? '#3fb950' : 'var(--color-text-secondary)') : 'var(--color-text-secondary)' }}>{num(e ? e.model : r.spearman, 2)}</td>
                  <td className="py-1 pr-3 tabular-nums text-right" style={{ color: e && !win ? '#d29922' : 'var(--color-text-muted)' }}>{e ? num(e.ecr, 2) : '–'}</td>
                  <td className="py-1 pr-3 tabular-nums text-right text-text-secondary">{num((r.ceiling_cov ?? 0) * 100, 0)}%</td>
                  <td className="py-1 pr-3 tabular-nums text-right text-text-secondary">{num((r.floor_cov ?? 0) * 100, 0)}%</td>
                </tr>
              );
            })}
          </tbody>
        </table>
        <div className="mt-2 text-[10px] leading-relaxed text-text-muted">ρ = Spearman rank accuracy vs actual half-PPR. Point metrics use the opportunity-bearing cohort; participation misses remain in the cohort audit instead of being counted as zero-point scoring misses. {ecrAll && `ECR is the ${ecrKind === 'weekly' ? 'pre-kickoff weekly FantasyPros consensus' : 'season-long FantasyPros consensus proxy'}.`}</div>
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

// One week inside a multi-week DST run. Each cell has the same width, so a
// schedule reads as a calm left-to-right strip instead of a different-sized set
// of chips for every defense.
function RunWeek({ w, thr }: { w: any; thr?: number | null }) {
  const base = 'dst-run-cell flex min-w-0 flex-col rounded-lg border px-1.5 py-1 text-left text-[10px] leading-tight';
  if (w.bye)
    return <span className={`${base} border-dashed border-border-light bg-transparent text-text-muted`}
                 title={`Week ${w.week} — bye`}>
             <span className="text-[8px] uppercase tracking-wide opacity-60">W{w.week}</span><span className="mt-0.5 font-semibold">Bye</span>
           </span>;
  if (w.opp_implied == null)
    return <span className={`${base} border-border-light bg-bg-card-hover text-text-muted`}
                 title={`Week ${w.week} — no line posted`}>
             <span className="text-[8px] uppercase tracking-wide opacity-60">W{w.week}</span><span className="mt-0.5 font-semibold">{w.is_home ? 'vs ' : '@ '}{w.opp}</span>
           </span>;
  const good = thr != null && w.opp_implied <= thr;
  const bad = thr != null && w.opp_implied >= thr + 3;
  const cls = good ? 'border-win/40 bg-win/10 text-win'
            : bad ? 'border-loss/30 bg-loss/10 text-loss'
            : 'border-border-light bg-bg-card-hover text-text-secondary';
  return <span className={`${base} ${cls}`}
               title={`Week ${w.week} — ${w.is_home ? 'vs' : '@'} ${w.opp}, ${w.opp_implied} implied opponent points`}>
           <span className="text-[8px] uppercase tracking-wide opacity-60">W{w.week}</span>
           <span className="mt-0.5 font-semibold">{w.is_home ? 'vs ' : '@ '}{w.opp}</span>
           <span className="mt-0.5 tabular-nums opacity-80">{w.opp_implied}</span>
         </span>;
}

function DstStream({ d }: { d: any }) {
  const board = d.board ?? [];
  const runs = d.runs ?? null;
  const runWeeks: number[] = arr(runs?.weeks);
  const runThr: number | null = runs?.good_threshold ?? null;
  const leagues: any[] = Array.isArray(d.leagues) ? d.leagues : d.leagues ? [d.leagues] : [];
  const dstLeagues = leagues.filter(lg => lg.uses_dst !== false);
  return (
    <Section id="dst_stream" title="DST Streaming" accent={SECTION_ACCENT.dst_stream}
      subtitle="Start the available defense facing the lowest opponent implied total."
      right={<span className="text-[10px] text-text-muted">Lower opponent points = better matchup</span>}>
      {!d.has_lines ? (
        <p className="text-xs text-text-muted py-2">No betting lines posted yet for this week — check back closer to kickoff.</p>
      ) : (
        <>
          <div className="space-y-3">
            {dstLeagues.map((lg, i) => {
              const options = arr(lg.top_available);
              const primary = options[0];
              const runsForLeague = arr(lg.top_runs).slice(0, 3);
              return (
                <div key={i} className="dst-league-card rounded-xl border border-border-light bg-bg-card/30 p-3">
                  <div className="flex flex-wrap items-center justify-between gap-2 border-b border-border-light/70 pb-2.5">
                    <div>
                      <div className="text-xs font-semibold text-text-primary">{lg.label}</div>
                      <div className="mt-0.5 text-[10px] text-text-muted">{lg.n_taken} defenses rostered</div>
                    </div>
                    {lg.my_dst && <span className="inline-flex items-center gap-1.5 text-[10px] text-text-muted"><span>Your DST</span><TeamChip team={lg.my_dst} /></span>}
                  </div>

                  <div className="grid gap-3 pt-3 lg:grid-cols-[minmax(12rem,0.8fr)_minmax(0,1.2fr)]">
                    <div className="dst-primary-pick rounded-lg border border-win/30 bg-win/5 px-3 py-2.5">
                      <div className="text-[9px] font-bold uppercase tracking-[0.14em] text-win">Best available this week</div>
                      {primary ? (
                        <div className="mt-2 flex items-end justify-between gap-3">
                          <div className="flex items-center gap-2"><TeamChip team={primary.team} big /><span className="text-xs text-text-secondary">{primary.is_home ? 'vs' : '@'} {primary.opp}</span></div>
                          <div className="text-right"><div className="text-lg font-bold tabular-nums text-text-primary">{num(primary.opp_implied)}</div><div className="text-[9px] uppercase tracking-wide text-text-muted">opp pts</div></div>
                        </div>
                      ) : <div className="mt-2 text-xs text-text-muted">No available DST is currently identified.</div>}
                    </div>

                    <div className="min-w-0">
                      <div className="mb-1.5 text-[9px] font-bold uppercase tracking-[0.14em] text-text-muted">Other available options</div>
                      <div className="grid gap-1.5 sm:grid-cols-2">
                        {options.slice(1, 5).map((a: any, j: number) => (
                          <div key={j} className="flex items-center justify-between gap-2 rounded-lg border border-border-light bg-bg-page/25 px-2.5 py-2">
                            <span className="flex min-w-0 items-center gap-2"><TeamChip team={a.team} /><span className="truncate text-[11px] text-text-secondary">{a.is_home ? 'vs' : '@'} {a.opp}</span></span>
                            <span className="shrink-0 text-[11px] font-semibold tabular-nums text-text-primary">{num(a.opp_implied)}</span>
                          </div>
                        ))}
                        {options.length <= 1 && <div className="rounded-lg border border-dashed border-border-light px-2.5 py-2 text-[11px] text-text-muted">No alternate streamers found.</div>}
                      </div>
                    </div>
                  </div>

                  {runsForLeague.length > 0 && runWeeks.length > 0 && (
                    <div className="mt-3 border-t border-border-light/70 pt-3">
                      <div className="mb-2 flex flex-wrap items-baseline justify-between gap-2">
                        <div><span className="text-[10px] font-semibold text-text-secondary">Best schedules to hold</span><span className="ml-1.5 text-[10px] text-text-muted">Weeks {runWeeks[0]}–{runWeeks[runWeeks.length - 1]}</span></div>
                        <span className="text-[9px] text-text-muted">Average opponent points</span>
                      </div>
                      <div className="grid gap-2 xl:grid-cols-3">
                        {runsForLeague.map((t: any, k: number) => (
                          <div key={k} className="rounded-lg border border-border-light/80 bg-bg-page/20 p-2.5">
                            <div className="flex items-center justify-between gap-2">
                              <div className="flex items-center gap-2"><TeamChip team={t.team} />{t.team === lg.my_dst && <span className="text-[9px] font-medium text-accent">Yours</span>}</div>
                              <div className="text-right"><span className="text-sm font-bold tabular-nums text-text-primary">{num(t.avg_opp_implied)}</span><span className="ml-1 text-[9px] text-text-muted">avg</span></div>
                            </div>
                            <div className="mt-2 grid grid-cols-4 gap-1">
                              {arr(t.weeks).map((w: any, wi: number) => <RunWeek key={wi} w={w} thr={runThr} />)}
                            </div>
                            <div className="mt-1.5 text-[9px] text-text-muted">{t.n_good == null ? `${t.n_covered} priced` : `${t.n_good}/${t.n_covered} favorable`}{t.n_bye > 0 ? ` · ${t.n_bye} bye` : ''}</div>
                          </div>
                        ))}
                      </div>
                    </div>
                  )}
                </div>
              );
            })}
          </div>

          <div className="dst-matchup-board rounded-xl border border-border-light bg-bg-page/20 p-3">
            <div className="mb-2 flex items-baseline justify-between gap-2"><div><span className="text-xs font-semibold text-text-primary">Matchup board</span><span className="ml-2 text-[10px] text-text-muted">Best overall DST environments</span></div><span className="text-[10px] text-text-muted">Top {Math.min(board.length, 12)}</span></div>
            <div className="grid gap-1.5 sm:grid-cols-2 xl:grid-cols-3">
              {board.slice(0, 12).map((r: any, i: number) => (
                <div key={i} className={`grid grid-cols-[1.4rem_auto_1fr_auto] items-center gap-2 rounded-lg border px-2.5 py-2 ${i === 0 ? 'border-accent/35 bg-accent/5' : 'border-border-light/70 bg-bg-card/20'}`}>
                  <span className="text-center text-[10px] tabular-nums text-text-muted">{r.rank}</span>
                  <TeamChip team={r.team} />
                  <span className="min-w-0 truncate text-[11px] text-text-secondary">{r.is_home ? 'vs' : '@'} {r.opp}</span>
                  <span className="text-right text-[11px] font-semibold tabular-nums text-text-primary">{num(r.opp_implied)}<span className="ml-0.5 text-[9px] font-normal text-text-muted">pts</span></span>
                </div>
              ))}
            </div>
          </div>
          {runs?.note && <p className="text-[10px] leading-relaxed text-text-muted">{runs.note}</p>}
          <p className="text-[10px] leading-relaxed text-text-muted">{d.rationale}</p>
        </>
      )}
    </Section>
  );
}

const RENDERERS: Record<string, (p: { d: any; onNav?: (s: number, w: number) => void }) => React.ReactNode> = {
  games_preview: GamesPreview, games_result: GamesResult, bet_card: BetCard, polymarket_bets: PolymarketBets, kalshi_lines: KalshiLines, lineups: MyLineups, waivers: Waivers, fantasy_ranks: FantasyRanks,
  projections: Projections, model_scorecard: ModelScorecard, fantasy_recap: FantasyRecap,
  weekly_accuracy: WeeklyAccuracy, league_report: LeagueReport, dst_stream: DstStream,
};
const PRE_ORDER = ['games_preview', 'bet_card', 'polymarket_bets', 'kalshi_lines', 'lineups', 'waivers', 'dst_stream', 'projections'];
const POST_ORDER = ['games_result', 'fantasy_ranks', 'league_report', 'model_scorecard', 'fantasy_recap', 'weekly_accuracy'];

// Sections grouped by the QUESTION being asked, so the page shows one coherent
// group at a time instead of a ten-section scroll mixing model, fantasy and league.
type GroupKey = 'games' | 'lineups' | 'fantasy' | 'results' | 'lab';
const GROUPS: { key: GroupKey; label: string; accent: string; sections: string[] }[] = [
  { key: 'games',   label: 'Games & Bets', accent: '#58a6ff', sections: ['games_result', 'games_preview', 'bet_card', 'polymarket_bets', 'kalshi_lines'] },
  // Lineups get their own tab rather than sitting fifth in the fantasy stack:
  // it's the one section that is about MY teams and gets acted on every week.
  { key: 'lineups', label: 'My Lineups',   accent: '#3fb950', sections: ['lineups'] },
  { key: 'fantasy', label: 'Roster Tools', accent: '#3fb950', sections: ['fantasy_ranks', 'dst_stream', 'waivers', 'projections'] },
  { key: 'results', label: 'Results',      accent: '#f778ba', sections: ['model_scorecard', 'fantasy_recap', 'league_report'] },
  { key: 'lab',     label: 'Model Lab',    accent: '#d29922', sections: ['weekly_accuracy'] },
];

/** Sub-tab keys this view accepts in a permalink. */
export const WEEKLY_GROUP_KEYS: string[] = GROUPS.map(g => g.key);

function WeeklyCommandBar({ season, week, updated, betCount, marketCount }: {
  season: number; week: number; updated?: string | null; betCount: number; marketCount: number;
}) {
  const updatedLabel = formatFreshness(updated);
  return (
    <section className="relative overflow-hidden rounded-2xl border border-border bg-bg-card/70 px-4 py-4 sm:px-5">
      <div className="pointer-events-none absolute inset-0" style={{ background: 'radial-gradient(650px 240px at 100% 0%, rgba(88,166,255,0.15), transparent 65%), linear-gradient(125deg, rgba(255,255,255,0.025), transparent 40%)' }} />
      <div className="relative flex flex-col gap-4 lg:flex-row lg:items-end lg:justify-between">
        <div>
          <div className="flex flex-wrap items-center gap-2 text-[9px] font-bold uppercase tracking-[0.16em] text-accent">
            <span>Weekly market brief</span>
          </div>
          <h2 className="mt-1 text-2xl font-semibold tracking-tight text-text-primary">Week {week} <span className="font-normal text-text-muted">· {season}</span></h2>
          <p className="mt-1 max-w-xl text-xs leading-relaxed text-text-secondary">A decision-ready view of the model&rsquo;s strongest positions, with executable prediction-market prices alongside the sportsbook card.</p>
        </div>
        <div className="grid grid-cols-3 divide-x divide-border-light rounded-xl border border-border-light bg-bg-page/35 text-center">
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className="text-lg font-bold tabular-nums text-text-primary">{betCount || '—'}</div>
            <div className="text-[8px] uppercase tracking-wider text-text-muted">positions</div>
          </div>
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className="text-lg font-bold tabular-nums text-[#c9a7ff]">{marketCount || '—'}</div>
            <div className="text-[8px] uppercase tracking-wider text-text-muted">market edges</div>
          </div>
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className="pt-1 text-[10px] font-medium text-text-secondary">{updatedLabel ?? 'Awaiting data'}</div>
            <div className="mt-1 text-[8px] uppercase tracking-wider text-text-muted">last refresh</div>
          </div>
        </div>
      </div>
    </section>
  );
}

function ResultsCommandBar({ season, week, updated, scorecard }: {
  season: number; week: number; updated?: string | null; scorecard?: any;
}) {
  const metrics = obj(scorecard?.metrics);
  const bets = obj(scorecard?.bet_summary);
  const pnl = Number(bets.pnl ?? 0);
  const games = metrics.games ?? '—';
  const ats = metrics.cover_acc == null ? '—' : `${Math.round(Number(metrics.cover_acc) * 100)}%`;
  const ml = metrics.ml_acc == null ? '—' : `${Math.round(Number(metrics.ml_acc) * 100)}%`;
  const record = bets.n_bets == null ? 'No settled positions' : `${bets.n_bets} position${bets.n_bets === 1 ? '' : 's'} · ${bets.wins ?? 0}-${bets.losses ?? 0}${bets.pushes ? `-${bets.pushes}` : ''}`;

  return (
    <section className="weekly-results-hero relative overflow-hidden rounded-2xl border border-border bg-bg-card/70 px-4 py-4 sm:px-5">
      <div className="pointer-events-none absolute inset-0" style={{ background: 'radial-gradient(650px 240px at 100% 0%, rgba(63,185,80,0.13), transparent 65%), linear-gradient(125deg, rgba(255,255,255,0.025), transparent 40%)' }} />
      <div className="relative flex flex-col gap-4 lg:flex-row lg:items-end lg:justify-between">
        <div>
          <div className="flex flex-wrap items-center gap-2 text-[9px] font-bold uppercase tracking-[0.16em] text-win">
            <span>Weekly result review</span>
            <span className="h-1 w-1 rounded-full bg-text-muted" />
            <span className="text-text-muted">Finalized</span>
          </div>
          <h2 className="mt-1 text-2xl font-semibold tracking-tight text-text-primary">Week {week} <span className="font-normal text-text-muted">· {season}</span></h2>
          <p className="mt-1 max-w-xl text-xs leading-relaxed text-text-secondary">A completed read on model calls, market benchmarks, and the positions that settled this week.</p>
        </div>
        <div className="grid grid-cols-2 divide-x divide-y divide-border-light rounded-xl border border-border-light bg-bg-page/35 text-center sm:grid-cols-4 sm:divide-y-0">
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className="text-lg font-bold tabular-nums text-text-primary">{games}</div>
            <div className="text-[8px] uppercase tracking-wider text-text-muted">final games</div>
          </div>
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className="text-lg font-bold tabular-nums text-win">{ats}</div>
            <div className="text-[8px] uppercase tracking-wider text-text-muted">ATS calls</div>
          </div>
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className="text-lg font-bold tabular-nums text-accent">{ml}</div>
            <div className="text-[8px] uppercase tracking-wider text-text-muted">moneyline</div>
          </div>
          <div className="min-w-[5.4rem] px-3 py-2">
            <div className={`text-lg font-bold tabular-nums ${pnl > 0 ? 'text-win' : pnl < 0 ? 'text-loss' : 'text-text-primary'}`}>{bets.pnl == null ? '—' : `${pnl >= 0 ? '+' : '−'}$${Math.abs(pnl)}`}</div>
            <div className="text-[8px] uppercase tracking-wider text-text-muted">{record}</div>
          </div>
        </div>
      </div>
      {updated && <div className="relative mt-3 text-[10px] text-text-muted">Report finalized {formatFreshness(updated)}</div>}
    </section>
  );
}

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
  gamesFallback?: (markets: { polymarket: any[]; kalshi: any[]; kalshiWin: any[] }) => React.ReactNode;
  /** EV-ordered top bets built from the season JSON. Only used when the week
   *  has no bet_card of its own, which is the richer source. */
  betsFallback?: React.ReactNode;
  /** Predictor and Fantasy HQ share the same weekly data but not the same job. */
  scope?: 'predictor' | 'fantasy';
}

export function WeeklyReport({ season, week, onNavigate, gamesFallback, betsFallback, group: groupProp, onGroupChange, scope = 'predictor' }: WeeklyReportProps) {
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
  // Polymarket and Kalshi used to render as two long, near-identical reports.
  // The MarketBoard below combines them into a single ranked comparison.
  const ready = all.filter(r =>
    !(r === 'games_preview' && (played || supersededByCards)) &&
    r !== 'polymarket_bets' && r !== 'kalshi_lines' &&
    !(played && r === 'bet_card')
  );
  // bet_card lists EVERY qualifying bet; the season JSON keeps only the
  // highest-EV one per game. Prefer the card, and drop the fallback bets when it
  // exists so the page can't show 10 bets under a header that says 13.
  const showBetsFallback = !played && !ready.includes('bet_card') && !!betsFallback;

  // Groups that actually have something to show. "Games" always qualifies because
  // the fallback game cards exist for every week.
  const permitted = scope === 'fantasy' ? new Set<GroupKey>(['lineups', 'fantasy']) : new Set<GroupKey>(['games', 'results', 'lab']);
  const groups = GROUPS
    .filter(g => permitted.has(g.key))
    .map(g => ({ ...g, label: played && g.key === 'games' ? 'Games' : g.label, ready: g.sections.filter(s => ready.includes(s)) }))
    .filter(g => g.ready.length > 0 || (g.key === 'games' && gamesFallback));
  const active = groups.find(g => g.key === group) ?? groups[0];
  const betCount = arr(data.bet_card?.bets).length;
  const marketCount = arr(data.polymarket_bets?.markets).filter(m => m.qualifies).length
    + arr(data.kalshi_lines?.lines).filter(m => m.qualifies).length;

  return (
    <div className="space-y-4">
      {groups.length > 1 && (
        <SubNav items={groups.map(g => ({ key: g.key, label: g.label, accent: g.accent }))}
          value={active?.key ?? 'games'} onChange={setGroup} />
      )}

      {active?.key === 'games' && !played && (
        <WeeklyCommandBar season={season} week={week} updated={entry?.updated}
          betCount={betCount} marketCount={marketCount} />
      )}

      {active?.key === 'results' && (
        <ResultsCommandBar season={season} week={week} updated={entry?.updated}
          scorecard={data.model_scorecard} />
      )}

      {loading ? (
        <div className="space-y-4"><Skeleton h={280} /><Skeleton h={200} /></div>
      ) : (
        <div className="space-y-4">
          {/* Cross-league activity belongs ahead of week-specific roster advice:
              it answers what changed before asking what to do this week. */}
          {active?.key === 'fantasy' && <>
            <RosterActionQueue lineups={data.lineups} waivers={data.waivers} dst={data.dst_stream}
              onOpenLineups={() => setGroup('lineups')} />
            <TransactionDesk />
          </>}
          {active?.key === 'games' && showBetsFallback && betsFallback}
          {active?.ready.map(r => {
            const C = RENDERERS[r];
            if (!C) return null;
            const reportData = r === 'model_scorecard'
              ? { ...data[r], game_results: arr(data.games_result?.games) }
              : data[r];
            // Each report is isolated: a malformed field costs that card, not the page.
            return (
              <SectionBoundary key={r} name={r.replace(/_/g, ' ')}>
                <C d={{ ...reportData, _freshness: entry?.updated }} onNav={onNavigate} />
              </SectionBoundary>
            );
          })}
          {active?.key === 'games' && !played && (
            <MarketBoard polymarket={arr(data.polymarket_bets?.markets)} kalshi={arr(data.kalshi_lines?.lines)} />
          )}
          {/* canonical game cards — the standard view for any unplayed week */}
          {active?.key === 'games' && !played && gamesFallback?.({ polymarket: arr(data.polymarket_bets?.markets), kalshi: arr(data.kalshi_lines?.lines), kalshiWin: arr(data.kalshi_win_probs?.markets) })}
        </div>
      )}
    </div>
  );
}
