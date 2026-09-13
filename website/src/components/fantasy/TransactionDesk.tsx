import { useEffect, useMemo, useState } from 'react';
import { useAllLeagues } from '../../hooks/useFantasyData';
import type { LeagueData, MarketFormat, MarketPlayer, Transaction } from '../../types/fantasy';
import { dataUrl } from '../../lib/dataUrl';
import { Skeleton, TeamChip } from '../ui/primitives';
import { PosChip, Segmented, fmtValue } from './fantasyUi';

type ActivityKind = 'add' | 'drop' | 'trade' | 'other';
type LogRow = Transaction & { league: LeagueData; kind: ActivityKind; when: number };
type Recommendation = 'claim' | 'watch' | 'pass';
type DropCandidate = LogRow & { player: MarketPlayer | null; recommendation: Recommendation; reason: string };

const readableFormat = (format: string) => format
  .replace('dynasty_', 'Dynasty ')
  .replace('redraft_', 'Redraft ')
  .replace('_sf', ' SF')
  .replace('_half', ' Half PPR')
  .replace('_ppr', ' PPR')
  .replace('_std', ' Standard');

const playerKey = (name: string | null | undefined) => (name ?? '').toLowerCase().replace(/[^a-z0-9]/g, '');
const toTime = (timestamp: string) => {
  const parsed = new Date(timestamp.replace(' ', 'T'));
  return Number.isNaN(parsed.getTime()) ? 0 : parsed.getTime();
};
const dateLabel = (timestamp: string) => {
  const date = new Date(timestamp.replace(' ', 'T'));
  return Number.isNaN(date.getTime()) ? timestamp : date.toLocaleDateString([], { month: 'short', day: 'numeric' });
};

function kindOf(txn: Transaction): ActivityKind {
  const detail = (txn.type_desc ?? '').toLowerCase();
  const type = (txn.type ?? '').toLowerCase();
  if (detail.includes('drop')) return 'drop';
  if (detail.includes('add')) return 'add';
  if (type.includes('trade') || txn.trade_partner) return 'trade';
  return 'other';
}

function playerRecommendation(player: MarketPlayer | null, pos: string | null): Pick<DropCandidate, 'recommendation' | 'reason'> {
  if (!player) return { recommendation: 'watch', reason: 'No current market match — verify role and availability.' };
  const starterDepth: Record<string, number> = { QB: 18, RB: 36, WR: 48, TE: 18 };
  const depth = starterDepth[pos ?? ''] ?? 30;
  const rank = player.my_pos_rank;
  const vor = player.my_vor;
  if ((vor != null && vor >= 12) || (rank != null && rank <= depth)) {
    return { recommendation: 'claim', reason: rank != null ? `Model ${pos}${rank} · ${Math.round(player.my_proj ?? 0)} projected pts` : `+${Math.round(vor ?? 0)} model VOR` };
  }
  if ((vor != null && vor >= 0) || (rank != null && rank <= Math.round(depth * 1.45)) || player.overall_rank <= 110) {
    return { recommendation: 'watch', reason: rank != null ? `Model ${pos}${rank} · ${Math.round(player.my_proj ?? 0)} projected pts` : `Market rank ${player.overall_rank}` };
  }
  return { recommendation: 'pass', reason: rank != null ? `Outside model replacement depth (${pos}${rank})` : `Market rank ${player.overall_rank}` };
}

const recommendationStyle: Record<Recommendation, string> = {
  claim: 'border-win/40 bg-win/10 text-win',
  watch: 'border-[#d29922]/40 bg-[#d29922]/10 text-[#d29922]',
  pass: 'border-border-light bg-bg-card-hover text-text-muted',
};

const kindStyle: Record<ActivityKind, string> = {
  add: 'text-win', drop: 'text-loss', trade: 'text-accent', other: 'text-text-muted',
};

async function fetchMarket(format: string): Promise<MarketFormat | null> {
  try {
    const response = await fetch(dataUrl(`/data/fantasy/market/${format}.json`));
    return response.ok ? await response.json() as MarketFormat : null;
  } catch { return null; }
}

/** One activity feed for every league, plus an availability-aware dropped-player radar. */
export function TransactionDesk() {
  const { data: leagues, loading } = useAllLeagues();
  const [markets, setMarkets] = useState<Record<string, MarketFormat>>({});
  const [filter, setFilter] = useState<'all' | ActivityKind>('all');
  const [leagueFilter, setLeagueFilter] = useState('all');
  const [logOpen, setLogOpen] = useState(false);

  const formats = useMemo(() => [...new Set(leagues.map(l => l.market_format).filter(Boolean))], [leagues]);
  useEffect(() => {
    if (!formats.length) return;
    let cancelled = false;
    Promise.all(formats.map(async format => [format, await fetchMarket(format)] as const)).then(rows => {
      if (!cancelled) setMarkets(Object.fromEntries(rows.filter(([, market]) => market)) as Record<string, MarketFormat>);
    });
    return () => { cancelled = true; };
  }, [formats]);

  const activity = useMemo<LogRow[]>(() => leagues.flatMap(league =>
    (league.transactions ?? []).map(txn => ({ ...txn, league, kind: kindOf(txn), when: toTime(txn.timestamp) })),
  ).sort((a, b) => b.when - a.when), [leagues]);

  const marketByFormat = useMemo(() => Object.fromEntries(Object.entries(markets).map(([format, market]) => [
    format,
    new Map(market.players.map(player => [playerKey(player.name), player])),
  ])) as Record<string, Map<string, MarketPlayer>>, [markets]);

  const drops = useMemo<DropCandidate[]>(() => {
    const seen = new Set<string>();
    const rows: DropCandidate[] = [];
    for (const txn of activity) {
      if (txn.kind !== 'drop' || !txn.player_name) continue;
      const key = `${txn.league.key}:${txn.player_id ?? playerKey(txn.player_name)}`;
      if (seen.has(key)) continue;
      seen.add(key);
      const rostered = txn.league.roster.some(player =>
        (txn.player_id != null && String(player.player_id) === String(txn.player_id)) || playerKey(player.player_name) === playerKey(txn.player_name),
      );
      if (rostered) continue;
      const player = marketByFormat[txn.league.market_format]?.get(playerKey(txn.player_name)) ?? null;
      rows.push({ ...txn, player, ...playerRecommendation(player, txn.pos) });
    }
    const order: Record<Recommendation, number> = { claim: 0, watch: 1, pass: 2 };
    return rows.sort((a, b) => order[a.recommendation] - order[b.recommendation] || b.when - a.when);
  }, [activity, marketByFormat]);

  const visibleActivity = activity.filter(row =>
    (filter === 'all' || row.kind === filter) && (leagueFilter === 'all' || row.league.key === leagueFilter),
  );
  const ratedDrops = drops.filter(drop => drop.player !== null);

  if (loading) return <div className="space-y-3"><Skeleton h={180} /><Skeleton h={320} /></div>;

  return (
    <section className="report-section overflow-hidden rounded-2xl border border-border bg-bg-card/60">
      <div className="report-section__header flex flex-wrap items-start justify-between gap-3 border-b border-border-light px-4 py-3" style={{ boxShadow: 'inset 3px 0 0 #a371f7' }}>
        <div>
          <h3 className="text-sm font-semibold tracking-tight text-text-primary">Transaction Desk</h3>
          <p className="mt-0.5 text-[11px] text-text-muted">League activity in one place, plus drops that are still available to claim.</p>
        </div>
        <span className="rounded-full border border-[#a371f7]/35 bg-[#a371f7]/10 px-2 py-0.5 text-[10px] font-semibold text-[#c9a7ff]">{leagues.length} leagues tracked</span>
      </div>
      <div className="report-section__body space-y-4 p-4">
        <div className="rounded-xl border border-border-light bg-bg-page/20 p-3">
          <div className="mb-2 flex flex-wrap items-baseline justify-between gap-2">
            <div><span className="text-xs font-semibold text-text-primary">Dropped-player radar</span><span className="ml-2 text-[10px] text-text-muted">Unrostered drops with a current market or model profile.</span></div>
            <span className="text-[10px] text-text-muted">Claim = starter-depth or positive-VOR signal</span>
          </div>
          {ratedDrops.length === 0 ? <p className="py-4 text-center text-xs text-text-muted">No currently unrostered drops have a current model or market profile.</p> : (
            <div className="grid gap-2 lg:grid-cols-2">
              {ratedDrops.slice(0, 10).map((drop, index) => (
                <div key={`${drop.league.key}-${drop.player_id}-${index}`} className="grid grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-2 rounded-lg border border-border-light/80 bg-bg-card/25 px-2.5 py-2.5">
                  <span className={`rounded-full border px-2 py-0.5 text-[9px] font-bold uppercase tracking-wide ${recommendationStyle[drop.recommendation]}`}>{drop.recommendation}</span>
                  <div className="min-w-0">
                    <div className="flex min-w-0 items-center gap-1.5"><span className="truncate text-xs font-semibold text-text-primary">{drop.player_name}</span>{drop.pos && <PosChip pos={drop.pos} />}{drop.team && <TeamChip team={drop.team} />}</div>
                    <div className="mt-0.5 truncate text-[10px] text-text-muted">{drop.league.label} · {readableFormat(drop.league.market_format)} · {drop.reason}</div>
                  </div>
                  <div className="text-right"><div className="text-[10px] tabular-nums text-text-secondary">{drop.player?.value != null ? fmtValue(drop.player.value) : '—'}</div><div className="mt-0.5 text-[9px] text-text-muted">{dateLabel(drop.timestamp)}</div></div>
                </div>
              ))}
            </div>
          )}
        </div>

        <div className="overflow-hidden rounded-xl border border-border-light bg-bg-page/20">
          <button
            type="button"
            aria-expanded={logOpen}
            aria-controls="transaction-log"
            onClick={() => setLogOpen(open => !open)}
            className="flex w-full items-center justify-between gap-3 px-3 py-2.5 text-left transition-colors hover:bg-bg-card-hover focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent"
          >
            <span>
              <span className="block text-xs font-semibold text-text-primary">Transaction log</span>
              <span className="mt-0.5 block text-[10px] text-text-muted">Adds, drops, and trades across your connected leagues.</span>
            </span>
            <span className="shrink-0 text-sm leading-none text-text-muted" aria-hidden="true">{logOpen ? '−' : '+'}</span>
          </button>
          {logOpen && <div id="transaction-log" className="space-y-3 border-t border-border-light p-3">
            <div className="flex flex-wrap items-center justify-between gap-2">
              <Segmented value={filter} options={[
                ['all', 'All activity'], ['add', 'Adds'], ['drop', 'Drops'], ['trade', 'Trades'],
              ] as const} onChange={setFilter} />
              <select value={leagueFilter} onChange={e => setLeagueFilter(e.target.value)} className="rounded-lg border border-border-light bg-bg-card/60 px-2.5 py-1.5 text-xs text-text-secondary focus:border-accent focus:outline-none">
                <option value="all">All leagues</option>
                {leagues.map(league => <option key={league.key} value={league.key}>{league.label}</option>)}
              </select>
            </div>
            <div className="overflow-hidden rounded-lg border border-border-light bg-bg-card/25">
              <div className="overflow-x-auto">
                <table className="w-full min-w-[740px] text-xs">
                  <thead className="border-b border-border-light text-[10px] uppercase tracking-wider text-text-muted"><tr className="text-left"><th className="px-3 py-2 font-medium">When</th><th className="px-3 py-2 font-medium">League</th><th className="px-3 py-2 font-medium">Move</th><th className="px-3 py-2 font-medium">Player</th><th className="px-3 py-2 font-medium">Team</th><th className="px-3 py-2 font-medium text-right">Bid</th></tr></thead>
                  <tbody>
                    {visibleActivity.slice(0, 100).map((row, index) => (
                      <tr key={`${row.league.key}-${row.timestamp}-${index}`} className="border-b border-border-light/60 last:border-0 hover:bg-bg-card-hover">
                        <td className="whitespace-nowrap px-3 py-2 text-[11px] tabular-nums text-text-muted">{dateLabel(row.timestamp)}</td>
                        <td className="max-w-40 truncate px-3 py-2 text-text-secondary">{row.league.label}</td>
                        <td className="px-3 py-2"><span className={`font-semibold capitalize ${kindStyle[row.kind]}`}>{row.kind}</span></td>
                        <td className="px-3 py-2 font-medium text-text-primary">{row.player_name ?? <span className="font-normal text-text-muted">{row.comments ?? '—'}</span>}</td>
                        <td className="px-3 py-2"><span className="flex items-center gap-1.5">{row.pos && <PosChip pos={row.pos} />}{row.team && <TeamChip team={row.team} />}</span></td>
                        <td className="px-3 py-2 text-right tabular-nums text-text-secondary">{row.bbid_spent != null ? `$${row.bbid_spent}` : '—'}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
              {visibleActivity.length > 100 && <div className="border-t border-border-light px-3 py-2 text-[10px] text-text-muted">Showing the newest 100 of {visibleActivity.length} matching moves.</div>}
              {visibleActivity.length === 0 && <div className="px-3 py-8 text-center text-xs text-text-muted">No activity matches these filters.</div>}
            </div>
          </div>}
        </div>
      </div>
    </section>
  );
}
