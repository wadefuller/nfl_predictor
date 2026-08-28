import { useMemo, useState } from 'react';
import { useMarket, useMovers } from '../../hooks/useFantasyData';
import type { MarketPlayer, Mover } from '../../types/fantasy';
import { PageHero, Skeleton, StatTile, TeamChip } from '../ui/primitives';
import { IndexBar, PosChip, Segmented, TierBadge, TrendPill, fmtValue, tierColor } from './fantasyUi';

const FORMATS = [
  ['dynasty_sf', 'Dynasty SF'],
  ['dynasty_1qb', 'Dynasty 1QB'],
  ['redraft_sf', 'Redraft SF'],
  ['redraft_ppr', 'Redraft PPR'],
  ['redraft_half', 'Redraft Half'],
  ['redraft_std', 'Redraft Std'],
] as const;

const POS_FILTERS = [
  ['ALL', 'All'], ['QB', 'QB'], ['RB', 'RB'], ['WR', 'WR'], ['TE', 'TE'], ['PICK', 'Picks'],
] as const;

const VIEWS = [['ranks', 'Rankings'], ['board', 'Tier Board'], ['movers', 'Movers']] as const;

type SortKey = 'value' | 'trend' | 'age' | 'mine' | 'delta' | 'vor';

// Where my model most disagrees with the market price. Sorting by |Δ| surfaces
// both directions at once, which is the actual question when you're on the clock.
const MY_BLUE = '#58a6ff';
// "Draftable" = either source has them inside its overall top 100. Matches the
// relevance gate the vs-ECR tab uses for its disagreement lists.
const relevant = (p: MarketPlayer) => p.overall_rank <= 100 || (p.my_overall ?? 999) <= 100;
function MyDelta({ d }: { d: number | null }) {
  if (d == null) return <span className="text-text-muted">–</span>;
  if (d === 0) return <span className="text-text-muted tabular-nums">0</span>;
  return (
    <span className="tabular-nums font-medium" style={{ color: d > 0 ? MY_BLUE : '#d29922' }}>
      {d > 0 ? `+${d}` : d}
    </span>
  );
}

export function MarketReport() {
  const [format, setFormat] = useState<(typeof FORMATS)[number][0]>('dynasty_sf');
  const [pos, setPos] = useState<(typeof POS_FILTERS)[number][0]>('ALL');
  const [view, setView] = useState<(typeof VIEWS)[number][0]>('ranks');
  const [q, setQ] = useState('');
  const [sort, setSort] = useState<SortKey>('value');

  const { data, loading } = useMarket(format);
  const { data: movers } = useMovers();

  const players = useMemo(() => {
    const all = data?.players ?? [];
    const needle = q.trim().toLowerCase();
    const filtered = all.filter(p =>
      (pos === 'ALL' ? true : p.pos === pos) &&
      (needle ? p.name.toLowerCase().includes(needle) : true)
    );
    const by: Record<SortKey, (a: MarketPlayer, b: MarketPlayer) => number> = {
      value: (a, b) => b.value - a.value,
      trend: (a, b) => (b.trend_pct ?? -999) - (a.trend_pct ?? -999),
      age: (a, b) => (a.age ?? 999) - (b.age ?? 999),
      // Unranked players sort last in both model views rather than pretending to
      // be rank 0 / delta 0 — picks and unmatched names have no model opinion.
      mine: (a, b) => (a.my_overall ?? 1e9) - (b.my_overall ?? 1e9),
      vor: (a, b) => (b.my_vor ?? -1e9) - (a.my_vor ?? -1e9),
      // Draftable players first, then by disagreement size. A raw |Δ| sort is
      // dominated by bench noise — WR60 vs WR94 is a bigger number than WR5 vs
      // WR19 and means far less, because nobody's draft turns on it.
      delta: (a, b) =>
        (relevant(b) ? 1 : 0) - (relevant(a) ? 1 : 0) ||
        Math.abs(b.my_delta ?? -1) - Math.abs(a.my_delta ?? -1),
    };
    return [...filtered].sort(by[sort]);
  }, [data, pos, q, sort]);

  const moverSet = movers?.formats.find(f => f.format === format);

  if (loading && !data) return <div className="space-y-3"><Skeleton h={120} /><Skeleton h={420} /></div>;
  if (!data) {
    return (
      <div className="rounded-2xl border border-border bg-bg-card p-6 text-center text-text-secondary">
        <p className="font-medium text-text-primary">No market data yet</p>
        <p className="text-sm mt-1">Run <code className="text-accent">Rscript export_fantasy.R</code> to populate it.</p>
      </div>
    );
  }

  const topValue = data.players[0]?.value ?? 1;
  const qbs = data.players.filter(p => p.pos === 'QB');
  const picks = data.players.filter(p => p.pos === 'PICK');

  return (
    <div className="space-y-4">
      <PageHero
        kicker="Market Index"
        title={data.label}
        badge={
          <span className="rounded-full border border-border bg-bg-card-hover px-2 py-0.5 text-[11px] text-text-secondary">
            {data.teams}-team · {data.qbs === 2 ? 'Superflex' : '1QB'} · {data.ppr} PPR
          </span>
        }
        right={<span className="text-[11px] text-text-muted">updated {data.generated}</span>}
      >
        <div className="grid grid-cols-2 sm:grid-cols-4 gap-2">
          <StatTile label="Players + picks" value={data.players.length} />
          <StatTile label="Top asset" value={data.players[0]?.name ?? '–'} sub={fmtValue(topValue)} />
          <StatTile label="QB1 value" value={qbs[0]?.name ?? '–'}
            sub={`${fmtValue(qbs[0]?.value)} · ${((qbs[0]?.value ?? 0) / topValue * 100).toFixed(0)}% of top`} />
          <StatTile label="1.01 pick" value={fmtValue(picks[0]?.value)} sub={picks[0]?.name ?? '–'} />
        </div>
      </PageHero>

      {/* filters: one row above the content */}
      <div className="flex flex-wrap items-center gap-2">
        <Segmented value={format} options={FORMATS} onChange={setFormat} />
        <Segmented value={view} options={VIEWS} onChange={setView} />
        {view !== 'movers' && <Segmented value={pos} options={POS_FILTERS} onChange={setPos} />}
        {view !== 'movers' && (
          <input
            value={q}
            onChange={e => setQ(e.target.value)}
            placeholder="Search player…"
            className="rounded-lg border border-border-light bg-bg-card/60 px-3 py-1.5 text-xs text-text-primary placeholder:text-text-muted focus:border-accent focus:outline-none"
          />
        )}
      </div>

      {view === 'ranks' && (
        <>
          <p className="text-[11px] text-text-muted -mt-1">
            <span className="text-text-secondary">My rk</span> is my season model's positional rank
            {' '}({data.ppr >= 1 ? 'full PPR' : data.ppr > 0 ? 'half PPR' : 'standard'} board);
            {' '}<span className="text-text-secondary">Δ</span> = market pos-rank − mine, so
            {' '}<span style={{ color: MY_BLUE }}>positive</span> means I'm higher on them than the market.
            {data.dynasty && (
              <> {' '}<span className="text-warn">Heads up:</span> these are dynasty prices, which pay for
              age and remaining career — my model projects the coming season only, so a young player's
              negative Δ is often the market buying future years rather than a disagreement about 2026.</>
            )}
          </p>
          <RankTable players={players} sort={sort} onSort={setSort} />
        </>
      )}
      {view === 'board' && <TierBoard players={players} />}
      {view === 'movers' && (
        <div className="grid gap-3 lg:grid-cols-2">
          <MoverPanel title="Risers" sub="30-day market gain" rows={moverSet?.risers ?? []} />
          <MoverPanel title="Fallers" sub="30-day market loss" rows={moverSet?.fallers ?? []} />
        </div>
      )}
    </div>
  );
}

function RankTable({ players, sort, onSort }:
  { players: MarketPlayer[]; sort: SortKey; onSort: (s: SortKey) => void }) {
  const SortHead = ({ k, children, className = '' }: { k: SortKey; children: React.ReactNode; className?: string }) => (
    <th className={`px-3 py-2 font-medium ${className}`}>
      <button
        onClick={() => onSort(k)}
        className={`hover:text-text-primary transition-colors ${sort === k ? 'text-text-primary' : ''}`}
      >
        {children}{sort === k ? ' ↓' : ''}
      </button>
    </th>
  );

  return (
    <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
      <div className="overflow-x-auto">
        <table className="w-full text-sm min-w-[1220px]">
          <thead className="text-[11px] uppercase tracking-wider text-text-muted border-b border-border">
            <tr className="text-left">
              <th className="px-3 py-2 font-medium w-12">#</th>
              <th className="px-3 py-2 font-medium">Player</th>
              <th className="px-3 py-2 font-medium w-16">Pos</th>
              <th className="px-3 py-2 font-medium w-20">Team</th>
              <SortHead k="age" className="w-16">Age</SortHead>
              <th className="px-3 py-2 font-medium w-16">Tier</th>
              <SortHead k="value" className="w-24 text-right">Value</SortHead>
              <th className="px-3 py-2 font-medium w-40">Index</th>
              <SortHead k="trend" className="w-24">30d</SortHead>
              <SortHead k="mine" className="w-20 text-right">My rk</SortHead>
              <SortHead k="delta" className="w-16 text-right">Δ</SortHead>
              <SortHead k="vor" className="w-20 text-right">VOR</SortHead>
              <th className="px-3 py-2 font-medium w-20 text-right">Proj</th>
            </tr>
          </thead>
          <tbody>
            {players.slice(0, 300).map((p, i) => (
              <tr key={`${p.player_id_fc}-${i}`} className="border-b border-border-light/60 hover:bg-bg-card-hover">
                <td className="px-3 py-2 tabular-nums text-text-muted">{p.overall_rank}</td>
                <td className="px-3 py-2 font-medium text-text-primary">
                  {p.name}
                  <span className="ml-2 text-[11px] text-text-muted tabular-nums">
                    {p.pos !== 'PICK' ? `${p.pos}${p.pos_rank}` : ''}
                  </span>
                </td>
                <td className="px-3 py-2"><PosChip pos={p.pos} /></td>
                <td className="px-3 py-2">{p.team ? <TeamChip team={p.team} /> : <span className="text-text-muted text-[11px]">–</span>}</td>
                <td className="px-3 py-2 tabular-nums text-text-secondary">{p.age?.toFixed(1) ?? '–'}</td>
                <td className="px-3 py-2"><TierBadge tier={p.tier} /></td>
                <td className="px-3 py-2 text-right tabular-nums font-semibold text-text-primary">{fmtValue(p.value)}</td>
                <td className="px-3 py-2"><IndexBar v={p.index} color={tierColor(p.tier)} /></td>
                <td className="px-3 py-2"><TrendPill pct={p.trend_pct} compact /></td>
                <td className="px-3 py-2 text-right tabular-nums text-text-secondary">
                  {p.my_pos_rank != null ? `${p.pos}${p.my_pos_rank}` : <span className="text-text-muted">–</span>}
                </td>
                <td className="px-3 py-2 text-right"><MyDelta d={p.my_delta} /></td>
                <td className="px-3 py-2 text-right tabular-nums font-medium text-text-primary">
                  {p.my_vor != null ? Math.round(p.my_vor) : <span className="text-text-muted font-normal">–</span>}
                </td>
                <td className="px-3 py-2 text-right tabular-nums text-text-secondary">
                  {p.my_proj != null ? Math.round(p.my_proj) : <span className="text-text-muted">–</span>}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {players.length > 300 && (
        <div className="px-3 py-2 text-[11px] text-text-muted border-t border-border-light">
          Showing top 300 of {players.length}. Narrow with the position filter or search.
        </div>
      )}
      {players.length === 0 && (
        <div className="px-3 py-8 text-center text-sm text-text-muted">No players match that filter.</div>
      )}
    </div>
  );
}

// Trade value chart. Assets are grouped into 10-point bands of the market index
// rather than the vendor's own tiers — those run one or two players deep at the
// top, so a tier-grouped board is mostly single-chip rows and gives no sense of
// what trades for what. A band is the useful unit: everything inside one is
// roughly swappable. Band is the sequential encoding (one hue, dark..light,
// monotonic lightness), and every chip prints its index, so color never carries
// the value alone.
const BAND_LO = '#0d419d';
const BAND_HI = '#79c0ff';
const bandColor = (t: number) => `color-mix(in oklab, ${BAND_HI} ${Math.round(t * 100)}%, ${BAND_LO})`;

function TierBoard({ players }: { players: MarketPlayer[] }) {
  const bands = useMemo(() => {
    const m = new Map<number, MarketPlayer[]>();
    for (const p of players) {
      // index 100 folds into the top band rather than opening an empty 11th.
      const b = Math.min(9, Math.floor(p.index / 10));
      if (!m.has(b)) m.set(b, []);
      m.get(b)!.push(p);
    }
    return [...m.entries()].sort((a, b) => b[0] - a[0]);
  }, [players]);

  if (players.length === 0) {
    return <div className="rounded-2xl border border-border bg-bg-card px-3 py-8 text-center text-sm text-text-muted">No players match that filter.</div>;
  }

  return (
    <div className="space-y-3">
      <div className="flex items-center gap-3 flex-wrap rounded-xl border border-border-light bg-bg-card/50 px-3 py-2">
        <span className="text-[10px] uppercase tracking-wider text-text-muted">Market index</span>
        <span className="flex items-center gap-0.5">
          {Array.from({ length: 10 }, (_, i) => (
            <span key={i} className="h-2.5 w-5 first:rounded-l-sm last:rounded-r-sm"
              style={{ background: bandColor(i / 9) }} />
          ))}
        </span>
        <span className="text-[11px] text-text-muted">0 → 100</span>
        <span className="text-[11px] text-text-muted ml-auto">10-point bands · the number on each chip is its index</span>
      </div>

      {bands.map(([band, group]) => (
        <div key={band} className="rounded-2xl border border-border bg-bg-card p-3">
          <div className="flex items-baseline gap-2 mb-2">
            <span className="inline-block rounded px-1.5 py-0.5 text-[10px] font-semibold tabular-nums"
              style={{
                color: bandColor(band / 9),
                backgroundColor: `color-mix(in oklab, ${bandColor(band / 9)} 16%, transparent)`,
                boxShadow: `inset 0 0 0 1px color-mix(in oklab, ${bandColor(band / 9)} 40%, transparent)`,
              }}>
              {band * 10}–{band === 9 ? 100 : band * 10 + 10}
            </span>
            <span className="text-xs text-text-muted">
              {group.length} asset{group.length !== 1 ? 's' : ''} · {fmtValue(group[group.length - 1].value)}–{fmtValue(group[0].value)}
            </span>
          </div>
          <div className="flex flex-wrap gap-1.5">
            {group.map((p, i) => {
              const c = bandColor(band / 9);
              return (
                <span
                  key={`${p.player_id_fc}-${i}`}
                  title={`${p.name} · ${p.pos}${p.pos === 'PICK' ? '' : p.pos_rank} · value ${fmtValue(p.value)} · index ${p.index.toFixed(1)}${p.trend_pct !== null ? ` · 30d ${p.trend_pct > 0 ? '+' : ''}${p.trend_pct}%` : ''}`}
                  className="inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-[11px] font-medium text-text-primary cursor-default"
                  style={{
                    backgroundColor: `color-mix(in oklab, ${c} 18%, transparent)`,
                    boxShadow: `inset 0 0 0 1px color-mix(in oklab, ${c} 45%, transparent)`,
                  }}
                >
                  {p.name}
                  <span className="tabular-nums text-text-muted">{p.index.toFixed(0)}</span>
                </span>
              );
            })}
          </div>
        </div>
      ))}
    </div>
  );
}

function MoverPanel({ title, sub, rows }: { title: string; sub: string; rows: Mover[] }) {
  return (
    <div className="rounded-2xl border border-border bg-bg-card overflow-hidden">
      <div className="px-3 py-2.5 border-b border-border">
        <div className="text-sm font-semibold text-text-primary">{title}</div>
        <div className="text-[11px] text-text-muted">{sub}</div>
      </div>
      {rows.length === 0 ? (
        <div className="px-3 py-8 text-center text-sm text-text-muted">No movement data.</div>
      ) : (
        <div className="overflow-x-auto">
          <table className="w-full text-sm min-w-[460px]">
          <tbody>
            {rows.map((m, i) => (
              <tr key={i} className="border-b border-border-light/60 last:border-0 hover:bg-bg-card-hover">
                <td className="px-3 py-2 font-medium text-text-primary">{m.name}</td>
                <td className="px-3 py-2 w-14"><PosChip pos={m.pos} /></td>
                <td className="px-3 py-2 w-16">{m.team ? <TeamChip team={m.team} /> : null}</td>
                <td className="px-3 py-2 w-20 text-right tabular-nums text-text-secondary">{fmtValue(m.value)}</td>
                <td className="px-3 py-2 w-24 text-right"><TrendPill pct={m.trend_pct} compact /></td>
              </tr>
            ))}
          </tbody>
          </table>
        </div>
      )}
    </div>
  );
}
