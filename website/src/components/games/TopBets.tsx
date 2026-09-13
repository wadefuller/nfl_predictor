import { useState } from 'react';
import type { Game } from '../../types';

/** One recommended position, normalized away from whichever source produced it. */
export interface TopBetRow {
  key: string; away: string; home: string;
  side: string; type: 'ml' | 'spread';
  odds: number; ev: number; stake: number; units?: number; toWin: number;
  result?: string | null; pnl?: number | null;
}

interface TopBetsProps {
  /** Season-JSON games carrying an attached `bet` (one per game, highest EV). */
  games?: Game[];
  /** Pre-normalized rows — used for a week that ships its own bet_card, which
   *  lists EVERY qualifying bet rather than one per game. Wins over `games`. */
  bets?: TopBetRow[];
  /** bankroll the stakes were sized against, for the explainer */
  bankroll?: number;
  unitSize?: number;
  /** How many rows to list. The season-JSON fallback stays at 8; a real bet card
   *  shows all of them, since it IS the week's full slate of positions. */
  limit?: number;
  subtitle?: string;
  /** Timestamp from the report run that produced these odds/model positions. */
  freshness?: string | null;
}

const BET = '#d29922';
const WIN = '#3fb950';
const LOSS = '#f85149';

const fmtOdds = (o: number) => (o > 0 ? `+${o}` : `${o}`);
/** 3 → "3u", 2.5 → "2.5u" */
const fmtUnits = (u: number) => `${Number.isInteger(u) ? u : u.toFixed(1)}u`;
const decimalOdds = (o: number) => o > 0 ? 1 + o / 100 : 1 + 100 / Math.abs(o);
const pct = (p: number) => `${Math.round(p * 100)}%`;

/**
 * The week's recommended positions, ranked by expected value — the single view
 * that answers "where is the model actually telling me to put money this week".
 * Reads the `bet` already attached to each game by export_json.R, so it's the
 * same engine (and same numbers) as the per-game cards and the season curve.
 */
export function TopBets({ games, bets: betRows, bankroll = 1000, unitSize = 10,
                         limit = 8, subtitle }: TopBetsProps) {
  const [showAll, setShowAll] = useState(false);
  const rows: TopBetRow[] = (betRows ?? (games ?? [])
    .filter(g => g.bet)
    .map(g => ({
      key: g.gameId, away: g.awayTeam, home: g.homeTeam,
      side: g.bet!.side, type: g.bet!.type as 'ml' | 'spread',
      odds: g.bet!.odds, ev: g.bet!.ev, stake: g.bet!.stake,
      units: g.bet!.units, toWin: g.bet!.toWin,
      result: g.bet!.result, pnl: g.bet!.pnl,
    })))
    .slice()
    .sort((a, b) => b.ev - a.ev);

  if (rows.length === 0) return null;

  const settled = rows.filter(x => x.result);
  const staked = rows.reduce((s, x) => s + x.stake, 0);
  const pnl = settled.reduce((s, x) => s + (x.pnl ?? 0), 0);
  const totalUnits = rows.reduce((s, x) => s + (x.units ?? x.stake / unitSize), 0);
  const lead = rows[0];
  const leadDecimal = decimalOdds(lead.odds);
  const leadMarketProbability = 1 / leadDecimal;
  const leadModelProbability = Math.min(1, Math.max(0, (1 + lead.ev) / leadDecimal));
  const leadEdge = (leadModelProbability - leadMarketProbability) * 100;
  const shownRows = showAll ? rows.slice(0, limit) : rows.slice(0, Math.min(3, limit));
  const hiddenCount = Math.max(0, Math.min(rows.length, limit) - shownRows.length);

  return (
    <section className="data-surface rounded-2xl border border-border bg-bg-card/60 overflow-hidden"
      style={{ boxShadow: `inset 3px 0 0 ${BET}` }}>
      <div className="data-surface__header relative overflow-hidden border-b border-border-light px-3 py-3 sm:px-4">
        <div className="pointer-events-none absolute inset-0 opacity-70" style={{ background: 'radial-gradient(460px 180px at 90% 0%, rgba(210,153,34,0.13), transparent 72%)' }} />
        <div className="relative grid gap-3 lg:grid-cols-[minmax(0,1.45fr)_repeat(3,minmax(0,0.7fr))] lg:items-end">
          <div>
            <div className="flex flex-wrap items-center gap-2">
              <span className="rounded-full border border-[#d29922]/40 bg-[#d29922]/10 px-2 py-0.5 text-[9px] font-bold uppercase tracking-[0.14em] text-[#f0b44d]">Highest edge</span>
            </div>
            <h3 className="mt-2 text-base font-semibold tracking-tight text-text-primary sm:text-lg">
              {lead.side} <span className="font-normal text-text-secondary">{lead.type === 'ml' ? 'moneyline' : 'spread'}</span>
              <span className="ml-1.5 text-sm font-medium tabular-nums text-[#f0b44d]">{fmtOdds(lead.odds)}</span>
            </h3>
            <p className="mt-0.5 text-[11px] text-text-secondary">{lead.away} @ {lead.home} · {subtitle ?? 'model-backed positions above the market break-even probability'}</p>
          </div>
          <div className="border-l border-border-light/80 pl-3 lg:pl-4">
            <div className="text-[9px] uppercase tracking-[0.12em] text-text-muted">Expected return</div>
            <div className="mt-0.5 text-xl font-bold tabular-nums text-[#f0b44d]">+{(lead.ev * 100).toFixed(1)}%</div>
            <div className="text-[10px] text-text-muted">per dollar risked</div>
          </div>
          <div className="border-l border-border-light/80 pl-3 lg:pl-4">
            <div className="text-[9px] uppercase tracking-[0.12em] text-text-muted">Model advantage</div>
            <div className="mt-0.5 text-xl font-bold tabular-nums text-text-primary">+{leadEdge.toFixed(1)}<span className="ml-0.5 text-xs font-medium text-text-muted">pts</span></div>
            <div className="text-[10px] text-text-muted">{pct(leadModelProbability)} model · {pct(leadMarketProbability)} implied</div>
          </div>
          <div className="border-l border-border-light/80 pl-3 lg:pl-4">
            <div className="text-[9px] uppercase tracking-[0.12em] text-text-muted">Portfolio risk</div>
            <div className="mt-0.5 text-xl font-bold tabular-nums" style={{ color: settled.length ? (pnl >= 0 ? WIN : LOSS) : 'var(--color-text-primary)' }}>
              {settled.length ? `${pnl >= 0 ? '+' : '−'}$${Math.abs(pnl)}` : `$${staked}`}
            </div>
            <div className="text-[10px] tabular-nums text-text-muted">{rows.length} positions · {fmtUnits(Math.round(totalUnits * 10) / 10)} of ${bankroll}</div>
          </div>
        </div>
      </div>

      <div className="hidden grid-cols-[1.25rem_minmax(0,1fr)_4.5rem_6.5rem_5rem] items-center gap-3 border-b border-border-light bg-bg-card-hover/40 px-4 py-1 text-[8px] font-medium uppercase tracking-wider text-text-muted sm:grid">
        <span>#</span><span>Pick · why it’s EV+</span><span className="text-right">EV return</span><span className="text-right">Stake</span><span className="text-right">Outcome</span>
      </div>
      <div className="divide-y divide-border-light">
        {shownRows.map((b, i) => {
          const u = b.units ?? b.stake / unitSize;
          const decimal = decimalOdds(b.odds);
          const marketProbability = 1 / decimal;
          // EV = model probability × decimal payout − 1; solve it to make the
          // model-versus-market reason visible without requiring another feed.
          const modelProbability = Math.min(1, Math.max(0, (1 + b.ev) / decimal));
          const probabilityLabel = b.type === 'ml' ? 'win' : 'cover';
          const edgePoints = (modelProbability - marketProbability) * 100;
          return (
            <div key={b.key}>
              {/* Phone layout: make every recommendation a readable mini-card instead of
                  compressing the desktop table's five columns into a 390px viewport. */}
              <div className="grid grid-cols-[1.25rem_minmax(0,1fr)] gap-x-2 px-3 py-3 sm:hidden">
                <span className="pt-0.5 text-[11px] tabular-nums text-text-muted">{i + 1}</span>
                <div className="min-w-0">
                  <div className="text-sm font-semibold text-text-primary">
                    {b.side} <span className="font-normal text-text-secondary">{b.type === 'ml' ? 'moneyline' : 'spread'}</span>
                    <span className="ml-1 tabular-nums text-text-muted">{fmtOdds(b.odds)}</span>
                  </div>
                  <div className="mt-0.5 text-[10px] text-text-muted">{b.away} @ {b.home}</div>
                  <div className="mt-1 text-[11px] leading-snug text-text-secondary">Model {pct(modelProbability)} to {probabilityLabel} vs {pct(marketProbability)} needed <span className="text-text-muted">({edgePoints >= 0 ? '+' : ''}{edgePoints.toFixed(1)} points)</span></div>
                  <div className="mt-2 grid grid-cols-3 gap-2 border-t border-border-light/60 pt-2 text-[10px] tabular-nums">
                    <div><span className="block text-[8px] uppercase tracking-wide text-text-muted">EV return</span><span className="font-semibold" style={{ color: BET }}>+{(b.ev * 100).toFixed(1)}%</span></div>
                    <div><span className="block text-[8px] uppercase tracking-wide text-text-muted">Risk</span><span className="font-semibold text-text-primary">{fmtUnits(Math.round(u * 10) / 10)} · ${b.stake}</span></div>
                    <div className="text-right"><span className="block text-[8px] uppercase tracking-wide text-text-muted">{b.result ? 'Settled P&L' : 'If it wins'}</span>{b.result ? <span className="font-semibold" style={{ color: b.result === 'win' ? WIN : LOSS }}>{(b.pnl ?? 0) >= 0 ? '+' : '−'}${Math.abs(b.pnl ?? 0)}</span> : <span className="font-semibold text-text-secondary">+${b.toWin}</span>}</div>
                  </div>
                </div>
              </div>

              <div className="hidden grid-cols-[1.25rem_minmax(0,1fr)_4.5rem_6.5rem_5rem] items-center gap-3 px-4 py-2 sm:grid">
                <span className="w-4 text-[11px] tabular-nums text-text-muted">{i + 1}</span>
                <div className="min-w-0 flex-1">
                  <div className="text-xs font-semibold text-text-primary">
                    {b.side} <span className="font-normal text-text-secondary">{b.type === 'ml' ? 'moneyline' : 'spread'}</span>
                    <span className="ml-1.5 tabular-nums text-text-muted">{fmtOdds(b.odds)}</span>
                  </div>
                  <div className="text-[10px] text-text-muted">
                    {b.away} @ {b.home} <span className="text-text-secondary">· Model {pct(modelProbability)} to {probabilityLabel} vs {pct(marketProbability)} needed ({edgePoints >= 0 ? '+' : ''}{edgePoints.toFixed(1)} pts)</span>
                  </div>
                </div>
                <span className="text-right text-xs font-semibold tabular-nums" style={{ color: BET }}>
                +{(b.ev * 100).toFixed(1)}%<span className="block text-[9px] font-normal text-text-muted">per $ risked</span>
                </span>
              <span className="text-right text-xs tabular-nums">
                <span className="font-bold text-text-primary">{fmtUnits(Math.round(u * 10) / 10)}</span>
                <span className="block text-[9px] text-text-muted">risk ${b.stake}</span>
                </span>
              <span className="text-right text-xs tabular-nums">
                {b.result ? (
                  <span className="font-bold" style={{ color: b.result === 'win' ? WIN : LOSS }}>
                    {(b.pnl ?? 0) >= 0 ? '+' : '−'}${Math.abs(b.pnl ?? 0)}<span className="block text-[9px] font-normal text-text-muted">settled P&L</span>
                  </span>
                ) : (
                  <span className="text-text-secondary">+${b.toWin}<span className="block text-[9px] text-text-muted">profit if it wins</span></span>
                )}
                </span>
              </div>
            </div>
          );
        })}
      </div>

      {hiddenCount > 0 && (
        <div className="border-t border-border-light bg-bg-card-hover/20 px-3 py-2 text-center sm:px-4">
          <button type="button" onClick={() => setShowAll(true)} className="rounded-md px-2 py-1 text-[11px] font-medium text-text-secondary transition-colors hover:bg-bg-card-hover hover:text-text-primary focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
            Show {hiddenCount} more position{hiddenCount === 1 ? '' : 's'}
          </button>
        </div>
      )}

      {/* the explainer — what a unit is and why stakes differ */}
      <div className="border-t border-border-light px-4 py-2 text-[10px] text-text-muted leading-relaxed">
        <span className="font-medium text-text-secondary">1 unit = ${unitSize}</span> (1% of a ${bankroll} bankroll).
        Stakes are fractional-Kelly: bigger edge → bigger position, capped at 5u (5%).
        <span className="font-medium text-text-secondary"> EV return</span> is the long-run expected profit per dollar risked—not the chance the bet wins.
        A play is EV+ when the model’s win/cover probability exceeds the probability implied by the listed odds; only plays above 5% EV are listed.
      </div>
    </section>
  );
}
