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
export function TopBets({ games, bets: betRows, bankroll = 500, unitSize = 5,
                         limit = 8, subtitle, freshness }: TopBetsProps) {
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
  const parsedFreshness = freshness ? new Date(freshness.replace(/([+-]\d{2})(\d{2})$/, '$1:$2')) : null;
  const freshnessLabel = parsedFreshness && !Number.isNaN(parsedFreshness.getTime())
    ? parsedFreshness.toLocaleString([], { month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit' })
    : freshness;

  return (
    <section className="rounded-2xl border border-border bg-bg-card/60 overflow-hidden"
      style={{ boxShadow: `inset 3px 0 0 ${BET}` }}>
      <div className="flex flex-col gap-2 border-b border-border-light px-3 py-3 sm:flex-row sm:items-center sm:justify-between sm:px-4">
        <div>
          <h3 className="text-sm font-semibold text-text-primary tracking-tight">Top bets this week</h3>
          <div className="mt-0.5 flex flex-wrap items-center gap-1.5">
            <p className="text-[11px] text-text-muted">{subtitle ?? 'only plays where the model beats the market’s break-even probability'}</p>
            {freshnessLabel && <span title={`Report generated ${freshness}`} className="rounded-full border border-border-light bg-bg-card-hover px-1.5 py-0.5 text-[9px] font-medium tabular-nums text-text-muted">Data as of {freshnessLabel}</span>}
          </div>
        </div>
        <div className="text-right">
          <div className="text-sm font-bold tabular-nums" style={{ color: settled.length ? (pnl >= 0 ? WIN : LOSS) : BET }}>
            {settled.length
              ? `${pnl >= 0 ? '+' : '−'}$${Math.abs(pnl)}`
              : `$${staked} total risk`}
          </div>
          <div className="text-[10px] text-text-muted tabular-nums">
            {rows.length} bet{rows.length !== 1 ? 's' : ''} · {fmtUnits(Math.round(totalUnits * 10) / 10)} · ${staked} of ${bankroll} bankroll
          </div>
        </div>
      </div>

      <div className="hidden grid-cols-[1.25rem_minmax(0,1fr)_4.5rem_6.5rem_5rem] items-center gap-3 border-b border-border-light bg-bg-card-hover/40 px-4 py-1 text-[8px] font-medium uppercase tracking-wider text-text-muted sm:grid">
        <span>#</span><span>Pick · why it’s EV+</span><span className="text-right">EV return</span><span className="text-right">Stake</span><span className="text-right">Outcome</span>
      </div>
      <div className="divide-y divide-border-light">
        {rows.slice(0, limit).map((b, i) => {
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
