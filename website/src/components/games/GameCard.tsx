import type { Game } from '../../types';
import { num } from '../ui/primitives';

interface GameCardProps {
  game: Game;
  /** Mapped Polymarket YES contracts for this game, when the public feed has them. */
  polymarket?: any[];
  /** Kalshi's near-even, executable game-spread contract. */
  kalshi?: any[];
  /** Kalshi's closest currently listed outcome: team wins by 2+ points. */
  kalshiWin?: any[];
  /** Final score from the live scoreboard, for a game that has finished but whose
   *  week has not been graded yet. Ignored once `game.result` exists. */
  liveFinal?: { home: number; away: number; homeTeam: string; awayTeam: string };
}

// Color encodes SOURCE, not team. Blue is always my model, grey is always the
// market — so every comparison on the card reads the same way. (Team colors were
// pretty but carried no signal, and fought with the win/loss colors.)
// Saturation carries a second signal: the stronger the model's disagreement with
// the market, the more colour the card earns — so a scan finds the live ones.
const MODEL = '#58a6ff';
const VEGAS = '#8b949e';
const BET = '#d29922';   // gold = money on the line
const WIN = '#3fb950';
const LOSS = '#f85149';

const moneylineToProb = (ml: number) => (ml < 0 ? -ml / (-ml + 100) : 100 / (ml + 100));

/** De-vigged market win probabilities from the two moneylines. */
function vegasProbs(homeMl: number | null, awayMl: number | null) {
  if (homeMl == null || awayMl == null) return null;
  const h = moneylineToProb(homeMl), a = moneylineToProb(awayMl);
  return { home: h / (h + a), away: a / (h + a) };
}

export function GameCard({ game, polymarket = [], kalshi = [], kalshiWin = [], liveFinal }: GameCardProps) {
  const { prediction: p } = game;
  // The graded result wins when it exists; the live scoreboard only fills the gap
  // between kickoff and Tuesday's grading. `provisional` is carried through so the
  // card can say which one it is rather than presenting a scraped score with the
  // same authority as a reconciled one.
  const provisional = game.result == null && liveFinal != null;
  const result = game.result ?? (liveFinal ? {
    homePoints: liveFinal.home,
    awayPoints: liveFinal.away,
    homeResult: liveFinal.home - liveFinal.away,
    homeWin: liveFinal.home > liveFinal.away,
    winningTeam: liveFinal.home === liveFinal.away ? null
      : liveFinal.home > liveFinal.away ? liveFinal.homeTeam : liveFinal.awayTeam,
    predCorrect: null, vegasCorrect: null,
  } as any : null);
  const done = result !== null;

  const homeWin = p.predWinner === game.homeTeam ? p.predWinPct : 1 - p.predWinPct;
  const vp = vegasProbs(game.homeMoneyline, game.awayMoneyline);

  // lineDiff = model margin − market spread, both "+ = home favored". So the sign
  // tells you WHICH SIDE the model is higher on; we render that as a named lean
  // instead of a bare signed number, which nobody can interpret at a glance.
  // leanPts still drives the card accent: a card the model disagrees with the
  // market on earns colour. The named lean itself is gone — the table's delta
  // column says the same thing in place, next to the number it refers to.
  const leanPts = Math.abs(p.lineDiff);
  const notable = leanPts >= 1;

  const pct = (v: number) => `${Math.round(v * 100)}%`;
  const bet = game.bet ?? null;

  // ---- one comparison frame -------------------------------------------------
  // Every market is restated from a SINGLE side: the team the model picks. The
  // card used to quote each line from its own favourite, so a game where the
  // model liked SEA and the book liked NE read "SEA -4" against "NE -1" — two
  // numbers that cannot be compared without doing the arithmetic yourself. With
  // one reference team the column is a direct read, and the delta against the
  // model is meaningful in every row.
  const refTeam = p.predWinner;
  const refIsHome = refTeam === game.homeTeam;
  const fromRef = (homeMargin: number) => (refIsHome ? homeMargin : -homeMargin);
  const fmtLine = (m: number | null) =>
    m == null ? null : Math.abs(m) < 0.05 ? 'PK' : `${m > 0 ? '−' : '+'}${num(Math.abs(m))}`;

  const modelMargin = fromRef(p.nflPredictorLine);
  const vegasMargin = fromRef(game.spreadLine);
  const modelWin = p.predWinPct;
  const vegasWin = vp ? (refIsHome ? vp.home : vp.away) : null;

  // Kalshi lists a spread RUNG ("team by more than N"), not a book line, and its
  // win market is a 2+ margin contract rather than an exact moneyline. Both are
  // carried here but flagged, because silently equating them with a sportsbook
  // line would overstate how comparable they are.
  const kSpreadRow = kalshi.find((m: any) => m.team === refTeam) ?? kalshi[0] ?? null;
  const kSpreadMargin = kSpreadRow
    ? (kSpreadRow.team === refTeam ? Number(kSpreadRow.threshold) : -Number(kSpreadRow.threshold))
    : null;
  const kWinRow = kalshiWin.find((m: any) => m.team === refTeam) ?? null;
  const pmRow = polymarket.find((m: any) => m.bet_team === refTeam) ?? null;

  type MarketRow = {
    label: string; color?: string; isModel?: boolean;
    margin: number | null; win: number | null; note?: string; href?: string;
  };
  const marketRows: MarketRow[] = [
    { label: 'My model', color: MODEL, isModel: true, margin: modelMargin, win: modelWin },
    { label: 'Vegas', color: VEGAS, margin: vegasMargin, win: vegasWin },
  ];
  if (kSpreadRow || kWinRow) marketRows.push({
    label: 'Kalshi', color: '#f0b44d',
    margin: kSpreadMargin,
    win: kWinRow ? Number(kWinRow.ask) : null,
    note: kWinRow ? '2+ margin contract, not an exact moneyline' : undefined,
    href: kSpreadRow ? `https://kalshi.com/markets/${kSpreadRow.event_ticker}` : undefined,
  });
  if (pmRow) marketRows.push({
    label: 'Polymarket', color: '#c9a7ff', margin: null, win: Number(pmRow.ask),
    note: 'YES ask; the two sides need not sum to 100¢',
    href: `https://polymarket.com/event/${pmRow.slug}`,
  });
  // ---- where to actually place the bet -------------------------------------
  // The card used to show only the sportsbook position. All three venues price
  // the same game, so the recommendation now ranks every qualifying option and
  // names the venue. This is only meaningful because the three EVs were put on
  // one basis upstream (calibrated expected profit per dollar staked); before
  // that, Kalshi was quoted per CONTRACT and Polymarket was left uncalibrated,
  // so a naive max() picked Polymarket on scale alone.
  type Option = { venue: string; color: string; pick: string; kind: string; ev: number; price: string; href?: string };
  const options: Option[] = [];
  if (bet) options.push({
    venue: 'Sportsbook', color: BET, pick: bet.side,
    kind: bet.type === 'ml' ? 'moneyline' : 'spread',
    ev: bet.ev, price: bet.odds > 0 ? `+${bet.odds}` : `${bet.odds}`,
  });
  if (kSpreadRow && kSpreadRow.qualifies) options.push({
    venue: 'Kalshi', color: '#f0b44d', pick: `${kSpreadRow.team} −${Number(kSpreadRow.threshold).toFixed(1)}`,
    kind: 'spread contract', ev: Number(kSpreadRow.ev),
    price: `${Math.round(Number(kSpreadRow.ask) * 100)}¢`,
    href: `https://kalshi.com/markets/${kSpreadRow.event_ticker}`,
  });
  polymarket.filter((m: any) => m.qualifies).forEach((m: any) => options.push({
    venue: 'Polymarket', color: '#c9a7ff', pick: `${m.bet_team} YES`,
    kind: 'winner contract', ev: Number(m.net_ev),
    price: `${Math.round(Number(m.ask) * 100)}¢`,
    href: `https://polymarket.com/event/${m.slug}`,
  }));
  options.sort((a, b) => b.ev - a.ev);
  const bestVenue = options[0] ?? null;
  const alsoAt = options.slice(1);

  // ---- what actually happened ----------------------------------------------
  // Once a game is final the same table becomes a scorecard: the actual margin
  // is quoted from the SAME side as every prediction, so each row's error is a
  // vertical read. Keeping one layout for both states means the pregame view and
  // the post-mortem cannot drift apart, and you grade the markets against each
  // other on exactly the numbers you were shown beforehand.
  const actualMargin = done ? (refIsHome ? result!.homeResult : -result!.homeResult) : null;
  const refWon = done ? result!.winningTeam === refTeam : null;
  const push = done && result!.homeResult === 0;

  const missing = [
    !kSpreadRow && !kWinRow ? 'Kalshi' : null,
    !pmRow ? 'Polymarket' : null,
  ].filter(Boolean) as string[];

  // A card with money on it gets a gold edge; a strong lean gets a blue one.
  // Everything else stays quiet, so colour marks what's worth looking at.
  const accent = bet ? BET : notable ? MODEL : null;

  return (
    <div className="rounded-xl border bg-bg-card overflow-hidden transition-colors"
      style={{
        borderColor: accent ? `${accent}59` : 'var(--color-border)',
        boxShadow: accent ? `inset 0 2px 0 ${accent}` : undefined,
      }}>
      {/* date + time inline, so the grid doesn't need per-day section headers */}
      <div className="flex items-center justify-between px-3 py-1.5 border-b border-border-light bg-bg-page/30">
        <span className="text-[10px] uppercase tracking-wide text-text-muted">
          {game.weekday.slice(0, 3)} {new Date(game.gameday + 'T12:00:00').toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
          {!done && ` · ${game.gametime}`}
        </span>
        <div className="flex items-center gap-1.5">
          {game.divGame && <span className="text-[9px] text-text-muted border border-border-light rounded px-1 py-px">DIV</span>}
          {done && (
            <span className={`rounded-full border px-1.5 py-px text-[9px] font-bold uppercase tracking-wide ${
              result.predCorrect ? 'text-win border-win/40 bg-win/10' : 'text-loss border-loss/40 bg-loss/10'}`}>
              {result.predCorrect ? 'Hit' : 'Miss'}
            </span>
          )}
        </div>
      </div>

      <div className="px-3 py-2.5 space-y-2.5">
        {/* teams + score/implied */}
        <div className="flex items-baseline justify-between">
          <span className="text-sm font-bold text-text-primary">
            {game.awayTeam} <span className="text-text-muted font-normal">@</span> {game.homeTeam}
          </span>
          <span className="text-[10px] text-text-muted">
            {done ? 'final' : `O/U ${game.totalLine}`}
          </span>
        </div>

        {/* WIN PROBABILITY — pregame only. Once the game is final the table below
            carries both what each source predicted and whether it was right, so
            the bar would be restating the model's number a third time. */}
        {!done && (
        <>
        {/* The fast scan across 16 cards. The bar splits where
            the MODEL splits it; the dashed marker is where VEGAS splits it, so the
            gap between them is the disagreement without reading any number. The
            per-source figures live in the table below rather than being repeated
            underneath the bar. */}
        <div>
          <div className="flex items-center justify-between text-[9px] uppercase tracking-wide text-text-muted mb-1">
            <span>Win probability</span>
            {vp && (
              <span className="normal-case tracking-normal">
                <span style={{ color: MODEL }}>bar = model</span>
                <span className="mx-1 opacity-50">·</span>
                <span>dashed = Vegas</span>
              </span>
            )}
          </div>
          <div className="relative h-5 rounded-md bg-bg-card-hover overflow-hidden">
            <div className="absolute inset-y-0 left-0"
              style={{ width: `${(1 - homeWin) * 100}%`,
                       background: p.predWinner === game.awayTeam ? `${MODEL}99` : `${MODEL}1f` }} />
            <div className="absolute inset-y-0 right-0"
              style={{ width: `${homeWin * 100}%`,
                       background: p.predWinner === game.homeTeam ? `${MODEL}99` : `${MODEL}1f` }} />
            {vp && (
              <div className="absolute inset-y-0 border-l-2 border-dashed" style={{ left: `${(1 - vp.home) * 100}%`, borderColor: VEGAS }} />
            )}
            <div className="absolute inset-0 flex items-center justify-between px-2 text-[10px] font-semibold">
              <span style={{ color: p.predWinner === game.awayTeam ? '#e6edf3' : 'var(--color-text-muted)' }}>
                {game.awayTeam} {pct(1 - homeWin)}
              </span>
              <span style={{ color: p.predWinner === game.homeTeam ? '#e6edf3' : 'var(--color-text-muted)' }}>
                {pct(homeWin)} {game.homeTeam}
              </span>
            </div>
          </div>
        </div>
        </>
        )}

        {/* MARKET COMPARISON — one table replacing four stacked blocks (Vegas
            spread, model spread, model lean, and a bordered box per prediction
            market). Rows are sources, columns are the two quantities the model
            emits, and every figure is quoted from the same side, so a vertical
            read IS the comparison. Sources with nothing listed are named once in
            a footer instead of each reserving an empty bordered row — Kalshi
            spreads and Polymarket are intermittent and are usually absent. */}
        <div className="rounded-lg border border-border-light overflow-hidden">
          <div className="grid grid-cols-[1fr_3.6rem_3.4rem] gap-x-2 px-2 py-1 bg-bg-card-hover/40 text-[9px] uppercase tracking-wide text-text-muted">
            <span>Market</span>
            <span className="text-right">{refTeam} line</span>
            <span className="text-right">{done ? 'called it' : `${refTeam} win`}</span>
          </div>
          <div className="divide-y divide-border-light">
            {done && (
              <div className="grid grid-cols-[1fr_3.6rem_3.4rem] items-center gap-x-2 bg-bg-card-hover/40 px-2 py-1 text-[11px]">
                <span className="flex min-w-0 items-center gap-1.5">
                  <span className="inline-block h-1.5 w-1.5 shrink-0 rounded-full bg-text-primary" />
                  <span className="font-semibold text-text-primary">Actual</span>
                </span>
                <span className="text-right font-semibold tabular-nums text-text-primary">{fmtLine(actualMargin)}</span>
                <span className="text-right text-[10px] font-semibold"
                  style={{ color: push ? 'var(--color-text-muted)' : refWon ? WIN : LOSS }}>
                  {push ? 'tie' : refWon ? `${refTeam} won` : `${refTeam} lost`}
                </span>
                {provisional && (
                  <span className="col-span-3 text-[9px] text-text-muted"
                    title="Score read from the live scoreboard. The graded result replaces it after the week is scored.">
                    live score · not yet graded
                  </span>
                )}
              </div>
            )}
            {marketRows.map((r, i) => {
              // Pregame the delta is against the model; once final it is against
              // the result, because that is the only comparison that still matters.
              const dM = done
                ? (r.margin != null && actualMargin != null ? r.margin - actualMargin : null)
                : (!r.isModel && r.margin != null ? modelMargin - r.margin : null);
              const dW = done ? null : (!r.isModel && r.win != null ? (modelWin - r.win) * 100 : null);
              // Did this source have the right side? A row with no win figure
              // cannot be graded and stays blank rather than guessing.
              const calledIt = done && r.win != null && !push ? ((r.win >= 0.5) === refWon) : null;
              const Cell = ({ v, d, unit }: { v: string | null; d: number | null; unit: string }) => (
                <span className="text-right tabular-nums">
                  {v == null ? <span className="text-text-muted">—</span> : (
                    <>
                      <span className={r.isModel ? 'font-semibold' : ''} style={{ color: r.isModel ? MODEL : 'var(--color-text-secondary)' }}>{v}</span>
                      {d != null && Math.abs(d) >= 0.05 && (
                        <span className="ml-1 text-[9px] text-text-muted">{d > 0 ? '+' : '−'}{num(Math.abs(d))}{unit}</span>
                      )}
                    </>
                  )}
                </span>
              );
              const body = (
                <>
                  <span className="flex min-w-0 items-center gap-1.5">
                    <span className="inline-block h-1.5 w-1.5 shrink-0 rounded-full" style={{ background: r.color }} />
                    <span className={r.isModel ? 'font-medium text-text-primary' : 'text-text-secondary'}>{r.label}</span>
                    {r.note && <span className="cursor-help text-text-muted" title={r.note}>*</span>}
                  </span>
                  <Cell v={fmtLine(r.margin)} d={dM} unit="" />
                  {done ? (
                    <span className="text-right text-[10px] tabular-nums">
                      {calledIt == null ? <span className="text-text-muted">—</span> : (
                        <span style={{ color: calledIt ? WIN : LOSS }}>
                          {calledIt ? '✓' : '✗'}
                          <span className="ml-1 text-text-muted">{pct(r.win!)}</span>
                        </span>
                      )}
                    </span>
                  ) : (
                    <Cell v={r.win == null ? null : pct(r.win)} d={dW} unit="pp" />
                  )}
                </>
              );
              const cls = `grid grid-cols-[1fr_3.6rem_3.4rem] items-center gap-x-2 px-2 py-1 text-[11px] ${r.isModel ? 'bg-bg-card-hover/20' : ''}`;
              return r.href
                ? <a key={i} href={r.href} target="_blank" rel="noreferrer" className={`${cls} hover:bg-bg-card-hover/40`}>{body}</a>
                : <div key={i} className={cls}>{body}</div>;
            })}
          </div>
          {/* What the asterisks mean is stated ONCE per grid, not once per card —
              sixteen copies of the same two sentences was more clutter than the
              blocks this table replaced. The caveat still travels with the row as
              a hover title. */}
          {missing.length > 0 && (
            <div className="border-t border-border-light px-2 py-1 text-[9px] text-text-muted">
              {missing.join(' · ')} — not listed
            </div>
          )}
        </div>

        {/* BETTING ENGINE — the recommended position, and WHERE to place it.
            Every venue pricing this game is ranked on one comparable figure:
            calibrated expected profit per dollar staked. Stake sizing is
            deliberately not shown; it belongs to the bankroll, not the game, and
            it lives on the bet card. What the card owes you here is which side,
            at which venue, at what price, and the edge that justifies it. */}
        {bestVenue && (
          <div className="rounded-lg px-2 py-1.5"
            style={{ background: `${bestVenue.color}14`, boxShadow: `inset 2px 0 0 ${bestVenue.color}` }}>
            <div className="flex items-center justify-between">
              <span className="text-[9px] uppercase tracking-wide" style={{ color: bestVenue.color }}>
                {bet?.result ? 'Bet placed' : options.length > 1 ? 'Best price' : 'Recommended bet'}
              </span>
              <span className="text-[10px] tabular-nums" style={{ color: bestVenue.color }}>
                {(bestVenue.ev * 100).toFixed(1)}% EV
              </span>
            </div>
            <div className="mt-0.5 flex items-baseline justify-between gap-2">
              <span className="min-w-0 text-xs font-bold text-text-primary">
                {bestVenue.pick}{' '}
                <span className="font-normal text-text-secondary">{bestVenue.kind}</span>
              </span>
              {bet?.result ? (
                <span className="shrink-0 text-xs font-bold tabular-nums"
                  style={{ color: bet.result === 'win' ? WIN : LOSS }}>
                  {(bet.pnl ?? 0) >= 0 ? '+' : '−'}${Math.abs(bet.pnl ?? 0)}
                </span>
              ) : (
                <span className="shrink-0 text-[10px] tabular-nums text-text-muted">
                  {bestVenue.href ? (
                    <a href={bestVenue.href} target="_blank" rel="noreferrer" className="hover:underline"
                      style={{ color: bestVenue.color }}>{bestVenue.venue}</a>
                  ) : (
                    <span style={{ color: bestVenue.color }}>{bestVenue.venue}</span>
                  )}
                  <span className="mx-1 opacity-50">·</span>{bestVenue.price}
                </span>
              )}
            </div>
            {/* Runners-up. Same game, priced elsewhere — useful when the best
                venue is one you do not hold an account with. */}
            {!bet?.result && alsoAt.length > 0 && (
              <div className="mt-1 border-t pt-1 text-[9px] leading-snug"
                style={{ borderColor: `${bestVenue.color}33` }}>
                {alsoAt.map((o, i) => (
                  <div key={i} className="flex items-center justify-between gap-2 text-text-muted">
                    <span className="min-w-0 truncate">
                      {o.href ? (
                        <a href={o.href} target="_blank" rel="noreferrer" className="hover:underline" style={{ color: o.color }}>{o.venue}</a>
                      ) : <span style={{ color: o.color }}>{o.venue}</span>}
                      <span className="mx-1 opacity-50">·</span>{o.pick} {o.price}
                    </span>
                    <span className="shrink-0 tabular-nums">{(o.ev * 100).toFixed(1)}% EV</span>
                  </div>
                ))}
              </div>
            )}
          </div>
        )}

        {/* points: actual when played, model-implied before kickoff */}
        <div className="flex items-center justify-between text-[11px]">
          <span className="text-[9px] uppercase tracking-wide text-text-muted">
            {done ? 'Final score' : 'Model implied pts'}
          </span>
          <span className="tabular-nums">
            {done ? (
              <>
                <span className={result.winningTeam === game.awayTeam ? 'text-text-primary font-bold' : 'text-text-muted'}>
                  {game.awayTeam} {result.awayPoints}
                </span>
                <span className="text-text-muted"> · </span>
                <span className={result.winningTeam === game.homeTeam ? 'text-text-primary font-bold' : 'text-text-muted'}>
                  {game.homeTeam} {result.homePoints}
                </span>
              </>
            ) : (
              <span className="text-text-secondary">
                {game.awayTeam} {p.awayImplPts.toFixed(0)} · {game.homeTeam} {p.homeImplPts.toFixed(0)}
              </span>
            )}
          </span>
        </div>
      </div>
    </div>
  );
}
