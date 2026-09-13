import type { Game } from '../../types';
import { num } from '../ui/primitives';

export interface LiveGame {
  status: 'live' | 'final';
  home: number; away: number; homeTeam: string; awayTeam: string;
  detail?: string;
}

interface GameCardProps {
  game: Game;
  polymarket?: any[];
  kalshi?: any[];
  kalshiWin?: any[];
  /** ESPN scoreboard state. Pipeline results always take precedence. */
  liveGame?: LiveGame;
}

const MODEL = '#58a6ff';
const VEGAS = '#8b949e';
const BET = '#d29922';
const WIN = '#3fb950';
const LOSS = '#f85149';
const KALSHI = '#f0b44d';
const POLYMARKET = '#c9a7ff';

const moneylineToProb = (ml: number) => (ml < 0 ? -ml / (-ml + 100) : 100 / (ml + 100));
function vegasProbs(homeMl: number | null, awayMl: number | null) {
  if (homeMl == null || awayMl == null) return null;
  const home = moneylineToProb(homeMl), away = moneylineToProb(awayMl);
  return { home: home / (home + away), away: away / (home + away) };
}

function StatusPill({ state, detail }: { state: 'scheduled' | 'live' | 'final_hit' | 'final_miss' | 'graded_hit' | 'graded_miss'; detail?: string }) {
  const styles = {
    scheduled: 'border-border-light bg-bg-card-hover/50 text-text-muted',
    live: 'border-loss/40 bg-loss/10 text-[#ff8b86]',
    final_hit: 'border-win/40 bg-win/10 text-win',
    final_miss: 'border-loss/40 bg-loss/10 text-[#ff8b86]',
    graded_hit: 'border-win/40 bg-win/10 text-win',
    graded_miss: 'border-loss/40 bg-loss/10 text-[#ff8b86]',
  };
  const labels = {
    scheduled: 'Scheduled', live: 'Live', final_hit: 'Final · model hit',
    final_miss: 'Final · model miss', graded_hit: 'Final · graded hit', graded_miss: 'Final · graded miss',
  };
  return (
    <span className={`inline-flex max-w-[10.75rem] items-center gap-1 rounded-full border px-1.5 py-px text-[8px] font-bold uppercase tracking-wide ${styles[state]}`}>
      {state === 'live' && <span className="h-1.5 w-1.5 shrink-0 animate-pulse rounded-full bg-current" />}
      <span className="truncate">{labels[state]}{detail ? ` · ${detail}` : ''}</span>
    </span>
  );
}

/** Full market context stays visible, with state labels resolved from live data when needed. */
export function GameCard({ game, polymarket = [], kalshi = [], kalshiWin = [], liveGame }: GameCardProps) {
  const { prediction: p } = game;
  const isGraded = game.result != null;
  const isLive = !isGraded && liveGame?.status === 'live';
  const isFinalPending = !isGraded && liveGame?.status === 'final';
  const result = game.result ?? (isFinalPending && liveGame ? {
    homePoints: liveGame.home, awayPoints: liveGame.away,
    homeResult: liveGame.home - liveGame.away,
    winningTeam: liveGame.home === liveGame.away ? null : liveGame.home > liveGame.away ? liveGame.homeTeam : liveGame.awayTeam,
    predCorrect: null,
  } as any : null);
  const complete = result !== null;
  // Before the pipeline writes the official grade, call a miss when the model's
  // predicted side fails to cover its own projected margin. This correctly marks
  // SEA (-4, won by 3) and LA (-7, lost by 20) as misses immediately.
  const provisionalModelHit = isFinalPending && result
    ? (p.nflPredictorLine >= 0
        ? result.homeResult >= p.nflPredictorLine
        : result.homeResult <= p.nflPredictorLine)
    : null;
  const status = isGraded ? (game.result?.predCorrect ? 'graded_hit' : 'graded_miss') : isFinalPending
    ? (provisionalModelHit ? 'final_hit' : 'final_miss')
    : isLive ? 'live' : 'scheduled';

  const homeWin = p.predWinner === game.homeTeam ? p.predWinPct : 1 - p.predWinPct;
  const vegas = vegasProbs(game.homeMoneyline, game.awayMoneyline);
  const pct = (v: number) => `${Math.round(v * 100)}%`;
  const bet = game.bet ?? null;
  const referenceTeam = p.predWinner;
  const referenceIsHome = referenceTeam === game.homeTeam;
  const fromReference = (homeMargin: number) => referenceIsHome ? homeMargin : -homeMargin;
  const formatLine = (margin: number | null) => margin == null ? null : Math.abs(margin) < 0.05 ? 'PK' : `${margin > 0 ? '−' : '+'}${num(Math.abs(margin))}`;
  const modelMargin = fromReference(p.nflPredictorLine);
  const vegasMargin = fromReference(game.spreadLine);
  const modelWin = p.predWinPct;
  const vegasWin = vegas ? (referenceIsHome ? vegas.home : vegas.away) : null;
  const notable = Math.abs(p.lineDiff) >= 1;

  const kalshiSpread = kalshi.find((market: any) => market.team === referenceTeam) ?? kalshi[0] ?? null;
  const kalshiWinMarket = kalshiWin.find((market: any) => market.team === referenceTeam) ?? null;
  const polymarketWin = polymarket.find((market: any) => market.bet_team === referenceTeam) ?? null;
  const kalshiMargin = kalshiSpread ? (kalshiSpread.team === referenceTeam ? Number(kalshiSpread.threshold) : -Number(kalshiSpread.threshold)) : null;
  type MarketRow = { label: string; color: string; margin: number | null; win: number | null; model?: boolean; note?: string; href?: string };
  const marketRows: MarketRow[] = [
    { label: 'My model', color: MODEL, margin: modelMargin, win: modelWin, model: true },
    { label: 'Vegas', color: VEGAS, margin: vegasMargin, win: vegasWin },
  ];
  if (kalshiSpread || kalshiWinMarket) marketRows.push({
    label: 'Kalshi', color: KALSHI, margin: kalshiMargin, win: kalshiWinMarket ? Number(kalshiWinMarket.ask) : null,
    note: '2+ win market; not an exact moneyline', href: kalshiSpread ? `https://kalshi.com/markets/${kalshiSpread.event_ticker}` : undefined,
  });
  if (polymarketWin) marketRows.push({
    label: 'Polymarket', color: POLYMARKET, margin: null, win: Number(polymarketWin.ask),
    note: 'YES ask; both sides may not equal 100¢', href: `https://polymarket.com/event/${polymarketWin.slug}`,
  });

  type VenueOption = { venue: string; color: string; pick: string; kind: string; ev: number; price: string; href?: string };
  const options: VenueOption[] = [];
  if (bet) options.push({ venue: 'Sportsbook', color: BET, pick: bet.side, kind: bet.type === 'ml' ? 'moneyline' : 'spread', ev: bet.ev, price: bet.odds > 0 ? `+${bet.odds}` : `${bet.odds}` });
  if (kalshiSpread?.qualifies) options.push({ venue: 'Kalshi', color: KALSHI, pick: `${kalshiSpread.team} −${Number(kalshiSpread.threshold).toFixed(1)}`, kind: 'spread contract', ev: Number(kalshiSpread.ev), price: `${Math.round(Number(kalshiSpread.ask) * 100)}¢`, href: `https://kalshi.com/markets/${kalshiSpread.event_ticker}` });
  polymarket.filter((market: any) => market.qualifies).forEach((market: any) => options.push({ venue: 'Polymarket', color: POLYMARKET, pick: `${market.bet_team} YES`, kind: 'winner contract', ev: Number(market.net_ev), price: `${Math.round(Number(market.ask) * 100)}¢`, href: `https://polymarket.com/event/${market.slug}` }));
  options.sort((a, b) => b.ev - a.ev);
  const bestVenue = options[0] ?? null;
  const alternatives = options.slice(1);
  const actualMargin = complete ? (referenceIsHome ? result!.homeResult : -result!.homeResult) : null;
  const referenceWon = complete ? result!.winningTeam === referenceTeam : null;
  const missing = [!kalshiSpread && !kalshiWinMarket ? 'Kalshi' : null, !polymarketWin ? 'Polymarket' : null].filter(Boolean) as string[];
  const accent = bestVenue ? bestVenue.color : notable ? MODEL : null;
  const currentScore = complete
    ? `${game.awayTeam} ${result!.awayPoints} · ${game.homeTeam} ${result!.homePoints}`
    : isLive && liveGame ? `${game.awayTeam} ${liveGame.away} · ${game.homeTeam} ${liveGame.home}` : null;

  return (
    <article id={`game-${game.gameId}`} className="data-surface game-card scroll-mt-28 rounded-xl border bg-bg-card overflow-hidden transition-[transform,box-shadow,border-color,background-color] duration-200 hover:-translate-y-px hover:shadow-lg hover:shadow-black/20"
      style={{ borderColor: accent ? `${accent}59` : 'var(--color-border)', boxShadow: accent ? `inset 0 2px 0 ${accent}` : undefined }}>
      <div className="flex items-center justify-between gap-2 border-b border-border-light bg-bg-page/30 px-3 py-1.5">
        <span className="min-w-0 truncate text-[10px] uppercase tracking-wide text-text-muted">
          {game.weekday.slice(0, 3)} {new Date(`${game.gameday}T12:00:00`).toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
          {!complete && !isLive && ` · ${game.gametime}`}
        </span>
        <div className="flex shrink-0 items-center gap-1.5">
          {game.divGame && <span className="rounded border border-border-light px-1 py-px text-[8px] font-medium text-text-muted">DIV</span>}
          <StatusPill state={status} detail={isLive ? liveGame?.detail : undefined} />
        </div>
      </div>

      <div className="space-y-2.5 px-3 py-2.5">
        <div className="flex items-baseline justify-between gap-3">
          <h3 className="text-sm font-bold tracking-tight text-text-primary">{game.awayTeam} <span className="font-normal text-text-muted">@</span> {game.homeTeam}</h3>
          <span className={`shrink-0 text-[10px] tabular-nums ${currentScore ? 'font-semibold text-text-primary' : 'text-text-muted'}`}>{currentScore ?? `O/U ${game.totalLine}`}</span>
        </div>

        {!complete && !isLive && (
          <div>
            <div className="mb-1 flex items-center justify-between text-[9px] uppercase tracking-wide text-text-muted"><span>Win probability</span><span className="normal-case tracking-normal"><span style={{ color: MODEL }}>fill = model</span>{vegas && <><span className="mx-1 opacity-50">·</span>dashed = Vegas</>}</span></div>
            <div className="relative h-5 overflow-hidden rounded-md bg-bg-card-hover">
              <div className="absolute inset-y-0 left-0" style={{ width: `${(1 - homeWin) * 100}%`, background: p.predWinner === game.awayTeam ? `${MODEL}99` : `${MODEL}1f` }} />
              <div className="absolute inset-y-0 right-0" style={{ width: `${homeWin * 100}%`, background: p.predWinner === game.homeTeam ? `${MODEL}99` : `${MODEL}1f` }} />
              {vegas && <div className="absolute inset-y-0 border-l-2 border-dashed" style={{ left: `${(1 - vegas.home) * 100}%`, borderColor: VEGAS }} />}
              <div className="absolute inset-0 flex items-center justify-between px-2 text-[10px] font-semibold"><span className={p.predWinner === game.awayTeam ? 'text-text-primary' : 'text-text-muted'}>{game.awayTeam} {pct(1 - homeWin)}</span><span className={p.predWinner === game.homeTeam ? 'text-text-primary' : 'text-text-muted'}>{pct(homeWin)} {game.homeTeam}</span></div>
            </div>
          </div>
        )}
        {isLive && <div className="flex items-center justify-between rounded-lg border border-loss/25 bg-loss/5 px-2 py-1.5 text-[10px]"><span className="font-medium text-[#ff8b86]">Live score</span><span className="text-text-secondary">Pregame model: {referenceTeam} {pct(modelWin)} win chance</span></div>}

        {bestVenue ? (
          <div className="rounded-lg px-2.5 py-2" style={{ background: `${bestVenue.color}14`, boxShadow: `inset 2px 0 0 ${bestVenue.color}` }}>
            <div className="flex items-center justify-between gap-2"><span className="text-[9px] font-medium uppercase tracking-wide" style={{ color: bestVenue.color }}>{options.length > 1 ? 'Best available price' : 'Recommended position'}</span><span className="text-[10px] font-semibold tabular-nums" style={{ color: bestVenue.color }}>+{(bestVenue.ev * 100).toFixed(1)}% EV</span></div>
            <div className="mt-0.5 flex items-baseline justify-between gap-2"><span className="min-w-0 truncate text-xs font-bold text-text-primary">{bestVenue.pick} <span className="font-normal text-text-secondary">{bestVenue.kind}</span></span><span className="shrink-0 text-[10px] tabular-nums text-text-secondary"><span style={{ color: bestVenue.color }}>{bestVenue.venue}</span><span className="mx-1 opacity-50">·</span>{bestVenue.price}</span></div>
          </div>
        ) : (
          <div className="flex items-center justify-between rounded-lg border border-border-light bg-bg-card-hover/30 px-2.5 py-2 text-[10px]"><span className="text-text-muted">Model lean</span><span className="font-medium tabular-nums text-text-primary">{referenceTeam} {formatLine(modelMargin)} · {pct(modelWin)} win</span></div>
        )}

        <div className="rounded-lg border border-border-light bg-bg-card/25">
          <div className="flex items-center justify-between gap-2 border-b border-border-light px-2.5 py-2 text-[10px] font-medium text-text-secondary"><span>Market comparison</span><span className="tabular-nums text-text-muted">{referenceTeam} {formatLine(modelMargin)} model · {formatLine(vegasMargin)} Vegas</span></div>
          <div className="space-y-2 border-t border-border-light px-2.5 py-2.5">
            <div className="overflow-hidden rounded-md border border-border-light">
              <div className="grid grid-cols-[1fr_3.6rem_3.4rem] gap-x-2 bg-bg-card-hover/40 px-2 py-1 text-[8px] uppercase tracking-wide text-text-muted"><span>Market</span><span className="text-right">{referenceTeam} line</span><span className="text-right">{complete ? 'model call' : `${referenceTeam} win`}</span></div>
              <div className="divide-y divide-border-light">
                {complete && <div className="grid grid-cols-[1fr_3.6rem_3.4rem] items-center gap-x-2 bg-bg-card-hover/35 px-2 py-1 text-[10px]"><span className="font-semibold text-text-primary">Actual</span><span className="text-right font-semibold tabular-nums text-text-primary">{formatLine(actualMargin)}</span><span className="text-right font-medium" style={{ color: referenceWon ? WIN : LOSS }}>{referenceWon ? `${referenceTeam} won` : `${referenceTeam} lost`}</span></div>}
                {marketRows.map((row, index) => {
                  const marginDelta = complete ? (row.margin != null && actualMargin != null ? row.margin - actualMargin : null) : (!row.model && row.margin != null ? modelMargin - row.margin : null);
                  const winDelta = !complete && !row.model && row.win != null ? (modelWin - row.win) * 100 : null;
                  const callCorrect = complete && row.win != null ? ((row.win >= 0.5) === referenceWon) : null;
                  const cells = (
                    <>
                      <span className={`flex min-w-0 items-center gap-1.5 ${row.model ? 'font-medium text-text-primary' : 'text-text-secondary'}`}>
                        <span className="h-1.5 w-1.5 shrink-0 rounded-full" style={{ background: row.color }} />
                        {row.label}{row.note && <span title={row.note} className="cursor-help text-text-muted">*</span>}
                      </span>
                      <span className="text-right tabular-nums">
                        <span style={{ color: row.model ? MODEL : undefined }}>{formatLine(row.margin) ?? '—'}</span>
                        {marginDelta != null && <span className="ml-1 text-[9px] text-text-muted">{marginDelta > 0 ? '+' : '−'}{num(Math.abs(marginDelta))}</span>}
                      </span>
                      {complete ? (
                        <span className="text-right tabular-nums" style={{ color: callCorrect == null ? 'var(--color-text-muted)' : callCorrect ? WIN : LOSS }}>
                          {callCorrect == null ? '—' : callCorrect ? '✓' : '✗'}
                          {row.win != null && <span className="ml-1 text-text-muted">{pct(row.win)}</span>}
                        </span>
                      ) : (
                        <span className="text-right tabular-nums">
                          <span style={{ color: row.model ? MODEL : undefined }}>{row.win == null ? '—' : pct(row.win)}</span>
                          {winDelta != null && <span className="ml-1 text-[9px] text-text-muted">{winDelta > 0 ? '+' : '−'}{num(Math.abs(winDelta))}pp</span>}
                        </span>
                      )}
                    </>
                  );
                  const className = `grid grid-cols-[1fr_3.6rem_3.4rem] items-center gap-x-2 px-2 py-1 text-[10px] ${row.model ? 'bg-bg-card-hover/20' : ''}`;
                  return row.href ? <a key={index} href={row.href} target="_blank" rel="noreferrer" className={`${className} hover:bg-bg-card-hover/40`}>{cells}</a> : <div key={index} className={className}>{cells}</div>;
                })}
              </div>
              {missing.length > 0 && <div className="border-t border-border-light px-2 py-1 text-[9px] text-text-muted">{missing.join(' · ')} — not listed</div>}
            </div>
            {alternatives.length > 0 && <div className="space-y-1 border-t border-border-light pt-2 text-[10px]">{alternatives.map((option, index) => <div key={index} className="flex items-center justify-between gap-2 text-text-muted"><span className="min-w-0 truncate"><span style={{ color: option.color }}>{option.venue}</span><span className="mx-1 opacity-50">·</span>{option.pick} {option.price}</span><span className="shrink-0 tabular-nums">+{(option.ev * 100).toFixed(1)}% EV</span></div>)}</div>}
            <div className="flex items-center justify-between border-t border-border-light pt-2 text-[10px]"><span className="uppercase tracking-wide text-text-muted">{complete ? 'Final score' : 'Model implied points'}</span><span className="tabular-nums text-text-secondary">{complete ? currentScore : `${game.awayTeam} ${p.awayImplPts.toFixed(0)} · ${game.homeTeam} ${p.homeImplPts.toFixed(0)}`}</span></div>
          </div>
        </div>
      </div>
    </article>
  );
}
