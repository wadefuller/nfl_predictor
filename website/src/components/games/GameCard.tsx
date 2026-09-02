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

/** "CHI -6.5" — spread stated from the favorite's side, which is how it's read. */
function spreadLabel(line: number, home: string, away: string) {
  if (line === 0) return 'PK';
  return line > 0 ? `${home} -${Math.abs(line)}` : `${away} -${Math.abs(line)}`;
}

export function GameCard({ game, polymarket = [], kalshi = [], kalshiWin = [] }: GameCardProps) {
  const { prediction: p, result } = game;
  const done = result !== null;

  const homeWin = p.predWinner === game.homeTeam ? p.predWinPct : 1 - p.predWinPct;
  const vp = vegasProbs(game.homeMoneyline, game.awayMoneyline);

  // lineDiff = model margin − market spread, both "+ = home favored". So the sign
  // tells you WHICH SIDE the model is higher on; we render that as a named lean
  // instead of a bare signed number, which nobody can interpret at a glance.
  const edge = p.lineDiff;
  const leanTeam = edge > 0 ? game.homeTeam : game.awayTeam;
  const leanPts = Math.abs(edge);
  const notable = leanPts >= 1;

  const pct = (v: number) => `${Math.round(v * 100)}%`;
  const bet = game.bet ?? null;

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

        {/* WIN PROBABILITY — one shared axis. The bar splits where MY MODEL splits
            it; the dashed marker is where VEGAS splits it. The gap between them is
            the disagreement, readable without comparing two separate numbers. */}
        <div>
          <div className="flex items-center justify-between text-[9px] uppercase tracking-wide text-text-muted mb-1">
            <span>Win probability</span>
            {/* Both encodings named together. Labelling only Vegas left the bar's
                own numbers reading as "the" probability rather than the model's. */}
            {vp && (
              <span className="normal-case tracking-normal">
                <span style={{ color: MODEL }}>bar = my model</span>
                <span className="mx-1 opacity-50">·</span>
                <span>dashed = Vegas</span>
              </span>
            )}
          </div>
          <div className="relative h-5 rounded-md bg-bg-card-hover overflow-hidden">
            {/* the model's pick side gets the saturated fill; the other stays faint */}
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
          {vp && (
            <div className="flex justify-between text-[9px] text-text-muted mt-0.5">
              <span>Vegas {pct(vp.away)}</span>
              <span>Vegas {pct(vp.home)}</span>
            </div>
          )}
          {kalshiWin.length > 0 && (
            <div className="mt-1 flex items-center justify-between gap-2 rounded border border-[#f0a00040] bg-[#f0a0000a] px-1.5 py-1 text-[9px] tabular-nums">
              <span className="shrink-0 text-[#f0b44d]">Kalshi 2+ win</span>
              <span className="min-w-0 text-right text-text-secondary">
                {kalshiWin.map((m: any) => `${m.team} ${Math.round(Number(m.ask) * 100)}¢`).join(' · ')}
                <span className="ml-1 text-text-muted">not exact ML</span>
              </span>
            </div>
          )}
        </div>

        {/* POLYMARKET — a separate, prediction-market view. We retain both
            sides rather than collapsing them into one number: each YES contract
            is its own tradable price and may not add to exactly 100¢. */}
        {polymarket.length > 0 ? (
          <div className="rounded-lg border border-[#a371f759] bg-[#a371f70a] overflow-hidden">
            <div className="flex items-center justify-between px-2 py-1 border-b border-[#a371f733]">
              <span className="text-[9px] uppercase tracking-wide text-[#c9a7ff]">Polymarket winner contracts</span>
              <span className="text-[9px] text-text-muted">YES ask vs model</span>
            </div>
            <div className="divide-y divide-[#a371f726]">
              {polymarket.map((m: any) => {
                const ask = Math.round(Number(m.ask) * 100);
                const model = Math.round(Number(m.model_probability) * 100);
                const gap = model - ask;
                return (
                  <a key={m.market_id ?? m.bet_team} href={`https://polymarket.com/event/${m.slug}`}
                    target="_blank" rel="noreferrer" className="flex items-center justify-between gap-2 px-2 py-1.5 hover:bg-[#a371f712]">
                    <span className="min-w-0 text-[11px] font-semibold text-text-primary">
                      {m.bet_team} YES
                      {m.qualifies && <span className="ml-1 rounded border border-win/40 bg-win/10 px-1 py-px text-[8px] uppercase tracking-wide text-win">EV+</span>}
                    </span>
                    <span className="shrink-0 text-right text-[10px] tabular-nums text-text-secondary">
                      <span className="text-[#c9a7ff]">{ask}¢</span>
                      <span className="mx-1 text-text-muted">·</span>
                      model {model}%
                      <span className={gap >= 0 ? 'ml-1 text-win' : 'ml-1 text-loss'}>{gap >= 0 ? '+' : ''}{gap} pts</span>
                    </span>
                  </a>
                );
              })}
            </div>
            <div className="px-2 py-1 text-[9px] text-text-muted">Contract price is the cost of a $1 YES payout. Open to inspect rules and order book.</div>
          </div>
        ) : !done && (
          <div className="flex items-center justify-between rounded-lg border border-border-light bg-bg-card/30 px-2 py-1.5 text-[10px]">
            <span className="font-medium text-[#c9a7ff]">Polymarket</span>
            <span className="text-text-muted">Game-winner contract not listed yet</span>
          </div>
        )}

        {/* KALSHI — its game-spread ladder provides a market line rather than a
            single moneyline. Selecting the contract closest to 50¢ creates a
            readable "fair spread" comparison with the point model. */}
        {kalshi.length > 0 ? kalshi.map((m: any) => {
          const ask = Math.round(Number(m.ask) * 100);
          const modelProbability = Math.round(Number(m.model_probability) * 100);
          const ev = Number(m.ev) * 100;
          return (
            <a key={m.market_ticker} href={`https://kalshi.com/markets/${m.event_ticker}`} target="_blank" rel="noreferrer"
              className="flex items-center justify-between gap-2 rounded-lg border border-[#f0a00059] bg-[#f0a0000a] px-2 py-1.5 text-[10px] hover:bg-[#f0a00012]">
              <span className="shrink-0 font-medium text-[#f0b44d]">Kalshi</span>
              <span className="min-w-0 text-right tabular-nums text-text-secondary">
                <span className="font-semibold text-text-primary">{m.team} -{Number(m.threshold).toFixed(1)}</span>
                <span className="mx-1 text-text-muted">·</span>{ask}¢ ask
                <span className="ml-1.5 text-text-muted">· {modelProbability}% model</span>
                <span className={ev >= 0 ? 'ml-1.5 font-medium text-win' : 'ml-1.5 font-medium text-loss'}>{ev >= 0 ? '+' : ''}{ev.toFixed(1)}% EV</span>
              </span>
            </a>
          );
        }) : !done && (
          <div className="flex items-center justify-between rounded-lg border border-border-light bg-bg-card/30 px-2 py-1.5 text-[10px]">
            <span className="font-medium text-[#f0b44d]">Kalshi</span><span className="text-text-muted">Game spread not listed yet</span>
          </div>
        )}

        {/* SPREAD — both lines stated the same way, stacked so the difference is
            a vertical read rather than mental arithmetic. */}
        <div className="rounded-lg border border-border-light divide-y divide-border-light text-[11px]">
          <div className="flex items-center justify-between px-2 py-1">
            <span className="text-text-muted">Vegas</span>
            <span className="tabular-nums" style={{ color: VEGAS }}>{spreadLabel(game.spreadLine, game.homeTeam, game.awayTeam)}</span>
          </div>
          <div className="flex items-center justify-between px-2 py-1">
            <span className="text-text-muted">My model</span>
            <span className="tabular-nums font-medium" style={{ color: MODEL }}>{spreadLabel(p.nflPredictorLine, game.homeTeam, game.awayTeam)}</span>
          </div>
          {/* the edge, in words: who the model is higher on, and by how much */}
          <div className="flex items-center justify-between px-2 py-1">
            <span className="text-text-muted">Model lean</span>
            <span className={notable ? 'font-semibold' : ''} style={{ color: notable ? MODEL : 'var(--color-text-muted)' }}>
              {leanPts < 0.05 ? 'matches Vegas' : `${leanTeam} by ${num(leanPts)} more`}
            </span>
          </div>
        </div>

        {/* BETTING ENGINE — the actual recommended position and stake. Only shown
            when a side cleared the EV threshold, so its presence is itself signal. */}
        {bet && (
          <div className="rounded-lg px-2 py-1.5"
            style={{ background: `${BET}14`, boxShadow: `inset 2px 0 0 ${BET}` }}>
            <div className="flex items-center justify-between">
              <span className="text-[9px] uppercase tracking-wide" style={{ color: BET }}>
                {bet.result ? 'Bet placed' : 'Recommended bet'}
              </span>
              <span className="text-[10px] tabular-nums text-text-muted">
                {(bet.ev * 100).toFixed(1)}% EV · {bet.odds > 0 ? `+${bet.odds}` : bet.odds}
              </span>
            </div>
            <div className="flex items-baseline justify-between mt-0.5">
              <span className="text-xs font-bold text-text-primary">
                {bet.side} <span className="font-normal text-text-secondary">{bet.type === 'ml' ? 'moneyline' : 'spread'}</span>
              </span>
              {bet.result ? (
                <span className="text-xs font-bold tabular-nums"
                  style={{ color: bet.result === 'win' ? WIN : LOSS }}>
                  {(bet.pnl ?? 0) >= 0 ? '+' : '−'}${Math.abs(bet.pnl ?? 0)}
                </span>
              ) : (
                <span className="text-xs font-bold tabular-nums" style={{ color: BET }}>
                  {(() => { const u = bet.units ?? bet.stake / 5; return Number.isInteger(u) ? u : u.toFixed(1); })()}u
                  <span className="text-[10px] font-normal text-text-muted"> ${bet.stake} → +${bet.toWin}</span>
                </span>
              )}
            </div>
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
