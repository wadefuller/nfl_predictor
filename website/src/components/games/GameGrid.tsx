import { useEffect, useState } from 'react';
import { availabilityEligible } from '../../lib/availability';
import type { Game } from '../../types';
import { getWeekLabel } from '../../data/weekLabels';
import { GameCard, type LiveGame } from './GameCard';

interface GameGridProps {
  games: Game[];
  week: number;
  /** Read-only Polymarket winner contracts keyed by the canonical game id. */
  polymarket?: any[];
  kalshi?: any[];
  kalshiWin?: any[];
}

/**
 * ESPN fills the gap between the last published model report and the pipeline's
 * Tuesday grading. Cards can now be explicitly Scheduled, Live, Final-awaiting-
 * grade, or Final-graded instead of treating every non-graded game as pregame.
 */
function useLiveGames(): Record<string, LiveGame> {
  const [games, setGames] = useState<Record<string, LiveGame>>({});
  useEffect(() => {
    let cancelled = false;
    const refresh = async () => {
      try {
        const r = await fetch('https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?limit=100');
        if (!r.ok) return;
        const board = await r.json();
        const out: Record<string, LiveGame> = {};
        (board.events ?? []).forEach((event: any) => {
          const comp = event.competitions?.[0];
          const state = comp?.status?.type?.state;
          if (state !== 'in' && state !== 'post') return;
          const home = comp.competitors?.find((c: any) => c.homeAway === 'home');
          const away = comp.competitors?.find((c: any) => c.homeAway === 'away');
          if (!home || !away) return;
          // ESPN writes LAR/WSH where this stack writes LA/WAS.
          const fix = (t: string) => (t === 'LAR' ? 'LA' : t === 'WSH' ? 'WAS' : t);
          const key = `${fix(away.team.abbreviation)}@${fix(home.team.abbreviation)}`;
          out[key] = {
            status: state === 'post' ? 'final' : 'live',
            home: Number(home.score), away: Number(away.score),
            homeTeam: fix(home.team.abbreviation), awayTeam: fix(away.team.abbreviation),
            detail: state === 'post' ? undefined : comp.status?.type?.shortDetail ?? comp.status?.type?.detail,
          };
        });
        if (!cancelled) setGames(out);
      } catch { /* the cards simply stay in their last known state */ }
    };
    void refresh();
    const interval = window.setInterval(() => { void refresh(); }, 60_000);
    return () => { cancelled = true; window.clearInterval(interval); };
  }, []);
  return games;
}

export function GameGrid({ games, week, polymarket = [], kalshi = [], kalshiWin = [] }: GameGridProps) {
  games = games.map(g => g.result || availabilityEligible(g.availability) ? g : { ...g, bet: null });
  const liveGames = useLiveGames();
  const [mobileFilter, setMobileFilter] = useState<'all' | 'plays' | 'in_progress'>('all');

  if (games.length === 0) {
    return (
      <div className="text-center py-16 text-text-muted">
        <p className="text-lg">No games found for {getWeekLabel(week)}</p>
        <p className="text-sm mt-1">Try selecting a different week</p>
      </div>
    );
  }

  // One flat, denser grid — each card carries its own date, so per-day section
  // headers (which cost a full row of vertical space each and left ragged gaps
  // when a day had 1-2 games) are no longer needed.
  const sorted = [...games].sort(
    (a, b) => (a.gameday + a.gametime).localeCompare(b.gameday + b.gametime)
  );
  const filtered = sorted.filter(game => {
    if (mobileFilter === 'all') return true;
    if (mobileFilter === 'plays') return Boolean(game.bet)
    const state = game.result ? 'graded' : liveGames[`${game.awayTeam}@${game.homeTeam}`]?.status;
    return state === 'live' || state === 'final' || state === 'graded';
  });
  const inProgressCount = sorted.filter(game => {
    const state = game.result ? 'graded' : liveGames[`${game.awayTeam}@${game.homeTeam}`]?.status;
    return state === 'live' || state === 'final' || state === 'graded';
  }).length;
  const liveCount = sorted.filter(game => !game.result && liveGames[`${game.awayTeam}@${game.homeTeam}`]?.status === 'live').length;
  const finalCount = sorted.filter(game => game.result || liveGames[`${game.awayTeam}@${game.homeTeam}`]?.status === 'final').length;
  const nextGame = sorted.find(game => !game.result && !liveGames[`${game.awayTeam}@${game.homeTeam}`]);
  const bestBet = [...sorted].filter(game => game.bet?.ev != null)
    .sort((a, b) => Number(b.bet?.ev) - Number(a.bet?.ev))[0];
  const kickoff = (game: Game) => {
    const [hours, minutes] = String(game.gametime ?? '').split(':').map(Number);
    if (!Number.isFinite(hours) || !Number.isFinite(minutes)) return game.weekday.slice(0, 3);
    const suffix = hours >= 12 ? 'PM' : 'AM';
    const hour = hours % 12 || 12;
    return `${game.weekday.slice(0, 3)} · ${hour}:${String(minutes).padStart(2, '0')} ${suffix}`;
  };
  const jumpTo = (game?: Game) => {
    if (!game) return;
    document.getElementById(`game-${game.gameId}`)?.scrollIntoView({ behavior: 'smooth', block: 'center' });
  };

  return (
    <div className="space-y-3">
      <section className="data-surface overflow-hidden rounded-xl border border-border-light bg-bg-card/40">
        <div className="flex flex-wrap items-baseline justify-between gap-2 border-b border-border-light px-3 py-2.5">
          <div>
            <h3 className="text-xs font-semibold text-text-primary">Slate pulse</h3>
            <p className="mt-0.5 text-[10px] text-text-muted">What needs your attention across this week&rsquo;s board.</p>
          </div>
          <span className="text-[10px] tabular-nums text-text-muted">{sorted.length} games · {sorted.filter(game => game.bet).length} model positions</span>
        </div>
        <div className="grid divide-y divide-border-light sm:grid-cols-3 sm:divide-x sm:divide-y-0">
          <button type="button" onClick={() => jumpTo(sorted.find(game => !game.result && liveGames[`${game.awayTeam}@${game.homeTeam}`]?.status === 'live') ?? sorted.find(game => game.result))}
            className="min-w-0 px-3 py-2.5 text-left transition-colors hover:bg-bg-card-hover focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
            <div className="text-[9px] font-semibold uppercase tracking-wider text-text-muted">Game state</div>
            <div className={`mt-1 text-sm font-semibold tabular-nums ${liveCount ? 'text-[#ff8b86]' : finalCount ? 'text-text-secondary' : 'text-text-primary'}`}>
              {liveCount ? `${liveCount} live` : finalCount ? `${finalCount} final` : 'Slate ahead'}
            </div>
            <div className="mt-0.5 truncate text-[10px] text-text-muted">{liveCount ? 'Open live game' : finalCount ? `${sorted.length - finalCount} still to play` : 'No games have started'}</div>
          </button>
          <button type="button" onClick={() => jumpTo(nextGame)} disabled={!nextGame}
            className="min-w-0 px-3 py-2.5 text-left transition-colors hover:bg-bg-card-hover disabled:cursor-default disabled:hover:bg-transparent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
            <div className="text-[9px] font-semibold uppercase tracking-wider text-text-muted">Next kickoff</div>
            <div className="mt-1 truncate text-sm font-semibold text-text-primary">{nextGame ? `${nextGame.awayTeam} @ ${nextGame.homeTeam}` : 'Slate complete'}</div>
            <div className="mt-0.5 truncate text-[10px] text-text-muted">{nextGame ? kickoff(nextGame) : 'All games are final'}</div>
          </button>
          <button type="button" onClick={() => jumpTo(bestBet)} disabled={!bestBet}
            className="min-w-0 px-3 py-2.5 text-left transition-colors hover:bg-bg-card-hover disabled:cursor-default disabled:hover:bg-transparent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent">
            <div className="text-[9px] font-semibold uppercase tracking-wider text-text-muted">Best model position</div>
            <div className="mt-1 truncate text-sm font-semibold text-text-primary">{bestBet ? `${bestBet.bet!.side} ${bestBet.bet!.type === 'ml' ? 'moneyline' : 'spread'}` : 'No position'}</div>
            <div className="mt-0.5 truncate text-[10px] text-text-muted">{bestBet ? `${bestBet.awayTeam} @ ${bestBet.homeTeam} · +${(Number(bestBet.bet!.ev) * 100).toFixed(1)}% EV` : 'No qualifying sportsbook play'}</div>
          </button>
        </div>
      </section>
      {/* On a phone, filters prevent a 16-game analytical grid from becoming a
          long undifferentiated feed. Desktop retains the full scan-first grid. */}
      <div className="rounded-xl border border-border-light bg-bg-card/40 p-1.5 md:hidden">
        <div className="flex items-center justify-between px-1.5 pb-1.5 text-[9px] uppercase tracking-wider text-text-muted">
          <span>Game feed</span><span>{filtered.length} of {sorted.length}</span>
        </div>
        <div className="grid grid-cols-3 gap-1">
          {([
            ['all', `All ${sorted.length}`],
            ['plays', `Plays ${sorted.filter(game => game.bet).length}`],
            ['in_progress', inProgressCount ? `Live/final ${inProgressCount}` : 'Live/final'],
          ] as const).map(([key, label]) => (
            <button key={key} type="button" onClick={() => setMobileFilter(key)}
              className={`rounded-md px-1.5 py-2 text-[10px] font-medium transition-colors ${mobileFilter === key ? 'bg-bg-card-hover text-text-primary shadow-sm' : 'text-text-muted hover:text-text-secondary'}`}>
              {label}
            </button>
          ))}
        </div>
      </div>
      <div className="grid grid-cols-1 gap-2.5 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
        {filtered.map(game => (
          <GameCard key={game.gameId} game={game}
            liveGame={liveGames[`${game.awayTeam}@${game.homeTeam}`]}
            polymarket={polymarket.filter(m => m.game_id === game.gameId)}
            kalshi={kalshi.filter(m => m.game_id === game.gameId)}
            kalshiWin={kalshiWin.filter(m => m.game_id === game.gameId)} />
        ))}
      </div>
      {/* Grid-level legend. The per-card table marks its two caveated sources
          with an asterisk and a hover title; the full explanation lives here so
          it is stated once rather than sixteen times. */}
      <details className="rounded-lg border border-border-light bg-bg-card/30 px-3 py-2 text-[11px] text-text-muted">
        <summary className="cursor-pointer select-none font-medium text-text-secondary">How to read the market comparison</summary>
        <div className="mt-2 space-y-2 leading-relaxed">
          <p>
            Every row is quoted from the <span className="font-medium text-text-secondary">same side — the team the model picks</span>, so the
            column is a direct comparison. The small figure beside each market is that row&rsquo;s
            gap from the model: <span className="font-medium text-text-secondary">+0.5</span> means the model is half a point higher on that
            team, <span className="font-medium text-text-secondary">+2.6pp</span> means 2.6 percentage points more likely to win. Sportsbook
            win probability is de-vigged from the two moneylines.
          </p>
          <p>
            <span className="font-medium text-[#f0b44d]">Kalshi *</span> — the line column is the contract rung priced nearest 50&cent;
            (a YES pays $1 if that team wins by more than the threshold), not a sportsbook line. The win column is
            Kalshi&rsquo;s closest listed outcome, <em>win by 2+</em>, which is not an exact moneyline: it excludes a one-point
            win, so it reads slightly low against the other sources.
          </p>
          <p>
            <span className="font-medium text-[#c9a7ff]">Polymarket *</span> — the YES ask for that team. Each side is its own
            tradable contract, so the two need not sum to 100&cent;, and no spread equivalent is listed.
          </p>
          <p>
            Both prediction markets are intermittent: contracts appear and are pulled as the week moves. A source with
            nothing listed for a game is named under the table rather than shown as an empty row.
          </p>
        </div>
      </details>
    </div>
  );
}
