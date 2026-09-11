import { useEffect, useState } from 'react';
import type { Game } from '../../types';
import { getWeekLabel } from '../../data/weekLabels';
import { GameCard } from './GameCard';

interface GameGridProps {
  games: Game[];
  week: number;
  /** Read-only Polymarket winner contracts keyed by the canonical game id. */
  polymarket?: any[];
  kalshi?: any[];
  kalshiWin?: any[];
}

/**
 * Final scores for games that have finished but whose week has not been graded.
 *
 * `game.result` is written by the pipeline and only lands once the week is
 * graded on Tuesday. Between a Thursday kickoff and that grading there is a
 * multi-day window where some games are over and the card still shows a pregame
 * win-probability bar, as though nothing had happened. ESPN's public scoreboard
 * closes that window without waiting on the pipeline; when the graded result
 * does arrive it takes precedence, since it is the number everything else on
 * the site is reconciled against.
 */
function useLiveResults(): Record<string, { home: number; away: number; homeTeam: string; awayTeam: string }> {
  const [finals, setFinals] = useState<Record<string, any>>({});
  useEffect(() => {
    let cancelled = false;
    (async () => {
      try {
        const r = await fetch('https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?limit=100');
        if (!r.ok) return;
        const board = await r.json();
        const out: Record<string, any> = {};
        (board.events ?? []).forEach((event: any) => {
          const comp = event.competitions?.[0];
          if (comp?.status?.type?.state !== 'post') return;
          const home = comp.competitors?.find((c: any) => c.homeAway === 'home');
          const away = comp.competitors?.find((c: any) => c.homeAway === 'away');
          if (!home || !away) return;
          // ESPN writes LAR/WSH where this stack writes LA/WAS.
          const fix = (t: string) => (t === 'LAR' ? 'LA' : t === 'WSH' ? 'WAS' : t);
          const key = `${fix(away.team.abbreviation)}@${fix(home.team.abbreviation)}`;
          out[key] = { home: Number(home.score), away: Number(away.score),
                       homeTeam: fix(home.team.abbreviation), awayTeam: fix(away.team.abbreviation) };
        });
        if (!cancelled) setFinals(out);
      } catch { /* the cards simply stay in their pregame state */ }
    })();
    return () => { cancelled = true; };
  }, []);
  return finals;
}

export function GameGrid({ games, week, polymarket = [], kalshi = [], kalshiWin = [] }: GameGridProps) {
  if (games.length === 0) {
    return (
      <div className="text-center py-16 text-text-muted">
        <p className="text-lg">No games found for {getWeekLabel(week)}</p>
        <p className="text-sm mt-1">Try selecting a different week</p>
      </div>
    );
  }

  const liveFinals = useLiveResults();

  // One flat, denser grid — each card carries its own date, so per-day section
  // headers (which cost a full row of vertical space each and left ragged gaps
  // when a day had 1-2 games) are no longer needed.
  const sorted = [...games].sort(
    (a, b) => (a.gameday + a.gametime).localeCompare(b.gameday + b.gametime)
  );

  return (
    <div className="space-y-3">
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-2.5">
        {sorted.map(game => (
          <GameCard key={game.gameId} game={game}
            liveFinal={liveFinals[`${game.awayTeam}@${game.homeTeam}`]}
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
