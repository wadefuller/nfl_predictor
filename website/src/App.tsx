import { useEffect, useState } from 'react';
import { useGameData } from './hooks/useGameData';
import { useSeasonWeek } from './hooks/useSeasonWeek';
import { Header } from './components/layout/Header';
import { GameGrid } from './components/games/GameGrid';
import { TopBets } from './components/games/TopBets';
import { WeeklyReport, WEEKLY_GROUP_KEYS } from './components/weekly/WeeklyReport';
import { SeasonReport, SEASON_GROUP_KEYS } from './components/season/SeasonReport';
import { Skeleton } from './components/ui/primitives';
import { MarketReport } from './components/fantasy/MarketReport';
import { LeagueReport } from './components/fantasy/LeagueReport';
import { useFantasyMeta } from './hooks/useFantasyData';
import { getWeekLabel } from './data/weekLabels';

// 'predictions' and 'rankings' were folded into 'weekly' and 'season'
// respectively; both are still accepted from old permalinks and redirected.
type View = 'weekly' | 'season' | 'market' | 'league';
type AnyView = View | 'predictions' | 'rankings';
type Product = 'predictor' | 'fantasy';

const PREDICTOR_TABS = [
  ['weekly', 'This Week'], ['season', 'Season'],
] as const;
const FANTASY_TABS = [
  ['weekly', 'Game Day'], ['league', 'My Teams'], ['market', 'Market'],
] as const;

const VIEW_ALIAS: Record<string, View> = { predictions: 'weekly', rankings: 'season' };


// Hash permalinks, deliberately hash-based rather than a router — no dependency,
// no server rewrite rules, and it survives being opened from a static build.
//
//   #/season/2025/betting        season report, Betting sub-tab
//   #/weekly/2025/7/fantasy      week 7 report, Fantasy sub-tab
//   #/league/mfl_65804           a specific league
//
// Segments after the view are matched by SHAPE, not position: a 4-digit token is
// the season, 1-2 digits the week, anything else the sub-tab (or league key).
// That keeps one parser working across views whose slots differ.
function parseHash(): { view?: View; season?: number; week?: number; sub?: string } {
  const h = window.location.hash.replace(/^#\/?/, '');
  if (!h) return {};
  const [v, ...rest] = h.split('/').filter(Boolean);
  const out: { view?: View; season?: number; week?: number; sub?: string } = {};
  const known: AnyView[] = ['weekly', 'season', 'market', 'league', 'predictions', 'rankings'];
  if ((known as string[]).includes(v)) out.view = (VIEW_ALIAS[v] ?? v) as View;
  for (const seg of rest) {
    if (/^\d{4}$/.test(seg)) out.season = Number(seg);
    else if (/^\d{1,2}$/.test(seg)) out.week = Number(seg);
    else out.sub = decodeURIComponent(seg);
  }
  return out;
}

function buildHash(view: View, season: number, week: number, sub: string | null): string {
  const parts: string[] = [view];
  // Only the views whose content actually varies by season/week carry them, so
  // a Market link doesn't gain a meaningless year.
  if (view === 'weekly') parts.push(String(season), String(week));
  else if (view === 'season') parts.push(String(season));
  if (sub) parts.push(encodeURIComponent(sub));
  return `#/${parts.join('/')}`;
}

export default function App() {
  const [view, setView] = useState<View>('weekly');
  // Active sub-tab within the current view. Cleared on view change so each view
  // falls back to its own default rather than inheriting a foreign key.
  const [sub, setSub] = useState<string | null>(null);
  const [leagueKey, setLeagueKey] = useState<string | null>(null);
  const [product, setProduct] = useState<Product>('predictor');
  const changeView = (v: View, nextSub: string | null = null) => {
    setView(v); setSub(nextSub);
    if (v === 'market' || v === 'league') setProduct('fantasy');
  };
  const changeProduct = (next: Product) => {
    setProduct(next);
    // Each product opens on the job users do most often: the current slate for
    // Predictor and their active teams for Fantasy HQ.
    if (next === 'fantasy') changeView('league');
    else changeView('weekly');
  };
  const { meta: fantasyMeta } = useFantasyMeta();
  const { meta, performance, loading, error, getGamesForWeek, loadSeason } =
    useGameData();

  const leagues = fantasyMeta?.leagues ?? [];
  const activeLeague = leagueKey ?? leagues[0]?.key ?? null;
  const isFantasy = view === 'market' || view === 'league';
  const { selectedSeason, selectedWeek, setSelectedSeason, setSelectedWeek, maxWeek } =
    useSeasonWeek(meta);

  // Apply an incoming permalink AFTER meta lands. useSeasonWeek re-initializes
  // season/week from meta whenever it arrives, so applying the hash on mount got
  // silently overwritten a moment later. Week is set after season because
  // setSelectedSeason resets the week for any non-current season.
  const [hashApplied, setHashApplied] = useState(false);
  useEffect(() => {
    if (hashApplied || !meta) return;
    const h = parseHash();
    if (h.view) setView(h.view);
    if (h.season) setSelectedSeason(h.season);
    if (h.week) setSelectedWeek(h.week);
    if (h.sub) {
      // For My League the trailing segment IS the league key; elsewhere it's a
      // sub-tab, which we drop unless the target view actually defines it — so a
      // stale or hand-edited link can't leave the URL claiming a tab we aren't on.
      if (h.view === 'league') setLeagueKey(h.sub);
      else if (h.view === 'season' && SEASON_GROUP_KEYS.includes(h.sub)) setSub(h.sub);
      else if (h.view === 'weekly' && WEEKLY_GROUP_KEYS.includes(h.sub)) setSub(h.sub);
    }
    if (h.view === 'market' || h.view === 'league' ||
        (h.view === 'weekly' && (h.sub === 'lineups' || h.sub === 'fantasy')))
      setProduct('fantasy');
    setHashApplied(true);
  }, [hashApplied, meta, setSelectedSeason, setSelectedWeek]);

  useEffect(() => {
    if (!hashApplied) return;
    const next = buildHash(view, selectedSeason, selectedWeek,
      view === 'league' ? activeLeague : sub);
    if (window.location.hash !== next) {
      window.history.replaceState(null, '', next);
    }
  }, [hashApplied, view, selectedSeason, selectedWeek, sub, activeLeague]);


  // Load season data when season changes
  useEffect(() => {
    if (selectedSeason > 0) {
      loadSeason(selectedSeason);
    }
  }, [selectedSeason, loadSeason]);

  const games = getGamesForWeek(selectedWeek);

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center text-loss">
        <p>Error: {error}</p>
      </div>
    );
  }

  return (
    <div className="min-h-screen">
      <Header
        selectedSeason={selectedSeason}
        selectedWeek={selectedWeek}
        availableSeasons={meta?.availableSeasons ?? []}
        maxWeek={maxWeek}
        onSeasonChange={setSelectedSeason}
        onWeekChange={setSelectedWeek}
        showControls={!isFantasy}
        product={product}
        onProductChange={changeProduct}
      />

      <main className="mx-auto max-w-7xl px-4 py-6 space-y-4">
        <div className="flex flex-wrap items-center gap-1 border-b border-border-light" aria-label={`${product === 'predictor' ? 'Predictor' : 'Fantasy HQ'} navigation`}>
          {(product === 'predictor' ? PREDICTOR_TABS : FANTASY_TABS).map(([k, label]) => (
            <button
              key={k}
              onClick={() => changeView(k, k === 'weekly' && product === 'fantasy' ? 'lineups' : null)}
              className={`px-3 py-1.5 text-sm font-medium -mb-px border-b-2 ${
                view === k
                  ? 'border-accent text-text-primary'
                  : 'border-transparent text-text-muted hover:text-text-secondary'
              }`}
            >
              {label}
            </button>
          ))}
          <span className="ml-auto hidden sm:block pb-1 text-[10px] text-text-muted">
            {product === 'predictor' ? 'model, markets, and performance' : 'teams, matchups, and roster decisions'}
          </span>
        </div>

        {view === 'market' ? (
          <MarketReport />
        ) : view === 'league' ? (
          <>
            {leagues.length > 1 && (
              <div className="flex flex-wrap gap-1.5">
                {leagues.map(l => {
                  const on = activeLeague === l.key;
                  const dyn = (l.market_format ?? '').startsWith('dynasty');
                  return (
                    <button
                      key={l.key}
                      onClick={() => setLeagueKey(l.key)}
                      className={`rounded-lg border px-3 py-1.5 text-left transition-colors ${
                        on
                          ? 'border-accent/50 bg-accent/10'
                          : 'border-border-light bg-bg-card/60 hover:border-border'
                      }`}
                    >
                      <span className={`block text-xs font-medium ${on ? 'text-text-primary' : 'text-text-secondary'}`}>
                        {l.label}
                      </span>
                      <span className="mt-0.5 flex items-center gap-1.5 text-[10px] text-text-muted">
                        <span className="uppercase tracking-wide">{l.platform}</span>
                        {l.market_format && (
                          <>
                            <span aria-hidden="true">·</span>
                            <span style={{ color: dyn ? '#58a6ff' : '#3fb950' }}>
                              {dyn ? 'dynasty' : 'season-long'}
                            </span>
                          </>
                        )}
                        {l.stale && <span className="text-loss">· stale</span>}
                      </span>
                    </button>
                  );
                })}
              </div>
            )}
            {activeLeague ? (
              <LeagueReport leagueKey={activeLeague} />
            ) : (
              <div className="rounded-2xl border border-border bg-bg-card p-6 text-center text-text-secondary">
                <p className="font-medium text-text-primary">No leagues configured</p>
                <p className="text-sm mt-1">
                  Add one to <code className="text-accent">fantasy_leagues.json</code> and run{' '}
                  <code className="text-accent">Rscript export_fantasy.R</code>.
                </p>
              </div>
            )}
          </>
        ) : view === 'weekly' ? (
          <WeeklyReport
            season={selectedSeason}
            week={selectedWeek}
            scope={product}
            group={sub}
            onGroupChange={setSub}
            onNavigate={(s, w) => { setSelectedSeason(s); setSelectedWeek(w); }}
            /* Split from the cards: a graded/previewed week ships its own
               bet_card, which carries EVERY qualifying bet. The season JSON
               attaches only the highest-EV bet per game, so rendering both would
               show 10 bets beside a header claiming 13. */
            betsFallback={loading ? null : (
              <TopBets games={games} bankroll={meta?.bankroll} unitSize={meta?.unitSize} />
            )}
            gamesFallback={loading ? (() => <Skeleton h={280} />) : ((markets) => (
              <div className="space-y-3">
                <div className="flex items-center justify-between">
                  <h3 className="text-sm font-semibold text-text-primary">{getWeekLabel(selectedWeek)}</h3>
                  <span className="text-xs text-text-muted">
                    {games.length} game{games.length !== 1 ? 's' : ''}
                  </span>
                </div>
                <GameGrid games={games} week={selectedWeek} polymarket={markets.polymarket} kalshi={markets.kalshi} kalshiWin={markets.kalshiWin} />
              </div>
            ))}
          />
        ) : (
          <SeasonReport
            performance={performance}
            season={selectedSeason}
            week={selectedWeek}
            group={sub}
            onGroupChange={setSub}
            onNavigate={(s, w) => { setSelectedSeason(s); setSelectedWeek(w); }}
          />
        )}
      </main>
    </div>
  );
}
