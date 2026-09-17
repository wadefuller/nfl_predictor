import { useState } from 'react';
import { clamp, hexToRgba, arr } from '../ui/primitives';
import { LeagueArchive } from './LeagueArchive';

// League-wide weekly recap. Every number here is rendered from the fact/matchup
// data, never from generated prose — the LLM overlay (when enabled) only supplies
// connective sentences, so a hallucinated figure can't reach the page.

const CAT_ACCENT: Record<string, string> = {
  manager: '#f778ba',
  matchup: '#58a6ff',
  luck: '#d29922',
  player: '#3fb950',
  preview: '#a371f7',
};

// Lineup efficiency is a magnitude on a fixed 0-100 scale: one hue, dark to
// light. Not a good/bad diverging scale — 79% isn't "bad", it's just lower.
const effColor = (pct: number) =>
  `color-mix(in oklab, #79c0ff ${Math.round(clamp((pct - 60) / 40) * 100)}%, #0d419d)`;

function EffBar({ pct, bench }: { pct: number | null; bench: number | null }) {
  if (pct === null || pct === undefined) return <span className="text-text-muted text-[11px]">–</span>;
  const c = effColor(pct);
  return (
    <span className="inline-flex items-center gap-2" title={`${pct}% lineup efficiency · ${bench ?? 0} left on bench`}>
      <span className="relative inline-block h-1.5 w-12 rounded-full overflow-hidden" style={{ background: 'rgba(255,255,255,0.07)' }}>
        <span className="absolute inset-y-0 left-0 rounded-full" style={{ width: `${clamp(pct)}%`, background: c }} />
      </span>
      <span className="tabular-nums text-[10px] text-text-muted">{pct.toFixed(0)}%</span>
    </span>
  );
}

function FactCard({ f }: { f: any }) {
  const accent = CAT_ACCENT[f.category] ?? '#8b949e';
  return (
    <div className="rounded-xl border border-border-light bg-bg-card/50 px-3 py-2.5"
      style={{ boxShadow: `inset 3px 0 0 ${accent}` }}>
      <div className="flex items-start justify-between gap-3">
        <p className="text-sm text-text-primary leading-snug">{f.headline}</p>
        <span className="shrink-0 rounded-full px-1.5 py-0.5 text-[9px] font-bold uppercase tracking-wider"
          style={{ color: accent, backgroundColor: hexToRgba(accent, 0.14) }}>
          {f.category}
        </span>
      </div>
      {f.players?.length > 0 && (
        <div className="mt-1.5 flex flex-wrap gap-1.5">
          {f.players.map((p: any, i: number) => (
            <span key={i} className="rounded px-1.5 py-0.5 text-[10px] text-text-secondary bg-white/5 tabular-nums">
              {p.player} <span className="text-text-muted">{p.pos}</span> {p.score}
            </span>
          ))}
        </div>
      )}
      {f.players_role === 'should_have_started' && (
        <p className="mt-1 text-[10px] text-text-muted">should have started</p>
      )}
    </div>
  );
}

// Rank movement: arrow + magnitude, so direction survives grayscale and CVD.
function Movement({ v }: { v: number | null }) {
  if (v === null || v === undefined) return <span className="text-[11px] text-text-muted">—</span>;
  if (v === 0) return <span className="text-[11px] text-text-muted">=</span>;
  const up = v > 0;
  const c = up ? '#3fb950' : '#f85149';
  return (
    <span className="tabular-nums text-[11px] font-semibold" style={{ color: c }}>
      {up ? '▲' : '▼'}{Math.abs(v)}
    </span>
  );
}

function MatchupCard({ m, blurb }: { m: any; blurb?: string }) {
  const homeWon = m.winner_id === m.home_id;
  const Side = ({ name, score, eff, bench, won }: any) => (
    <div className="flex items-center justify-between gap-2">
      <span className={`text-sm truncate ${won ? 'font-semibold text-text-primary' : 'text-text-secondary'}`}>{name}</span>
      <span className="flex items-center gap-2 shrink-0">
        <EffBar pct={eff} bench={bench} />
        <span className={`tabular-nums text-sm ${won ? 'font-bold text-text-primary' : 'text-text-secondary'}`}>
          {score.toFixed(1)}
        </span>
      </span>
    </div>
  );
  return (
    <div className="rounded-xl border border-border-light bg-bg-card/40 px-3 py-2.5 space-y-1.5">
      <Side name={m.home} score={m.home_score} eff={m.eff_h} bench={m.bench_h} won={homeWon} />
      <Side name={m.away} score={m.away_score} eff={m.eff_a} bench={m.bench_a} won={!homeWon} />
      {blurb && <p className="text-[11px] text-text-muted leading-snug pt-1 border-t border-border-light/60">{blurb}</p>}
    </div>
  );
}

export function LeagueReportSection({ data, onNavigate }:
  { data: any; onNavigate?: (s: number, w: number) => void }) {
  const leagues: any[] = data?.leagues ?? [];
  const [idx, setIdx] = useState(0);
  const lg = leagues[idx];
  if (!lg) return <p className="text-sm text-text-muted">No league data for this week.</p>;

  const prose = lg.prose ?? {};
  const refreshed = lg.source?.refreshed
    ? new Date(String(lg.source.refreshed).replace(/([+-]\d{2})(\d{2})$/, '$1:$2')).toLocaleString([], {
        month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit',
      })
    : data?.generated;
  const blurbFor = (m: any) =>
    arr(prose.blurbs).find((b: any) => b.game === `${m.home} vs ${m.away}`)?.text;

  return (
    <div className="space-y-4">
      {leagues.length > 1 && (
        <div className="flex flex-wrap gap-1.5">
          {leagues.map((l, i) => (
            <button key={l.league_id} onClick={() => setIdx(i)}
              className={`rounded-full border px-2.5 py-1 text-[11px] font-medium ${
                i === idx ? 'border-accent text-accent bg-accent/10'
                          : 'border-border bg-bg-card text-text-secondary hover:text-text-primary'}`}>
              {l.label}
            </button>
          ))}
        </div>
      )}

      <div className="flex flex-wrap items-center gap-x-2 gap-y-1 text-[11px] text-text-muted">
        <span className="rounded-full border border-border-light bg-bg-card/40 px-2 py-0.5 text-text-secondary">League-native scoring</span>
        <span>Week {lg.week} submitted lineups and roster snapshot</span>
        {refreshed && <span>· refreshed {refreshed}</span>}
      </div>

      {/* the column */}
      <div className="rounded-2xl border border-border bg-gradient-to-br from-bg-card to-bg-page/60 p-4">
        <h4 className="text-base font-bold text-text-primary tracking-tight leading-snug">
          {prose.headline}
        </h4>
        <p className="mt-2 text-sm text-text-secondary leading-relaxed">{prose.lede}</p>
        {prose.source && prose.source !== 'llm' && (
          <p className="mt-2 text-[10px] text-text-muted">
            {prose.source === 'template' ? 'Written from the fact list.' : 'Cached narrative.'}
          </p>
        )}
      </div>

      {/* power rankings */}
      {lg.power && (
        <div>
          <div className="text-[10px] uppercase tracking-[0.14em] text-text-muted mb-2">Power Rankings</div>
          <div className="rounded-2xl border border-border bg-bg-card/40 overflow-hidden">
            <div className="overflow-x-auto">
              <table className="w-full text-sm min-w-[520px]">
                <tbody>
                  {lg.power.rows.map((r: any) => (
                    <tr key={r.franchise_id} className="border-b border-border-light/60 last:border-0">
                      <td className="px-3 py-1.5 w-10 tabular-nums text-text-muted">{r.rank}</td>
                      <td className="px-3 py-1.5 w-16"><Movement v={r.movement} /></td>
                      <td className="px-3 py-1.5 text-text-primary">{r.franchise}</td>
                      <td className="px-3 py-1.5 w-20 text-right tabular-nums text-text-secondary">
                        {r.wins}–{r.games - r.wins}
                      </td>
                      <td className="px-3 py-1.5 w-24 text-right tabular-nums text-text-muted">
                        {r.points_for.toFixed(1)}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
          <p className="mt-1.5 text-[10px] text-text-muted">
            {Object.entries(lg.power.method.weights).map(([k, v]: any) => `${k} ${Math.round(v * 100)}%`).join(' · ')}
            {' — '}market {lg.power.method.market_component}
          </p>
        </div>
      )}

      {/* matchups */}
      <div>
        <div className="text-[10px] uppercase tracking-[0.14em] text-text-muted mb-2">Results</div>
        <div className="grid gap-2 sm:grid-cols-2">
          {arr(lg.matchups).map((m: any, i: number) => (
            <MatchupCard key={i} m={m} blurb={blurbFor(m)} />
          ))}
        </div>
        <p className="mt-2 text-[10px] text-text-muted">
          Bars show lineup efficiency — points scored as a share of the best lineup that roster could have started.
        </p>
      </div>

      {/* storylines */}
      <div>
        <div className="text-[10px] uppercase tracking-[0.14em] text-text-muted mb-2">Storylines</div>
        <div className="grid gap-2 sm:grid-cols-2">
          {arr(lg.facts).filter((f: any) => !f.duplicate_of).slice(0, 6).map((f: any, i: number) => (
            <FactCard key={i} f={f} />
          ))}
        </div>
      </div>

      {/* transaction beat */}
      {lg.transactions?.franchises?.length > 0 && (
        <div>
          <div className="text-[10px] uppercase tracking-[0.14em] text-text-muted mb-2">Transaction Beat</div>
          <div className="flex flex-wrap gap-1.5">
            {lg.transactions.franchises.slice(0, 6).map((f: any) => (
              <span key={f.franchise_id}
                className="rounded-lg border border-border-light bg-bg-card/50 px-2 py-1 text-[11px] text-text-secondary">
                {f.franchise} <span className="tabular-nums text-text-primary font-semibold">{f.moves}</span>
                <span className="text-text-muted"> moves</span>
                {f.trades > 0 && <span className="text-text-muted"> · {f.trades} trade</span>}
              </span>
            ))}
          </div>
          <p className="mt-1.5 text-[10px] text-text-muted">Value of moves {lg.transactions.valuation}.</p>
        </div>
      )}

      {/* next week */}
      {lg.preview && (
        <div>
          <div className="text-[10px] uppercase tracking-[0.14em] text-text-muted mb-2">
            Watch in Week {lg.preview.week}
          </div>
          <div className="grid gap-2 sm:grid-cols-2">
            {arr(lg.preview.facts).map((f: any, i: number) => <FactCard key={i} f={f} />)}
          </div>
          {lg.preview.bye_teams?.length > 0 && (
            <p className="mt-2 text-[10px] text-text-muted">
              On bye: {lg.preview.bye_teams.join(', ')}
            </p>
          )}
          <p className="mt-1 text-[10px] text-text-muted">
            Projected totals assume each roster starts its best possible lineup, so they run optimistic.
          </p>
        </div>
      )}

      <LeagueArchive season={lg.season} week={lg.week} onNavigate={onNavigate} />
    </div>
  );
}
