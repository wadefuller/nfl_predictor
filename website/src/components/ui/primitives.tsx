import { getTeamColor } from '../../data/teamColors';

// ---- formatting / color helpers --------------------------------------------
export const num = (v: any, d = 1) =>
  v === null || v === undefined || (typeof v === 'number' && isNaN(v)) ? '–' : Number(v).toFixed(d);
export const clamp = (v: number, lo = 0, hi = 100) => Math.max(lo, Math.min(hi, v));
export const hexToRgba = (color: string, a: number) => {
  // Accept "#rgb" / "#rrggbb" AND "rgb(r,g,b)" (readableColor may emit either).
  const m = /^rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/i.exec(color || '');
  if (m) return `rgba(${m[1]},${m[2]},${m[3]},${a})`;
  const h = (color || '#888').replace('#', '');
  const f = h.length === 3 ? h.split('').map(x => x + x).join('') : h;
  const n = parseInt(f, 16);
  return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${a})`;
};
// R serializes a single-row data frame as a one-element array; unwrap to the object.
export const obj = (x: any) => (Array.isArray(x) ? x[0] ?? {} : x ?? {});

/** Coerce an R-generated field to an array.
 *  jsonlite emits `{}` (not `[]`) for an empty/NULL list-column, and `{}` is
 *  truthy — so `arr(x).map` throws and, uncaught, blanks the whole page.
 *  Anything that came from R and is iterated MUST go through this. */
export const arr = (x: any): any[] => (Array.isArray(x) ? x : []);

// ---- readable team colors ---------------------------------------------------
// Raw NFL palette colors like NE #002244 / HOU #03202F are near-invisible as
// text or fills on the dark background. Lighten any color below a target
// luminance by blending toward white just enough to clear the threshold, so
// bright colors (KC red, CIN orange) pass through untouched.
const parseHex = (hex: string) => {
  const h = (hex || '#888').replace('#', '');
  const f = h.length === 3 ? h.split('').map(x => x + x).join('') : h;
  const n = parseInt(f, 16);
  return [(n >> 16) & 255, (n >> 8) & 255, n & 255] as const;
};
const toHex = (r: number, g: number, b: number) =>
  '#' + [r, g, b].map(c => Math.max(0, Math.min(255, c)).toString(16).padStart(2, '0')).join('');
// Always returns a #hex so it composes with hexToRgba().
export function readableColor(hex: string, target = 0.42): string {
  const [r, g, b] = parseHex(hex);
  const lum = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255;
  if (lum >= target) return hex.startsWith('#') ? hex : toHex(r, g, b);
  const t = (target - lum) / (1 - lum); // linear blend toward white to hit target
  const mix = (c: number) => Math.round(c + (255 - c) * t);
  return toHex(mix(r), mix(g), mix(b));
}
export const readableTeamColor = (team: string, target = 0.42) =>
  readableColor(getTeamColor(team), target);

// ---- shared visual primitives ----------------------------------------------
export function StatTile({ label, value, sub, accent, hint }:
  { label: string; value: React.ReactNode; sub?: React.ReactNode; accent?: string; hint?: string }) {
  return (
    <div className="stat-tile flex flex-col rounded-xl border border-border bg-gradient-to-b from-bg-card to-bg-card/30 px-3 py-2.5">
      <div className="text-[10px] uppercase tracking-wider text-text-muted">{label}</div>
      <div className="text-xl font-bold tabular-nums leading-tight mt-0.5" style={accent ? { color: accent } : undefined}>{value}</div>
      {sub && <div className="text-[11px] text-text-secondary mt-0.5">{sub}</div>}
      {/* Explainer sits at the tile's foot — a number nobody can interpret is
          decoration. mt-auto keeps it flush across tiles of uneven height. */}
      {hint && (
        <div className="mt-auto pt-1.5 text-[10px] leading-snug text-text-muted/70">{hint}</div>
      )}
    </div>
  );
}

export function TeamChip({ team, big }: { team: string; big?: boolean }) {
  const c = readableTeamColor(team, 0.35);
  return (
    <span className={`inline-flex items-center gap-1.5 rounded-md border font-bold tracking-wide ${big ? 'px-2 py-1 text-sm' : 'px-1.5 py-0.5 text-[11px]'}`}
      style={{ backgroundColor: hexToRgba(c, 0.16), borderColor: hexToRgba(c, 0.5), color: '#e6edf3' }}>
      <span className="rounded-full" style={{ backgroundColor: c, width: big ? 8 : 6, height: big ? 8 : 6 }} />
      {team}
    </span>
  );
}

export function EdgeBadge({ v, label = 'edge' }: { v: number; label?: string }) {
  const big = Math.abs(v) >= 2;
  const cls = big ? (v > 0 ? 'text-win border-win/40 bg-win/10' : 'text-loss border-loss/40 bg-loss/10')
                  : 'text-text-muted border-border bg-bg-card-hover';
  return <span className={`rounded-full border px-2 py-0.5 text-[11px] font-semibold tabular-nums ${cls}`}>{label} {v > 0 ? '+' : ''}{num(v)}</span>;
}

// floor → ceiling range bar with a projection marker, tinted to team color.
// Band uses a solid readable tint (was a near-invisible 0.4 alpha); the proj
// marker is a bright tick with a dark outline so it pops against the band.
export function RangeBar({ floor, proj, ceiling, max, color }:
  { floor: number; proj: number; ceiling: number; max: number; color: string }) {
  const c = readableColor(color, 0.5);
  const p = (v: number) => clamp((v / max) * 100);
  const left = p(floor);
  const width = Math.max(clamp(p(ceiling) - p(floor), 0, 100), 2); // keep a sliver even when floor≈ceiling
  return (
    <div className="relative h-2.5 w-full rounded-full overflow-hidden"
      style={{ background: 'rgba(255,255,255,0.07)', boxShadow: 'inset 0 0 0 1px rgba(255,255,255,0.04)' }}>
      {/* floor→ceiling band: brighter toward the middle so the spread is obvious */}
      <div className="absolute top-0 bottom-0 rounded-full"
        style={{ left: `${left}%`, width: `${width}%`,
          background: `linear-gradient(90deg, ${hexToRgba(c, 0.45)}, ${hexToRgba(c, 0.85)}, ${hexToRgba(c, 0.45)})` }} />
      {/* projection marker */}
      <div className="absolute top-1/2 -translate-y-1/2 rounded-full"
        style={{ left: `calc(${p(proj)}% - 1.5px)`, width: 3, height: 16, background: c,
          boxShadow: '0 0 0 1px rgba(13,17,23,0.85)' }} />
    </div>
  );
}

// Diverging mini-bar for signed rank deltas: bar grows right (accent) for
// positive, left (amber) for negative, around a center spine. Instant
// direction+magnitude read without parsing the number.
export function DeltaBar({ v, max = 25 }: { v: number; max?: number }) {
  const pct = clamp((Math.abs(v) / max) * 50, 0, 50);
  const pos = v > 0;
  const color = v === 0 ? 'var(--color-text-muted)' : pos ? '#58a6ff' : '#d29922';
  return (
    <span className="inline-flex items-center gap-1.5">
      <span className="relative inline-block h-2 w-14 rounded-full bg-bg-card-hover overflow-hidden align-middle">
        <span className="absolute top-0 bottom-0 w-px bg-border" style={{ left: '50%' }} />
        {v !== 0 && (
          <span className="absolute top-0 bottom-0 rounded-full"
            style={pos ? { left: '50%', width: `${pct}%`, background: color }
                       : { right: '50%', width: `${pct}%`, background: color }} />
        )}
      </span>
      <span className="tabular-nums text-[11px] font-medium" style={{ color }}>{v > 0 ? `+${v}` : v}</span>
    </span>
  );
}

// Pulsing placeholder while a report's JSON loads.
export function Skeleton({ h = 120, className = '' }: { h?: number; className?: string }) {
  return <div className={`animate-pulse rounded-2xl border border-border-light bg-bg-card/50 ${className}`} style={{ height: h }} />;
}

// Compact page hero: small uppercase kicker (which report), big contextual
// title (season/week — the thing that changes), optional badge + right slot.
// Replaces the old pattern of repeating the tab name as a giant H2.
export function PageHero({ kicker, title, badge, right, children }:
  { kicker: string; title: React.ReactNode; badge?: React.ReactNode; right?: React.ReactNode; children?: React.ReactNode }) {
  return (
    <div className="page-hero relative overflow-hidden rounded-2xl border border-border bg-gradient-to-br from-bg-card to-bg-page/60 p-4 space-y-3">
      <div className="flex items-center justify-between flex-wrap gap-2">
        <div>
          <div className="text-[10px] uppercase tracking-[0.14em] text-text-muted">{kicker}</div>
          <div className="flex items-center gap-2.5 mt-0.5">
            <h2 className="text-xl font-bold text-text-primary tracking-tight">{title}</h2>
            {badge}
          </div>
        </div>
        {right}
      </div>
      {children}
    </div>
  );
}

// Segmented sub-navigation inside a tab. Keeps long report pages from becoming
// one endless scroll: the page shows one group at a time instead of everything.
export function SubNav<T extends string>({ items, value, onChange }:
  { items: { key: T; label: string; accent?: string; count?: number }[]; value: T; onChange: (k: T) => void }) {
  return (
    <div className="report-subnav flex flex-wrap gap-1 rounded-xl border border-border-light bg-bg-card/40 p-1">
      {items.map(it => {
        const on = it.key === value;
        return (
          <button key={it.key} onClick={() => onChange(it.key)} aria-pressed={on}
            className={`rounded-lg px-3 py-1.5 text-xs font-medium transition-colors ${
              on ? 'bg-bg-card text-text-primary shadow-sm' : 'text-text-muted hover:text-text-secondary'}`}
            style={on && it.accent ? { boxShadow: `inset 2px 0 0 ${it.accent}` } : undefined}>
            {it.label}
            {it.count != null && <span className="ml-1.5 text-[10px] text-text-muted tabular-nums">{it.count}</span>}
          </button>
        );
      })}
    </div>
  );
}

// dual horizontal compare bar (e.g., model vs vegas accuracy)
export function CompareBar({ aLabel, aVal, bLabel, bVal, aColor = '#58a6ff', bColor = '#6e7681', fmt = (v: number) => `${(v * 100).toFixed(1)}%` }:
  { aLabel: string; aVal: number; bLabel: string; bVal: number; aColor?: string; bColor?: string; fmt?: (v: number) => string }) {
  const max = Math.max(aVal, bVal, 0.0001);
  return (
    <div className="space-y-1.5">
      {[[aLabel, aVal, aColor], [bLabel, bVal, bColor]].map(([l, v, c]: any, i) => (
        <div key={i} className="flex items-center gap-2">
          <span className="w-12 text-[10px] text-text-muted">{l}</span>
          <div className="flex-1 h-2.5 rounded-full bg-bg-card-hover overflow-hidden">
            <div className="h-full rounded-full" style={{ width: `${clamp((v / max) * 100)}%`, background: c }} />
          </div>
          <span className="w-12 text-right text-[11px] font-semibold tabular-nums" style={{ color: c }}>{fmt(v)}</span>
        </div>
      ))}
    </div>
  );
}
