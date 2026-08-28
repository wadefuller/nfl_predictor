import { clamp, hexToRgba } from '../ui/primitives';

// ---- color roles -----------------------------------------------------------
// Only two encodings carry color here, each with one job:
//   tier  -> sequential (magnitude): one hue, light..dark, monotonic lightness
//   trend -> diverging (polarity): the site's win/loss pair + an arrow glyph,
//            so direction is never color-alone.
// Position is deliberately NOT color-coded: no 4-hue set clears colorblind
// separation on an all-pairs basis, and the two-letter label already names it.

// GitHub-blue sequential ramp, brightest = tier 1 (best).
const TIER_RAMP = ['#79c0ff', '#58a6ff', '#388bfd', '#1f6feb', '#1158c7', '#0d419d'];

export const tierColor = (tier: number | null | undefined) => {
  if (tier === null || tier === undefined || !isFinite(tier)) return '#6e7681';
  return TIER_RAMP[Math.min(Math.max(Math.round(tier) - 1, 0), TIER_RAMP.length - 1)];
};

export const POSITIONS = ['QB', 'RB', 'WR', 'TE', 'PICK'] as const;

export function PosChip({ pos }: { pos: string | null }) {
  if (!pos) return null;
  const isPick = pos === 'PICK';
  return (
    <span
      className={`inline-block rounded px-1.5 py-0.5 text-[10px] font-bold tracking-wide tabular-nums ${
        isPick ? 'text-text-muted' : 'text-text-secondary'
      }`}
      style={{ backgroundColor: 'rgba(255,255,255,0.06)', boxShadow: 'inset 0 0 0 1px rgba(255,255,255,0.06)' }}
    >
      {isPick ? 'PICK' : pos}
    </span>
  );
}

export function TierBadge({ tier }: { tier: number | null }) {
  if (tier === null || tier === undefined) return <span className="text-text-muted text-[11px]">–</span>;
  const c = tierColor(tier);
  return (
    <span
      className="inline-block rounded px-1.5 py-0.5 text-[10px] font-semibold tabular-nums"
      style={{ backgroundColor: hexToRgba(c, 0.16), color: c, boxShadow: `inset 0 0 0 1px ${hexToRgba(c, 0.4)}` }}
    >
      T{tier}
    </span>
  );
}

// Magnitude bar for the 0-100 market index. 4px rounded data-end anchored to a
// zero baseline; the value is labeled so the bar is redundant, not load-bearing.
export function IndexBar({ v, color = '#58a6ff', width = 72 }: { v: number | null; color?: string; width?: number }) {
  if (v === null || v === undefined) return <span className="text-text-muted text-[11px]">–</span>;
  return (
    <span className="inline-flex items-center gap-2 align-middle">
      <span
        className="relative inline-block h-2 rounded-full overflow-hidden"
        style={{ width, background: 'rgba(255,255,255,0.07)' }}
      >
        <span
          className="absolute left-0 top-0 bottom-0 rounded-full"
          style={{ width: `${clamp(v)}%`, background: color }}
        />
      </span>
      <span className="tabular-nums text-[11px] font-semibold text-text-secondary w-8 text-right">
        {v.toFixed(1)}
      </span>
    </span>
  );
}

// Diverging polarity pill. Arrow glyph + sign carry direction independently of
// hue, so this stays readable in grayscale and for CVD readers.
export function TrendPill({ pct, compact }: { pct: number | null; compact?: boolean }) {
  if (pct === null || pct === undefined || !isFinite(pct)) {
    return <span className="text-text-muted text-[11px]">–</span>;
  }
  const flat = Math.abs(pct) < 0.5;
  const up = pct > 0;
  const color = flat ? '#8b949e' : up ? '#3fb950' : '#f85149';
  const glyph = flat ? '·' : up ? '▲' : '▼';
  return (
    <span
      className={`inline-flex items-center gap-1 rounded-full font-semibold tabular-nums ${
        compact ? 'text-[10px] px-1.5 py-0.5' : 'text-[11px] px-2 py-0.5'
      }`}
      style={{ color, backgroundColor: hexToRgba(color, 0.12), boxShadow: `inset 0 0 0 1px ${hexToRgba(color, 0.32)}` }}
    >
      <span aria-hidden="true">{glyph}</span>
      {up && !flat ? '+' : ''}{pct.toFixed(1)}%
    </span>
  );
}

// Segmented filter row — one row above the content, per interaction spec.
export function Segmented<T extends string>({ value, options, onChange }:
  { value: T; options: readonly (readonly [T, string])[]; onChange: (v: T) => void }) {
  return (
    <div className="inline-flex flex-wrap gap-1 rounded-lg border border-border-light bg-bg-card/60 p-1">
      {options.map(([k, label]) => (
        <button
          key={k}
          onClick={() => onChange(k)}
          aria-pressed={value === k}
          className={`rounded-md px-2.5 py-1 text-xs font-medium transition-colors ${
            value === k
              ? 'bg-bg-card-hover text-text-primary shadow-[inset_0_0_0_1px_rgba(255,255,255,0.08)]'
              : 'text-text-muted hover:text-text-secondary'
          }`}
        >
          {label}
        </button>
      ))}
    </div>
  );
}

export const fmtValue = (v: number | null | undefined) =>
  v === null || v === undefined ? '–' : Math.round(v).toLocaleString();

export const fmtMoney = (v: number | null | undefined) =>
  v === null || v === undefined ? '–' : `$${Math.round(v)}`;
