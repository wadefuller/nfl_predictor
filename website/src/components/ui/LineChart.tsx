import { useCallback, useLayoutEffect, useRef, useState } from 'react';
import { hexToRgba } from './primitives';

export interface Series {
  name: string;
  color: string;
  data: (number | null)[]; // aligned to the x array; null = gap
  width?: number;
  dash?: boolean;
  dot?: boolean;
}

interface LineChartProps {
  x: number[];
  series: Series[];
  height?: number;
  yFmt?: (n: number) => string;
  yDomain?: [number, number];
  baseline?: number;
  xPrefix?: string;
  endLabels?: boolean; // print each line's final value at the line end
  /** Plot never gets flatter than this width:height ratio; height grows instead. */
  maxAspect?: number;
  /** Don't stretch past this pixel width — the chart centres in wider containers. */
  maxWidth?: number;
}

const VW_FALLBACK = 720;
/** Ceiling on the width-driven height, so a wide chart can't become a tower. */
const MAX_H = 420;

/**
 * Track the rendered pixel width of an element. The chart draws in a viewBox
 * sized to match, so the SVG-user-unit -> pixel scale stays 1:1 and text,
 * strokes and dash patterns never stretch on wide screens.
 */
function useWidth() {
  const ref = useRef<HTMLDivElement>(null);
  const [w, setW] = useState(VW_FALLBACK);

  // Only commits a genuine change, so the post-render check below converges in
  // one extra render instead of looping.
  const measure = useCallback(() => {
    const el = ref.current;
    if (!el) return;
    const cw = Math.round(el.getBoundingClientRect().width);
    if (cw > 0) setW(prev => (prev === cw ? prev : cw));
  }, []);

  useLayoutEffect(() => {
    measure();
    const el = ref.current;
    if (!el) return;
    const ro = new ResizeObserver(measure);
    ro.observe(el);
    window.addEventListener('resize', measure);
    document.addEventListener('visibilitychange', measure);
    return () => {
      ro.disconnect();
      window.removeEventListener('resize', measure);
      document.removeEventListener('visibilitychange', measure);
    };
  }, [measure]);

  // Backstop: a hidden document has its rendering lifecycle suspended, and
  // ResizeObserver callbacks are never delivered while that lasts. A chart that
  // mounts in a background tab would otherwise keep the fallback width and draw
  // letterboxed. Re-measuring after every render lets it self-correct.
  useLayoutEffect(() => { measure(); });

  return [ref, w] as const;
}

export function LineChart({ x, series, height = 200, yFmt = n => `${n}`, yDomain, baseline, xPrefix = '', endLabels = true, maxAspect = 3.4, maxWidth = 1400 }: LineChartProps) {
  const [wrapRef, W] = useWidth();
  const padL = 40, padR = endLabels ? 52 : 14, padT = 12, padB = 22;
  // `height` is the floor. Wide containers get a taller plot rather than a
  // flat ribbon that squashes the series into a few vertical pixels.
  const H = Math.round(Math.max(height, Math.min(W / maxAspect, MAX_H)));
  const flat = series.flatMap(s => s.data.filter((v): v is number => v != null));
  let yMin = yDomain ? yDomain[0] : Math.min(...flat, baseline ?? Infinity);
  let yMax = yDomain ? yDomain[1] : Math.max(...flat, baseline ?? -Infinity);
  if (!isFinite(yMin) || !isFinite(yMax)) { yMin = 0; yMax = 1; }
  if (yMin === yMax) { yMin -= 1; yMax += 1; }
  const xMin = x[0] ?? 0, xMax = x[x.length - 1] ?? 1;

  const sx = (i: number) => padL + ((x[i] - xMin) / Math.max(1, xMax - xMin)) * (W - padL - padR);
  const sy = (v: number) => padT + (1 - (v - yMin) / (yMax - yMin)) * (H - padT - padB);

  const path = (data: (number | null)[]) => {
    let d = '', pen = false;
    data.forEach((v, i) => {
      if (v == null) { pen = false; return; }
      d += `${pen ? 'L' : 'M'}${sx(i).toFixed(1)} ${sy(v).toFixed(1)} `;
      pen = true;
    });
    return d.trim();
  };

  // Keep gridline density roughly constant in pixels as the chart resizes.
  const nY = Math.max(3, Math.min(7, Math.round((H - padT - padB) / 60)));
  const yTicks = Array.from({ length: nY + 1 }, (_, i) => yMin + ((yMax - yMin) * i) / nY);
  const nX = Math.max(4, Math.min(x.length, Math.round((W - padL - padR) / 110)));
  const xTickIdx = x.map((_, i) => i).filter((_, i) => i % Math.ceil(x.length / nX) === 0 || i === x.length - 1);

  return (
    <div ref={wrapRef} className="w-full" style={{ maxWidth, marginInline: 'auto' }}>
    <svg viewBox={`0 0 ${W} ${H}`} width="100%" height={H} style={{ display: 'block' }}>
      {yTicks.map((t, i) => (
        <g key={i}>
          <line x1={padL} x2={W - padR} y1={sy(t)} y2={sy(t)} stroke="var(--color-border-light)" strokeWidth={1} />
          <text x={padL - 6} y={sy(t) + 3} textAnchor="end" fontSize={9} fill="var(--color-text-muted)">{yFmt(t)}</text>
        </g>
      ))}
      {baseline != null && (
        <line x1={padL} x2={W - padR} y1={sy(baseline)} y2={sy(baseline)} stroke="var(--color-text-muted)" strokeWidth={1} strokeDasharray="3 3" />
      )}
      {xTickIdx.map(i => (
        <text key={i} x={sx(i)} y={H - 6} textAnchor="middle" fontSize={9} fill="var(--color-text-muted)">{xPrefix}{x[i]}</text>
      ))}
      {series.map((s, i) => (
        <g key={i}>
          <path d={path(s.data)} fill="none" stroke={s.color} strokeWidth={s.width ?? 2}
            strokeDasharray={s.dash ? '5 4' : undefined} strokeLinejoin="round" strokeLinecap="round"
            opacity={s.width && s.width < 2 ? 0.7 : 1} />
          {(() => {
            const li = [...s.data.keys()].reverse().find(k => s.data[k] != null);
            if (li == null) return null;
            const lx = sx(li), ly = sy(s.data[li] as number);
            const minor = !!(s.width && s.width < 2);
            return (
              <g>
                {(s.dot ?? true) && <circle cx={lx} cy={ly} r={minor ? 2 : 3} fill={s.color} />}
                {endLabels && (
                  <text x={lx + 6} y={ly + (minor ? -3 : 3) + (i % 2 ? 8 : 0)} fontSize={9}
                    fontWeight={minor ? 400 : 600} fill={s.color}>
                    {yFmt(s.data[li] as number)}
                  </text>
                )}
              </g>
            );
          })()}
        </g>
      ))}
    </svg>
    </div>
  );
}

export function Legend({ items }: { items: { name: string; color: string; bold?: boolean }[] }) {
  return (
    <div className="flex flex-wrap gap-x-3 gap-y-1">
      {items.map((it, i) => (
        <span key={i} className={`inline-flex items-center gap-1.5 text-[11px] ${it.bold ? 'text-text-primary font-semibold' : 'text-text-secondary'}`}>
          <span className="inline-block rounded-full" style={{ width: 8, height: 8, background: it.color, boxShadow: it.bold ? `0 0 0 2px ${hexToRgba(it.color, 0.3)}` : undefined }} />
          {it.name}
        </span>
      ))}
    </div>
  );
}
