// readableTeamColor lifts near-black team colors (NE, HOU navy) to a minimum
// luminance so the bar segments stay distinguishable from the dark background.
import { readableTeamColor } from '../ui/primitives';

interface WinProbabilityBarProps {
  awayTeam: string;
  homeTeam: string;
  awayWinPct: number;
  homeWinPct: number;
}

export function WinProbabilityBar({
  awayTeam,
  homeTeam,
  awayWinPct,
  homeWinPct,
}: WinProbabilityBarProps) {
  const awayPct = Math.round(awayWinPct * 100);
  const homePct = Math.round(homeWinPct * 100);

  return (
    <div className="relative h-6 w-full rounded-sm overflow-hidden flex text-[11px] font-semibold">
      <div
        className="flex items-center justify-start pl-2 text-white transition-all duration-300"
        style={{
          width: `${awayPct}%`,
          backgroundColor: readableTeamColor(awayTeam, 0.28),
          minWidth: awayPct > 15 ? undefined : '36px',
        }}
      >
        {awayPct > 15 && <span>{awayPct}%</span>}
      </div>
      <div
        className="flex items-center justify-end pr-2 text-white transition-all duration-300"
        style={{
          width: `${homePct}%`,
          backgroundColor: readableTeamColor(homeTeam, 0.28),
          minWidth: homePct > 15 ? undefined : '36px',
        }}
      >
        {homePct > 15 && <span>{homePct}%</span>}
      </div>
    </div>
  );
}
