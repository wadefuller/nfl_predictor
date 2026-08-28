import { useState, useEffect, useRef } from 'react';
import type { MetaData } from '../types';

interface UseSeasonWeekReturn {
  selectedSeason: number;
  selectedWeek: number;
  setSelectedSeason: (season: number) => void;
  setSelectedWeek: (week: number) => void;
  maxWeek: number;
}

export function useSeasonWeek(meta: MetaData | null): UseSeasonWeekReturn {
  const [selectedSeason, setSelectedSeason] = useState<number>(0);
  const [selectedWeek, setSelectedWeek] = useState<number>(1);

  // Seed the defaults from meta EXACTLY once. Re-seeding on every `meta` change
  // would stomp on a season/week that a permalink (or the user) has since set —
  // and under StrictMode the effect is invoked twice, so an unguarded version
  // silently reverted any hash-applied week on every cold load.
  const seeded = useRef(false);
  useEffect(() => {
    if (!meta || seeded.current) return;
    seeded.current = true;
    setSelectedSeason(meta.currentSeason);
    setSelectedWeek(meta.currentWeek || 1);
  }, [meta]);

  const maxWeek = meta?.weeksPerSeason?.[String(selectedSeason)] ?? 22;

  const handleSetSeason = (season: number) => {
    setSelectedSeason(season);
    // Reset to the last completed week or week 1
    if (meta && season === meta.currentSeason) {
      setSelectedWeek(meta.currentWeek || 1);
    } else {
      setSelectedWeek(1);
    }
  };

  return {
    selectedSeason,
    selectedWeek,
    setSelectedSeason: handleSetSeason,
    setSelectedWeek,
    maxWeek,
  };
}
