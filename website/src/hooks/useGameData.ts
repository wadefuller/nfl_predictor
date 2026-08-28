import { useState, useEffect, useCallback } from 'react';
import type { MetaData, SeasonData, SeasonPerformance, Game } from '../types';
import { dataUrl } from '../lib/dataUrl';

interface UseGameDataReturn {
  meta: MetaData | null;
  seasonData: SeasonData | null;
  performance: SeasonPerformance | null;
  loading: boolean;
  error: string | null;
  getGamesForWeek: (week: number) => Game[];
  loadSeason: (season: number) => void;
}

export function useGameData(): UseGameDataReturn {
  const [meta, setMeta] = useState<MetaData | null>(null);
  const [seasonData, setSeasonData] = useState<SeasonData | null>(null);
  const [performance, setPerformance] = useState<SeasonPerformance | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Load meta on mount
  useEffect(() => {
    fetch(dataUrl('/data/meta.json'))
      .then(res => res.json())
      .then(setMeta)
      .catch(err => setError(`Failed to load metadata: ${err.message}`))
      .finally(() => setLoading(false));
  }, []);

  const loadSeason = useCallback((season: number) => {
    setLoading(true);
    setError(null);

    Promise.all([
      fetch(dataUrl(`/data/seasons/${season}.json`)).then(res => {
        if (!res.ok) throw new Error(`Season ${season} not found`);
        return res.json();
      }),
      // Performance is optional (preseason). Also guard against Vite's SPA
      // fallback, which serves index.html with HTTP 200 for missing files —
      // res.json() would choke on the HTML, so swallow parse errors too.
      fetch(dataUrl(`/data/performance/${season}.json`))
        .then(res => (res.ok ? res.json() : null))
        .catch(() => null),
    ])
      .then(([seasonJson, perfJson]) => {
        setSeasonData(seasonJson);
        setPerformance(perfJson);
      })
      .catch(err => setError(err.message))
      .finally(() => setLoading(false));
  }, []);

  const getGamesForWeek = useCallback(
    (week: number): Game[] => {
      if (!seasonData) return [];
      return seasonData.games.filter(g => g.week === week);
    },
    [seasonData]
  );

  return { meta, seasonData, performance, loading, error, getGamesForWeek, loadSeason };
}
