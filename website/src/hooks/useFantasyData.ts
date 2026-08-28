import { useState, useEffect, useCallback, useRef } from 'react';
import type { FantasyMeta, MarketFormat, MoversData, LeagueData } from '../types/fantasy';
import { dataUrl } from '../lib/dataUrl';

// Vite's dev server answers missing files with index.html at HTTP 200, so a
// bare res.json() would throw a confusing parse error. Treat non-JSON as absent.
async function getJson<T>(url: string): Promise<T | null> {
  try {
    const res = await fetch(url);
    if (!res.ok) return null;
    const text = await res.text();
    if (text.trimStart().startsWith('<')) return null;
    return JSON.parse(text) as T;
  } catch {
    return null;
  }
}

export function useFantasyMeta() {
  const [meta, setMeta] = useState<FantasyMeta | null>(null);
  const [loading, setLoading] = useState(true);
  useEffect(() => {
    getJson<FantasyMeta>(dataUrl('/data/fantasy/meta.json'))
      .then(setMeta)
      .finally(() => setLoading(false));
  }, []);
  return { meta, loading };
}

// Market formats are ~120KB each; cache per format so switching tabs back and
// forth doesn't refetch.
export function useMarket(format: string) {
  const [data, setData] = useState<MarketFormat | null>(null);
  const [loading, setLoading] = useState(true);
  const cache = useRef<Record<string, MarketFormat>>({});

  useEffect(() => {
    let cancelled = false;
    if (cache.current[format]) {
      setData(cache.current[format]);
      setLoading(false);
      return;
    }
    setLoading(true);
    getJson<MarketFormat>(dataUrl(`/data/fantasy/market/${format}.json`)).then(d => {
      if (cancelled) return;
      if (d) cache.current[format] = d;
      setData(d);
      setLoading(false);
    });
    return () => { cancelled = true; };
  }, [format]);

  return { data, loading };
}

export function useMovers() {
  const [data, setData] = useState<MoversData | null>(null);
  const [loading, setLoading] = useState(true);
  useEffect(() => {
    getJson<MoversData>(dataUrl('/data/fantasy/movers.json'))
      .then(setData)
      .finally(() => setLoading(false));
  }, []);
  return { data, loading };
}

export function useLeague(key: string | null) {
  const [data, setData] = useState<LeagueData | null>(null);
  const [loading, setLoading] = useState(true);
  const cache = useRef<Record<string, LeagueData>>({});

  const load = useCallback((k: string) => {
    if (cache.current[k]) { setData(cache.current[k]); setLoading(false); return; }
    setLoading(true);
    getJson<LeagueData>(dataUrl(`/data/fantasy/leagues/${k}.json`)).then(d => {
      if (d) cache.current[k] = d;
      setData(d);
      setLoading(false);
    });
  }, []);

  useEffect(() => {
    if (!key) { setData(null); setLoading(false); return; }
    load(key);
  }, [key, load]);

  return { data, loading };
}
