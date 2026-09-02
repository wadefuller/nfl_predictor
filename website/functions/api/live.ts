// Small, allow-listed proxy for the public live scoring feeds. MFL does not
// grant CORS access to a Pages domain, so the browser cannot safely call it
// directly. This endpoint deliberately accepts only validated league/week
// parameters — it is not a general-purpose URL fetcher.
export async function onRequestGet(context: { request: Request }): Promise<Response> {
  const url = new URL(context.request.url);
  const source = url.searchParams.get('source');
  const league = url.searchParams.get('league') ?? '';
  const week = url.searchParams.get('week') ?? '';
  const season = url.searchParams.get('season') ?? '';
  const headers = {
    'Access-Control-Allow-Origin': '*',
    'Cache-Control': 'public, max-age=20, s-maxage=20',
    'Content-Type': 'application/json; charset=utf-8',
  };
  const validLeague = /^\d{4,20}$/.test(league);
  const validWeek = /^(?:[1-9]|1\d|2[0-2])$/.test(week);
  const validSeason = /^20\d{2}$/.test(season);
  if (!validLeague || !validWeek || (source === 'mfl' && !validSeason))
    return new Response(JSON.stringify({ error: 'Invalid live-score request.' }), { status: 400, headers });

  let upstream: string;
  if (source === 'sleeper') {
    upstream = `https://api.sleeper.app/v1/league/${league}/matchups/${week}`;
  } else if (source === 'mfl') {
    upstream = `https://api.myfantasyleague.com/${season}/export?TYPE=liveScoring&L=${league}&W=${week}&DETAILS=1&JSON=1`;
  } else {
    return new Response(JSON.stringify({ error: 'Unknown live-score source.' }), { status: 400, headers });
  }
  const response = await fetch(upstream, { headers: { Accept: 'application/json' } });
  return new Response(await response.text(), { status: response.status, headers });
}
