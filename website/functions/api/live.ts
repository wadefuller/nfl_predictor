// Small, allow-listed proxy for the public live scoring feeds. MFL does not
// grant CORS access to a Pages domain, so the browser cannot safely call it
// directly. This endpoint deliberately accepts only validated league/week
// parameters — it is not a general-purpose URL fetcher.
export async function onRequestGet(context: { request: Request; env?: Record<string, string> }): Promise<Response> {
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

  // Sleeper's league-wide stat feed is not scoped to a league, so it is the one
  // source here that takes no league id. It is keyed by sleeper player id and
  // covers the whole NFL in ~10KB, which is what lets a single fetch supply a
  // live stat line for every roster on the page — including the MFL league,
  // whose own feed carries scores but no box-score detail.
  if (source === 'sleeper_stats') {
    if (!validSeason || !validWeek)
      return new Response(JSON.stringify({ error: 'Invalid stats request.' }), { status: 400, headers });
    const statsResponse = await fetch(`https://api.sleeper.app/v1/stats/nfl/regular/${season}/${week}`,
      { headers: { Accept: 'application/json' } });
    return new Response(await statsResponse.text(), { status: statsResponse.status, headers });
  }

  // ESPN is the one authenticated source here. A private league answers an
  // anonymous request with 401, so the browser cannot read it at all and this
  // endpoint signs the request with the league owner's session cookies.
  //
  // Two deliberate constraints, because those cookies are a real credential and
  // this endpoint is public:
  //   · the league id must equal ESPN_LEAGUE_ID. Without that check anyone could
  //     use this Worker as a signed-in ESPN proxy for ANY league — the cookies
  //     would be doing the reading, not them.
  //   · only the normalised fields below are returned. The upstream payload is
  //     never passed through, so nothing incidental (member emails, ownership
  //     records) leaves with it.
  // Credentials are read from the environment and never echoed.
  if (source === 'espn') {
    const team = url.searchParams.get('team') ?? '';
    const env = (context as any).env ?? {};
    const s2 = env.ESPN_S2 ?? '';
    const swid = env.ESPN_SWID ?? '';
    const allowed = String(env.ESPN_LEAGUE_ID ?? '');
    if (!s2 || !swid || !allowed)
      return new Response(JSON.stringify({ error: 'ESPN live scoring is not configured.' }), { status: 503, headers });
    if (!validLeague || !validWeek || !validSeason || !/^\d{1,6}$/.test(team))
      return new Response(JSON.stringify({ error: 'Invalid ESPN request.' }), { status: 400, headers });
    if (league !== allowed)
      return new Response(JSON.stringify({ error: 'League not served by this endpoint.' }), { status: 403, headers });

    const upstreamUrl = `https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/${season}`
      + `/segments/0/leagues/${league}?view=mMatchupScore&view=mRoster&view=mTeam&scoringPeriodId=${week}`;
    const espnResponse = await fetch(upstreamUrl, {
      headers: { Cookie: `espn_s2=${s2}; SWID=${swid}`, Accept: 'application/json' },
    });
    if (!espnResponse.ok)
      return new Response(JSON.stringify({ error: `ESPN ${espnResponse.status}` }), { status: 502, headers });
    const payload: any = await espnResponse.json();

    // 20 = bench, 21 = injured reserve. Every other slot id is a starting spot,
    // flex positions included, so testing for the two non-starting ones is both
    // shorter and safe against ESPN adding another flex.
    const BENCH_SLOTS = new Set([20, 21]);
    const wk = Number(week);
    const teamsById = new Map<number, any>((payload?.teams ?? []).map((t: any) => [t.id, t]));
    const livePoints = (player: any) => {
      const stat = (player?.stats ?? []).find((s: any) => s.scoringPeriodId === wk && s.statSourceId === 0);
      return stat ? Number(stat.appliedTotal ?? 0) : null;
    };
    const readRoster = (teamId: number) => {
      const entries = teamsById.get(teamId)?.roster?.entries ?? [];
      const starters: string[] = [];
      const points: Record<string, number> = {};
      for (const entry of entries) {
        const player = entry?.playerPoolEntry?.player;
        const id = String(entry?.playerId ?? player?.id ?? '');
        if (!id) continue;
        if (!BENCH_SLOTS.has(Number(entry.lineupSlotId))) starters.push(id);
        const pts = livePoints(player);
        if (pts != null) points[id] = pts;
      }
      return { starters, points };
    };

    const target = Number(team);
    const matchup = (payload?.schedule ?? []).find((g: any) =>
      g?.matchupPeriodId === wk && (g?.home?.teamId === target || g?.away?.teamId === target));
    const mineSide = matchup?.home?.teamId === target ? matchup?.home : matchup?.away;
    const oppSide = matchup?.home?.teamId === target ? matchup?.away : matchup?.home;
    const oppId = Number(oppSide?.teamId ?? NaN);
    const mineRoster = readRoster(target);
    const oppRoster = Number.isFinite(oppId) ? readRoster(oppId) : { starters: [], points: {} };
    const teamName = (t: any) => t?.name ?? [t?.location, t?.nickname].filter(Boolean).join(' ') ?? null;

    return new Response(JSON.stringify({
      mine: Number(mineSide?.totalPointsLive ?? mineSide?.totalPoints ?? 0),
      opponent: Number(oppSide?.totalPointsLive ?? oppSide?.totalPoints ?? 0),
      opponentName: teamName(teamsById.get(oppId)),
      starters: mineRoster.starters, playerPoints: mineRoster.points,
      opponentStarters: oppRoster.starters, opponentPlayerPoints: oppRoster.points,
    }), { status: 200, headers });
  }

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
