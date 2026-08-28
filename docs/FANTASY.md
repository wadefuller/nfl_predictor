# Fantasy section

A self-hosted take on what LeagueLogs sells: a **market value index** plus a
**league dashboard**, rendered as two extra tabs on the existing website.

```bash
./run_pipeline.sh fantasy
```

## What it does

`export_fantasy.R` writes JSON into `website/public/data/fantasy/`, which the
React app fetches at runtime — the same R → JSON → React flow the rest of the
site uses. There is no server and no database.

### Market index

Values come from the **FantasyCalc** public API, which derives them from actual
completed trades. Six formats are pulled, each parameterized to how it's really
played:

| Key | Format | Teams | QBs | PPR |
|---|---|---|---|---|
| `dynasty_sf` | Dynasty Superflex | 10 | 2 | 1.0 |
| `dynasty_1qb` | Dynasty 1QB | 12 | 1 | 1.0 |
| `redraft_sf` | Redraft Superflex | 12 | 2 | 1.0 |
| `redraft_ppr` | Redraft PPR | 12 | 1 | 1.0 |
| `redraft_half` | Redraft Half PPR | 12 | 1 | 0.5 |
| `redraft_std` | Redraft Standard | 12 | 1 | 0.0 |

`dynasty_sf` is set to **10 teams / 2 QB**, matching UNH league 65804 — so the
QB scarcity premium in those numbers is the one that actually applies to your
roster, not a generic 12-team index.

Each player gets a raw `value` plus a 0–100 `index` (linear in value, so the
numbers stay additive: two players summing to 60 really do equal one 60). Rookie
and future draft picks are included as assets.

### League dashboard

`ffscrapr` pulls live league state. MFL and Sleeper both go through the same
`ff_*` verbs, so adding a Sleeper league is a config entry, not new code.

Market values are joined onto rosters by the platform's own player id
(`mflId` / `sleeperId`, both supplied by FantasyCalc — ~99% match rate on MFL
65804). That join is what makes the interesting columns possible:

- **Power** — franchises ranked by market value of assets held, not by a 0-0
  preseason record. `Starters` is the best startable lineup by value, using the
  `lineup` block in the config (SuperFlex-aware).
- **Val/$** — market value per salary dollar. Only meaningful on cap leagues,
  and it's the column that answers "who is actually efficient", as opposed to
  "who just spent the most".
- **Positions** — heatmap shaded *within* each position column, so a column
  reads as "who is strong at RB" rather than being swamped by WRs costing more
  than TEs league-wide.
- **Activity** / **History** — transaction feed and all-time records.

## Configuration

`fantasy_leagues.json`:

```json
{
  "leagues": [
    {
      "key": "unhffl",
      "label": "UNH Fantasy Football",
      "platform": "mfl",
      "league_id": "65804",
      "season": 2026,
      "history_seasons": 5,
      "enabled": true,
      "market_format": "dynasty_sf",
      "lineup": { "QB": 1, "RB": 2, "WR": 2, "TE": 1, "FLEX": 2, "SUPERFLEX": 1 }
    }
  ]
}
```

To add a Sleeper league, append an entry with `"platform": "sleeper"` and the
numeric id from `sleeper.com/leagues/<id>`. Multiple leagues render as a
selector row above the dashboard.

## Caching — read this before re-running

**MFL rate-limits hard and bans for an extended window once you trip it.** Every
league call therefore goes through a disk cache at `data/fantasy_cache/`, TTL 6
hours, so iterating on the website costs zero API calls:

```bash
FANTASY_TTL_HOURS=0 ./run_pipeline.sh fantasy   # force a refresh
```

Completed prior seasons are cached permanently (they never change) — the
`unhffl_<year>_standings.rds` files. Deleting those forces a re-pull and is the
easiest way to get banned again; don't, unless a past season was wrong.

If a league pull fails, the previous export is **kept** rather than overwritten,
and its `meta.json` entry is flagged `stale: true`. A transient API failure
degrades to slightly-old data, never to a blank page.

Market values have no such limit and refresh on every run. A dated snapshot of
each format is appended to `data/fantasy_snapshots/<format>.csv`, building a
longitudinal record so trends can eventually be computed locally instead of
relying on the vendor's 30-day number.

## Known gaps

- Risers/fallers use FantasyCalc's `trend30Day`. The local snapshot history is
  being accumulated but isn't used for trends yet.
- `market_format` is per league; a Sleeper redraft league should point at a
  `redraft_*` format rather than the dynasty default.
- No trade analyzer or draft grader — the value join needed for both is in
  place, but the UI isn't built.
