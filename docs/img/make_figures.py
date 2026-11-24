#!/usr/bin/env python3
"""Regenerate the README figures in this directory.

    python3 nfl_predictor/docs/img/make_figures.py

Reads existing files only (no model builds, no downloads). Every number drawn
is computed here from those files; the script fails if a season is missing.
Needs numpy. Honours NFL_STACK_ROOT like shared/paths.R.
"""
import csv, math, os, re, sqlite3, sys, datetime
from collections import Counter, defaultdict
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.environ.get("NFL_STACK_ROOT") or os.path.abspath(os.path.join(HERE, "..", "..", ".."))
P = lambda *a: os.path.join(ROOT, *a)
RNG = np.random.default_rng(20261111)
NBOOT = 2000
W = 640


def mdate(path):
    return datetime.date.fromtimestamp(os.path.getmtime(path)).strftime("%b %-d, %Y")


def need_seasons(found, want, what):
    missing = sorted(set(want) - set(found))
    if missing:
        sys.exit(f"{what}: missing seasons {missing}")


# ----------------------------------------------------------------- loading
def load_games():
    wf_path = P("nfl_predictor", "data", "walk_forward_predictions.csv")
    feat_path = P("nfl_predictor", "data", "game_w_features.csv")
    impl, n_feat_rows, n_feat_cols, fs = {}, 0, 0, set()
    with open(feat_path) as f:
        rd = csv.DictReader(f)
        n_feat_cols = len(rd.fieldnames)
        for x in rd:
            n_feat_rows += 1
            fs.add(int(x["game_id"][:4]))
            impl[x["game_id"]] = x["impl_prob_home"]
    rows = []
    for x in csv.DictReader(open(wf_path)):
        p = impl.get(x["game_id"])
        if p in (None, "", "NA"):
            sys.exit(f"no market probability for {x['game_id']}")
        rows.append((int(x["season"]), int(x["week"]), float(x["spread_line"]),
                     float(x["home_result"]), int(x["home_win_01"]),
                     float(x["nfl_predictor_line"]), float(x["pred_win_pct"]), float(p)))
    a = np.array(rows, dtype=float)
    need_seasons(a[:, 0].astype(int), range(2016, 2026), "game walk-forward")
    return dict(season=a[:, 0].astype(int), week=a[:, 1].astype(int), vegas_line=a[:, 2],
                margin=a[:, 3], win=a[:, 4], model_line=a[:, 5], p_model=a[:, 6],
                p_vegas=a[:, 7], run=mdate(wf_path), feat_rows=n_feat_rows,
                feat_cols=n_feat_cols, feat_span=(min(fs), max(fs)))


QS = [0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95]
QC = ["q05", "q10", "q25", "q50", "q75", "q90", "q95"]


def load_players():
    path = P("weekly_fantasy", "weekly_predictions.csv")
    cols = ["season", "week", "active", "half_ppr", "floor", "ceiling", "cond_mean",
            "roll_hppg_short", "season_proj_ppg"] + QC
    num, pos = [], []
    for x in csv.DictReader(open(path)):
        num.append([float(x[c]) for c in cols])
        pos.append(x["position"])
    a = np.array(num)
    d = {c: a[:, i] for i, c in enumerate(cols)}
    d["season"] = d["season"].astype(int); d["week"] = d["week"].astype(int)
    d["pos"] = np.array(pos); d["run"] = mdate(path)
    d["q"] = np.column_stack([d[c] for c in QC])
    need_seasons(d["season"], range(2017, 2026), "player walk-forward")
    return d


def cluster_boot(values, clusters, stat=np.mean):
    """95% interval for the mean of `values`, resampling whole clusters."""
    ids, inv = np.unique(clusters, return_inverse=True)
    sums = np.bincount(inv, weights=values); cnts = np.bincount(inv)
    k = len(ids)
    pick = RNG.integers(0, k, size=(NBOOT, k))
    est = sums[pick].sum(1) / cnts[pick].sum(1)
    return float(values.mean()), float(np.percentile(est, 2.5)), float(np.percentile(est, 97.5))


def wilson(k, n, z=1.96):
    if n == 0:
        return (0, 0)
    p = k / n; den = 1 + z * z / n
    c = (p + z * z / (2 * n)) / den
    h = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / den
    return c - h, c + h


# ----------------------------------------------------------------- svg kit
STYLE = """<style>
.bg{fill:#ffffff}.card{fill:#f6f8fa}.ink{fill:#1f2328}.muted{fill:#59636e}
.ink-s{stroke:#1f2328}.grid{stroke:#d1d9e0}.axis{stroke:#8c959f}
.model{fill:#2f6fd0}.model-s{stroke:#2f6fd0}.vegas{fill:#6e7781}.vegas-s{stroke:#6e7781}
.warn{fill:#b4532a}.warn-s{stroke:#b4532a}.none{fill:none}.hollow{fill:#ffffff}
.pQB{fill:#2f6fd0}.pQB-s{stroke:#2f6fd0}.pRB{fill:#1a7f64}.pRB-s{stroke:#1a7f64}
.pWR{fill:#8250df}.pWR-s{stroke:#8250df}.pTE{fill:#b4532a}.pTE-s{stroke:#b4532a}
.empty{fill:#e3e8ee}.test{fill:#1f2328}
text{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif}
.h{font-size:25px;font-weight:700}.sub{font-size:15px}.lab{font-size:13px;font-weight:700;letter-spacing:1.2px}
.t{font-size:15px}.tb{font-size:15px;font-weight:700}.s{font-size:14px}.sb{font-size:14px;font-weight:700}
.num{font-variant-numeric:tabular-nums}
@media (prefers-color-scheme: dark){
.bg{fill:#0d1117}.card{fill:#161b22}.ink{fill:#e6edf3}.muted{fill:#9198a1}
.ink-s{stroke:#e6edf3}.grid{stroke:#30363d}.axis{stroke:#6e7681}
.model{fill:#58a6ff}.model-s{stroke:#58a6ff}.vegas{fill:#9aa3ad}.vegas-s{stroke:#9aa3ad}
.warn{fill:#e58a5f}.warn-s{stroke:#e58a5f}.hollow{fill:#0d1117}
.pQB{fill:#58a6ff}.pQB-s{stroke:#58a6ff}.pRB{fill:#3fb950}.pRB-s{stroke:#3fb950}
.pWR{fill:#bc8cff}.pWR-s{stroke:#bc8cff}.pTE{fill:#e58a5f}.pTE-s{stroke:#e58a5f}
.empty{fill:#262d36}.test{fill:#e6edf3}
}
</style>"""


def esc(s):
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


class Svg:
    def __init__(self, h, label):
        self.h, self.label, self.o = h, label, []
        self.o.append(f'<rect width="{W}" height="{h}" rx="14" class="bg"/>')

    def add(self, s):
        self.o.append(s)

    def text(self, x, y, s, cls="t ink", anchor=None, extra=""):
        a = f' text-anchor="{anchor}"' if anchor else ""
        self.o.append(f'<text x="{x:.1f}" y="{y:.1f}" class="{cls}"{a}{extra}>{esc(s)}</text>')

    def line(self, x1, y1, x2, y2, cls="grid", w=1, extra=""):
        self.o.append(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" class="{cls}" stroke-width="{w}"{extra}/>')

    def rect(self, x, y, w, h, cls, rx=0, extra=""):
        self.o.append(f'<rect x="{x:.1f}" y="{y:.1f}" width="{max(w,0):.1f}" height="{max(h,0):.1f}" rx="{rx}" class="{cls}"{extra}/>')

    def circle(self, x, y, r, cls, extra=""):
        self.o.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" class="{cls}"{extra}/>')

    def path(self, pts, cls, w=2, extra=""):
        d = "M" + " L".join(f"{x:.1f},{y:.1f}" for x, y in pts)
        self.o.append(f'<path d="{d}" class="none {cls}" stroke-width="{w}" stroke-linejoin="round"{extra}/>')

    def title(self, head, sub):
        self.text(28, 46, head, "h ink")
        self.text(28, 72, sub, "sub muted")

    def card(self, y, h):
        self.rect(16, y, W - 32, h, "card", rx=12)

    def save(self, name):
        body = "\n".join(self.o)
        out = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {self.h}" role="img" '
               f'aria-label="{esc(self.label)}">\n{STYLE}\n{body}\n</svg>\n')
        with open(os.path.join(HERE, name), "w") as f:
            f.write(out)
        print(f"wrote {name}  ({len(out)//1024} KB)")


def fmt(n):
    return f"{int(n):,}"


def sgn(v, d):
    return f"{v:+.{d}f}".replace("-", "−")


# ----------------------------------------------------------------- figure 1
def fig1(G, Pl):
    n = len(G["win"])
    mae_m = np.abs(G["model_line"] - G["margin"]).mean()
    mae_v = np.abs(G["vegas_line"] - G["margin"]).mean()
    r = np.corrcoef(G["model_line"], G["vegas_line"])[0, 1]
    gap = np.abs(G["model_line"] - G["vegas_line"]).mean()

    s = Svg(868, f"Two prediction targets. Game margin: model line against the Vegas line for {n} "
                 f"out-of-sample games, correlation {r:.2f}. Player points: one stored quantile forecast.")
    s.title("Two targets, each with a benchmark", "Everything below is out-of-sample: predicted before the game was played.")

    # --- panel A: scatter
    s.card(92, 446)
    s.text(36, 122, f"GAME MARGIN · {fmt(n)} GAMES, 2016–2025", "lab muted")
    s.text(36, 146, "Spread = points the home team is expected to win by.", "s muted")
    x0, y0, side, lim = 86, 166, 316, 21
    sx = lambda v: x0 + (v + lim) / (2 * lim) * side
    sy = lambda v: y0 + side - (v + lim) / (2 * lim) * side
    for t in (-14, -7, 0, 7, 14):
        s.line(sx(t), y0, sx(t), y0 + side, "grid"); s.line(x0, sy(t), x0 + side, sy(t), "grid")
        s.text(sx(t), y0 + side + 18, sgn(t, 0) if t else "0", "s muted num", "middle")
        s.text(x0 - 8, sy(t) + 5, sgn(t, 0) if t else "0", "s muted num", "end")
    s.line(sx(-lim), sy(-lim), sx(lim), sy(lim), "axis", 1, ' stroke-dasharray="4 4"')
    pts = "".join(f'<circle cx="{sx(max(-lim,min(lim,a))):.1f}" cy="{sy(max(-lim,min(lim,b))):.1f}" r="1.6"/>'
                  for a, b in zip(G["vegas_line"], G["model_line"]))
    s.add(f'<g class="model" opacity=".45">{pts}</g>')
    s.text(x0 + side / 2, y0 + side + 40, "Vegas closing spread (points)", "s ink", "middle")
    s.text(34, y0 + side / 2, "Model spread (points)", "s ink", "middle",
           f' transform="rotate(-90 34 {y0 + side / 2})"')
    tx = x0 + side + 22
    s.text(tx, 196, "The model mostly", "t ink"); s.text(tx, 216, "agrees with Vegas", "t ink")
    s.text(tx, 250, f"r = {r:.3f}", "tb ink num")
    s.text(tx, 270, f"mean gap {gap:.2f} pts", "s muted num")
    s.text(tx, 316, "Error vs final margin", "t ink")
    s.text(tx, 336, "(mean absolute, pts)", "s muted")
    s.rect(tx, 350, 12, 12, "model", 2); s.text(tx + 18, 361, f"model  {mae_m:.2f}", "tb ink num")
    s.rect(tx, 374, 12, 12, "vegas", 2); s.text(tx + 18, 385, f"Vegas  {mae_v:.2f}", "tb ink num")
    s.text(tx, 424, "A tie. Figures 3 and 5", "s muted"); s.text(tx, 442, "test it properly.", "s muted")

    # --- panel B: one stored quantile forecast
    ex = None
    for x in csv.DictReader(open(P("weekly_fantasy", "weekly_predictions_2026_wk09.csv"))):
        if x["player_display_name"] == "Joe Burrow":
            ex = x
    actual = None
    for x in csv.DictReader(open(P("weekly_fantasy", "weekly_grade_2026_wk09.csv"))):
        if x["player"] == "Joe Burrow":
            actual = float(x["actual"])
    if ex is None or actual is None:
        sys.exit("worked example row not found")
    q = [float(ex[c]) for c in QC]
    s.card(552, 300)
    s.text(36, 582, "PLAYER POINTS · ONE FORECAST AS STORED", "lab muted")
    s.text(36, 606, "Joe Burrow (QB), Week 9 2026. Half-PPR fantasy points: yards and", "s muted")
    s.text(36, 624, "touchdowns, plus half a point per catch. Seven quantiles are predicted.", "s muted")
    ax0, ax1, ay = 56, 584, 700
    px = lambda v: ax0 + v / 40 * (ax1 - ax0)
    s.rect(px(q[0]), ay - 14, px(q[6]) - px(q[0]), 28, "model", 4, ' opacity=".22"')
    s.rect(px(q[1]), ay - 14, px(q[5]) - px(q[1]), 28, "model", 0, ' opacity=".32"')
    s.rect(px(q[2]), ay - 14, px(q[4]) - px(q[2]), 28, "model", 0, ' opacity=".55"')
    s.line(px(q[3]), ay - 19, px(q[3]), ay + 19, "ink-s", 3)
    for i, (lv, v) in enumerate(zip(QS, q)):
        up = i % 2 == 0
        s.line(px(v), ay - 14, px(v), ay - (38 if up else 20), "axis")
        s.text(px(v), ay - (43 if up else 24), f"{v:.1f}", "s ink num", "middle")
    s.text(px(q[0]) - 6, ay + 5, "5%", "s muted", "end"); s.text(px(q[6]) + 6, ay + 5, "95%", "s muted")
    s.line(ax0, ay + 44, ax1, ay + 44, "axis")
    for t in range(0, 41, 10):
        s.line(px(t), ay + 44, px(t), ay + 49, "axis"); s.text(px(t), ay + 66, t, "s muted num", "middle")
    # markers under the band
    rec, prior = float(ex["roll_hppg_short"]), float(ex["season_proj_ppg"])
    def tri(x, cls):
        s.add(f'<path d="M{x:.1f},{ay+20} l-6,11 h12 z" class="{cls}"/>')
    tri(px(actual), "warn"); tri(px(rec), "vegas"); tri(px(prior), "vegas")
    s.text(36, ay + 96, "Bands: 5–95%, 10–90%, 25–75%; thick line = median.", "s ink")
    s.add(f'<path d="M40,{ay+121} l-6,11 h12 z" class="warn"/>')
    s.text(52, ay + 132, f"actual {actual:.1f}", "s ink num")
    s.add(f'<path d="M170,{ay+121} l-6,11 h12 z" class="vegas"/>')
    s.text(182, ay + 132, f"naive benchmarks: last-3-game average {rec:.1f}, preseason {prior:.1f}", "s ink num")
    s.save("01-targets.svg")
    return dict(n=n, r=r, mae_m=mae_m, mae_v=mae_v, gap=gap)


# ----------------------------------------------------------------- figure 2
def fig2(G, Pl):
    con = sqlite3.connect(P("fantasy_db", "pbp.db"))
    plays, s0, s1, ns = con.execute("select count(*),min(season),max(season),count(distinct season) from pbp").fetchone()
    if ns != s1 - s0 + 1:
        sys.exit("pbp.db has a gap in seasons")
    con = sqlite3.connect(P("fantasy_db", "fantasy_football.db"))
    rw, r0, r1 = con.execute("select count(*),min(season),max(season) from rosters_weekly").fetchone()
    ps = con.execute("select count(*) from player_stats").fetchone()[0]
    feats = {x["feature"] for x in csv.DictReader(open(P("weekly_fantasy", "weekly_importance.csv")))}
    stab = next(csv.DictReader(open(P("nfl_predictor", "data", "glm_stability_summary.csv"))))
    nz = float(stab["nonzero_count_mean"])

    s = Svg(770, "Pipeline from public play-by-play data to the published site, with row counts at each stage.")
    s.title("From public data to a published forecast", "Row counts are from the files on disk when this figure was generated.")
    y = 94
    def stage(lab, h):
        nonlocal y
        s.card(y, h); s.text(36, y + 28, lab, "lab muted")
        top = y; y += h + 30
        if y < 760:
            s.line(W / 2, top + h + 4, W / 2, top + h + 24, "axis", 2)
            s.add(f'<path d="M{W/2-6},{top+h+18} l6,8 l6,-8 z" class="muted"/>')
        return top
    def box(x, yy, w, head, lines, cls="hollow"):
        s.rect(x, yy, w, 30 + 20 * len(lines), cls, 8, ' stroke-width="1"')
        s.add(f'<rect x="{x}" y="{yy}" width="{w}" height="{30 + 20*len(lines)}" rx="8" class="none axis" stroke-width="1"/>')
        s.text(x + 12, yy + 22, head, "tb ink")
        for i, l in enumerate(lines):
            s.text(x + 12, yy + 42 + 20 * i, l, "s muted num")

    t = stage("1 · PUBLIC SOURCES", 104)
    box(36, t + 40, 180, "Play-by-play", [f"{fmt(plays)} plays"])
    box(230, t + 40, 180, "Rosters, game info", [f"{fmt(rw)} roster-weeks"])
    box(424, t + 40, 180, "Betting lines", ["spread, total, moneyline"])
    t = stage("2 · WAREHOUSE (SQLITE)", 104)
    box(36, t + 40, 277, f"Plays, {s0}–{s1}", [f"{ns} seasons, one pinned schema"])
    box(327, t + 40, 277, f"Player-games, {r0}–{r1}", [f"{fmt(ps)} stat rows, one ID per player"])
    t = stage("3 · FEATURES (known before kickoff only)", 124)
    box(36, t + 40, 277, "Game table", [f"{fmt(G['feat_rows'])} games",
                                         f"{G['feat_span'][0]}–{G['feat_span'][1]}"])
    box(327, t + 40, 277, "Player-week table", [f"{fmt(len(Pl['pos']))} player-weeks in the backtest"])
    t = stage("4 · MODELS", 144)
    box(36, t + 40, 277, "Game: penalised regression", [
                                                        "margin → calibrated win probability",
                                                        "benchmark: Vegas closing line"])
    box(327, t + 40, 277, "Player: boosted quantile trees", ["one model per position and quantile",
                                                               "plus a will-he-play classifier",
                                                               "benchmark: preseason projection"])
    t = stage("5 · PUBLISHED", 104)
    box(36, t + 40, 568, "Static site, rebuilt at 06:30 and 17:30 every day", ["the finished week is graded against results every Tuesday"])
    s.h = y - 14
    s.o[0] = f'<rect width="{W}" height="{s.h}" rx="14" class="bg"/>'
    s.save("02-pipeline.svg")
    return dict(plays=plays, pbp_span=(s0, s1), roster_weeks=rw, stat_rows=ps, n_feat=len(feats), nz=nz)


# ----------------------------------------------------------------- figure 3
def fig3(G):
    n = len(G["win"]); cl = G["season"] * 100 + G["week"]
    bm = (G["p_model"] - G["win"]) ** 2; bv = (G["p_vegas"] - G["win"]) ** 2
    d, lo, hi = cluster_boot(bm - bv, cl)
    s = Svg(770, f"Reliability diagram for home win probability over {n} out-of-sample games. "
                 f"Brier score model {bm.mean():.4f}, market {bv.mean():.4f}.")
    s.title("Is the game model calibrated?", f"{fmt(n)} out-of-sample games, 2016–2025. Backtest run {G['run']}.")
    s.card(92, 662)
    x0, y0, side = 92, 122, 420
    sx = lambda v: x0 + v * side
    sy = lambda v: y0 + side - v * side
    for t in (0, .25, .5, .75, 1):
        s.line(sx(t), y0, sx(t), y0 + side, "grid"); s.line(x0, sy(t), x0 + side, sy(t), "grid")
        s.text(sx(t), y0 + side + 18, f"{int(t*100)}%", "s muted num", "middle")
        s.text(x0 - 8, sy(t) + 5, f"{int(t*100)}%", "s muted num", "end")
    s.line(sx(0), sy(0), sx(1), sy(1), "axis", 1, ' stroke-dasharray="4 4"')
    s.text(sx(.07), sy(.10), "perfect", "s muted", "middle", f' transform="rotate(-45 {sx(.07):.1f} {sy(.10):.1f})"')
    edges = np.linspace(0, 1, 11)
    out = {}
    for key, cls, off in (("p_vegas", "vegas", -4), ("p_model", "model", 4)):
        p = G[key]; b = np.clip(np.digitize(p, edges) - 1, 0, 9); pts, cnt = [], []
        for i in range(10):
            m = b == i; k = int(m.sum()); cnt.append(k)
            if k < 15:
                continue
            obs = G["win"][m].mean(); l, h = wilson(G["win"][m].sum(), k)
            x = sx(p[m].mean()) + off
            s.line(x, sy(l), x, sy(h), f"{cls}-s", 2)
            pts.append((x, sy(obs)))
        s.path(pts, f"{cls}-s", 2)
        for x, yv in pts:
            s.circle(x, yv, 4.5, cls)
        out[key] = cnt
    s.text(x0 + side / 2, y0 + side + 40, "Predicted probability the home team wins", "s ink", "middle")
    s.text(36, y0 + side / 2, "Share of those games the home team won", "s ink", "middle",
           f' transform="rotate(-90 36 {y0 + side / 2})"')
    s.circle(sx(.06), sy(.93), 5, "model"); s.text(sx(.06) + 12, sy(.93) + 5, "model", "tb ink")
    s.circle(sx(.06), sy(.87), 5, "vegas"); s.text(sx(.06) + 12, sy(.87) + 5, "Vegas (from moneyline)", "tb ink")
    s.text(sx(.06) - 5, sy(.81) + 5, "bars: 95% interval per bin", "s muted")
    # counts strip
    by = y0 + side + 62
    s.text(x0 - 8, by + 24, "games", "s muted", "end"); s.text(x0 - 8, by + 40, "per bin", "s muted", "end")
    mx = max(out["p_model"] + out["p_vegas"])
    for i in range(10):
        for j, (key, cls) in enumerate((("p_vegas", "vegas"), ("p_model", "model"))):
            k = out[key][i]; hgt = k / mx * 44
            s.rect(sx(i / 10) + 5 + j * 17, by + 46 - hgt, 15, hgt, cls)
        s.text(sx(i / 10 + .05), by + 62, out["p_model"][i], "s muted num", "middle")
    s.text(x0 + side + 12, by + 62, "model", "s muted")
    # verdict
    vy = by + 92
    s.text(36, vy, f"Brier score (lower is better): model {bm.mean():.4f}, Vegas {bv.mean():.4f}.", "t ink num")
    s.text(36, vy + 22, f"Difference {sgn(d,4)}, 95% interval {sgn(lo,4)} to {sgn(hi,4)} (weeks resampled).", "t ink num")
    verdict = ("Both are well calibrated. The interval spans zero: no measurable edge."
               if lo < 0 < hi else "Both are well calibrated; the difference excludes zero.")
    s.text(36, vy + 44, verdict, "tb ink")
    s.save("03-game-calibration.svg")
    return dict(brier_m=bm.mean(), brier_v=bv.mean(), d=d, lo=lo, hi=hi, bins_model=out["p_model"])


# ----------------------------------------------------------------- figure 4
def fig4(Pl):
    act = Pl["active"] == 1
    y = Pl["half_ppr"]; q = Pl["q"]; cl = Pl["season"] * 100 + Pl["week"]
    n_all, n_act = len(y), int(act.sum())
    cov80_act = cluster_boot(((y >= q[:, 1]) & (y <= q[:, 5]))[act].astype(float), cl[act])
    cov80_all = float(((y >= Pl["floor"]) & (y <= Pl["ceiling"])).mean())
    s = Svg(826, f"Quantile calibration for player forecasts over {n_act} player-weeks. "
                 f"The 10th to 90th percentile band covered {cov80_act[0]*100:.1f} percent.")
    s.title("Are the player ranges honest?",
            f"{fmt(n_act)} player-weeks where the player played, 2017–2025. Run {Pl['run']}.")
    # panel A: empirical - nominal by position
    s.card(92, 372)
    s.text(36, 122, "PREDICTED QUANTILE VS HOW OFTEN THE SCORE FELL BELOW IT", "lab muted")
    x0, x1, yt, yb = 92, 470, 146, 396
    emp = {}
    for p in ("QB", "RB", "WR", "TE"):
        m = act & (Pl["pos"] == p)
        emp[p] = [(float((y[m] <= q[m, j]).mean()), int(m.sum())) for j in range(7)]
    dev = [e[0] - lv for p in emp for e, lv in zip(emp[p], QS)]
    lim = max(0.04, math.ceil(max(abs(v) for v in dev) * 25) / 25)
    sx = lambda v: x0 + v * (x1 - x0)
    sy = lambda v: (yt + yb) / 2 - v / lim * (yb - yt) / 2
    step = 0.02 if lim <= 0.04 else 0.04
    t = -lim
    while t <= lim + 1e-9:
        s.line(x0, sy(t), x1, sy(t), "axis" if abs(t) < 1e-9 else "grid", 1.5 if abs(t) < 1e-9 else 1)
        s.text(x0 - 8, sy(t) + 5, sgn(t * 100, 0) if abs(t) > 1e-9 else "0", "s muted num", "end")
        t += step
    for lv in QS:
        s.line(sx(lv), yb, sx(lv), yb + 5, "axis")
    for lv in (0.05, 0.25, 0.50, 0.75, 0.95):
        s.text(sx(lv), yb + 22, f"{int(lv*100)}%", "s muted num", "middle")
    s.text((x0 + x1) / 2, yb + 44, "Predicted quantile", "s ink", "middle")
    s.text(36, (yt + yb) / 2, "Observed minus predicted (% points)", "s ink", "middle",
           f' transform="rotate(-90 36 {(yt + yb) / 2})"')
    s.text(x1 - 4, sy(lim) + 16, "scores too low", "s muted", "end")
    s.text(x1 - 4, sy(-lim) - 8, "scores too high", "s muted", "end")
    ends = []
    for p in ("QB", "RB", "WR", "TE"):
        pts = [(sx(lv), sy(e[0] - lv)) for lv, e in zip(QS, emp[p])]
        s.path(pts, f"p{p}-s", 2.5)
        for xx, yy in pts:
            s.circle(xx, yy, 3.5, f"p{p}")
        ends.append([pts[-1][1], p])
    ends.sort()
    for i in range(1, 4):                       # de-collide the direct labels
        ends[i][0] = max(ends[i][0], ends[i - 1][0] + 34)
    shift = max(0, ends[-1][0] - (yb - 16))
    for yy, p in ends:
        yy -= shift
        s.rect(x1 + 14, yy - 10, 12, 12, f"p{p}", 2)
        s.text(x1 + 32, yy + 1, p, "tb ink")
        s.text(x1 + 32, yy + 17, f"n = {fmt(emp[p][0][1])}", "s muted num")
    # panel B: PIT
    s.card(478, 332)
    s.text(36, 508, "WHERE ACTUAL SCORES LANDED INSIDE THE FORECAST (PIT)", "lab muted")
    s.text(36, 532, "If the forecasts were perfect, every bar would sit on the dashed line.", "s muted")
    edges = [0] + QS + [1]
    bins = (y[act][:, None] > q[act]).sum(1)
    share = np.bincount(bins, minlength=8) / n_act
    dens = [share[i] / (edges[i + 1] - edges[i]) for i in range(8)]
    px0, px1, pt, pb = 92, 600, 556, 706
    dm = max(1.6, max(dens) * 1.1)
    bx = lambda v: px0 + v * (px1 - px0)
    by = lambda v: pb - v / dm * (pb - pt)
    for t in (0, 0.5, 1.0, 1.5):
        if t <= dm:
            s.line(px0, by(t), px1, by(t), "grid"); s.text(px0 - 8, by(t) + 5, f"{t:.1f}", "s muted num", "end")
    for i in range(8):
        s.rect(bx(edges[i]) + 1, by(dens[i]), bx(edges[i + 1]) - bx(edges[i]) - 2, pb - by(dens[i]), "model")
    s.line(px0, by(1), px1, by(1), "ink-s", 1.5, ' stroke-dasharray="5 4"')
    for lv in (0.05, 0.25, 0.50, 0.75, 0.95):
        s.text(bx(lv), pb + 18, f"{int(lv*100)}%", "s muted num", "middle")
    s.text((px0 + px1) / 2, pb + 40, "Forecast percentile of the actual score", "s ink", "middle")
    s.text(36, (pt + pb) / 2, "Density", "s ink", "middle", f' transform="rotate(-90 36 {(pt + pb) / 2})"')
    c, l, h = cov80_act
    s.text(36, 772, f"10th–90th band: {c*100:.1f}% covered (target 80%; 95% interval {l*100:.1f}–{h*100:.1f}).", "tb ink num")
    s.text(36, 794, f"Including players who sat out and scored 0: {cov80_all*100:.1f}% of {fmt(n_all)}. Flattering, so not used.", "s muted num")
    s.save("04-player-coverage.svg")
    return dict(n_act=n_act, n_all=n_all, cov80_act=cov80_act, cov80_all=cov80_all, emp=emp, dens=dens)


# ----------------------------------------------------------------- figure 5
def fig5(G, Pl):
    s = Svg(932, "Walk-forward evaluation folds for both models, with the per-season difference "
                 "against the benchmark and bootstrap intervals.")
    s.title("Train on the past, test on the next season",
            "One row per fold. Right: model minus benchmark; left of 0 = model better.")
    RH = 24

    def section(top, head, first_train, tests, cal, deltas, pooled, unit, lim, dp, pdp, bench):
        nrow = len(tests)
        s.card(top, 134 + RH * (nrow + 1))
        s.text(36, top + 30, head, "lab muted")
        years = list(range(first_train, tests[-1] + 1))
        gx, cw = 82, 250 / len(years)
        gy = top + 92
        s.text(gx, gy - 12, str(first_train), "s muted num"); s.text(gx + 250, gy - 12, str(tests[-1]), "s muted num", "end")
        px0, px1 = 392, 600
        sx = lambda v: (px0 + px1) / 2 + max(-lim, min(lim, v)) / lim * (px1 - px0) / 2
        for t in (-lim, 0, lim):
            s.line(sx(t), gy - 4, sx(t), gy + RH * (nrow + 1) + 4, "axis" if t == 0 else "grid", 1.5 if t == 0 else 1)
            s.text(sx(t), gy + RH * (nrow + 1) + 22, sgn(t, dp) if t else "0", "s muted num", "middle")
        s.text((px0 + px1) / 2, gy + RH * (nrow + 1) + 42, unit, "s ink", "middle")
        for i, S in enumerate(tests):
            yy = gy + i * RH
            s.text(gx - 8, yy + 15, S, "s ink num", "end")
            for j, yr in enumerate(years):
                cls = "empty"
                if yr < S: cls = "model"
                if cal and yr == S - 1: cls = "vegas"
                if yr == S: cls = "test"
                op = ' opacity=".55"' if cls == "model" else ""
                s.rect(gx + j * cw, yy + 3, cw - 1.5, RH - 7, cls, 1.5, op)
            d, lo, hi = deltas[S]
            s.line(sx(lo), yy + 11, sx(hi), yy + 11, "ink-s", 1.5)
            s.circle(sx(d), yy + 11, 4, "model" if d < 0 else "warn")
        yy = gy + nrow * RH + 4
        s.text(gx - 8, yy + 15, "all", "sb ink", "end")
        s.text(gx, yy + 15, f"pooled: {sgn(pooled[0], pdp)} ({sgn(pooled[1], pdp)} to {sgn(pooled[2], pdp)})", "sb ink num")
        s.line(sx(pooled[1]), yy + 11, sx(pooled[2]), yy + 11, "ink-s", 3)
        s.add(f'<path d="M{sx(pooled[0]):.1f},{yy+3} l7,8 l-7,8 l-7,-8 z" class="{"model" if pooled[0] < 0 else "warn"}"/>')
        ly = top + 52
        s.rect(36, ly - 11, 12, 12, "model", 2, ' opacity=".55"'); s.text(53, ly, "train", "s ink")
        xx = 100
        if cal:
            s.rect(xx, ly - 11, 12, 12, "vegas", 2); s.text(xx + 17, ly, "calibrate", "s ink"); xx += 92
        s.rect(xx, ly - 11, 12, 12, "test", 2); s.text(xx + 17, ly, "test", "s ink")
        s.text(600, ly, bench, "s muted", "end")
        return top + 134 + RH * (nrow + 1)

    # game model: Brier(model) - Brier(Vegas) per holdout season
    bd = (G["p_model"] - G["win"]) ** 2 - (G["p_vegas"] - G["win"]) ** 2
    gd = {}
    for S in range(2016, 2026):
        m = G["season"] == S
        gd[S] = cluster_boot(bd[m], G["week"][m])
    gp = cluster_boot(bd, G["season"] * 100 + G["week"])
    glim = 0.01
    end = section(92, f"GAME MODEL · {fmt(len(bd))} TEST GAMES", 2008, list(range(2016, 2026)), True,
                  gd, gp, "Brier score, model − Vegas", glim, 2, 4, "benchmark: Vegas moneyline")

    # player model: MAE(conditional mean) - MAE(best naive baseline), players who played
    act = Pl["active"] == 1
    y = Pl["half_ppr"][act]; se = Pl["season"][act]; wk = Pl["week"][act]
    e_model = np.abs(Pl["cond_mean"][act] - y)
    base = {"last-3-game average": np.abs(Pl["roll_hppg_short"][act] - y),
            "preseason projection": np.abs(Pl["season_proj_ppg"][act] - y)}
    best = min(base, key=lambda k: base[k].mean())
    pdiff = e_model - base[best]
    pd_ = {}
    for S in range(2017, 2026):
        m = se == S
        pd_[S] = cluster_boot(pdiff[m], wk[m])
    pp = cluster_boot(pdiff, se * 100 + wk)
    plim = math.ceil(max(abs(v[1]) for v in pd_.values()) * 10 + 1) / 10
    section(end + 14, f"PLAYER MODEL · {fmt(len(y))} TEST PLAYER-WEEKS", 2011, list(range(2017, 2026)), False,
            pd_, pp, "Error in points, model − naive", plim, 1, 2, f"benchmark: {best}")
    s.h = end + 14 + 134 + RH * 10 + 16
    s.o[0] = f'<rect width="{W}" height="{s.h}" rx="14" class="bg"/>'
    s.save("05-walk-forward.svg")
    return dict(game_pooled=gp, game=gd, player_pooled=pp, player=pd_, best=best,
                mae_model=float(e_model.mean()), mae_base={k: float(v.mean()) for k, v in base.items()},
                game_better=sum(1 for v in gd.values() if v[0] < 0))


# ----------------------------------------------------------------- figure 6
LEDGERS = [("Game model", "nfl_predictor"), ("Player weekly", "weekly_fantasy"),
           ("Season-long", "fantasy_db"), ("Expected points", "xfp"), ("College prospects", "college_nfl_model")]
VERDICTS = ["win", "tie", "harm", "unclear", "invalid"]
# Rows plotted in the lower panel of figure 6: ledger ids listed in a file kept
# outside this repo. Deltas and intervals are parsed from the ledger itself.
FOREST_IDS = P("docs", "ablations", "readme_figure_rows.txt")
NUM = r"[+\-−]?\d*\.\d+"


def ledger_rows(mod):
    rows = []
    for l in open(P("docs", "ablations", f"{mod}_ledger.md")):
        if re.match(r"\|\s*20\d\d-", l):
            c = [x.strip() for x in l.strip().strip("|").split("|")]
            if len(c) >= 11:
                rows.append(c)
    return rows


def fig6():
    tally = {}
    for name, mod in LEDGERS:
        c = Counter()
        for r in ledger_rows(mod):
            v = re.sub(r"[*`]", "", r[9]).split()[0].lower() if r[9] else "unclear"
            c[v if v in VERDICTS else "unclear"] += 1
        tally[name] = c
    total = Counter()
    for c in tally.values():
        total.update(c)
    N = sum(total.values())
    wf = {r[1]: r for r in ledger_rows("weekly_fantasy")}
    forest = []
    f = lambda t: float(t.replace("−", "-"))
    ids = [l.strip() for l in open(FOREST_IDS) if l.strip() and not l.startswith("#")]
    for lid in ids:
        if lid not in wf:
            sys.exit(f"ledger id {lid} not found")
        r = wf[lid]; metric, dcell, ccell = r[6], r[7], r[8]
        scale = 2.0 if metric.lower().lstrip().startswith(("pinball", "mean pinball")) else 1.0  # CRPS = 2 x pinball
        if not metric.lower().lstrip().startswith(("pinball", "mean pinball", "crps")):
            sys.exit(f"{lid}: unexpected metric {metric}")
        m = re.search(NUM, dcell)
        if dcell.startswith("~0"):
            d = 0.0
        elif m is None:
            sys.exit(f"{lid}: no numeric delta in ledger row")
        else:
            d = f(m.group()) * scale
        ci = re.search(r"\[\s*(" + NUM + r")\s*,\s*(" + NUM + r")\s*\]", ccell)
        lo, hi = (f(ci.group(1)) * scale, f(ci.group(2)) * scale) if ci else (None, None)
        v = re.sub(r"[*`]", "", r[9]).split()[0].lower()
        forest.append((d, lo, hi, v))
    forest.sort(key=lambda t: t[0])

    s = Svg(1010, f"{N} recorded experiments across five models: {total['win']} wins, {total['tie']} ties, "
                 f"{total['harm']} harmful. Forest plot of feature experiments on the player model.")
    s.title("Most ideas did not work", f"{N} recorded experiments across five models. Every result is kept, wins or not.")
    VCLS = {"win": ("model", ""), "tie": ("vegas", ' opacity=".55"'), "harm": ("warn", ""),
            "unclear": ("empty", ""), "invalid": ("empty", ' opacity=".5"')}
    s.card(92, 284)
    s.text(36, 122, "VERDICTS BY MODEL", "lab muted")
    lx = 36
    for v in VERDICTS:
        cls, op = VCLS[v]
        s.rect(lx, 136, 12, 12, cls, 2, op)
        lab = f"{v} {total[v]}"
        s.text(lx + 17, 147, lab, "s ink num"); lx += 30 + len(lab) * 8.2
    bx0, bx1 = 170, 566
    mx = max(sum(c.values()) for c in tally.values())
    for i, (name, _) in enumerate(LEDGERS):
        yy = 168 + i * 34; c = tally[name]; x = bx0
        s.text(bx0 - 10, yy + 17, name, "s ink", "end")
        for v in VERDICTS:
            w = c[v] / mx * (bx1 - bx0); cls, op = VCLS[v]
            s.rect(x, yy, max(w - 1.5, 0), 24, cls, 2, op)
            if v in ("win", "harm") and w > 18:
                s.add(f'<text x="{x + w/2:.1f}" y="{yy+17}" class="sb num" text-anchor="middle" fill="#ffffff">{c[v]}</text>')
            x += w
        s.text(x + 6, yy + 17, sum(c.values()), "s muted num")
    s.text(36, 358, "Win = interval excludes zero, or a material full-sample gain.", "s muted")

    top = 390; RH = 24
    s.card(top, 76 + RH * len(forest) + 112)
    s.text(36, top + 30, f"PLAYER MODEL · {len(forest)} FEATURE GROUPS, ONE ROW EACH", "lab muted")
    s.text(36, top + 54, "CRPS (distribution error; lower is better), 2017–2025 walk-forward.", "s muted")
    px0, px1, lo_ax, hi_ax = 170, 600, -0.022, 0.016
    sx = lambda v: px0 + (max(lo_ax, min(hi_ax, v)) - lo_ax) / (hi_ax - lo_ax) * (px1 - px0)
    gy = top + 76
    for t in (-0.02, -0.01, 0, 0.01):
        s.line(sx(t), gy - 4, sx(t), gy + RH * len(forest) + 4, "axis" if t == 0 else "grid", 1.5 if t == 0 else 1)
        s.text(sx(t), gy + RH * len(forest) + 22, sgn(t, 2) if t else "0", "s muted num", "middle")
    for i, (d, lo, hi, v) in enumerate(forest):
        yy = gy + i * RH + 12
        s.text(px0 - 12, yy + 5, f"group {i + 1}", "s muted num", "end")
        cls = {"win": "model", "harm": "warn"}.get(v, "vegas")
        if d < lo_ax:                                   # off the axis: arrow plus the value
            s.add(f'<path d="M{px0+2},{yy} l10,-6 v12 z" class="{cls}"/>')
            s.text(px0 + 18, yy + 5, f"{sgn(d,3)} (off scale)", "s ink num")
        elif lo is not None:
            s.line(sx(lo), yy, sx(hi), yy, "ink-s", 2)
            s.circle(sx(d), yy, 4.5, cls)
        else:
            s.add(f'<circle cx="{sx(d):.1f}" cy="{yy}" r="4" class="hollow {cls}-s" stroke-width="2"/>')
    by = gy + RH * len(forest) + 44
    s.text((px0 + px1) / 2, by, "← better          worse →", "s ink", "middle")
    s.circle(42, by + 22, 4.5, "vegas"); s.line(50, by + 22, 62, by + 22, "ink-s", 2)
    s.text(70, by + 27, "with 95% interval", "s ink")
    s.add(f'<circle cx="216" cy="{by+22}" r="4" class="hollow vegas-s" stroke-width="2"/>')
    s.text(228, by + 27, "no interval recorded", "s ink")
    s.circle(392, by + 22, 4.5, "model"); s.text(402, by + 27, "win", "s ink")
    s.circle(446, by + 22, 4.5, "warn"); s.text(456, by + 27, "harm", "s ink")
    s.circle(510, by + 22, 4.5, "vegas"); s.text(520, by + 27, "tie / unclear", "s ink")
    s.text(36, by + 52, "Each change is measured against its own same-run baseline.", "s muted")
    s.h = top + 76 + RH * len(forest) + 112 + 16
    s.o[0] = f'<rect width="{W}" height="{s.h}" rx="14" class="bg"/>'
    s.save("06-what-was-tried.svg")
    return dict(total=dict(total), N=N, tally={k: dict(v) for k, v in tally.items()}, forest=forest)


if __name__ == "__main__":
    G, Pl = load_games(), load_players()
    for name, res in (("fig1", fig1(G, Pl)), ("fig2", fig2(G, Pl)), ("fig3", fig3(G)),
                      ("fig4", fig4(Pl)), ("fig5", fig5(G, Pl)), ("fig6", fig6())):
        print(name, res)
