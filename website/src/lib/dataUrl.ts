/**
 * Resolve a data path against the deployment's base URL.
 *
 * Every JSON fetch in the app used a root-absolute path (`/data/...`), which is
 * correct only when the site is served from the domain root — true on Cloudflare
 * Pages, false on a GitHub Pages *project* site, where the app lives under
 * `/nfl_predictor/`. There a root-absolute fetch resolves to
 * `wadefuller.github.io/data/...` and 404s, so every panel silently renders empty.
 *
 * `import.meta.env.BASE_URL` is `/` under `vite dev` and whatever `base` is set to
 * in the production build, so one helper covers both deployments.
 */
export const dataUrl = (p: string): string =>
  `${import.meta.env.BASE_URL}${p.replace(/^\//, '')}`;
