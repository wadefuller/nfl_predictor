import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

// `base` differs by deployment target, so it is driven by an env var rather than
// hardcoded: GitHub Pages serves this as a PROJECT site under /nfl_predictor/,
// while Cloudflare Pages serves it from the domain root. Setting a fixed base
// would break whichever target it was not set for.
//   - GitHub Actions sets VITE_BASE=/nfl_predictor/
//   - `npm run build` and `npm run deploy` (Cloudflare) leave it unset -> '/'
export default defineConfig({
  base: process.env.VITE_BASE ?? '/',
  plugins: [react(), tailwindcss()],
})
