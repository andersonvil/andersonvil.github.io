# Phase 4B implementation plan

## Scope

Build a complete, local-only, noindex preview of Anderson Vil's academic site on `codex/phase-4b-preview`. Preserve Zola and the existing GitHub Pages publication mechanism. Do not deploy.

## Implementation sequence

1. Replace the theme-dependent presentation with repository-owned Zola templates and a small CSS/JavaScript layer.
2. Create one factual source of truth for identity, research, documents, profiles, teaching states, code/data states, Insights, and Talks.
3. Build the required routes and direct downloads, including three paper pages, one teaching resource, two Insights, Talks, preview index, 404, and legacy-route handling.
4. Isolate synthetic fixtures under `content/preview-synthetic/` and `static/files/demo/`; register every fixture in `data/preview_inventory.toml`.
5. Add preview and production build commands. Production must fail while any fixture or blocked publication status remains.
6. Validate builds, links, downloads, redirects, English-only public UI, structured data, responsive layouts, keyboard navigation, demo execution, and production blocking.

## Visual thesis

An editorial research notebook: high-contrast navy and paper-white surfaces, restrained copper accents, serif research titles, compact sans-serif navigation, and a recurring money/time measure motif. The design is academic, direct, image-free until portrait rights are confirmed, and optimized for fast scanning by search committees.
