# Anderson Vil — academic site

This Zola site is published at https://andersonvil.github.io/. The approved editorial version is `ad690757b16cf724453cc072c40989b8e4b2cb4d`; production changes are integrated into `main`.

Current maintenance, publishing and rollback instructions are in [guide-site.md](guide-site.md). Historical private review reports remain in `docs/` and are not served.

## Build and local preview

Requirements: Zola, Node.js, PowerShell; Python for a simple local HTTP server.

```powershell
npm ci
& .\scripts\build-production.ps1
python scripts/serve-preview.py --directory public-production --port 1114
```

Open `http://127.0.0.1:1114/`. On Windows, stop the HTTP server before rebuilding to avoid open-file locks, then start it again.
The server binds to loopback and adds a local noindex HTTP header. Production HTML is indexable, and robots.txt permits crawling.

The build renders the exact author Markdown retained in `docs/editorial-sources`, plus manuscript-verified supplements in `data/editorial-supplements.json`, with pinned KaTeX 0.16.22. Generated HTML+MathML, local fonts and CSS are committed. Zola still owns the site, routes and templates. HTML minification is disabled because its optimizer corrupted the KaTeX MathML/SVG markup.

## Checks

```powershell
& .\scripts\run-tests.ps1
```

Browser checks use Playwright from the desktop runtime, Chrome, and the local server. Set `CODEX_NODE_MODULES` to the runtime package directory, then run `node scripts/verify-production.js http://127.0.0.1:1114`. Public checks accept the HTTPS site URL and expected full source SHA as arguments. Reports and screenshots are written under `tmp/publication-qa/`.

Pushes to `main` and manual workflow runs build with Node.js 22 and Zola 0.19.2, check the generated artifact, then commit only its files to `gh-pages` without force-push. Pages retains its existing legacy branch source, `gh-pages:/`, with HTTPS. `deployment-commit.txt` identifies the served source commit. Private preview fixtures, internal documents and source files are excluded from the served output. The local backup and exact normal-commit rollback procedure are documented in `guide-site.md`.
