# Anderson Vil — private academic preview

This checkout is a Zola site. The final private revision is on `codex/final-private-v5`.
No push, merge, or public deployment is part of this task.

The review report, coverage matrix, source decisions, remaining inputs and recovery procedure are in [docs/final-v5-review-report.md](docs/final-v5-review-report.md).

## Build and local preview

Requirements: Zola, Node.js, PowerShell; Python for a simple local HTTP server.

```powershell
npm ci
& .\scripts\build-preview.ps1
python scripts/serve-preview.py --port 1114
```

Open `http://127.0.0.1:1114/`. On Windows, stop the HTTP server before rebuilding to avoid open-file locks, then start it again.
The server binds to loopback; noindex metadata and robots.txt supplement this private local delivery.

The build renders the exact author Markdown retained in `docs/editorial-sources`, plus manuscript-verified supplements in `data/editorial-supplements.json`, with pinned KaTeX 0.16.22. Generated HTML+MathML, local fonts and CSS are committed. Zola still owns the site, routes and templates. HTML minification is disabled because its optimizer corrupted the KaTeX MathML/SVG markup.

## Checks

```powershell
& .\scripts\run-tests.ps1
```

Browser checks use Playwright from the desktop runtime, Chrome, and the local preview. Set `CODEX_NODE_MODULES` to the package directory returned by the workspace dependency tool, then run `node scripts/verify-final-v5.js`. The source viewer and local formula scrolling have a separate check in `scripts/verify-sources-final-v5.js`.

Production remains blocked by `scripts/check-production.ps1`. The GitHub workflow runs only on `main` and now runs that guard before its deployment step. Publishing requires a distinct authorized task to remove demonstrations and review the remaining metadata and rights.
