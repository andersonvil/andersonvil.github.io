# Home v4 consolidated revision — implementation and verification

Date: 2026-09-30 (America/New_York)  
Working branch: `codex/home-v4-revision`  
Pre-change recovery reference: `archive/home-v4-before-revision` at `a65612dffebb53993300d51fcf066ca47f500227`  
Local private preview: `http://127.0.0.1:1112/`  
Deployment status: not pushed, not merged, and not deployed. The GitHub Pages workflow runs only from `main`.

## Delivered revision

- Replaced the Home biography with the two requested paragraphs and removed the repeated role line.
- Preserved the portrait, JMP, CV, abstracts, downloads, and two additional featured papers.
- Rebuilt the Home section access as six compact blocks with exactly a label, subtitle, and exploration link. Desktop geometry is 1 / 2 / 2 / 1; mobile order is Research, Teaching, Code & Data, Insights, Beyond Economics, Contact.
- Replaced the Research title and added the dedicated research-program introduction only on the Research page.
- Applied bounded typography: Home name 36–40 px desktop and 28–32 px mobile; page and paper titles 30–36 px desktop and 25–30 px mobile; section subtitles 20–24 px desktop and 18–21 px mobile; body 16 px; labels 11.84 px.
- Replaced the site mark and favicon with a lightweight SVG interpretation of variant 4 — Editorial from `ChatGPT Image Sep 30, 2026, 07_24_39 PM.png`.
- Reworked all three paper pages: informative subtitles, a single Research return, estimated equations, separate keyboard-accessible symbol definitions, compact provenance, complete descriptive groups, physical PDF-page links, and manuscript-grounded limits.
- Added the requested Teaching introduction and explicit reading order without duplicating institutional group labels.
- Renamed the Code & Data title, simplified Insights for its two synthetic entries, preserved the distinctions between real/provisional/synthetic resources, and fixed podcast-image aspect handling.

## Scientific traceability

### The Household Demand for Leisure, the Price of Time and the Full Cost of Children

Canonical file: `03-research/paper-3.pdf` (site copy: `household-demand-leisure.pdf`).

| Site object | Manuscript source |
|---|---|
| Full leisure, productive time price, time/full cost | model section; printed pp. 17–24 and empirical definition on printed p. 30 |
| Childcare system | equations (9)–(12), printed pp. 29–30 |
| Monetary cost | equation (14), printed p. 31 |
| Quadratic full-leisure demands | equations (15)–(18), printed pp. 31–32 |
| Recursive ML then 3SLS estimation | section 4.1, printed pp. 32–33 |
| Broad PSID hours, bound, tanh correction, ATUS calibration | section 3.1, printed pp. 24–26 |
| Raw/adjusted hours and spending | Table 1, physical PDF page 26 |
| Benchmark sample | Table 2, physical PDF page 27 |
| Monetary-cost instability and ridge inference caveat | section 4.2.2, printed pp. 37–40 |
| Sensitivity of time cost to raw/adjusted hours | section 4.2.3, printed pp. 41–44 |

Resolution: ATUS is described as a calibration/benchmark source, not an estimation sample. Monetary-cost instability and time-measure sensitivity are presented as two separate reservations.

### Children Costs in a One-Adult Household: Empirical Evidence from the UK

Canonical file: `03-research/paper-1.pdf` (site copy: `children-costs-one-adult-household.pdf`).

| Site object | Manuscript source |
|---|---|
| Assignable-good demand and identification pillars | equation (12), section 2.3, printed pp. 7–10 |
| Quadratic log-expenditure demands | equations (13)–(15), printed pp. 10–11 |
| Logistic sharing rule and Barten scale | equations (16)–(18), printed pp. 11–12 |
| Unified equation and iterated nonlinear 2SLS | equation (19), section 3.2, printed pp. 12–13 |
| Complete women/men and 1/2/3-child groups | Table 2, physical PDF page 14 |
| Resource shares and uncertainty | section 5.1 and result tables |

Resolution: the site no longer turns the individual-consumption intuition into a maintained general assumption. It states that the mother–father difference is not statistically established and explicitly avoids interpreting that as proof of equality. The descriptive clothing decline is separated from total expenditure and from causal interpretation.

### How to Incorporate Children into Labor Supply Equations? An Equivalence-Scale Approach

Canonical file: `03-research/paper-2.pdf` (site copy: `children-labor-supply-equations.pdf`).

| Site object | Manuscript source |
|---|---|
| General identification result, nonlinear and linear cases | Assumption L, Proposition, equations (9)–(14), sections 2.1–2.2, printed pp. 7–12 |
| Participant/nonparticipant distinction and reservation wage | sections 2.1–2.3 and 3.3, printed pp. 11–12 and 17–19 |
| Quadratic reference labor supply | equation (16), printed p. 15 |
| Time/monetary costs and final labor supply | equations (17)–(21), printed pp. 15–16 |
| Wage/resource control functions | equations (22)–(23), printed pp. 16–17 |
| Complete sample groups and standard deviations | Table 1, physical PDF page 14 |
| Marginal responses and cost distributions | Tables 4–5, printed pp. 22–25 |

Resolution: the general linear case remains partially identified; the more restrictive parametric linear specification is explained separately. “Modest data inputs” is not presented as fewer assumptions. The diagram labels are Income effect and Substitution effect. Generic small-sample/extrapolation language was removed from the overview and limits while the technical participation assumptions remain explicit.

## Coverage matrix

| Request group | Status | Resolution |
|---|---|---|
| Branch, recovery point, no production deployment | Treated | Dedicated branch and archive ref created; workflow trigger checked; no push/merge. |
| Home bio, JMP/CV, compact 1/2/2/1 access | Treated | Exact requested copy and routes; six blocks contain exactly three elements. |
| Research title and dedicated introduction | Treated | Replaced old competing title; introduction appears only on Research. |
| Global title scale, spacing, portrait alignment | Treated | Shared CSS corrected; measurements recorded in `verification.json`. |
| Editorial monogram | Treated | Source plan found; variant 4 integrated as SVG in header/favicon. |
| Paper subtitles and Econometric model & methods label | Treated | Applied to body and section navigation for all three papers. |
| Estimated equations and separate definitions | Treated | Manuscript equation ranges exposed with keyboard-accessible definitions. |
| Data tables and physical PDF links | Treated | Variables label, complete requested groups, local horizontal scrolling, correct Table/page links. |
| JMP ATUS/PSID explanation and two sensitivities | Adapted for scientific accuracy | Wording follows the manuscript and distinguishes calibration, estimation, monetary instability, and hours sensitivity. |
| UK interpretation and inference | Adapted for scientific accuracy | Clothing and total spending separated; no equality claim from nonsignificance. |
| Labor-supply identification | Adapted for scientific accuracy | General theorem, restricted parametric model, and nonparticipants separated. |
| Teaching introduction/order/photo | Treated | Requested copy/order; photo and factual 2021 caption preserved. |
| Code & Data / Insights / Beyond / Contact | Treated | Real resources retained; synthetic fixtures remain explicit; no autoplay or fake booking. |
| Private preview access | Treated | Localhost preview retained; `noindex` remains supplemental, not described as access control. |
| Production approval, image/paper rights, final editorial statuses | Blocked | Existing source inventory still marks these as pending; no unsupported approval was invented. |
| Earlier review video | Not uniquely identifiable | Multiple unrelated videos exist in Downloads, but no canonical review recording was identifiable. Existing screenshots/reports and the current source/manuscripts were used; this did not block specified changes. |

## Verification performed

- `scripts/run-tests.ps1`: pass, including Zola build, 54 HTML-file validation, reading times, 29 byte-identical source copies, executable synthetic demo/notebook, and negative/positive production gates.
- `scripts/verify-home-v4.js`: pass on 13 routes at 1440, 1280, 768, 390, and 360 px (65 route/viewport checks), with no global horizontal overflow and no console errors.
- Theme: light/dark toggle, saved preference, and initial bootstrap verified. The in-app browser was also used for a visible live check.
- Interactions: disclosures, table links, Home route order/geometry, Teaching order, audio non-autoplay, image aspect, 404, Talks, preview index, and all three paper pages verified.
- Captures and measurements: `docs/screenshots/home-v4/`.

Measured CSS at 1440 px: Home name 40 px; page title 36 px; paper title 36 px; paper subtitle 24 px; Home access subtitle 23.04 px; label 11.84 px; body 16 px. At 390 px: Home name 31.2 px; access subtitle 20 px; body 16 px. At 360 px: page/paper titles 25.2 px and Home name 28.8 px.

## Remaining confirmations

- Final production permission and credit for portrait, teaching photograph, manuscripts, poems, and audio.
- Final public editorial statuses, role wording, and replacement of provisional CV/manuscript labels when approved.
- Stable public authentication/access mechanism if this private localhost preview is later hosted; `noindex` alone must not be used as access control.

## Rollback

After committing or stashing any later work, return to the preserved state with:

```powershell
git switch archive/home-v4-before-revision
```

The exact preserved commit is `a65612dffebb53993300d51fcf066ca47f500227`. No history rewrite is required.
