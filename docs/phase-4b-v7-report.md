# Academic OS — Phase 4B preview v7 report

Date: 29 September 2026  
Working branch: `codex/phase-4b-preview-v7`  
Recoverable pre-v7 ref: `archive/phase-4b-v6-before-v7`  
Starting commit: `d7bbeb37bb991f5e73b6b07bd8172b4375b7f8ef`  
Publication: not performed; preview remains `noindex` and production-blocked.

## Outcome

The existing Zola site was restructured in place. Its editorial identity, palette, theme system, direct CV link, contextual navigation, poetry presentation, and private-preview safeguards were retained. Pages are denser and faster to scan, while the three research pages now share a source-faithful progressive structure.

## Demand → change → verification → status

| Demand | Change | Verification | Status |
|---|---|---|---|
| Preserve v6 | Created `archive/phase-4b-v6-before-v7` before editing | Ref resolves to starting commit | Complete |
| Exact eight-item navigation; no About/Job Market | Existing menu retained; CV remains a direct PDF | `validate-site.ps1` exact-order test | Complete |
| More compact site | Reduced global/page heading maxima, section spacing, paper spacing, and teaching rows | Visual review at 1440, 1280, 768, 390, 360 px | Complete |
| Stronger AV mark | Replaced literal AV circle with an original overlapping vector monogram | Desktop/mobile screenshots | Complete |
| Home in first person, broader program | Kept the approved three-paragraph biography; rewrote the separate program statement around latent quantities and documented works in progress | Home DOM review and exact-bio regression test | Complete |
| Essential mobile links after portrait | Added JMP, CV, Research, and Contact grid | 360 px browser review | Complete |
| Research index broader and faster | New framing and direct jump to papers | 1280 px browser review | Complete |
| Shared paper architecture | Added Question, Contribution, 120–180-word Overview, foldable Abstract, Economic Model, Identification, Econometric Model, Data & Sources, Findings & Limits, Documents/Contact | All three pages validated; ten common section IDs | Complete |
| Scientific fidelity | Re-read all three manuscripts; added source map and conservative tables/results/limits | `phase-4b-v7-scientific-sources.md`; manuscript spot checks | Complete |
| JMP time-price and transformation | Added productive time price, money/time/full cost, exact tanh formula, reasons, implications, and measurement caveats | Browser review and validator tokens | Complete |
| One-adult results and uncertainty | Added exact population, shares, standard errors, comparison, and limitations | Source map; descriptive table | Complete |
| Labor income/time channels | Added tractable equation and separate income vs time/availability explanations | Page review; corrected plural “demographic controls” | Complete |
| Teaching courses first | Reordered page: courses/materials precede principles and photo | Mobile screenshot | Complete |
| Teaching wording/photo | Added exact “Explain why, not only how”; caption “Student training in Stata, 2021.”; removed redundant UCO Niort institution label | Template/data inspection | Complete |
| Niort econometrics integration | Added course PDFs, Python/R/Stata scripts, four notebooks, seven `.dta` files, simulator, and French practical README | 27 byte-identical source-copy checks; link validation | Complete |
| Microeconomics simulators | Integrated repeated-games and matching-pennies HTML tools | Build link validation and noindex wrappers | Complete |
| L3 Data Analysis | Added three notebook sessions, exercise PDFs, CSV, and dictionary | Source-copy checks and teaching/code pages | Complete |
| Honest execution claims | README distinguishes author-reported three-language runs from supplied technical checks that did not run Stata or R notebooks | Editorial review | Complete |
| Insights purpose | Clarified future editorial purpose; kept two conspicuously synthetic fixtures | Reading-time and synthetic-label tests | Complete |
| Poetry music | Enabled only the approved Calligraphie pairing; disabled the other three page integrations | Site validator requires exactly one enabled local track | Complete |
| Book-a-meeting pathway | Added an interactive, explicitly non-booking scheduling demo with purpose, sample slot, detected time zone, review, and no-calendar confirmation | Browser interaction test in dark mobile view | Complete, demo only |
| Preview safety | Retained meta noindex, robots disallow, visible preview ribbon, inventory, and negative production test | 54 HTML files validated; negative production test blocks | Complete |

## Teaching package details

The practical Niort guide is at `static/files/teaching/niort/econometrics/session-01/README.md`. It documents:

- the exact file inventory;
- Python, R, and Stata dependencies;
- Wooldridge data origin and `.dta` generation;
- commands for each language;
- expected output categories;
- Python and R notebook/Colab constraints;
- common errors and responsible reuse.

The materials are deliberately described as teaching resources, never as an article replication package.

## Meeting integration audit and activation

No Calendly, Cal.com, Microsoft Bookings, Google Calendar, Outlook Calendar, scheduling URL, API key, or calendar sync was present in the repository. The preview therefore makes no booking or conflict-detection claim.

To activate a real pathway later:

1. choose and approve a scheduling provider or institutional booking URL;
2. connect only the intended calendar(s) with least-privilege availability access;
3. define event types, duration, buffers, working hours, notice, and cancellation rules;
4. use `Europe/Paris` as the organizer zone while displaying slots in the visitor’s zone;
5. configure provider-side conflict checking across every calendar that can make Anderson unavailable;
6. add privacy wording and confirmation/cancellation email behavior;
7. replace the demo component with the approved link/embed and test a sandbox booking before publication.

## Insights ideas grounded in the manuscripts

1. **Why one observed household budget does not reveal individual living standards** — a plain-language explanation of assignable goods, sharing rules, and why household totals hide adult and child resource shares.
2. **What is one hour of parental childcare worth?** — contrasts wage valuation with a behaviorally identified productive time price, including why leisure-like experience matters.
3. **Children are mechanisms, not controls** — explains how monetary costs and time availability can push labor supply through different channels without treating family composition as a black-box coefficient.

These are proposals only; the two current Insights remain synthetic until real text is supplied and approved.

## QA evidence

Automated suite:

- Zola build: pass, 44 pages plus three orphan preview pages;
- site validation: pass, 54 HTML files;
- internal links/download targets/noindex/menu/404/status labels: pass;
- reading-time tests: pass;
- source-copy tests: pass, 27 byte-identical copies;
- synthetic script and notebook execution: pass;
- negative production test: pass (blocked as intended);
- positive clean-fixture production test: pass.

Browser checks:

- responsive widths: 1440, 1280, 768, 390, and 360 px;
- no horizontal document overflow at all sampled widths;
- light and dark themes;
- abstract disclosure;
- scrollspy structure and anchors;
- mobile keyboard menu (Enter opens and focuses Home; Escape closes and restores focus);
- meeting-demo selection and confirmation, with proof that no appointment was created;
- visible focus styles and 44 px minimum controls are retained;
- reduced-motion rule and contrast-aware theme tokens remain in CSS;
- the 360/720-width checks exercise the reflow expected under narrow screens and high zoom.

After screenshots: `docs/screenshots/phase4b-v7/`. Before screenshots: `docs/screenshots/phase4b-v6/`. Responsive measurements: `docs/screenshots/phase4b-v7/responsive-metrics.json`.

### Before/after measures

| Measure | v6 | v7 |
|---|---:|---:|
| Teaching course groups | 5 | 7 |
| Enabled poem music pairings | 4 | 1 |
| Shared top-level paper sections | 7–8 | 10 |
| Page-hero maximum heading | 4.8rem | 4.0rem |
| Global h1 maximum | 5.3rem | 4.35rem |
| Section-space maximum | 4rem | 3rem |
| Generated site pages | 42 | 44 |
| HTML files checked | 52 (v6 report) | 54 |

## Remaining provisional or missing items

- Current-role wording and dates still need author approval.
- Manuscript status labels, public distribution, and final canonical PDFs remain blocked.
- Portrait and teaching-photo publication rights/credits remain unresolved.
- Teaching material distribution rights remain unresolved.
- Three poems remain first-stanza-only; their full PDFs are absent from the repository.
- Only Calligraphie de bonheur has an approved music pairing; the other candidates stay disabled.
- Podcast republication rights and transcript review remain unresolved.
- The two Insights and Talks records remain synthetic and excluded from production.
- Real calendar activation requires a provider choice and explicit authorization.

## Local preview

From the repository root:

```powershell
powershell -ExecutionPolicy Bypass -File scripts\build-preview.ps1
python -m http.server 1112 --bind 127.0.0.1 --directory public-preview
```

Open `http://127.0.0.1:1112/`. The current browser preview was left open for review.

## Change inventory and rollback

Principal changed areas:

- content routes for the three papers, Teaching, Code & Data, and Contact;
- `data/academic.toml`, teaching, audio, photo, and preview inventory data;
- new v7 templates for papers, teaching, Code & Data, and Contact;
- global CSS/JS and vector monogram;
- current CV replacement;
- Niort and CY teaching assets;
- validators, scientific source map, report, and screenshots.

No push, merge, deployment, or production build was performed. To return to the untouched v6 snapshot, switch to `archive/phase-4b-v6-before-v7`. To discard only uncommitted v7 work, use Git’s normal restore/clean workflow after reviewing the file list; no destructive rollback was run automatically.
