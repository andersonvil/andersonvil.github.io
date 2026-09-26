# Phase 4B — Provisional content integration v3

Date: 25 September 2026  
Scope: local/private preview only  
Working repository: `C:\dev\andersonvil.github.io`  
Read-only sources: `C:\dev\andersonvil-academic-os-sources`

## 1. Git protection and isolation

- Preserved commit: `8ac381bb0d0d43d2410397bbded23dba145c6f1e`.
- Archive branch: `archive/phase-4b-success-before-content-integration`.
- Annotated tag: `phase-4b-success-2026-09-25`.
- Working branch: `codex/phase-4b-provisional-content-v3`.
- `main` remained at `1e8f05a78127e1441927f1652876c28b3d4049a1`.
- The source tree was inspected read-only and was not renamed, moved, or edited.
- No push, pull request, merge, deployment, publication, GitHub Actions run, public preview, or external-profile change was performed.

Local commits created for this iteration:

- `25a8b40bea80f10385e3060bab15abd3611ff5c9` — `feat: integrate provisional Phase 4B content`.
- A documentation-only successor records this report; its exact hash is available as the branch tip in `git log -2 --oneline` because a commit cannot embed its own hash.

To inspect the preserved state without changing the current worktree:

```powershell
git show --stat archive/phase-4b-success-before-content-integration
git show --stat phase-4b-success-2026-09-25
```

To return safely by creating a new local branch at that state:

```powershell
git switch -c codex/phase-4b-restored phase-4b-success-2026-09-25
```

To move the existing worktree back after first preserving any uncommitted work:

```powershell
git status
git switch archive/phase-4b-success-before-content-integration
```

## 2. Local preview and privacy

Open a private loopback preview with:

```powershell
zola serve --interface 127.0.0.1 --port 1111 --output-dir public-preview --force
```

Then open `http://127.0.0.1:1111/`. The preview retains `noindex, nofollow`, a restrictive `robots.txt`, no public sitemap, no analytics, and no public preview URL. `scripts/check-production.ps1` continues to reject a production build while provisional, blocked, synthetic, or preview-only content remains.

## 3. Page-by-page result

- **Home:** exact three-paragraph biography beside the portrait; `Anderson Vil` stays on one line; accessible professional links remain; current papers use their real provisional manuscripts and exact abstracts.
- **Research:** all three real manuscripts, author order, displayed version dates, exact PDF abstracts, separate detail/download actions, and visible provisional status.
- **Individual paper pages:** exact abstract shown directly, scientific structure retained, preview version identified, and a source-title divergence disclosed for paper 2.
- **Teaching:** four evidence-based groups with institution, level, language, resource type, year status, topics, PDF page count, size, and working downloads.
- **Beyond Economics:** short introduction followed by Poetry and Conversations only; no empty categories.
- **Calligraphie de bonheur:** complete authorized HTML poem, French language metadata, preserved stanza/line structure, rights notice, no PDF download, plus optional silent immersive reading.
- **Ma chérie, Mes nuits, On me l'a interdit:** first stanza only, sober availability notice, title-specific email request, rights notice, and no download.
- **On reading preferences:** neutral title, static authorized poster, audio-only web copy with native controls and no autoplay, and a French transcript clearly marked pending review.
- **Preview inventory:** records provenance, consumer, real/provisional/synthetic status, intended visibility, missing right/validation, publication condition, and replacement/unblocking path.

## 4. Biography confirmation

The Home biography is stored as exactly three separate paragraphs and rendered without paraphrase:

> I am a household and labor economist studying how families allocate money and time, and how children and caregiving reshape consumption, labor supply, leisure, and the distribution of resources within households.
>
> I develop and estimate structural models using applied microeconometric methods, expenditure data, and time-use data to recover economic quantities that household surveys do not directly reveal.
>
> I hold a Ph.D. in Economics from CY Cergy Paris Université, where I was supervised by Olivier Donni. In 2023, I was a visiting researcher at KU Leuven, hosted by Frederic Vermeulen.

It is not duplicated in full on Contact and no About page was created.

## 5. Research-paper and abstract provenance

| Paper shown in preview | Source | PDF version/date | Result |
|---|---|---|---|
| Children Costs in a One-Adult Household: Empirical Evidence from the UK. | `03-research/paper-1.pdf` | 12 January 2026 | Prior summary replaced with the manuscript's exact abstract; author `Anderson Vil` confirmed. |
| How to Incorporate Children into Labor Supply Equations? An Equivalence-Scale Approach | `03-research/paper-2.pdf` | 4 September 2026 | Prior summary replaced with the manuscript's exact abstract; authors `Olivier Donni, Anderson Vil` confirmed. The PDF title spells `Childen`; the higher-priority canonical source supplies corrected `Children`, and the discrepancy is disclosed. |
| The Household Demand for Leisure, the Price of Time and the Full Cost of Children: A Structural Model and Evidence from the PSID | `03-research/paper-3.pdf` | 18 September 2026 | Prior summary replaced with the manuscript's exact abstract; authors `Olivier Donni, Anderson Vil` confirmed. |

The short site descriptions remain separate from the scientific abstracts. The three copied PDFs are byte-identical to the supplied files, marked `real = true`, `provisional = true`, `synthetic = false`, and `publicationStatus = "blocked"`.

## 6. CV

`04-cv-teaching/cv-academic.pdf` is copied byte-for-byte to the stable private-preview path `/files/cv/anderson-vil-cv-provisional.pdf`. It is a real provisional document with publication blocked pending final approval and privacy review. There is no HTML intermediary.

## 7. Teaching inventory

### Integrated

| Group | Institution | Language | Academic year | Integrated resources |
|---|---|---|---|---|
| Econometrics — Undergraduate (L3) | CY Cergy Paris Université | French; every item labelled `French-language material` | Academic year to confirm | Eight exercise sheets, chapters I–VIII; 3, 5, 5, 4, 3, 4, 5, and 6 pages. |
| Econometrics — Master's Year 1 (M1) | CY Cergy Paris Université | English | Academic year to confirm | One 17-page exercise collection. |
| Econometrics — Master's Year 2 (M2) | CY Cergy Paris Université | English | Academic year to confirm | One 19-page advanced exercise collection. |
| Microeconomics — Undergraduate (L1) | Sciences Po Saint-Germain-en-Laye | French; labelled `French-language material` | 2026–2027, confirmed on the title slide | One 61-page session support. |

All eleven integrated PDFs are byte-identical to their external sources. The L3 files are internally undated despite a `2026-27` parent folder. The M1 filename contains `2025`, the M2 filename contains `2025-26`, and both live below `2026-27`; their document bodies are undated. The requested 2025–2026 label therefore was not assumed.

### Excluded

- `docs_to_read_micro_St-Germain.zip` was not copied. It contains seven third-party readings whose redistribution rights are not established: `168-prixdunumerique-fr-2019-10-28-w.pdf`, `APRP_021_0023.pdf`, `BMV_clemence_2006.pdf`, `ia36.pdf`, `n39-notesIPP-avril2019.pdf`, `Note_IPP_Encadrement_des_loyers.pdf`, and `WP-165-Grislain-Letremy-Mauroux.pdf`.
- No student names, email addresses, grades, submissions, confidential exams, marking schemes, administrative documents, or separate answer keys were found or copied.

All teaching resources remain blocked pending distribution-right confirmation; L3, M1, and M2 also await academic-year confirmation.

## 8. Poetry visibility matrix

| Work | HTML visibility | Request access | Full PDF in repository/build |
|---|---|---|---|
| Calligraphie de bonheur | Full text | No | Excluded |
| Ma chérie | First stanza only | Yes, title-specific email | Excluded |
| Mes nuits | First stanza only | Yes, title-specific email | Excluded |
| On me l'a interdit | First stanza only | Yes, title-specific email | Excluded |

`Calligraphie de bonheur` is dated 19 October 2023; the restricted excerpts are dated 3 March 2023, 4 May 2024, and 22 August 2026 respectively. All four are French and carry rights notices. No poem PDF is offered for download.

The three restricted complete PDFs were never copied. The repository stores only the authorized first stanza and minimum metadata for each restricted work. Automated checks also search the repository and generated build for restricted PDF filenames and later-stanza markers; none were found.

## 9. Immersive reading and music

The full authorized poem includes an optional dialog-based immersive mode with one stanza per screen, previous/next controls, visible progress, an always-visible exit action, Left/Right/Escape keyboard operation, and reduced-motion support. The accessible HTML poem remains the primary content. There is no autoplay and no audio track. Music remains an expected-content item because no rights-cleared recording with documented composition and recording rights was supplied.

The three restricted poems do not receive an immersive payload beyond their authorized excerpt.

## 10. Podcast/media audit

Source `podcast/SugBook2.mp4` was inspected without modification:

- duration: 307.52 seconds (5:07.52);
- size: 25,967,518 bytes;
- container/video/audio: MP4, H.264 1920 × 1080 at 30 fps, AAC 48 kHz stereo;
- aggregate bitrate: approximately 676 kb/s.

Because the source is a static portrait video, the preview uses a lighter audio-only M4A copy plus a poster extracted from the supplied image:

- M4A: AAC, 48 kHz, stereo, 307.52 seconds, 9,289,323 bytes, approximately 242 kb/s;
- poster: JPEG, 27,348 bytes;
- native controls, keyboard play/pause, volume control, `preload="metadata"`, no autoplay and no loop.

A machine-assisted French transcript was manually corrected where intelligible but remains explicitly marked `Transcript pending review`; uncertainty is identified rather than silently guessed. No subtitle track is claimed because the transcript has not been verified. Publication remains blocked pending republication permission and Anderson Vil's transcript review. No show title, date, host, or organization was invented.

## 11. Verification results

`scripts/run-tests.ps1` passed after the final integration:

- Zola build: pass, 38 pages (3 orphan sections/pages reported by Zola) and 45 generated HTML files validated;
- internal links and anchors: pass;
- site assertions: pass, including exact bio, paper structure, teaching metadata, poem visibility, media and privacy attributes;
- reading-time tests: pass;
- source-copy integrity: pass, 15 byte-identical provisional PDF copies;
- restricted poem PDFs: absent;
- executable synthetic notebook: pass;
- negative production test: pass because the preview is correctly blocked;
- clean production fixture: pass.

Manual browser review covered Home, Research, Teaching, Beyond Economics, all four poem pages, immersive mode, and the podcast page. Light and dark themes were checked, including persistent preference. Native audio play/pause was exercised from the keyboard. The immersive previous/next/Escape interactions were exercised. Responsive overflow checks passed at 320, 375, 768, 1024, and 1440 px across the reviewed routes. `Anderson Vil` computed as `white-space: nowrap` and stayed on one line. The portrait has intrinsic dimensions, avoiding a layout shift while loading.

## 12. Screenshots

The following review evidence is stored in `docs/screenshots/phase4b-v3/`:

- `home-desktop-light.png`
- `home-desktop-dark.png`
- `home-mobile-375.png`
- `research-desktop.png`
- `teaching-desktop.png`
- `beyond-economics-desktop.png`
- `calligraphie-de-bonheur-desktop.png`
- `ma-cherie-mobile.png`
- `mes-nuits-mobile.png`
- `on-me-l-a-interdit-mobile.png`
- `immersive-reading-desktop.png`
- `podcast-desktop.png`

## 13. Files changed

### Added

- `content/beyond-economics/{calligraphie-de-bonheur,ma-cherie,mes-nuits,on-me-l-a-interdit,on-reading-preferences}.md`
- `content/teaching/{econometrics-l3,econometrics-m1,econometrics-m2,microeconomics-l1}.md`
- `data/beyond_economics.toml`, `data/teaching_resources.toml`
- `docs/phase-4b-provisional-content-v3-report.md` and the twelve screenshots listed above
- `scripts/capture-phase4b-v3.js`, `scripts/validate-source-copies.ps1`
- `templates/media-work.html`, `templates/poem.html`, `templates/teaching-detail.html`
- `static/files/cv/anderson-vil-cv-provisional.pdf`
- `static/files/papers/` (three real provisional manuscripts)
- `static/files/teaching/` (eleven real provisional resources)
- `static/files/media/on-reading-preferences.m4a`
- `static/img/podcast-reading-preferences-poster.jpg`

### Modified

- `config.toml`
- `content/beyond-economics/_index.md`
- `data/academic.toml`, `data/preview_inventory.toml`, `data/preview_synthetic.toml`
- `docs/source-conflicts.md`
- `scripts/build-demo-pdfs.py`, `scripts/check-production.ps1`, `scripts/run-tests.ps1`, `scripts/test-reading-times.ps1`, `scripts/validate-site.ps1`
- `static/js/site.js`, `static/styles/site.css`
- `templates/base.html`, `templates/beyond-economics.html`, `templates/home.html`, `templates/paper.html`, `templates/preview-index.html`, `templates/research.html`, `templates/teaching.html`

### Removed

- `content/preview-synthetic/beyond-economics/notes-on-attention.md`
- the synthetic CV and three synthetic paper PDFs formerly under `static/files/demo/`

The remaining synthetic Insights, Code & Data project, teaching-function fixture, and Talks fixture stay clearly labelled because they still test functions for which no equivalent authorized real content was provided.

## 14. Outstanding validations and rights

1. Final public approval of the exact Home biography and current role wording/dates.
2. Final canonical status, distribution permission, metadata, and version choice for each manuscript.
3. Final privacy review and publication approval for the CV.
4. Distribution rights for every teaching PDF.
5. Academic year for L3, M1, and M2; the supplied evidence conflicts or is incomplete.
6. Public HTML/excerpt approval, typography approval, and publication rights for all four poems.
7. Any music track: file, composition status, recording status, performer, source, licence, and licence URL.
8. Podcast republication permission and final approval of the audio export/poster.
9. Human review of the French transcript before captions or subtitles can be represented as verified.
10. Portrait rights and any required credit.
11. Final replacement or removal of all remaining synthetic fixtures and preview-only routes/assets before production.

These open items are deliberately represented as `publicationStatus = "blocked"`; they do not prevent private local review but do prevent production release.
# Complement v4 — research data, slides, Niort, and poetry design (2026-09-26)

## 1. JMP slides: inventory and compilation

- Source PDF: `03-research/EEA_17-08-26.pdf`; source entry point: `03-research/EEA_17-08-26.tex`.
- No included TeX files and no bibliography dependency were found. The deck is a 42-page, 16:9 PDF (634,638 bytes), dated August 17, 2026 for the 41st Annual Congress of the European Economic Association.
- A clean temporary `pdflatex -interaction=nonstopmode -halt-on-error` run reached the figure section and stopped because `MoneyCostNkde.pdf` is absent. `TimeCostNkde.pdf` and `website.png` are also referenced but absent. The supplied PDF therefore could not be reproduced byte-for-byte from the available source bundle.
- Only the supplied, real PDF was copied to `/files/slides/household-demand-leisure-eea-2026.pdf`. Metadata are centralized in `data/academic.toml`; `real = true`, `provisional = true`, and `publicationStatus = "blocked"` are preserved.

Displayed slide 10 is physical PDF page 10 and the frame `Identification of the time cost of children` in `EEA_17-08-26.tex` lines 493–576. Displayed slide 11 is physical PDF page 11 and the frame `Identification of the monetary cost of children` at lines 580–643. Neither frame has an active overlay. Both are native TikZ/text/equation compositions and require no missing external figure.

The web version uses selectable HTML, inline SVG, and MathML. It preserves observed inputs, wage-substitution logic, shadow prices, childcare technology, time-cost equation, full-leisure Engel curves, the monetary-cost equation, assumptions A.2/A.3, and the Rothbarth principle. Spacing and responsive hierarchy were adapted; scientific notation and sequence were not changed.

## 2. Research visuals

Private lab: `/preview/design-lab/research-visuals/`.

Each paper has three original inline-SVG candidates: (1) sober editorial concept, (2) human/contextual option with neutral figures, and (3) abstract diagram. Source, rights status, alt text, advantages, and limits are shown beside every candidate. Candidate 1 is provisionally selected for all three papers because it is clearest at card and page scale without suggesting an empirical result. The choice is centralized in `data/research_visuals.toml`; candidate numbers do not appear on normal paper pages.

## 3. Data sources and access matrix

| Paper | Main dataset | Provider | Exact wave/studies/files | Official access URL | Conditions | Supplementary source |
|---|---|---|---|---|---|---|
| Children Costs in a One-Adult Household | Harmonized FES/EFS/LCF, 1978–2022 | ONS / UK Data Service | Manuscript confirms series and period; annual study numbers and construction cannot be recovered because no programs were supplied | `https://datacatalogue.ukdataservice.ac.uk/series/series/200016` and LCF series `2000028` | Registration/licence; annual studies, not one raw file | None established |
| How to Incorporate Children into Labor Supply Equations? | 2019 PSID | University of Michigan ISR / PSID | 1,270 single women/single mothers; variables described in manuscript, exact IDs and file join not recoverable without programs | `https://simba.isr.umich.edu/data/data.aspx` | Registration and customized variable basket | 2019 IPUMS ATUS as external benchmark, not estimation data |
| Household Demand for Leisure (JMP) | 2019 PSID | University of Michigan ISR / PSID | 825 dual-earner couples from the 9,569-household Family File universe; exact IDs/join not recoverable without programs | `https://simba.isr.umich.edu/data/data.aspx` | Registration and customized variable basket | 2017 PSID robustness; ATUS published/microdata benchmark. Manuscript period/table citation conflict is disclosed |

The site links only to official provider, catalogue, Data Center, and documentation pages. It redistributes no raw data and never labels a registration-dependent extract as a direct download.

CTA sets are now exact: JMP — `Download paper`, `Download slides`, `Data sources`, `Copy citation`; papers 1–2 — `Download paper`, `Data sources`, `Copy citation`. No non-JMP slides placeholder exists.

## 4. Niort teaching source

The new group is `Microeconomics III — Undergraduate (L3), Niort`, French-language material, academic year 2026–2027. It contains `Séance 1 — Introduction générale` (59 PDF pages) and `Séances II à IV — Théorie des jeux : décisions stratégiques et équilibre de Nash` (109 pages). Topics cover competitive and strategic decisions, risk, private information, market foundations, normal-form and repeated games, pure/mixed Nash equilibrium, and backward induction.

Both title slides print Anderson Vil, THEMA, and CY Cergy Paris Université, while the folder says Niort and current role metadata mentions Université Catholique de l’Ouest. The preview therefore stores `institutionalWordingToConfirm = true`, states the conflict, and makes no silent attribution decision. Only the two supplied course PDFs are exposed; no correction, student data, or internal material is included.

## 5. Source organization audit

The full diagnosis, target tree, path mapping, naming/version rules, ambiguities, and staged migration plan are in `docs/source-organization-v4.md`. The principal risks are flat research storage, teaching roots split by inconsistent institution/location labels, absent declared TeX assets, opaque revision suffixes, and ambiguous short dates. This was a read-only audit; no source move or rename occurred.

## 6. Poetry backgrounds and typography

All four TeX sources compile with XeLaTeX until the missing `../assets/...` background reference. Missing assets are exactly `calligraphie_03_encre.png`, `cherie_J.png`, `cherie_B.png`, and `sarah_E.png`. The PDFs embed one 1055×1491 RGB background each and separately embed text using Segoe Script, Gabriola, Garamond, and Perpetua respectively.

The web backgrounds were extracted from the embedded PDF image objects—without poem text—then optimized to WebP. Real HTML text is overlaid in source-informed positions. No proprietary font file is bundled. Normal pages provisionally use the centralized `literary-serif` choice (Georgia/system-serif fallback) for readability and consistency.

Private lab: `/preview/design-lab/poetry-typography/`. For each poem it shows one selectable variant at a time: original local system face, literary serif (recommended), and contemporary system sans. Family, weight, line-height, rights/licence implications, and cost are documented. Calligraphie uses one representative stanza in the lab; the three restricted poems use only a subset of their already authorized first stanza. Normal restricted pages still expose exactly one stanza and no full PDF.

## 7. Verification and safety

- Preview build, internal-link validation, exact CTA assertions, nine research candidates, four × three typography samples, slide and Niort downloads, MathML/SVG identification, data sections, poem backgrounds, restricted-content rules, audio behavior, theme behavior, reading-time tests, synthetic Python script, and notebook execution pass.
- Eighteen real PDFs are byte-identical to their read-only sources. The four poetry WebPs are derived background-only assets, not full-page poem captures.
- The preview remains `noindex`; design labs are absent from public navigation and live under `content/preview-synthetic`, so the production gate blocks them.
- Light/dark and desktop/mobile review is recorded in the v4 screenshot set after the final visual pass.
- No source in `C:\dev\andersonvil-academic-os-sources` was modified or moved. No push, pull request, merge, deployment, or `main` change occurred.
