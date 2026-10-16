# Source organization audit — Phase 4B complement

Audit date: 2026-09-26. Scope: read-only inventory of `C:\dev\andersonvil-academic-os-sources`. No source file was moved, renamed, or edited.

## Diagnosis

The governance (`01-governance`), phase (`02-phases`), and research (`03-research`) prefixes already provide a useful top-level order. Research manuscripts, the JMP deck, and the research statement nevertheless share one flat directory. Teaching is split between `04-cv-teaching` and three institution/location roots. Media creation is split between `05-media-links`, `podcast`, and `poésie`. The poem TeX files correctly sit beside their compiled PDFs, but their `../assets/...` references point to four absent background files. The JMP TeX likewise sits beside its PDF but lacks three referenced assets. Revision numbers are encoded inconsistently in filenames, and a folder location (`Niort`) conflicts with the institution printed in the files.

No duplicate file with identical SHA-256 was found among the supplied research, teaching, and poetry PDFs during this iteration. Naming ambiguities remain: numbered `paper-1.pdf` files hide titles; `Séance I.pdf` mixes Roman numerals and accents; poem suffixes such as `_3_J2` are not self-describing versions; and `EEA_17-08-26` uses an ambiguous short date.

## Recommended structure

```text
andersonvil-academic-os-sources/
  01-governance/
  02-phases/
  03-research/
    paper-01-one-adult-household/
      manuscript/  slides/  figures/  data-documentation/
    paper-02-labor-supply-equivalence-scales/
      manuscript/  slides/  figures/  data-documentation/
    paper-03-job-market-paper/
      manuscript/  slides/  figures/  data-documentation/
    research-statement/
  04-teaching/
    cy-cergy-paris-universite/2026-2027/econometrics/{l3,m1,m2}/
    institution-to-confirm/2026-2027/microeconomics/l3-niort/
    sciences-po-saint-germain-en-laye/2026-2027/microeconomics/l1/
  05-media/
    portraits/  podcast/
  06-beyond-economics/
    poetry/<poem-slug>/{source,pdf,assets}/
```

`institution-to-confirm` is deliberate: the Niort source path, printed CY Cergy Paris Université/THEMA affiliation, and current Université Catholique de l’Ouest role must be reconciled before migration.

## Current path → proposed path

| Current path | Proposed path |
|---|---|
| `03-research/paper-1.pdf` | `03-research/paper-01-one-adult-household/manuscript/children-costs-one-adult-household_2026-01-12.pdf` |
| `03-research/paper-2.pdf` | `03-research/paper-02-labor-supply-equivalence-scales/manuscript/children-labor-supply-equations_2026-09-04.pdf` |
| `03-research/paper-3.pdf` | `03-research/paper-03-job-market-paper/manuscript/household-demand-leisure_2026-09-18.pdf` |
| `03-research/EEA_17-08-26.{tex,pdf}` | `03-research/paper-03-job-market-paper/slides/eea-2026_2026-08-17.{tex,pdf}` |
| `03-research/Research-Statement.pdf` | `03-research/research-statement/research-statement_<confirmed-date>.pdf` |
| `04-cv-teaching/cv-academic.pdf` | `01-governance/cv/anderson-vil-cv_<confirmed-date>.pdf` |
| `CY-Cergy-Paris-Université/2026-27/L3-Econometrics/*` | `04-teaching/cy-cergy-paris-universite/2026-2027/econometrics/l3/*` |
| `CY-Cergy-Paris-Université/2026-27/M1-Econometrics/*` | `04-teaching/cy-cergy-paris-universite/2026-2027/econometrics/m1/*` |
| `CY-Cergy-Paris-Université/2026-27/M2-Econometrics/*` | `04-teaching/cy-cergy-paris-universite/2026-2027/econometrics/m2/*` |
| `Saint-Germain-en-Laye/2026-27/Conférence-de-Méthode-Micro/*` | `04-teaching/sciences-po-saint-germain-en-laye/2026-2027/microeconomics/l1/*` |
| `Niort/2026-27/Microéconomie III/*` | `04-teaching/institution-to-confirm/2026-2027/microeconomics/l3-niort/*` |
| `podcast/SugBook2.mp4` | `05-media/podcast/on-reading-preferences/source/on-reading-preferences.mp4` |
| `poésie/<name>.tex` | `06-beyond-economics/poetry/<slug>/source/<slug>.tex` |
| `poésie/<name>.pdf` | `06-beyond-economics/poetry/<slug>/pdf/<slug>_<yyyy-mm-dd>.pdf` |
| missing `assets/<background>` references | `06-beyond-economics/poetry/<slug>/assets/<descriptive-name>.<ext>` |

## Naming and version rules

- Use lowercase ASCII slugs for directories and machine-facing filenames; retain accents and formal titles in metadata.
- Use `YYYY-MM-DD`, never two-digit ambiguous dates.
- Keep the canonical editable source, its compiled artifact, and declared assets in sibling subdirectories under one work.
- Put the semantic version or dated release in metadata and in immutable release filenames; avoid unexplained suffixes such as `J2` or `B3`.
- Maintain one `manifest.toml` per work with title, authors, source entry point, build engine, dependencies, rights, publication status, and SHA-256 of approved artifacts.
- Treat generated PDFs as build outputs. Keep only approved releases; place intermediate `.aux`, `.log`, `.nav`, `.snm`, and `.toc` files outside the source tree.
- Add data documentation only—study references, variable manifests, and access conditions—never restricted raw survey data.

## Migration order

1. Resolve the Niort institutional wording and the role of `04-cv-teaching`.
2. Create manifests and calculate hashes before moving anything.
3. Migrate one research paper and one poem as pilots; update TeX asset paths and verify reproducible builds.
4. Migrate teaching by institution/year/level/course.
5. Deduplicate only after byte hashes and human confirmation; preserve an immutable migration map.
6. Run site copy validation against the new manifest paths before retiring old aliases.
