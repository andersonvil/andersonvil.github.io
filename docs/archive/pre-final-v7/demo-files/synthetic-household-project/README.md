# Synthetic household time-allocation example

**Synthetic demonstration content — not for publication**

Technical flags: `synthetic: true`, `placeholder: true`, `publicationStatus: blocked`.

This project exists only to test the Code & Data page, downloads, documentation, and clean execution. It is not related to a real paper, does not reproduce a research result, and carries no license claim.

## Files

- `synthetic_households.json`: six fictional records and explicit units.
- `analyze_demo.py`: standard-library validation and grouped means.
- `demo_notebook.ipynb`: the same analysis in a minimal notebook.

## Run

```text
python analyze_demo.py synthetic_households.json
```

Expected output:

```text
Synthetic demonstration content — not for publication
Group A: market work=32.0 hours; care=18.0 hours
Group B: market work=40.0 hours; care=9.0 hours
```

## Replacement requirement

Before production, replace this entire folder with a documented, authorized resource. Record its related project, software version, environment, license or rights basis, data-access conditions, and clean execution result.
