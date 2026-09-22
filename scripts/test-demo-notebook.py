"""Execute code cells from the synthetic demonstration notebook."""

from __future__ import annotations

import json
from pathlib import Path


project_dir = Path(__file__).resolve().parents[1] / "static" / "files" / "demo" / "synthetic-household-project"
notebook = json.loads((project_dir / "demo_notebook.ipynb").read_text(encoding="utf-8"))
namespace: dict[str, object] = {}
original_cwd = Path.cwd()

try:
    import os

    os.chdir(project_dir)
    for cell in notebook["cells"]:
        if cell.get("cell_type") == "code":
            exec("".join(cell.get("source", [])), namespace)
finally:
    os.chdir(original_cwd)

print("NOTEBOOK EXECUTION: PASS")
