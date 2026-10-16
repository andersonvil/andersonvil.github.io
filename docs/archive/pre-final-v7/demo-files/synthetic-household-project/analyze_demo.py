"""Synthetic demonstration content — not for publication.

Usage: python analyze_demo.py synthetic_households.json
"""

from __future__ import annotations

import json
import statistics
import sys
from pathlib import Path


def load_records(path: Path) -> list[dict[str, object]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if payload.get("synthetic") is not True:
        raise ValueError("This demonstration accepts only explicitly synthetic data.")
    records = payload.get("records")
    if not isinstance(records, list) or not records:
        raise ValueError("The data file must contain a non-empty records list.")
    return records


def grouped_means(records: list[dict[str, object]]) -> dict[str, dict[str, float]]:
    groups: dict[str, list[dict[str, object]]] = {}
    for record in records:
        group = str(record["group"])
        groups.setdefault(group, []).append(record)
    return {
        group: {
            "market_work_hours": statistics.mean(float(row["market_work_hours"]) for row in rows),
            "care_hours": statistics.mean(float(row["care_hours"]) for row in rows),
        }
        for group, rows in sorted(groups.items())
    }


def main() -> None:
    data_path = Path(sys.argv[1] if len(sys.argv) > 1 else "synthetic_households.json")
    results = grouped_means(load_records(data_path))
    print("Synthetic demonstration content — not for publication")
    for group, measures in results.items():
        print(
            f"Group {group}: market work={measures['market_work_hours']:.1f} hours; "
            f"care={measures['care_hours']:.1f} hours"
        )


if __name__ == "__main__":
    main()
