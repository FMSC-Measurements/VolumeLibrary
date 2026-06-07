#!/usr/bin/env python3
"""Compare warning inventories against a baseline.

Exits non-zero on regression.
"""

from __future__ import annotations

import argparse
import csv
import re
import sys
from collections import Counter
from pathlib import Path

from warning_taxonomy import Tier

FORTRAN_BUILD = Path(__file__).resolve().parent
DEFAULT_BASELINE = FORTRAN_BUILD / "warnings_inventory_baseline.csv"
DEFAULT_CURRENT = FORTRAN_BUILD / "warnings_inventory.csv"

_FORTRAN_TYPE_QUOTED_RE = re.compile(
    r"'(REAL|INTEGER|LOGICAL|COMPLEX)\((\d+)\)'",
    re.IGNORECASE,
)


def normalize_warning_message(message: str) -> str:
    """Normalize gfortran wording drift for stable comparison keys.

    gfortran versions differ in prefixes (``Possible change`` vs
    ``Change of value``) and whether type names are quoted.
    """
    msg = message.strip()
    if msg.startswith("Possible "):
        msg = msg[len("Possible ") :]
    msg = msg.replace("\u2018", "'").replace("\u2019", "'")
    msg = _FORTRAN_TYPE_QUOTED_RE.sub(r"\1(\2)", msg)
    return msg.casefold()


def load_rows(path: Path) -> list[dict]:
    """Load warning inventory rows from a CSV file.

    Args:
        path: Path to an inventory CSV from
            :mod:`parse_build_warnings`.

    Returns:
        List of row dicts keyed by CSV column names.
    """
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def row_key(row: dict) -> tuple:
    """Return a hashable key that identifies one warning occurrence.

    Args:
        row: Inventory row dict.

    Returns:
        Tuple of file, line, column, warning type, and normalized
        message text.
    """
    return (
        row.get("file", ""),
        row.get("line", ""),
        row.get("column", ""),
        row.get("warning_type", ""),
        normalize_warning_message(row.get("message", "")),
    )


def main() -> int:
    """Compare current and baseline inventories.

    Returns:
        0 on pass, 1 on regression.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "current",
        nargs="?",
        default=str(DEFAULT_CURRENT),
        help="Current inventory CSV",
    )
    parser.add_argument(
        "-b",
        "--baseline",
        default=str(DEFAULT_BASELINE),
        help="Baseline inventory CSV",
    )
    parser.add_argument(
        "--allow-increase",
        type=int,
        default=0,
        help="Permitted increase in total warning count (default 0)",
    )
    args = parser.parse_args()

    baseline_path = Path(args.baseline)
    current_path = Path(args.current)
    if not baseline_path.exists():
        print(f"Missing baseline: {baseline_path}", file=sys.stderr)
        return 1
    if not current_path.exists():
        print(f"Missing current inventory: {current_path}", file=sys.stderr)
        return 1

    baseline = load_rows(baseline_path)
    current = load_rows(current_path)

    base_tier = Counter(r.get("tier", "?") for r in baseline)
    cur_tier = Counter(r.get("tier", "?") for r in current)

    base_keys = {row_key(r) for r in baseline}
    cur_keys = {row_key(r) for r in current}
    new_rows = cur_keys - base_keys
    fixed_rows = base_keys - cur_keys

    new_tier_a = sum(
        1
        for r in current
        if row_key(r) in new_rows and r.get("tier") == Tier.A.label
    )
    delta = len(current) - len(baseline)

    print(f"Baseline: {len(baseline)} warnings ({dict(base_tier)})")
    print(f"Current:  {len(current)} warnings ({dict(cur_tier)})")
    print(f"Delta:    {delta:+d} (fixed {len(fixed_rows)}, new {len(new_rows)})")

    failed = False
    if delta > args.allow_increase:
        print(f"FAIL: total warnings increased by {delta} (allowed {args.allow_increase})")
        failed = True
    if new_tier_a:
        print(f"FAIL: {new_tier_a} new Tier A warning(s)")
        failed = True
        for r in current:
            if row_key(r) in new_rows and r.get("tier") == Tier.A.label:
                print(f"  + {r['file']}:{r['line']} {r['warning_type']}: {r['message'][:80]}")
    if not failed:
        print("PASS: no warning regression")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
