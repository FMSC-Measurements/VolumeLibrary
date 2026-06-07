#!/usr/bin/env python3
"""Parse a gfortran build log into a structured warning inventory."""

from __future__ import annotations

import argparse
import csv
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

from warning_taxonomy import (CLASSIFICATION_RULES, INVENTORY_FIELDNAMES,
                              OTHER_UNKNOWN, Tier)

FORTRAN_BUILD = Path(__file__).resolve().parent
SOURCE_LIST = FORTRAN_BUILD / "nvel_fortran_sources.txt"
DEFAULT_BUILD_LOG = FORTRAN_BUILD / "gfortran_build.log"
DEFAULT_INVENTORY_CSV = FORTRAN_BUILD / "warnings_inventory.csv"
DEFAULT_SUMMARY_MD = FORTRAN_BUILD / "warnings_summary.md"

FILE_LOC_RE = re.compile(r"^([\w.-]+\.(?:f|F|for|inc|INC|F77)):(\d+):(\d+):$")
WARN_RE = re.compile(r"^Warning:\s*(.+)$")
TAB_RE = re.compile(r"^f951:\s*Warning:\s*(.+)$")
GFORTRAN_CMD_RE = re.compile(r"gfortran\s+.*\s-c(?:\s+-o\s+\S+\.o)?\s+(\S+\.(?:f|F|for))\s*$")

UnclassifiedWarning = tuple[str, str]


def load_source_map() -> dict[str, list[str]]:
    """Map basename to repo-relative paths from the source manifest.

    Returns:
        Dict keyed by Fortran filename; values are manifest paths.
    """
    mapping: dict[str, list[str]] = defaultdict(list)
    for line in SOURCE_LIST.read_text().splitlines():
        line = line.strip()
        if line:
            mapping[Path(line).name].append(line)
    return mapping


def classify(
    message: str,
    *,
    unclassified: set[UnclassifiedWarning] | None = None,
) -> tuple[str, Tier]:
    """Assign category and tier to a gfortran warning message.

    Args:
        message: Warning text from the build log.
        unclassified: When set, records ``(warning_type, message)`` for
            warnings that matched no :data:`CLASSIFICATION_RULES` entry.

    Returns:
        Tuple of ``(warning_type, tier)``.
    """
    message_lower = message.lower()
    flag = ""
    if "[-" in message:
        flag = message.rsplit("[-", 1)[-1].rstrip("]")

    for rule in CLASSIFICATION_RULES:
        if rule.matches(message_lower):
            return rule.warning_type, rule.tier

    other_type = f"other_{flag or OTHER_UNKNOWN}"
    if unclassified is not None:
        unclassified.add((other_type, message))
    # Unmatched warnings land in Tier B until a rule is added.
    return other_type, Tier.B


def report_unclassified(unclassified: set[UnclassifiedWarning]) -> None:
    """Print deduplicated notices for warnings with no taxonomy rule."""
    if not unclassified:
        return
    for warning_type, message in sorted(unclassified):
        print(
            f"Unclassified gfortran warning [{warning_type}]: {message}",
            file=sys.stderr,
        )
    print(
        f"Found {len(unclassified)} distinct unclassified warning(s); "
        "add rules in warning_taxonomy.py",
        file=sys.stderr,
    )


def resolve_repo_path(
    loc_file: str, current_source: str | None, source_map: dict[str, list[str]]
) -> str:
    """Resolve a log location to a repo-relative source path.

    Args:
        loc_file: Filename from a ``file:line:col:`` log prefix.
        current_source: Basename of the unit being compiled, if known.
        source_map: Basename-to-path map from :func:`load_source_map`.

    Returns:
        Best-effort repo-relative path for the warning location.
    """
    if loc_file == current_source:
        paths = source_map.get(loc_file, [])
        return paths[0] if paths else loc_file

    paths = source_map.get(loc_file, [])
    if len(paths) == 1:
        return paths[0]
    if paths:
        return paths[0]

    if current_source:
        parent_paths = source_map.get(current_source, [])
        if parent_paths:
            parent_dir = str(Path(parent_paths[0]).parent)
            return f"{parent_dir}/{loc_file}" if parent_dir != "." else loc_file
    return loc_file


def parse_build_log(
    build_log: Path,
    source_map: dict[str, list[str]],
    *,
    unclassified: set[UnclassifiedWarning] | None = None,
) -> list[dict]:
    """Extract structured warning rows from a gfortran build log.

    Expects output from :program:`build_gfortran_warnings.sh`: echoed
    ``gfortran -c ... file.f`` commands, then per-warning location and
    message lines::

        f_west.f:142:18:
        Warning: conversion from REAL(8) to REAL(4) ...

    The parser walks the log once, tracking the current compile unit and
    pairing each ``file:line:col:`` prefix with the following warning
    text. Lines that do not match that shape are skipped.

    Args:
        build_log: Path to ``gfortran_build.log``.
        source_map: Basename-to-path map from :func:`load_source_map`.
        unclassified: Optional set populated by :func:`classify` for
            warnings that matched no taxonomy rule.

    Returns:
        List of dicts with warning metadata and repo-relative paths.
    """
    rows: list[dict] = []
    current_source: str | None = None
    # gfortran: location line ("file:line:col:") then "Warning: ..." on next line
    pending_loc: tuple[str, int, int] | None = None

    for raw_line in build_log.read_text(errors="replace").splitlines():
        line = raw_line.rstrip()

        cmd_match = GFORTRAN_CMD_RE.search(line)
        if cmd_match:
            current_source = cmd_match.group(1)

        loc_match = FILE_LOC_RE.match(line)
        if loc_match:
            # Defer until the next line has the warning text.
            pending_loc = (loc_match.group(1), int(loc_match.group(2)), int(loc_match.group(3)))
            continue

        warn_match = WARN_RE.match(line) or TAB_RE.match(line)
        if warn_match and pending_loc:
            loc_file, line_no, col_no = pending_loc
            message = warn_match.group(1).strip()
            category, tier = classify(message, unclassified=unclassified)
            repo_path = resolve_repo_path(loc_file, current_source, source_map)
            rows.append(
                {
                    "warning_type": category,
                    "tier": tier.label,
                    "tier_weight": tier.weight,
                    "file": repo_path,
                    "line": line_no,
                    "column": col_no,
                    "message": message,
                    "compile_unit": current_source or "",
                    "loc_file": loc_file,
                }
            )
            pending_loc = None  # paired; next warning needs a fresh location line

    return rows


def write_summary(rows: list[dict], build_log: Path, out_summary: Path, out_csv: Path) -> None:
    """Write a markdown summary of parsed warnings.

    Args:
        rows: Parsed warning rows from :func:`parse_build_log`.
        build_log: Source log path (referenced in the summary header).
        out_summary: Output markdown file path.
        out_csv: Inventory CSV path (referenced in the summary header).
    """
    by_category = Counter(r["warning_type"] for r in rows)
    by_tier = Counter(r["tier"] for r in rows)
    by_file = Counter(r["file"] for r in rows)

    ranked = sorted(by_file.items(), key=lambda kv: (-kv[1], kv[0]))[:25]

    lines = [
        "# VolumeLibrary gfortran Warning Summary",
        "",
        f"Source log: `{build_log}`",
        f"Inventory: `{out_csv.name}`",
        "",
        f"- **Total warnings:** {len(rows)}",
        f"- **Files:** {len(by_file)}",
        f"- **Tier A / B / C:** {by_tier.get(Tier.A.label, 0)} / "
        f"{by_tier.get(Tier.B.label, 0)} / {by_tier.get(Tier.C.label, 0)}",
        "",
        "## By category",
        "",
        "| Category | Count |",
        "|----------|------:|",
    ]
    for cat, count in by_category.most_common():
        lines.append(f"| `{cat}` | {count} |")

    lines.extend(["", "## Top files", "", "| File | Count |", "|------|------:|"])
    for path, count in ranked:
        lines.append(f"| `{path}` | {count} |")

    lines.append("")
    out_summary.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> int:
    """Parse a build log into CSV inventory and markdown summary."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "build_log",
        nargs="?",
        default=str(DEFAULT_BUILD_LOG),
        help="Path to gfortran build log",
    )
    parser.add_argument(
        "-o",
        "--csv",
        default=str(DEFAULT_INVENTORY_CSV),
        help="Output CSV path",
    )
    parser.add_argument(
        "-s",
        "--summary",
        default=str(DEFAULT_SUMMARY_MD),
        help="Output summary markdown path",
    )
    args = parser.parse_args()

    build_log = Path(args.build_log)
    if not build_log.exists():
        print(f"Missing build log: {build_log}", file=sys.stderr)
        return 1

    source_map = load_source_map()
    unclassified: set[UnclassifiedWarning] = set()
    rows = parse_build_log(build_log, source_map, unclassified=unclassified)
    report_unclassified(unclassified)

    fieldnames = list(INVENTORY_FIELDNAMES)
    out_csv = Path(args.csv)
    with out_csv.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)

    write_summary(rows, build_log, Path(args.summary), out_csv)
    print(f"Wrote {len(rows)} warnings to {out_csv}")
    print(f"Wrote {args.summary}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
