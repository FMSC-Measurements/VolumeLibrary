#!/usr/bin/env python3
"""Generate nvel_fortran_sources.txt compile list for VolumeLibrary."""

from __future__ import annotations

import argparse
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FORTRAN_BUILD = Path(__file__).resolve().parent
DEFAULT_OUT = FORTRAN_BUILD / "nvel_fortran_sources.txt"

# Root-level snapshots / duplicates not linked into the library
EXCLUDE_NAMES = {
    "volumelibrary_20210727test.f",
}


# Compiled first — Fortran MODULE definitions required by other units
MODULE_PRIORITY = (
    "charmod.f",
    "debug_mod.f",
    "mrules_mod.f",
    "volinput_mod.f",
    "clkcoef_mod.f",
    "r8dib.f",
    "r8vlist.f",
)


def discover_sources(root: Path) -> list[str]:
    """Collect Fortran sources under ``root`` in compile order.

    Args:
        root: Repository root containing ``*.f`` sources.

    Returns:
        Relative posix paths: module priority files first, then A–Z.
    """
    paths: list[Path] = []
    for pattern in ("*.f", "*.F", "*.for", "*.FOR"):
        paths.extend(root.glob(pattern))
    rel: list[str] = []
    for path in sorted(paths, key=lambda p: p.name.lower()):
        if path.name in EXCLUDE_NAMES:
            continue
        rel.append(path.relative_to(root).as_posix())

    priority = [p for p in MODULE_PRIORITY if p in rel]
    rest = sorted(set(rel) - set(priority), key=str.lower)
    return priority + rest


def main() -> int:
    """Write the NVEL Fortran source manifest."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=DEFAULT_OUT,
        help=f"Output manifest path (default: {DEFAULT_OUT.name})",
    )
    parser.add_argument(
        "--stdout",
        action="store_true",
        help="Write manifest to stdout instead of a file",
    )
    args = parser.parse_args()

    lines = discover_sources(ROOT)
    text = "\n".join(lines) + "\n"
    if args.stdout:
        print(text, end="")
    else:
        args.output.write_text(text, encoding="utf-8")
        print(f"Wrote {len(lines)} sources to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
