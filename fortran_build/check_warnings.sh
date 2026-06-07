#!/usr/bin/env bash
# Warning regression: compile, parse, compare to baseline.
# Usage (from repo root):
#   fortran_build/check_warnings.sh

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

if [[ ! -f fortran_build/nvel_fortran_sources.txt ]]; then
  python3 fortran_build/generate_source_manifest.py
fi

gfortran --version | head -1

fortran_build/build_gfortran_warnings.sh
python3 fortran_build/parse_build_warnings.py fortran_build/gfortran_build.log
python3 fortran_build/compare_warnings.py
