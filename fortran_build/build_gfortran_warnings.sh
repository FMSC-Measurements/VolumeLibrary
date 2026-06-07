#!/usr/bin/env bash
# Compile VolumeLibrary Fortran sources with gfortran flags to capture warnings.
# Usage (from repo root):
#   fortran_build/build_gfortran_warnings.sh
#   python3 fortran_build/parse_build_warnings.py

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FORTRAN_BUILD="$ROOT/fortran_build"
OUT_DIR="$FORTRAN_BUILD/gfortran_obj"
SOURCE_LIST="$FORTRAN_BUILD/nvel_fortran_sources.txt"
LOG="$FORTRAN_BUILD/gfortran_build.log"

# shellcheck source=build_flags.conf
source "$FORTRAN_BUILD/build_flags.conf"
FFLAGS="$FFLAGS -J$OUT_DIR -I$OUT_DIR"

if [[ ! -f "$SOURCE_LIST" ]]; then
  python3 "$FORTRAN_BUILD/generate_source_manifest.py"
fi

mkdir -p "$OUT_DIR"
cd "$ROOT"
: > "$LOG"

echo "VolumeLibrary gfortran warning capture" | tee -a "$LOG"
echo "ROOT=$ROOT" | tee -a "$LOG"
echo "SOURCE_LIST=$SOURCE_LIST" | tee -a "$LOG"
echo "FFLAGS=$FFLAGS" | tee -a "$LOG"
echo "---" | tee -a "$LOG"

echo "---" | tee -a "$LOG"

while IFS= read -r src || [[ -n "$src" ]]; do
    [[ -z "$src" ]] && continue
    if [[ ! -f "$src" ]]; then
      echo "SKIP missing: $src" | tee -a "$LOG"
      continue
    fi
    base="$(basename "$src")"
    obj="${base%.*}.o"
    echo "gfortran $FFLAGS -c -o $OUT_DIR/$obj $src" | tee -a "$LOG"
    gfortran $FFLAGS -c -o "$OUT_DIR/$obj" "$src" 2>&1 | tee -a "$LOG" || true
done < "$SOURCE_LIST"

echo "---" | tee -a "$LOG"
echo "Done. Log: $LOG" | tee -a "$LOG"
echo "Parse: python3 fortran_build/parse_build_warnings.py $LOG"
