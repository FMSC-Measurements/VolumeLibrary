#!/usr/bin/env bash
# Compile and link NVEL sources into build/libnvel.so
# Usage (from repo root):
#   fortran_build/build_gfortran_shared.sh

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FORTRAN_BUILD="$ROOT/fortran_build"
OBJ_DIR="$ROOT/build/gfortran_obj"
LIB_DIR="$ROOT/build"
SOURCE_LIST="$FORTRAN_BUILD/nvel_fortran_sources.txt"
LIB="$LIB_DIR/libnvel.so"

# shellcheck source=build_flags.conf
source "$FORTRAN_BUILD/build_flags.conf"
FFLAGS="$FFLAGS -J$OBJ_DIR -I$OBJ_DIR"

if [[ ! -f "$SOURCE_LIST" ]]; then
  python3 "$FORTRAN_BUILD/generate_source_manifest.py"
fi

mkdir -p "$OBJ_DIR" "$LIB_DIR"
cd "$ROOT"

objects=()
while IFS= read -r src || [[ -n "$src" ]]; do
  [[ -z "$src" ]] && continue
  if [[ ! -f "$src" ]]; then
    echo "ERROR: missing source: $src" >&2
    exit 1
  fi
  base="$(basename "$src")"
  obj="$OBJ_DIR/${base%.*}.o"
  echo "gfortran $FFLAGS -c -o $obj $src"
  gfortran $FFLAGS -c -o "$obj" "$src"
  objects+=("$obj")
done < "$SOURCE_LIST"

echo "gfortran $LDFLAGS -o $LIB ${objects[*]}"
gfortran $LDFLAGS -o "$LIB" "${objects[@]}"
echo "Built $LIB ($(wc -c < "$LIB") bytes)"
