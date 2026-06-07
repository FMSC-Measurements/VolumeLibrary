# Fork layout

This fork diverges from
[FMSC-Measurements/VolumeLibrary](https://github.com/FMSC-Measurements/VolumeLibrary)
in two ways:

## Active development (root)

| Path | Purpose |
|------|---------|
| `fortran_build/` | gfortran build scripts, source manifest, warning baseline |
| `nvel/` | Python ctypes wrapper for `libnvel.so` |
| `tests/` | pytest golden-file regression tests |
| Root `*.f` / `*.for` / `*.inc` | NVEL Fortran sources |

Build: `fortran_build/build_gfortran_shared.sh` → `build/libnvel.so`

## Legacy archive (`_legacy/`)

Pre-existing Windows/Intel tooling, C# test harness, duplicate case
directories, and dated snapshots live under [`_legacy/`](_legacy/README.md).
That content is **fork housekeeping** — not intended for upstream PRs.

## Syncing with upstream

Merge or rebase `upstream/main` into this fork for new Fortran releases.
Conflicts on archived paths are rare; resolve by keeping upstream changes
in `_legacy/` if needed.

## PRs to upstream

Open PRs only for shared work: Fortran fixes, `fortran_build/`, `nvel/`,
`tests/`, CI. Do not PR `_legacy/` moves to FMSC.
