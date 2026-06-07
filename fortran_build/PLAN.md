# VolumeLibrary Fortran Warning Remediation Plan

## Goal

Reduce gfortran `-Wall` warnings across **repo-native NVEL sources**, without changing
volume/biomass numerical results for regression-tested scenarios.

## Strategy

This fork treats VolumeLibrary as a **self-contained project**:

- Own source manifest (`nvel_fortran_sources.txt`), compiler flags (`build_flags.conf`),
  warning baseline, and pytest golden tests
- **Phase 0** (prerequisite): Python ctypes wrapper + `libnvel.so` — see [`tests/README.md`](../tests/README.md) when implemented
- **Phase 1**: Warning remediation batches backed by warning CI + pytest
- **FVS** is a downstream consumer (embeds NVEL at `volume/NVEL`); optional manual smoke
  before large upstream PRs — not a daily gate

Upstream target: [FMSC-Measurements/VolumeLibrary](https://github.com/FMSC-Measurements/VolumeLibrary)

## Execution order

```mermaid
flowchart LR
  manifest[Source manifest + flags]
  wrapper[Python wrapper + libnvel.so]
  goldens[Record pytest goldens]
  warnings[Warning fix batches]
  upstream[PR to FMSC]

  manifest --> wrapper --> goldens --> warnings --> upstream
```

| Phase | Work | Gate |
|-------|------|------|
| 0 | Repo manifest, shared library, ctypes, initial goldens | `pytest tests/` passes |
| 1a | Re-capture warning baseline on repo-native manifest | Inventory committed |
| 1b | Tier A/B fix batches | Warning count down; pytest unchanged for Tier A |
| 2 | Upstream PRs to FMSC | Fork CI green; optional FVS smoke |

## Context

### Repo-native baseline (active)

| Item | Value / path |
|------|----------------|
| Source manifest | `nvel_fortran_sources.txt` (121 files) |
| Warning baseline | **1,365** warnings in **94** files (2026-06-06 capture) |
| Tier A / B / C | 360 / 996 / 9 |
| Numerical tests | `tests/goldens/cases.json` + `pytest tests/` |

Historical FVS inventory (1,370 warnings): [`reference/fvs_migration/`](reference/fvs_migration/)

Suggested compiler flags (document in `build_flags.conf`):

```
-fPIC -g -cpp -Wall
-ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace
```

Add `-D` defines only when needed; do not assume FVS `-DCMPgcc` unless a specific
`#ifdef` branch requires it.

## Tier classification

### Tier A — Fix first (correctness risk)

| Category | Count (FVS baseline) | Typical fix |
|----------|---------------------:|-------------|
| `type_conversion` | ~320 | Explicit `REAL()` / `DBLE()` / `INT()` / single-precision literals |
| `character_truncation` | ~33 | Substring `(1:n)` or align declarations |
| `uninitialized` | ~14 | Initialize at declaration or before use |

**Tier A batches require pytest goldens** recorded before edits. Re-run `pytest` after fixes.

### Tier B — Hygiene

| Category | Count (FVS baseline) | Typical fix |
|----------|---------------------:|-------------|
| `unused_variable` | ~570 | Remove dead locals |
| `tab_character` | ~300 | Spaces instead of tabs |
| `unused_dummy_argument` | ~80 | Remove from interface or document + scratch use |
| `other_Wunused-label` | ~33 | Remove unused labels |

Tier B batches need warning regression only.

### Tier C — Defer

| Category | Count | Notes |
|----------|------:|-------|
| `large_stack_array` | ~22 | Document; NVEL single-threaded usage |

## Fix batches

Batch ordering unchanged; counts are from the FVS-migrated inventory and will be
refreshed after repo-native baseline capture.

### Batch 1 — Tier A regional shape files

Highest impact, similar patterns (`REAL(8)` literals → `REAL*4`):

1. `f_west.f` (~70 Tier A)
2. `f_other.f` (~53)
3. `f_alaska.f` (~29)
4. `r10vol1.f` (~28)
5. `honer.f` (~21)
6. `f_ingy.f`, `sf_taper.f`, `nsvb.f`, `fiaeq2nveleq.for`, `r10volo.f`

**Pattern:** DATA blocks use `D0` double literals for `REAL*4` arrays; assignments from
`REAL*8` intermediates to `REAL*4` outputs. Fix with single-precision literals or
explicit `REAL()` at assignment.

Add pytest golden cases for each region touched (e.g. R10 western, Alaska).

### Batch 2 — Tier A taper/volume routines

`r6vol1.f`, `formclas.f`, `r10tap.f`, `r1tap.f`, `r2tap.f`, `r5harv.f`, `r8vol2.f`,
`scrib.f`, `sf_yhat.f`, `volinit.f`, `r6dibs.f`, `volinit2.f`, `r8clkdib.f`

### Batch 3 — Wrapper / entry points (high warning count, mixed tiers)

`fia_vol_r5610.for`, `vollibfia.f`, `volumelibrary.f`, `pmtprofile.f`, `r9clark.f`,
`r9clarkdib.f`, `vollibcs.f`

Fix after underlying routines stabilize. Expand pytest coverage for `vollib_r` paths.

### Batch 4 — Tier B bulk

Unused variables and tab characters across remaining files.

### Batch 5 — Tier C

Document `large_stack_array` warnings; optional `-fmax-stack-var-size` note.

## Verification

### Warning regression (every batch)

```bash
fortran_build/build_gfortran_warnings.sh
python3 fortran_build/parse_build_warnings.py
python3 fortran_build/compare_warnings.py   # when implemented
```

Compare to `warnings_inventory_baseline.csv`. Fail if total count increases or new
Tier A warnings appear in touched files.

### Numerical regression (Tier A and wrapper batches)

```bash
fortran_build/build_gfortran_shared.sh      # when implemented
pytest tests/ -v
```

Record goldens before Tier A edits:

```bash
python3 tests/record_goldens.py
```

### Optional FVS smoke (before large upstream PRs)

Not required per batch. FVS embeds this repo and may compile a slightly different file
subset; use as a courtesy check if FMSC asks or before merging large Tier A work.

```bash
# In ForestVegetationSimulator: point volume/NVEL at your commit, then:
cd bin && make clean && make FVSpn
cd ../tests/FVSpn && make
```

### ifort (manual, Windows)

Production DLL builds use Intel ifort (`compdll.bat`, `vollib.vfproj`). Spot-check
Release builds after Tier A batches if you maintain DLL releases. Not a CI gate.

## Upstream workflow

```mermaid
flowchart LR
  fork[VolumeLibrary_fork]
  ci[Fork_CI_warnings_pytest]
  pr[PR_to_FMSC]
  fvs_smoke[Optional_FVS_smoke]

  fork --> ci --> pr
  pr -.-> fvs_smoke
```

1. Branch on fork
2. Fix batch; run warning capture + pytest (Tier A)
3. PR to FMSC-Measurements/VolumeLibrary with CI evidence (see `PR_EVIDENCE.md` when added)
4. Optional: FVS smoke before or after merge

Do **not** bundle Phase 0 wrapper infrastructure with warning-fix PRs to upstream.

## Out of scope

**FVS repo fixes** (not NVEL sources): `base/comprs.f`, `vbase/initre.f`, `fire/`, etc.

**Deferred wrapper work:** `VOLLIBCS`, biomass, merchandising rules, PyPI packaging.

## Success criteria

- Repo-native warning baseline captured and trending down
- Zero Tier A warnings in active capture (or documented exceptions)
- `pytest tests/` passes; goldens cover regions/equations in each Tier A batch
- Upstream PRs accepted by FMSC with fork CI evidence
- Optional FVS smoke documented when run
