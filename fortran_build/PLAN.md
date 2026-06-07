# VolumeLibrary Fortran Warning Remediation Plan

## Goal

Reduce gfortran `-Wall` warnings across **repo-native NVEL sources**, without changing volume/biomass numerical results for regression-tested scenarios.

## Strategy

This fork treats VolumeLibrary as a **self-contained project**:

- Own source manifest (`nvel_fortran_sources.txt`), compiler flags (`build_flags.conf`), warning baseline, and pytest golden tests
- **Phase 0** (prerequisite): Python ctypes wrapper + `libnvel.so` — see [`tests/README.md`](../tests/README.md) when implemented
- **Phase 1**: Warning remediation batches backed by warning CI + pytest
- **FVS** is a downstream consumer (embeds NVEL at `volume/NVEL`); optional manual smoke before large upstream PRs — not a daily gate

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

| Phase | Work | Gate | Status |
|-------|------|------|--------|
| 0 | Repo manifest, shared library, ctypes, initial goldens | `pytest tests/` passes | COMPLETE |
| 1a | Re-capture warning baseline on repo-native manifest | Inventory committed | COMPLETE |
| 1b | Tier A/B fix batches | Warning count down; pytest unchanged for Tier A | READY TO START |
| 2 | Upstream PRs to FMSC | Fork CI green; optional FVS smoke | READY TO START |

## Context

### Repo-native baseline (active)

| Item | Value / path |
|------|----------------|
| Source manifest | `nvel_fortran_sources.txt` (121 files) |
| Warning baseline | **1,371** warnings in **94** files (2026-06-06 capture) |
| Tier A / B / C | 365 / 997 / 9 |
| Numerical tests | `tests/goldens/cases.json` + `pytest tests/` |

Suggested compiler flags (document in `build_flags.conf`):

```
-fPIC -g -cpp -Wall
-ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace
```

Add `-D` defines only when needed; do not assume FVS `-DCMPgcc` unless a specific `#ifdef` branch requires it.

## Tier classification

Counts below are from the **current repo-native baseline** (`warnings_inventory_baseline.csv`, 2026-06-06 capture on the VolumeLibrary manifest).

### Tier A — Fix first (correctness risk)

| Category | Warnings | Files | Typical fix |
|----------|--------:|------:|-------------|
| `type_conversion` | 317 | 39 | Explicit `REAL()` / `DBLE()` / `INT()` / single-precision literals |
| `character_truncation` | 29 | 2 | Substring `(1:n)` or align declarations |
| `uninitialized` | 17 | 9 | Initialize at declaration or before use |
| `integer_division` | 2 | 1 | Use `REAL()` / `DBLE()` before division or explicit `NINT()` |

**Tier A batches require pytest goldens** recorded before edits. Re-run `pytest` after fixes.

### Tier B — Hygiene

| Category | Warnings | Files | Typical fix |
|----------|--------:|------:|-------------|
| `unused_variable` | 552 | 54 | Remove dead locals |
| `tab_character` | 302 | 18 | Spaces instead of tabs |
| `unused_dummy_argument` | 82 | 37 | Remove from interface or document + scratch use |
| `unused_label` | 32 | 16 | Remove unused labels |
| `deleted_feature` | 27 | 6 | Replace deleted Fortran features (e.g. `PAUSE`, `DO` without loop var) |
| `extension` | 2 | 2 | Remove or guard non-standard extensions |

Tier B batches need warning regression only.

### Tier C — Defer

| Category | Warnings | Files | Notes |
|----------|--------:|------:|-------|
| `large_stack_array` | 9 | 3 | Document; NVEL single-threaded usage |

## Fix batches

Batch ordering unchanged. Per-file counts below are approximate Tier A totals from the repo-native baseline (`warnings_inventory_baseline.csv`).

### Batch 1 — Tier A regional shape files

Highest impact, similar patterns (`REAL(8)` literals → `REAL*4`):

1. `f_west.f` (~70 Tier A)
2. `f_other.f` (~53)
3. `f_alaska.f` (~29)
4. `r10vol1.f` (~28)
5. `honer.f` (~21)
6. `f_ingy.f`, `sf_taper.f`, `nsvb.f`, `fiaeq2nveleq.for`, `r10volo.f`

**Pattern:** DATA blocks use `D0` double literals for `REAL*4` arrays; assignments from `REAL*8` intermediates to `REAL*4` outputs. Fix with single-precision literals or explicit `REAL()` at assignment.

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

Compare to `warnings_inventory_baseline.csv`. Fail if total count increases or new Tier A warnings appear in touched files.

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

Not required per batch. FVS embeds this repo and may compile a slightly different file subset; use as a courtesy check if FMSC asks or before merging large Tier A work.

```bash
# In ForestVegetationSimulator: point volume/NVEL at your commit, then:
cd bin && make clean && make FVSpn
cd ../tests/FVSpn && make
```

### ifort (manual, Windows)

Production DLL builds use Intel ifort (`compdll.bat`, `vollib.vfproj`). Spot-check Release builds after Tier A batches if you maintain DLL releases. Not a CI gate.

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
