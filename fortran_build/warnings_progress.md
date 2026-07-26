# VolumeLibrary Warning Remediation Progress

## Baseline

| Artifact | Path | Notes |
|----------|------|-------|
| Active inventory | `warnings_inventory_baseline.csv` | **1,369** rows; GitHub Actions `ubuntu-latest` (gfortran 13) |
| Active summary | `warnings_summary_baseline.md` | Tier A: 363, B: 997, C: 9 |
| Source list | `nvel_fortran_sources.txt` | 121 root `.f`/`.for` files |
| Build log | `gfortran_build.log` | From `build_gfortran_warnings.sh` |
| Current inventory | `warnings_inventory.csv` | From `parse_build_warnings.py` |
| FVS reference | `reference/fvs_migration/` | Historical 1,370-warning FVS inventory |

## Approach

- Real code fixes preferred over suppression
- PR fixes upstream to FMSC-Measurements/VolumeLibrary — see [upstream-workflow.md](upstream-workflow.md)
- Phase 0 (wrapper + pytest): **done**
- Numerical gate: `pytest tests/`; optional FVS smoke before large upstream PRs

## Batch status

| Batch | Scope | Status | Warnings removed | Notes |
|-------|-------|--------|------------------|-------|
| 0b | Python wrapper + pytest goldens | **done** | — | `build/libnvel.so`, `tests/` |
| 0c | Repo-native warning baseline | **done** | — | 1,371 warnings; CI `ubuntu-latest` gfortran |
| 0d | Tier A: `sf_zero.f` `integer_division` (workflow rehearsal) | **done** | 2 | First upstream PR candidate; see notes below |
| 1 | Tier A: `f_west.f`, `f_other.f`, `f_alaska.f` | pending | — | Record goldens first |
| 2 | Tier A: taper/volume (`r10vol1.f`, `honer.f`, …) | pending | — | |
| 3 | Wrappers (`volumelibrary.f`, `vollibfia.f`, …) | pending | — | |
| 4 | Tier B bulk | pending | — | |
| 5 | Tier C stack arrays | deferred | — | |

## Fixes applied in this repo

| File | Date | Warnings | Summary |
|------|------|----------|---------|
| `sf_zero.f` | 2026-07-26 | 2 `integer_division` | `AMACH`'s `RM1`/`DM1` exponent-halving rewritten as `REAL`/`DBLE` division + explicit `INT()`/`IDINT()` truncation (was raw integer division). Bit-identical `R1MACH`/`D1MACH` output confirmed for indices 1-5; new regression test `tests/test_sf_zero_machine_constants.py`. Upstream PR: pending. |

## Suppressions retained

| File | Warning | Reason |
|------|---------|--------|
| — | — | — |

## Next steps

1. Start Batch 1 on `f_west.f` — record goldens, then type_conversion fixes
2. `check_warnings.sh` + `pytest` after each batch
3. Open upstream PR when batch is clean + fork CI green
