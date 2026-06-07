# VolumeLibrary Fortran Build and Remediation

Planning and tooling for the National Volume Estimator Library (NVEL) on this fork: warning cleanup, repo-native builds, and Python regression tests.

## Relationship to upstream and FVS

| Role | Repo |
|------|------|
| Upstream NVEL | [FMSC-Measurements/VolumeLibrary](https://github.com/FMSC-Measurements/VolumeLibrary) |
| This fork | Warning fixes + test infrastructure; PR upstream when batches are ready |
| FVS (downstream) | [ForestVegetationSimulator](https://github.com/USDAForestService/ForestVegetationSimulator) embeds NVEL at `volume/NVEL` |

## Workstreams

| Phase | Description | Doc |
|-------|-------------|-----|
| **0** | Python ctypes wrapper, `libnvel.so`, pytest goldens | [`tests/README.md`](../tests/README.md) |
| **1** | gfortran `-Wall` warning remediation | [PLAN.md](PLAN.md) |

Phase 0 infrastructure is in place. Execute Tier A warning batches only after recording pytest goldens.

## Files in this directory

### Active

| File | Purpose |
|------|---------|
| [PLAN.md](PLAN.md) | Remediation plan, tiers, batches, verification |
| [warnings_progress.md](warnings_progress.md) | Batch status tracker |
| [nvel_fortran_sources.txt](nvel_fortran_sources.txt) | Canonical source list (121 root `.f`/`.for`) |
| [build_flags.conf](build_flags.conf) | gfortran flags for this project |
| [generate_source_manifest.py](generate_source_manifest.py) | Regenerate `nvel_fortran_sources.txt` |
| [build_gfortran_warnings.sh](build_gfortran_warnings.sh) | Compile sources, capture warnings |
| [build_gfortran_shared.sh](build_gfortran_shared.sh) | Compile + link `build/libnvel.so` |
| [parse_build_warnings.py](parse_build_warnings.py) | Parse build log → inventory CSV |
| [compare_warnings.py](compare_warnings.py) | Diff inventory vs baseline |
| [check_warnings.sh](check_warnings.sh) | Warning regression orchestration |
| [warnings_inventory_baseline.csv](warnings_inventory_baseline.csv) | Repo-native baseline (**1,371** warnings; GitHub Actions `ubuntu-latest`, gfortran 13) |
| [warnings_summary_baseline.md](warnings_summary_baseline.md) | Baseline tier/category stats |

## Quick start

### Build and test (Phase 0)

```bash
python3 fortran_build/generate_source_manifest.py   # if manifest stale
chmod +x fortran_build/build_gfortran_shared.sh
fortran_build/build_gfortran_shared.sh
pip install pytest
pytest tests/ -v
```

Record goldens before Tier A edits:

```bash
python3 tests/record_goldens.py
```

### Warning regression

Matches the GitHub Actions `warnings` job (`ubuntu-latest`, apt `gfortran`):

```bash
chmod +x fortran_build/check_warnings.sh
fortran_build/check_warnings.sh
```

### Fix a batch and verify

1. Tier A: `python3 tests/record_goldens.py` first, then fix (see [PLAN.md](PLAN.md))
2. `fortran_build/check_warnings.sh`
3. `pytest tests/ -v`
4. Upstream PR with fork CI evidence

### Optional FVS smoke

Before a large upstream PR, see [PLAN.md](PLAN.md).

## Compiler notes

| Toolchain | Use on this fork |
|-----------|------------------|
| **gfortran** (GitHub Actions `ubuntu-latest`) | Warning baseline and CI regression gate |
| **Dev container** (follow-up PR) | Local development; rebaseline when adopted |
| **ifort** | Production DLL on Windows; manual spot-check after Tier A |

## Approach

- Prefer **real code fixes** (casts, remove dead code, align declarations)
- Suppress only when a shared interface requires unused dummy arguments
- Tier C (`large_stack_array`): defer — single-threaded usage
- Separate upstream PRs for wrapper infrastructure vs warning fixes
