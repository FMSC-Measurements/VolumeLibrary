# NVEL Python tests

Golden-file regression tests for the repo-native gfortran build, using the [`nvel`](../nvel/README.md) package.

## Dev container

See [`.devcontainer/`](../.devcontainer/) and [`nvel/README.md`](../nvel/README.md#development). The container installs dev dependencies with `uv`, builds `libnvel.so` on first open, and configures ruff/mypy/pre-commit.

## Prerequisites (local)

- `gfortran`
- `python3`, `pytest`

## Build shared library

From repo root:

```bash
python3 fortran_build/generate_source_manifest.py
chmod +x fortran_build/build_gfortran_shared.sh
fortran_build/build_gfortran_shared.sh
```

Output: `build/libnvel.so`

Override library path: `export NVEL_LIB_PATH=/path/to/libnvel.so`

## Run tests

```bash
pip install -e ".[dev]"
ruff check .
ruff format --check .
mypy
pytest -v
```

## Record golden expected values

After building `libnvel.so`, refresh `tests/goldens/cases.json`:

```bash
python3 tests/record_goldens.py
pytest -v
```

Record goldens **before** making changes to Fortran source files; re-run after changes to confirm no regression. Tier rules: [fortran_build/PLAN.md](../fortran_build/PLAN.md). Upstream PR process: [fortran_build/upstream-workflow.md](../fortran_build/upstream-workflow.md).

## API

See [`nvel/README.md`](../nvel/README.md) for the public `NVEL` interface.

## VOL index (vollib_r)

1. Total cubic volume (ground to tip)
2. Gross Scribner board feet (region-dependent)
3. Net Scribner board feet
4–15. Merchantable / secondary / stump / tip volumes (see RSForInvt NVEL docs)
