# nvel

Python ctypes wrapper for the NVEL volume equation shared library (`libnvel.so`).

## Install

From the repo root (after building `build/libnvel.so`):

```bash
pip install -e ".[dev]"
```

Override the library path:

```bash
export NVEL_LIB_PATH=/path/to/libnvel.so
```

## Development

A **dev container** is provided under [`.devcontainer/`](.devcontainer/) for VS Code and Cursor. Open the repository with **Dev Containers: Reopen in Container** to get gfortran, Python tooling, and extensions preconfigured. On first open, `postCreateCommand` runs `uv sync --frozen --extra dev`, builds `libnvel.so`, and installs pre-commit hooks. After pulling changes that touch the Dockerfile or lockfile, **rebuild** the dev container (or run `uv sync --extra dev` manually).

The `.venv` directory lives on your host via the bind-mounted workspace. You can delete it and re-run `uv sync --extra dev` to recreate.

Without a dev container:

```bash
pip install -e ".[dev]"
ruff check .
ruff format --check .
mypy
pytest -v
```

## Quick start

```python
from nvel import NVEL, Site

with NVEL() as nvel:
    print(nvel.version())

    result = nvel.calc_volume(
        region=9,
        forest="01",
        district="1",
        species=837,
        dbh=5.1,
        total_height=25.4,
        height_to_second_product=9.0,
    )
    print(result.volume_equation, result.total_cuft)

    site = Site(region=9, forest="01", district="1", species=837)
    nvel.calc_volume(site, dbh=12.0, total_height=80.0)

    nvel.calc_volume(
        site,
        dbh=5.1,
        total_height=25.4,
        volume_equation="616BEHW000",
    )

    print(nvel.get_voleq(site))
```

## Public API

| Symbol | Description |
|--------|-------------|
| `NVEL` | Loads `libnvel.so` and exposes volume routines |
| `Site` | `region`, `forest`, `district`, `species` |
| `VolumeResult` | `volume_equation`, `total_cuft`, `gross_scribner`, `net_scribner`, `vol` |
| `VolIndex` | Named indices into `VolumeResult.vol` |
| `NvelError` | Raised when Fortran `errflag != 0` |

### `NVEL.calc_volume`

Primary entry point. Accepts a `Site` or keyword location keys (`region`, `forest`, `district`, `species`) plus tree dimensions such as `dbh`, `total_height`, and `height_to_second_product`. Resolves `volume_equation` via `getvoleq_r` unless overridden.

### `NVEL.get_voleq`

Secondary helper to inspect the default equation id for a site without computing volume.

## Fortran entry points

| Python | Fortran |
|--------|---------|
| `version()` | `vernum_r` |
| `get_voleq()` | `getvoleq_r` |
| `calc_volume()` | `getvoleq_r` (unless overridden) + `vollib_r` |

## VOL array indices

See `VolIndex` for named 0-based indices into `VolumeResult.vol` (Fortran `VOL(n)` → `VolIndex(n - 1)`). Common entries: `TOTAL_CUFT`, `GROSS_SCRIBNER`, `NET_SCRIBNER`.

Integration tests live in [`tests/`](../tests/README.md).
