# Legacy archive (fork-only)

Pre-existing VolumeLibrary content moved here to keep the repo root
focused on the gfortran/Python development workflow (`fortran_build/`,
`nvel/`, `tests/`).

## Contents

| Path | What it was |
|------|-------------|
| `volCStest/`, `volcstest/` | C# WinForms GUI for manual `vollib.dll` testing |
| `volMT/`, `volmt/` | C++ OpenMP batch demo loading the DLL |
| `vollib/` | Intel Fortran VS project (builds `vollib.dll` from root sources) |
| `deployment/` | Visual Studio Installer project |
| `files_not_in_vollib/` | Sources intentionally excluded from the release DLL |
| `vs/` | Visual Studio IDE cache (was `.vs/`) |
| `root-snapshots/` | Dated backup files and debug log |
| `volumelibrary.sln`, `comp*.bat`, manifests, `readme.txt` | Windows/Intel build and distribution tooling |

## Not used by

- `fortran_build/build_gfortran_shared.sh` → `build/libnvel.so`
- `nvel` Python package and `pytest` regression tests
- GitHub Actions CI on this fork

## Upstream

Do **not** submit changes under `_legacy/` as PRs to
[FMSC-Measurements/VolumeLibrary](https://github.com/FMSC-Measurements/VolumeLibrary).
Upstream keeps these paths at the repo root for Windows DLL releases.

## Restore

To put something back at its original location:

```bash
git mv _legacy/<path> .
```

Or restore the entire pre-archive tree from git history:

```bash
git checkout origin/main -- <path>
```
