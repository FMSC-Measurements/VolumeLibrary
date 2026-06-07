# Upstream PR template

Copy into the upstream PR body (`gh pr create --body-file fortran_build/upstream_pr_template.md` after filling in placeholders).

## Summary

- Files changed and why.
- Intended behavioral impact (e.g. “no numerical change; warning cleanup only”).

## Build & test setup

- gfortran on Ubuntu: `sudo apt-get install gfortran`
- Build: `fortran_build/build_gfortran_shared.sh` → `build/libnvel.so`
- Tests: `pip install -e ".[dev]" && pytest -v`
- Warnings: `fortran_build/check_warnings.sh`
- Optional: `.devcontainer/` for a preconfigured environment

## Numerical regression

| Check | Result |
|-------|--------|
| pytest goldens | e.g. 12/12 passed |
| Fork CI run | link to Actions run |
| Cases covering changed code paths | list case names from `cases.json` |
| Tolerance | e.g. `1e-3` on `vollib_r` outputs |

## Warning regression

| Metric | Before | After |
|--------|--------|-------|
| Total warnings (repo manifest) | … | … |
| Warnings in touched file(s) | … | … |
| New Tier A in touched files | — | none |

## Cross-links

- Fork development PR: `d-diaz/VolumeLibrary#N`
- Note that full baselines and tooling live on the fork; this PR is source-only.

## Optional FVS smoke

State whether it was run, with commit hash and outcome, or explicitly note that pytest covers the affected regions.
