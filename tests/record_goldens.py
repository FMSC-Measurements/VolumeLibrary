"""Record expected outputs in tests/goldens/cases.json from the current libnvel.so."""

from __future__ import annotations

import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from nvel import NVEL, Site
from nvel._params import site_to_fortran_names

CASES_PATH = Path(__file__).resolve().parent / "goldens" / "cases.json"


def main() -> int:
    """Refresh golden expected values from the current shared library."""
    nvel = NVEL()
    data = json.loads(CASES_PATH.read_text(encoding="utf-8"))

    for case in data["cases"]:
        api = case.get("api")
        if api == "vernum":
            case["expected"] = {"version": nvel.version()}
            continue
        if api == "getvoleq_r":
            inp = case["inputs"]
            out = nvel._invoke(
                "getvoleq_r",
                raise_on_error=False,
                **site_to_fortran_names(
                    Site(
                        region=inp["region"],
                        forest=inp["forest"],
                        district=inp["district"],
                        species=inp["species"],
                    )
                ),
            )
            case["expected"] = {
                "voleq": out["voleq"],
                "errflag": out["errflag"],
            }
            continue
        if api == "vollib_r":
            vol, err = nvel.invoke_vollib(
                raise_on_error=False, **case["inputs"]
            )
            case["expected"] = {"errflag": err, "vol": vol}
            continue
        print(f"Skip unknown api: {api}", file=sys.stderr)

    CASES_PATH.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")
    print(f"Updated {CASES_PATH}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
