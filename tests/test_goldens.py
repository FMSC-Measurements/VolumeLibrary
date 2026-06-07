import json
from pathlib import Path

import pytest

CASES_PATH = Path(__file__).resolve().parent / "goldens" / "cases.json"


def load_cases():
    data = json.loads(CASES_PATH.read_text(encoding="utf-8"))
    return [c for c in data["cases"] if c.get("api") == "vollib_r"]


@pytest.mark.parametrize("case", load_cases(), ids=lambda c: c["name"])
def test_volume_golden(nvel, case):
    inputs = case["inputs"]
    expected = case["expected"]
    tol = case.get("tolerance", 1e-3)

    result = nvel.calc_volume(**inputs)
    if "volume_equation" in inputs:
        assert result.volume_equation == inputs["volume_equation"]
    for i, exp in enumerate(expected["vol"]):
        if exp == 0.0 and result.vol[i] == 0.0:
            continue
        assert abs(result.vol[i] - exp) <= tol, (
            f"vol[{i}] {result.vol[i]} != {exp} (tol {tol})"
        )
