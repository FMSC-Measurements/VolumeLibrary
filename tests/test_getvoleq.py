import json
from pathlib import Path

import pytest

CASES = Path(__file__).resolve().parent / "goldens" / "cases.json"


@pytest.fixture(scope="session")
def golden_cases():
    return json.loads(CASES.read_text(encoding="utf-8"))["cases"]


def test_getvoleq_r9_837(nvel, golden_cases):
    case = next(c for c in golden_cases if c["name"] == "getvoleq_r9_837")
    inp = case["inputs"]
    exp = case["expected"]
    volume_equation = nvel.get_voleq(
        region=inp["region"],
        forest=inp["forest"],
        district=inp["district"],
        species=inp["species"],
    )
    assert volume_equation == exp["voleq"]
