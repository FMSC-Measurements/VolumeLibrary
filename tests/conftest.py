import pytest

from nvel import NVEL


@pytest.fixture(scope="session")
def nvel() -> NVEL:
    return NVEL()
