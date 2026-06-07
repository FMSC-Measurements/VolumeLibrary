"""Tests for symbol resolution without a real shared library."""

from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest

from nvel._core import NvelCore


def _skip_init(_self: object, _lib_path: object = None) -> None:
    return None


def test_resolve_routines_missing_symbol() -> None:
    lib = MagicMock()
    lib.vernum_r_ = MagicMock()
    lib.getvoleq_r_ = None
    lib.getvoleq_r = None
    lib.vollib_r_ = MagicMock()

    with patch.object(NvelCore, "__init__", _skip_init):
        core = NvelCore.__new__(NvelCore)
        core._lib = lib
        with pytest.raises(ImportError, match="getvoleq_r"):
            core._resolve_routines()


def test_resolve_routines_unmangled_name() -> None:
    lib = MagicMock()
    lib.vernum_r_ = None
    lib.vernum_r = MagicMock()
    lib.getvoleq_r_ = MagicMock()
    lib.vollib_r_ = MagicMock()

    with patch.object(NvelCore, "__init__", _skip_init):
        core = NvelCore.__new__(NvelCore)
        core._lib = lib
        core._resolve_routines()
        assert core._vernum_r is lib.vernum_r
