"""Unit tests for the declarative Routine invoker."""

from __future__ import annotations

from ctypes import POINTER, c_int, cast

import pytest

from nvel._routines import NVEL_ROUTINES, Intent, Param, Routine, _fortran_str
from nvel.constants import VOLEQ_LEN
from nvel.exceptions import NvelError


def test_fortran_str_padding() -> None:
    assert _fortran_str("1", 2) == b"1 "


def test_vernum_routine_stub() -> None:
    routine = NVEL_ROUTINES["vernum_r"]

    def stub(version: object) -> None:
        cast(version, POINTER(c_int)).contents.value = 20260415

    result = routine.call(stub, raise_on_error=False)
    assert result == 20260415


def test_getvoleq_routine_stub() -> None:
    routine = NVEL_ROUTINES["getvoleq_r"]

    def stub(
        regn: object,
        _forst: object,
        _dist: object,
        spec: object,
        voleq: object,
        errflag: object,
    ) -> None:
        assert cast(regn, POINTER(c_int)).contents.value == 9
        assert cast(spec, POINTER(c_int)).contents.value == 837
        voleq.value = _fortran_str("900CLKE837", VOLEQ_LEN)  # type: ignore[attr-defined]
        cast(errflag, POINTER(c_int)).contents.value = 0

    out = routine.call(
        stub,
        raise_on_error=False,
        regn=9,
        forst="01",
        dist="1",
        spec=837,
    )
    assert out == {"voleq": "900CLKE837", "errflag": 0}


def test_vollib_routine_raises_on_errflag() -> None:
    routine = NVEL_ROUTINES["vollib_r"]

    def stub(*args: object) -> None:
        cast(args[-1], POINTER(c_int)).contents.value = 1

    with pytest.raises(NvelError, match="errflag 1"):
        routine.call(
            stub,
            voleq="616BEHW000",
            regn=9,
            forst="01",
            dist="1",
            spec=837,
            dbh=5.1,
            ht=25.4,
            mtopp=0.0,
            mtops=0.0,
            ht1prd=0.0,
            ht2prd=9.0,
            upsht1=0.0,
            upsd1=0.0,
            stump=0.0,
            fclass=0,
            dbtbh=0.0,
            btr=0.0,
        )


def test_routine_rejects_unknown_kwargs() -> None:
    routine = Routine(
        params=(Param("version", POINTER(c_int), Intent.OUT),),
        rc_param=None,
        rc_policy=None,
    )

    def stub(_version: object) -> None:
        pass

    with pytest.raises(TypeError, match="unknown kwargs"):
        routine.call(stub, extra=1)
