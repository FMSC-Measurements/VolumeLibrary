"""Regression test for the sf_zero.f `integer_division` warning fix.

`sf_zero.f`'s internal `AMACH` subroutine computes the single/double
precision machine constants returned by `R1MACH`/`D1MACH` (indices 1-5, see
the comment block above line 920 in `sf_zero.f` for the full definitions):

    1 = smallest positive (underflow) magnitude
    2 = largest (overflow) magnitude
    3 = smallest relative spacing (machine epsilon / 2)
    4 = largest relative spacing (machine epsilon)
    5 = log10(base)

Indices 1 (`RM1`/`DM1`) were flagged by gfortran's `-Winteger-division`: the
exponent for the underflow limit is computed by deliberately truncating an
integer division in half (the source comment says "DON'T SIMPLIFY THEM" —
it's a documented overflow-avoidance trick, not a bug), so the warning was
resolved by rewriting the division as `REAL`/`DBLE` math with an explicit
`INT()`/`IDINT()` truncation rather than by changing the computed value.

`R1MACH(1)`/`D1MACH(1)` are not called from anywhere reachable through the
wrapped NVEL entry points (`getvoleq_r`, `vollib_r`, ...), so no existing
golden test in `tests/goldens/cases.json` would notice if this rewrite had
silently changed the constant. This test calls `R1MACH`/`D1MACH` directly
and asserts the exact bit pattern of the result is unchanged, for all 5
indices as a broader check that the surrounding `PARAMETER` block wasn't
otherwise disturbed.

Values below were captured from the pre-fix build and are asserted bit-exact
(via `ctypes.c_uint32`/`c_uint64` reinterpretation) rather than compared with
a numeric tolerance, since these are compile-time constants that should not
change at all.
"""

import ctypes

import pytest

# R1MACH(index) -> expected IEEE-754 single-precision bit pattern
EXPECTED_R1MACH = {
    1: 0x00800000,  # smallest positive magnitude (underflow limit)
    2: 0x7F7FFFFF,  # largest magnitude (overflow limit)
    3: 0x33800000,  # smallest relative spacing
    4: 0x34000000,  # largest relative spacing (machine epsilon)
    5: 0x3E9A209B,  # log10(2)
}

# D1MACH(index) -> expected IEEE-754 double-precision bit pattern
EXPECTED_D1MACH = {
    1: 0x0010000000000000,  # smallest positive magnitude (underflow limit)
    2: 0x7FEFFFFFFFFFFFFF,  # largest magnitude (overflow limit)
    3: 0x3CA0000000000000,  # smallest relative spacing
    4: 0x3CB0000000000000,  # largest relative spacing (machine epsilon)
    5: 0x3FD34413509F79FF,  # log10(2)
}


@pytest.fixture(scope="module")
def machine_constants(nvel):
    lib = nvel._lib
    lib.r1mach_.restype = ctypes.c_float
    lib.r1mach_.argtypes = [ctypes.POINTER(ctypes.c_int)]
    lib.d1mach_.restype = ctypes.c_double
    lib.d1mach_.argtypes = [ctypes.POINTER(ctypes.c_int)]
    return lib


def _r1mach_hex(lib, index: int) -> int:
    value = lib.r1mach_(ctypes.byref(ctypes.c_int(index)))
    return ctypes.c_uint32.from_buffer(ctypes.c_float(value)).value


def _d1mach_hex(lib, index: int) -> int:
    value = lib.d1mach_(ctypes.byref(ctypes.c_int(index)))
    return ctypes.c_uint64.from_buffer(ctypes.c_double(value)).value


@pytest.mark.parametrize("index", sorted(EXPECTED_R1MACH))
def test_r1mach_bit_exact(machine_constants, index):
    assert _r1mach_hex(machine_constants, index) == EXPECTED_R1MACH[index]


@pytest.mark.parametrize("index", sorted(EXPECTED_D1MACH))
def test_d1mach_bit_exact(machine_constants, index):
    assert _d1mach_hex(machine_constants, index) == EXPECTED_D1MACH[index]
