"""NVEL shared constants."""

from __future__ import annotations

NEEDED_ROUTINES: tuple[str, ...] = ("vernum_r", "getvoleq_r", "vollib_r")

DEFAULT_LIB_RELATIVE = ("build", "libnvel.so")

VOL_LEN = 15
VOLEQ_LEN = 10
FORST_LEN = 2
DIST_LEN = 2

ERRFLAG_MESSAGES: dict[int, str] = {
    0: "ok",
}
