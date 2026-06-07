"""Declarative registry of NVEL *_r routine signatures."""

from __future__ import annotations

from collections.abc import Callable, Mapping
from ctypes import (
    POINTER,
    byref,
    c_char_p,
    c_double,
    c_int,
    create_string_buffer,
)
from dataclasses import dataclass
from enum import Flag, auto
from typing import Any

from nvel.constants import DIST_LEN, FORST_LEN, VOL_LEN, VOLEQ_LEN
from nvel.exceptions import check_errflag


class Intent(Flag):
    """Fortran argument intent for :class:`Param` declarations."""

    IN = auto()
    OUT = auto()
    INOUT = IN | OUT


def _fortran_str(value: str, length: int) -> bytes:
    return value.encode("ascii", errors="replace").ljust(length)[:length]


@dataclass(frozen=True, slots=True)
class Param:
    """One Fortran formal argument in a :class:`Routine` signature."""

    name: str
    ctype: type
    intent: Intent = Intent.IN
    str_len: int | None = None


@dataclass(frozen=True, slots=True)
class Routine:
    """ctypes call wrapper for one NVEL ``*_r`` entry point."""

    params: tuple[Param, ...]
    restype: type | None = None
    rc_param: str | None = "errflag"
    rc_policy: Callable[[int], None] | None = check_errflag
    name: str = ""

    def call(
        self,
        func: Any,
        /,
        *,
        raise_on_error: bool = True,
        **kwargs: Any,
    ) -> Any:
        """Invoke the bound Fortran function with validated kwargs.

        Args:
            func: ctypes function object from the loaded shared library.
            raise_on_error: When ``True``, call :func:`check_errflag` on
                the routine's return-code parameter.
            **kwargs: Input values keyed by Fortran argument names.

        Returns:
            Unwrapped OUT parameter value(s), or a dict when multiple
            OUT params remain and ``raise_on_error`` is ``False``.

        Raises:
            TypeError: If required kwargs are missing or unknown kwargs
                are given.
            NvelError: If ``errflag`` is non-zero and
                ``raise_on_error`` is ``True``.
        """
        param_names = {p.name for p in self.params}
        expected_inputs = {
            p.name for p in self.params if p.intent != Intent.OUT
        }
        provided = set(kwargs)

        missing = expected_inputs - provided
        if missing:
            msg = f"{self._label()}: missing required kwargs {sorted(missing)}"
            raise TypeError(msg)

        unknown = provided - param_names
        if unknown:
            msg = f"{self._label()}: unknown kwargs {sorted(unknown)}"
            raise TypeError(msg)

        out_in_kwargs = provided & {
            p.name for p in self.params if p.intent == Intent.OUT
        }
        if out_in_kwargs:
            msg = (
                f"{self._label()}: OUT params must not be provided by the "
                f"caller: {sorted(out_in_kwargs)}"
            )
            raise TypeError(msg)

        argtypes: list[type] = [p.ctype for p in self.params]
        out_bufs: dict[str, Any] = {}
        call_args: list[Any] = []

        for p in self.params:
            if p.intent == Intent.OUT:
                storage, call_arg = self._alloc_out(p)
                out_bufs[p.name] = storage
                call_args.append(call_arg)
                continue
            call_args.append(self._prepare_in(p, kwargs[p.name]))

        func.argtypes = argtypes
        func.restype = self.restype
        func(*call_args)

        rc = out_bufs[self.rc_param].value if self.rc_param else 0
        if (
            raise_on_error
            and self.rc_policy is not None
            and self.rc_param is not None
        ):
            self.rc_policy(rc)

        return self._unwrap_out(out_bufs, rc=rc, include_rc=not raise_on_error)

    def _alloc_out(self, param: Param) -> tuple[Any, Any]:
        if param.str_len is not None:
            buf = create_string_buffer(
                _fortran_str("", param.str_len), param.str_len
            )
            return buf, buf
        if param.name == "vol":
            vol = (c_double * VOL_LEN)()
            return vol, vol
        inner = getattr(param.ctype, "_type_", None)
        if inner is c_int:
            val = c_int(0)
            return val, byref(val)
        msg = f"{self._label()}: cannot auto-allocate OUT param {param.name!r}"
        raise TypeError(msg)

    def _prepare_in(self, param: Param, value: Any) -> Any:
        if param.str_len is not None:
            if param.intent == Intent.IN:
                return c_char_p(_fortran_str(str(value), param.str_len))
            return create_string_buffer(
                _fortran_str(str(value), param.str_len), param.str_len
            )
        inner = getattr(param.ctype, "_type_", None)
        if inner is c_int:
            return byref(c_int(int(value)))
        if inner is c_double:
            return byref(c_double(float(value)))
        msg = f"{self._label()}: unsupported IN param {param.name!r} ctype {param.ctype!r}"
        raise TypeError(msg)

    def _unwrap_out(
        self,
        out_bufs: Mapping[str, Any],
        *,
        rc: int = 0,
        include_rc: bool = False,
    ) -> Any:
        non_rc = {k: v for k, v in out_bufs.items() if k != self.rc_param}
        if include_rc and self.rc_param is not None:
            if len(non_rc) == 1:
                name, buf = next(iter(non_rc.items()))
                value = self._read_out(name, buf)
                return {name: value, self.rc_param: rc}
            result: dict[str, Any] = {self.rc_param: rc}
            for name, buf in non_rc.items():
                result[name] = self._read_out(name, buf)
            return result

        if not non_rc:
            return None
        if len(non_rc) == 1:
            name, buf = next(iter(non_rc.items()))
            return self._read_out(name, buf)
        return {name: self._read_out(name, buf) for name, buf in non_rc.items()}

    def _read_out(self, name: str, buf: Any) -> Any:
        if name == "vol":
            return [buf[i] for i in range(VOL_LEN)]
        if name == "voleq":
            return buf.value.decode("ascii").strip()
        if isinstance(buf, c_int):
            return buf.value
        if hasattr(buf, "value") and not isinstance(buf.value, bytes):
            return buf.value
        return buf.value.decode("ascii").strip()

    def _label(self) -> str:
        return self.name or "Routine"


NVEL_ROUTINES: dict[str, Routine] = {
    "vernum_r": Routine(
        name="vernum_r",
        params=(Param("version", POINTER(c_int), Intent.OUT),),
        rc_param=None,
        rc_policy=None,
    ),
    "getvoleq_r": Routine(
        name="getvoleq_r",
        params=(
            Param("regn", POINTER(c_int), Intent.IN),
            Param("forst", c_char_p, Intent.IN, str_len=FORST_LEN),
            Param("dist", c_char_p, Intent.IN, str_len=DIST_LEN),
            Param("spec", POINTER(c_int), Intent.IN),
            Param("voleq", c_char_p, Intent.OUT, str_len=VOLEQ_LEN),
            Param("errflag", POINTER(c_int), Intent.OUT),
        ),
    ),
    "vollib_r": Routine(
        name="vollib_r",
        params=(
            Param("voleq", c_char_p, Intent.IN, str_len=VOLEQ_LEN),
            Param("regn", POINTER(c_int), Intent.IN),
            Param("forst", c_char_p, Intent.IN, str_len=FORST_LEN),
            Param("dist", c_char_p, Intent.IN, str_len=DIST_LEN),
            Param("spec", POINTER(c_int), Intent.IN),
            Param("dbh", POINTER(c_double), Intent.IN),
            Param("ht", POINTER(c_double), Intent.IN),
            Param("mtopp", POINTER(c_double), Intent.IN),
            Param("mtops", POINTER(c_double), Intent.IN),
            Param("ht1prd", POINTER(c_double), Intent.IN),
            Param("ht2prd", POINTER(c_double), Intent.IN),
            Param("upsht1", POINTER(c_double), Intent.IN),
            Param("upsd1", POINTER(c_double), Intent.IN),
            Param("stump", POINTER(c_double), Intent.IN),
            Param("fclass", POINTER(c_int), Intent.IN),
            Param("dbtbh", POINTER(c_double), Intent.IN),
            Param("btr", POINTER(c_double), Intent.IN),
            Param("vol", POINTER(c_double * VOL_LEN), Intent.OUT),
            Param("errflag", POINTER(c_int), Intent.OUT),
        ),
    ),
}
