"""Core ctypes loader and routine dispatch for NVEL."""

from __future__ import annotations

import os
from ctypes import CDLL
from pathlib import Path
from typing import Any

from typing_extensions import Self

from nvel._routines import NVEL_ROUTINES
from nvel.constants import DEFAULT_LIB_RELATIVE, NEEDED_ROUTINES

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_LIB = ROOT.joinpath(*DEFAULT_LIB_RELATIVE)


class NvelCore:
    """Load libnvel.so and invoke registered *_r routines."""

    def __init__(
        self, lib_path: Path | str | os.PathLike[str] | None = None
    ) -> None:
        """Load the NVEL shared library and bind ``*_r`` entry points.

        Args:
            lib_path: Path to ``libnvel.so``. Defaults to
                ``build/libnvel.so`` under the repo root, or
                ``NVEL_LIB_PATH`` when set.

        Raises:
            FileNotFoundError: If the shared library file is missing.
            ImportError: If required symbols are missing from the loaded
                library.
        """
        if lib_path is not None:
            path = Path(lib_path)
        else:
            env_path = os.environ.get("NVEL_LIB_PATH")
            path = Path(env_path) if env_path else DEFAULT_LIB
        if not path.is_file():
            msg = f"NVEL shared library not found: {path}"
            raise FileNotFoundError(msg)
        self.lib_path = path
        self._lib: CDLL | None = CDLL(str(path))
        self._resolve_routines()

    def _resolve_routines(self) -> None:
        lib = self._lib
        if lib is None:
            msg = "NVEL library is not loaded"
            raise RuntimeError(msg)
        missing: list[str] = []
        for name in NEEDED_ROUTINES:
            func = None
            for candidate in (f"{name}_", name):
                attr = getattr(lib, candidate, None)
                if attr is not None and callable(attr):
                    func = attr
                    break
            if func is None:
                missing.append(name)
                continue
            setattr(self, f"_{name}", func)

        if missing:
            msg = (
                f"{', '.join(missing)} are needed routines that are not available "
                "in the loaded library (maybe they weren't exported when built)"
            )
            raise ImportError(msg)

    def _invoke(
        self,
        name: str,
        /,
        *,
        raise_on_error: bool = True,
        **kwargs: Any,
    ) -> Any:
        routine = NVEL_ROUTINES[name]
        func: Any = getattr(self, f"_{name}")
        return routine.call(func, raise_on_error=raise_on_error, **kwargs)

    def close(self) -> None:
        """Release the loaded library handle."""
        self._lib = None

    def __enter__(self) -> Self:
        """Enter a context manager that closes the library on exit."""
        return self

    def __exit__(self, *args: object) -> None:
        """Close the library handle."""
        self.close()
