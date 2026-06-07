"""NVEL error types."""

from __future__ import annotations

from nvel.constants import ERRFLAG_MESSAGES


class NvelError(Exception):
    """Raised when an NVEL routine returns a non-zero errflag."""

    def __init__(self, code: int, message: str | None = None) -> None:
        """Initialize from a Fortran ``errflag`` code.

        Args:
            code: Non-zero error code from an ``*_r`` routine.
            message: Optional override; defaults to
                :data:`ERRFLAG_MESSAGES`.
        """
        self.code = code
        detail = message or ERRFLAG_MESSAGES.get(code, "unknown error")
        super().__init__(f"NVEL errflag {code}: {detail}")
        self.message = detail


def check_errflag(code: int) -> None:
    """Raise :class:`NvelError` when ``code`` is non-zero.

    Args:
        code: ``errflag`` value returned by an ``*_r`` routine.

    Raises:
        NvelError: When ``code`` is not zero.
    """
    if code != 0:
        raise NvelError(code)
