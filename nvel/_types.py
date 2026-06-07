"""Public result types for NVEL."""

from __future__ import annotations

from dataclasses import dataclass
from enum import IntEnum


class VolIndex(IntEnum):
    """0-based indices into the 15-element VOL array from ``vollib_r``.

    Fortran ``VOL(n)`` corresponds to ``VolIndex(n - 1)``.
    """

    TOTAL_CUFT = 0
    GROSS_SCRIBNER = 1
    NET_SCRIBNER = 2
    GROSS_CUFT_PRIMARY = 3
    NET_CUFT_PRIMARY = 4
    CORD_PRIMARY = 5
    GROSS_CUFT_SECONDARY = 6
    NET_CUFT_SECONDARY = 7
    CORD_SECONDARY = 8
    GROSS_INTL_QUARTER = 9
    NET_INTL_QUARTER = 10
    GROSS_BDFT_TOPWOOD = 11
    NET_BDFT_TOPWOOD = 12
    STUMP = 13
    TIP = 14


@dataclass(frozen=True)
class Site:
    """Regional location keys for a stand/species context.

    Attributes:
        region: FVS region code.
        forest: Two-character forest code.
        district: Two-character district code.
        species: FIA species code.
    """

    region: int
    forest: str
    district: str
    species: int


@dataclass(frozen=True)
class VolumeResult:
    """Volume equation output from :meth:`NVEL.calc_volume`.

    Attributes:
        volume_equation: Equation id used for the calculation.
        vol: Full 15-element VOL array from ``vollib_r``.

    Properties:
        total_cuft: Total cubic volume, ground to tip
            (:attr:`VolIndex.TOTAL_CUFT`).
        gross_scribner: Gross Scribner board feet as primary product
            (:attr:`VolIndex.GROSS_SCRIBNER`).
        net_scribner: Net Scribner board feet as primary product
            (:attr:`VolIndex.NET_SCRIBNER`).
    """

    volume_equation: str
    vol: tuple[float, ...]

    @property
    def total_cuft(self) -> float:
        return self.vol[VolIndex.TOTAL_CUFT]

    @property
    def gross_scribner(self) -> float:
        return self.vol[VolIndex.GROSS_SCRIBNER]

    @property
    def net_scribner(self) -> float:
        return self.vol[VolIndex.NET_SCRIBNER]

    @classmethod
    def from_vol_array(
        cls,
        volume_equation: str,
        vol: list[float] | tuple[float, ...],
    ) -> VolumeResult:
        """Build a result from a raw Fortran VOL array.

        Args:
            volume_equation: Equation id used for the call.
            vol: 15-element volume array from ``vollib_r``.

        Returns:
            Parsed :class:`VolumeResult`.
        """
        return cls(
            volume_equation=volume_equation,
            vol=tuple(float(v) for v in vol),
        )
