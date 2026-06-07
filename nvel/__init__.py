"""Friendly Python interface to NVEL volume equations."""

from __future__ import annotations

from typing import Any, cast

from nvel._core import NvelCore
from nvel._params import site_to_fortran_names, vollib_to_fortran
from nvel._types import Site, VolIndex, VolumeResult
from nvel.exceptions import NvelError

__all__ = ["NVEL", "NvelError", "Site", "VolIndex", "VolumeResult"]


def _resolve_site(
    site: Site | None,
    region: int | None,
    forest: str | None,
    district: str | None,
    species: int | None,
) -> Site:
    if site is not None:
        if any(v is not None for v in (region, forest, district, species)):
            msg = "Pass either site or region/forest/district/species, not both"
            raise TypeError(msg)
        return site
    if region is None or forest is None or district is None or species is None:
        missing = [
            name
            for name, value in (
                ("region", region),
                ("forest", forest),
                ("district", district),
                ("species", species),
            )
            if value is None
        ]
        msg = f"Missing required location keys: {', '.join(missing)}"
        raise TypeError(msg)
    return Site(
        region=region,
        forest=forest,
        district=district,
        species=species,
    )


class NVEL(NvelCore):
    """High-level wrapper for NVEL volume library routines.

    Loads ``libnvel.so`` and exposes tree volume calculations through a
    Python-friendly API. Use :meth:`calc_volume` as the primary entry
    point.
    """

    def version(self) -> int:
        """Return the library version integer from ``vernum_r``.

        Returns:
            Version encoded as ``YYYYMMDD`` (e.g. ``20260415``).
        """
        return int(self._invoke("vernum_r", raise_on_error=False))

    def get_voleq(
        self,
        site: Site | None = None,
        *,
        region: int | None = None,
        forest: str | None = None,
        district: str | None = None,
        species: int | None = None,
        raise_on_error: bool = True,
    ) -> str:
        """Return the default volume equation id for a site.

        Calls ``getvoleq_r`` with regional location keys.

        Args:
            site: Stand context. If given, ``region``/``forest``/
                ``district``/``species`` must not also be passed.
            region: FVS region code.
            forest: Two-character forest code.
            district: Two-character district code.
            species: FIA species code.
            raise_on_error: When ``True``, raise :class:`NvelError` when
                ``errflag`` is non-zero.

        Returns:
            Default volume equation id (10-character string).

        Raises:
            TypeError: For invalid ``site``/location keyword use, or
                missing keys.
            NvelError: On lookup failure when ``raise_on_error`` is
                ``True``.
        """
        loc = _resolve_site(site, region, forest, district, species)
        return str(
            self._invoke(
                "getvoleq_r",
                **site_to_fortran_names(loc),
                raise_on_error=raise_on_error,
            )
        )

    def calc_volume(
        self,
        site: Site | None = None,
        *,
        region: int | None = None,
        forest: str | None = None,
        district: str | None = None,
        species: int | None = None,
        dbh: float,
        total_height: float,
        volume_equation: str | None = None,
        primary_merchantable_top: float = 0.0,
        secondary_merchantable_top: float = 0.0,
        height_to_first_product: float = 0.0,
        height_to_second_product: float = 0.0,
        upper_stem_height: float = 0.0,
        upper_stem_diameter: float = 0.0,
        stump_height: float = 0.0,
        form_class: int = 0,
        diameter_outside_bark: float = 0.0,
        butt_swelling_ratio: float = 0.0,
        raise_on_error: bool = True,
    ) -> VolumeResult:
        """Calculate tree volume for one tree.

        Resolves ``volume_equation`` via :meth:`get_voleq` unless
        overridden, then calls ``vollib_r``.

        Args:
            site: Stand context. Do not pass location keywords when set.
            region: FVS region code.
            forest: Two-character forest code.
            district: Two-character district code.
            species: FIA species code.
            dbh: Diameter at breast height (inches).
            total_height: Total tree height (feet).
            volume_equation: Optional equation override; skips default
                lookup when set.
            primary_merchantable_top: Primary merchantable top (feet).
            secondary_merchantable_top: Secondary merchantable top
                (feet).
            height_to_first_product: Height to first product (feet).
            height_to_second_product: Height to second product (feet).
            upper_stem_height: Upper stem height for 3rd-point entry
                (feet).
            upper_stem_diameter: Upper stem diameter for 3rd-point entry
                (inches).
            stump_height: Stump height (feet).
            form_class: Form class code.
            diameter_outside_bark: Diameter outside bark at breast
                height (inches).
            butt_swelling_ratio: Butt swelling ratio.
            raise_on_error: When ``True``, raise :class:`NvelError` when
                ``errflag`` is non-zero.

        Returns:
            Volume components and the equation id used.

        Raises:
            TypeError: For invalid ``site``/location keyword use, or
                missing keys.
            NvelError: On calculation failure when ``raise_on_error`` is
                ``True``.
        """
        loc = _resolve_site(site, region, forest, district, species)
        eq = volume_equation or self.get_voleq(
            loc, raise_on_error=raise_on_error
        )
        vol = self._invoke(
            "vollib_r",
            **vollib_to_fortran(
                volume_equation=eq,
                region=loc.region,
                forest=loc.forest,
                district=loc.district,
                species=loc.species,
                dbh=dbh,
                total_height=total_height,
                primary_merchantable_top=primary_merchantable_top,
                secondary_merchantable_top=secondary_merchantable_top,
                height_to_first_product=height_to_first_product,
                height_to_second_product=height_to_second_product,
                upper_stem_height=upper_stem_height,
                upper_stem_diameter=upper_stem_diameter,
                stump_height=stump_height,
                form_class=form_class,
                diameter_outside_bark=diameter_outside_bark,
                butt_swelling_ratio=butt_swelling_ratio,
            ),
            raise_on_error=raise_on_error,
        )
        return VolumeResult.from_vol_array(eq, vol)

    def invoke_vollib(
        self,
        *,
        raise_on_error: bool = False,
        **inputs: Any,
    ) -> tuple[list[float], int]:
        """Call ``vollib_r`` directly with public parameter names.

        Intended for golden recording and low-level regression tests.

        Args:
            raise_on_error: When ``True``, raise :class:`NvelError` when
                ``errflag`` is non-zero.
            **inputs: Tree and location kwargs using public names (see
                :func:`nvel._params.vollib_to_fortran`).

        Returns:
            Tuple of ``(vol, errflag)`` where ``vol`` is the 15-element
            VOL array from Fortran.

        Raises:
            TypeError: If an unknown keyword is passed.
            NvelError: If ``errflag`` is non-zero and
                ``raise_on_error`` is ``True``.
        """
        out = self._invoke(
            "vollib_r",
            raise_on_error=raise_on_error,
            **vollib_to_fortran(**inputs),
        )
        if isinstance(out, dict):
            vol = cast(list[float], out["vol"])
            return vol, int(out["errflag"])
        return cast(list[float], out), 0
