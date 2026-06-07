"""Map public Python parameter names to Fortran registry kwargs."""

from __future__ import annotations

from typing import Any

from nvel._types import Site

SITE_PUBLIC_TO_FORTRAN: dict[str, str] = {
    "region": "regn",
    "forest": "forst",
    "district": "dist",
    "species": "spec",
}

VOLLIB_PUBLIC_TO_FORTRAN: dict[str, str] = {
    **SITE_PUBLIC_TO_FORTRAN,
    "volume_equation": "voleq",
    "dbh": "dbh",
    "total_height": "ht",
    "primary_merchantable_top": "mtopp",
    "secondary_merchantable_top": "mtops",
    "height_to_first_product": "ht1prd",
    "height_to_second_product": "ht2prd",
    "upper_stem_height": "upsht1",
    "upper_stem_diameter": "upsd1",
    "stump_height": "stump",
    "form_class": "fclass",
    "diameter_outside_bark": "dbtbh",
    "butt_swelling_ratio": "btr",
}


def site_to_fortran_names(site: Site) -> dict[str, Any]:
    """Map a :class:`~nvel._types.Site` to Fortran routine kwargs.

    Args:
        site: Regional location keys.

    Returns:
        Dict with Fortran names ``regn``, ``forst``, ``dist``, and
        ``spec``.
    """
    return {
        SITE_PUBLIC_TO_FORTRAN[name]: getattr(site, name)
        for name in SITE_PUBLIC_TO_FORTRAN
    }


def vollib_to_fortran(**kwargs: Any) -> dict[str, Any]:
    """Map public ``vollib_r`` kwargs to Fortran registry names.

    Args:
        **kwargs: Public parameter names (e.g. ``region``,
            ``total_height``).

    Returns:
        Dict keyed by Fortran ``vollib_r`` argument names.

    Raises:
        TypeError: If a keyword is not in
            :data:`VOLLIB_PUBLIC_TO_FORTRAN`.
    """
    translated: dict[str, Any] = {}
    for key, value in kwargs.items():
        if key not in VOLLIB_PUBLIC_TO_FORTRAN:
            msg = f"Unknown vollib parameter: {key!r}"
            raise TypeError(msg)
        translated[VOLLIB_PUBLIC_TO_FORTRAN[key]] = value
    return translated
