from nvel import Site


def test_calc_volume_resolves_voleq(nvel):
    site = Site(region=9, forest="01", district="1", species=837)
    result = nvel.calc_volume(
        site, dbh=5.1, total_height=25.4, height_to_second_product=9.0
    )
    assert result.volume_equation == nvel.get_voleq(site)
    assert result.total_cuft > 0
