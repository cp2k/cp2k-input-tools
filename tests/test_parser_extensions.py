import pytest

from . import TEST_DIR


def test_coords(parser):
    with (TEST_DIR / "inputs" / "NaCl.inp").open("r") as fhandle:
        parser.parse(fhandle)

    coords = list(parser.coords())
    assert len(coords) == 8
    assert coords[0] == ("Na", (0.0, 0.0, 0.0), None)
    assert coords[4] == ("Cl", (0.5, 0.5, 0.5), None)
    assert coords[7] == ("Cl", (0.0, 0.0, 0.5), None)

    # get coords from inexistant second force_eval
    with pytest.raises(StopIteration):
        next(parser.coords(1))
