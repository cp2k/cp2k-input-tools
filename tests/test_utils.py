from decimal import Decimal

from cp2k_input_tools import utils


def test_dformat():
    d1 = Decimal("-.1")
    d2 = Decimal("1")
    d3 = Decimal("1.23")
    d4 = Decimal("0.0000000000")  # becomes Decimal('0E-10')
    d5 = Decimal("1E-10")

    print(utils.dformat(d1, 10, 16))
    print(utils.dformat(d2, 10, 16))
    print(utils.dformat(d3, 10, 16))
    assert all(len(utils.dformat(val, 10, 16)) == 16 for val in (d1, d2, d3, d4, d5))

    assert utils.dformat(d1, 10, 16) == "   -0.1         "
    assert utils.dformat(d2, 10, 16) == "    1           "
    assert utils.dformat(d3, 10, 16) == "    1.23        "
    assert utils.dformat(d4, 10, 16) == "    0.0000000000"
    assert utils.dformat(d5, 10, 16) == "    0.0000000001"
