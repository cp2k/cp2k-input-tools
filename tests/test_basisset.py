import pytest

from cp2k_input_tools.basissets import BasisSetData
from cp2k_input_tools.utils import SYM2NUM

from . import TEST_DIR

INPUTS_DIR = TEST_DIR / "inputs"


def test_single_basisset_import():
    with (TEST_DIR / "inputs" / "BASIS_MOLOPT.H").open() as fhandle:
        lines = [line for line in fhandle]

    bset = BasisSetData.from_lines(lines)

    assert bset.element == "H"
    assert bset.n_el == 1


def test_single_basisset_import_invalid_nexp():
    with (TEST_DIR / "inputs" / "BASIS_MOLOPT.invalid_Rn").open() as fhandle:
        lines = [line for line in fhandle]

    with pytest.raises(ValueError):
        BasisSetData.from_lines(lines)


def test_single_basisset_roundtrip():
    with (TEST_DIR / "inputs" / "BASIS_MOLOPT.H").open() as fhandle:
        lines = [line.rstrip() for line in fhandle]
        bset = BasisSetData.from_lines(lines)
        assert list(bset.cp2k_format_line_iter()) == lines


def test_check_formatting():
    """check that reformatting a formatted file leaves it as is"""
    bsetfilein = INPUTS_DIR / "BASIS_SET.Au.unformatted"
    bsetfileout = INPUTS_DIR / "BASIS_SET.Au.formatted"

    with bsetfilein.open() as fhandlein, bsetfileout.open() as fhandleout:
        assert [line for entry in BasisSetData.datafile_iter(fhandlein) for line in entry.cp2k_format_line_iter()] == [
            line.rstrip() for line in fhandleout.readlines()
        ]


def test_datafile_lint(script_runner):
    """check that reformatting a formatted file leaves it as is"""
    bsetfile = INPUTS_DIR / "BASIS_SET.formatted"
    ret = script_runner.run(["cp2k-datafile-lint", "basisset", str(bsetfile)])

    assert ret.stderr == ""
    assert ret.success
    assert ret.stdout == bsetfile.read_text()


def test_datafile_lint_crystal(script_runner):
    """check that reformatting a formatted file leaves it as is"""
    bsetfile = INPUTS_DIR / "29_Cu.pob-DZVP-rev2"
    ret = script_runner.run(
        ["cp2k-datafile-lint", "basisset", "--input-basis-format", "crystal", "--output-basis-format", "crystal", str(bsetfile)]
    )

    assert ret.stderr == ""
    assert ret.success
    assert ret.stdout == bsetfile.read_text()


def test_bset_from_dicts():
    floated_dict = {
        "element": "Au",
        "identifiers": ["TZ-GTH"],
        "n_el": None,
        "blocks": [
            {
                "n": 1,
                "l": [[0, 3], [1, 3], [2, 3]],
                "coefficients": [
                    [
                        3.333,
                        -0.2996385052,
                        -0.0253458759,
                        -0.0505901975,
                        0.047560000000,
                        0.0233626853,
                        -0.0684498204,
                        -0.0411742318,
                        -0.0069631085,
                        -0.0874460677,
                    ],
                    [
                        1.582,
                        -0.0735011750,
                        0.0529347225,
                        0.3174596863,
                        -0.10200000000,
                        -0.0112096790,
                        0.3475804049,
                        1.1908036284,
                        0.7214138304,
                        1.5934218487,
                    ],
                    [
                        0.7514,
                        3.4198421760,
                        0.0063068212,
                        -1.0018695196,
                        0.002000000,
                        -0.1543550102,
                        -0.9013800582,
                        1.4081117116,
                        1.000000000,
                        1.2910850825,
                    ],
                    [
                        0.3568,
                        1.000000000,
                        -0.3216103136,
                        1.00000000000,
                        -0.3480000000,
                        -0.1918211838,
                        1.0,
                        1.000000000,
                        0.7172330126,
                        1.000000000,
                    ],
                    [
                        0.1694,
                        -7.5936501839,
                        1.000000000,
                        -0.4219772167,
                        0.9589000000,
                        1.000000000,
                        -0.4056275576,
                        0.6305837811,
                        -0.1109815597,
                        3.2469320583,
                    ],
                    [
                        0.0804,
                        -1.4837051517,
                        0.1624309950,
                        1.6448580273,
                        1.0000000000,
                        -0.1069487306,
                        0.4438513365,
                        0.0688923738,
                        -1.0201823698,
                        -2.0909150178,
                    ],
                ],
            }
        ],
    }

    stringified_dict = {
        "element": "Au",
        "identifiers": ["TZ-GTH"],
        "n_el": None,
        "blocks": [
            {
                "n": 1,
                "l": [[0, 3], [1, 3], [2, 3]],
                "coefficients": [
                    [
                        "3.333",
                        "-0.2996385052",
                        "-0.0253458759",
                        "-0.0505901975",
                        "0.047560000000",
                        "0.0233626853",
                        "-0.0684498204",
                        "-0.0411742318",
                        "-0.0069631085",
                        "-0.0874460677",
                    ],
                    [
                        "1.582",
                        "-0.0735011750",
                        "0.0529347225",
                        "0.3174596863",
                        "-0.10200000000",
                        "-0.0112096790",
                        "0.3475804049",
                        "1.1908036284",
                        "0.7214138304",
                        "1.5934218487",
                    ],
                    [
                        "0.7514",
                        "3.4198421760",
                        "0.0063068212",
                        "-1.0018695196",
                        "0.002000000",
                        "-0.1543550102",
                        "-0.9013800582",
                        "1.4081117116",
                        "1.000000000",
                        "1.2910850825",
                    ],
                    [
                        "0.3568",
                        "1.000000000",
                        "-0.3216103136",
                        "1.00000000000",
                        "-0.3480000000",
                        "-0.1918211838",
                        "1.0",
                        "1.000000000",
                        "0.7172330126",
                        "1.000000000",
                    ],
                    [
                        "0.1694",
                        "-7.5936501839",
                        "1.000000000",
                        "-0.4219772167",
                        "0.9589000000",
                        "1.000000000",
                        "-0.4056275576",
                        "0.6305837811",
                        "-0.1109815597",
                        "3.2469320583",
                    ],
                    [
                        "0.0804",
                        "-1.4837051517",
                        "0.1624309950",
                        "1.6448580273",
                        "1.0000000000",
                        "-0.1069487306",
                        "0.4438513365",
                        "0.0688923738",
                        "-1.0201823698",
                        "-2.0909150178",
                    ],
                ],
            }
        ],
    }

    # NOTE: they are not identical since the first one goes via the internal bit-representation of the float
    BasisSetData.model_validate(floated_dict)
    BasisSetData.model_validate(stringified_dict)


def test_new_style_ae_basisset_import():
    """Test that for new style -ae basis sets the number of electrons is properly set"""

    with (TEST_DIR / "inputs" / "BASIS_MOLOPT.new_style_ae").open() as fhandle:
        for entry in BasisSetData.datafile_iter(fhandle):
            assert entry.n_el, "Number of electrons is not set"
            assert entry.n_el == SYM2NUM[entry.element], "Invalid number of electrons"
