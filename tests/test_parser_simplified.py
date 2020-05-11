from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParserSimplified


def test_simplified_no_unpack():
    cp2k_parser = CP2KInputParserSimplified(key_trafo=str.upper, multi_value_unpack=False, repeated_section_unpack=False)

    with (TEST_DIR / "inputs" / "deltatest_C_0.98.inp").open("r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert tree["FORCE_EVAL"]["SUBSYS"]["KIND"] == {
        "_": "C",
        "ELEMENT": "C",
        "POTENTIAL": "GTH-PBE-q4",
        "BASIS_SET": "ORB TZVP-MOLOPT-SR-GTH",
    }


def test_simplified_aiida():
    cp2k_parser = CP2KInputParserSimplified(
        key_trafo=str.upper, multi_value_unpack=False, repeated_section_unpack=False, level_reduction_blacklist=["KIND"]
    )

    with (TEST_DIR / "inputs" / "deltatest_C_0.98.inp").open("r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert tree["FORCE_EVAL"]["SUBSYS"]["KIND"][0] == {
        "_": "C",
        "ELEMENT": "C",
        "POTENTIAL": "GTH-PBE-q4",
        "BASIS_SET": "ORB TZVP-MOLOPT-SR-GTH",
    }
