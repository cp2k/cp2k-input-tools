from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParserSimplified, CP2KInputParserAiiDA


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


def test_repeated_keywords():
    """Verify bug https://github.com/cp2k/cp2k-input-tools/issues/33 for repeated BASIS_SET_FILE_NAME is fixed"""
    cp2k_parser = CP2KInputParserSimplified(key_trafo=str.lower)

    with (TEST_DIR / "inputs" / "repeated_keywords.inp").open("r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree["force_eval"]["dft"]["basis_set_file_name"] == ["BASIS_MOLOPT", "BASIS_MOLOPT_UCL"]
    assert tree["force_eval"]["subsys"]["kind"]["H"]["basis_set"] == ["TZV2P-MOLOPT-GTH", ("AUX_FIT", "pFIT3")]


def test_repeated_keywords_tuples():
    """Verify bug https://github.com/cp2k/cp2k-input-tools/issues/32 for repeated BASIS_SET is fixed"""
    cp2k_parser = CP2KInputParserSimplified(key_trafo=str.lower)

    with (TEST_DIR / "inputs" / "repeated_keywords_tuples.inp").open("r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree["force_eval"]["subsys"]["kind"]["H"]["basis_set"] == [("AUX_FIT", "pFIT3"), "TZV2P-MOLOPT-GTH"]


def test_simplified_aiida():
    cp2k_parser = CP2KInputParserAiiDA()

    with (TEST_DIR / "inputs" / "deltatest_C_0.98.inp").open("r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert tree["FORCE_EVAL"]["SUBSYS"]["KIND"][0] == {
        "_": "C",
        "ELEMENT": "C",
        "POTENTIAL": "GTH-PBE-q4",
        "BASIS_SET": "ORB TZVP-MOLOPT-SR-GTH",
    }
