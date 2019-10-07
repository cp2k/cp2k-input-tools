import pytest

from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserSimplified
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML


def test_simple_simplified():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test01.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "global" in tree


def test_simple_canonical():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test01.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "+global" in tree


def test_simple_simplified_unitconv():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test01_units.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    # this value is given in Bohr while the default is Angstrom, make sure the conversion was correct
    assert tree["force_eval"]["subsys"]["cell"]["a"][0] == pytest.approx(4.07419, 1e-3)


def test_simple_simplified_inclusion():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML, base_dir=TEST_DIR.joinpath("inputs/"))

    with open(TEST_DIR.joinpath("inputs/test02.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "global" in tree
    assert "force_eval" in tree
    # the simplified parser collapses repeated sections containing a single element
    assert isinstance(tree["force_eval"], dict)


def test_simple_canonical_inclusion():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=TEST_DIR.joinpath("inputs/"))

    with open(TEST_DIR.joinpath("inputs/test02.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "+global" in tree
    assert "+force_eval" in tree
    assert isinstance(tree["+force_eval"], list)
    assert isinstance(tree["+force_eval"][0], dict)


def test_conditional_inclusion():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML, base_dir=TEST_DIR.joinpath("inputs/"))

    with open(TEST_DIR.joinpath("inputs/test03.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "global" in tree
    assert "force_eval" not in tree


def test_line_continuation():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/line_continuation.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert tree == {"+global": {"print_level": "medium", "project_name": "fatman.calc", "run_type": "energy"}}


def test_repeated_kinds_simplified():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test04.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree["force_eval"]["subsys"]["kind"] == {
        "O": {"element": "O", "potential": "GTH-PBE-q6", "basis_set": ["ORB", "TZVP-MOLOPT-SR-GTH"]},
        "C": {"element": "C", "potential": "GTH-PBE-q4", "basis_set": ["ORB", "TZVP-MOLOPT-SR-GTH"]},
        "Ti": {"element": "Ti", "potential": "GTH-PBE-q12", "basis_set": ["ORB", "TZVP-MOLOPT-SR-GTH"]},
    }


def test_repeated_kinds_canonical():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test04.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree["+force_eval"][0]["+subsys"]["+kind"] == [
        {"_": "O", "element": "O", "potential": "GTH-PBE-q6", "basis_set": [["ORB", "TZVP-MOLOPT-SR-GTH"]]},
        {"_": "C", "element": "C", "potential": "GTH-PBE-q4", "basis_set": [["ORB", "TZVP-MOLOPT-SR-GTH"]]},
        {"_": "Ti", "element": "Ti", "potential": "GTH-PBE-q12", "basis_set": [["ORB", "TZVP-MOLOPT-SR-GTH"]]},
    ]


def test_no_lone_keyword_addition_for_section_params():
    """
    initially we added the lone keyword value for the section parameter
    while this is needed for keywords, we can and should omit it for section parameters
    and simply leave it away to avoid adding values in the tree
    """

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test04_minimal.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree == {"+force_eval": [{"+dft": {"+print": {"+moments": {"periodic": False, "reference": "com"}}}}]}


def test_start_empty_lines():

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/empty_lines.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree


def test_inline_comment():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/inline_comment.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree


def test_fractional_values():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/fractional_values.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["a"][0] == 4.0  # specified as 8/2
