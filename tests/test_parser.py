import pathlib

import pytest

from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserSimplified
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML

TEST_DIR = pathlib.Path(__file__).resolve().parent


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
