import pathlib

from cp2k_input_tools import __version__
from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML

TEST_DIR = pathlib.Path(__file__).resolve().parent


def test_version():
    assert __version__ == "0.1.0"


def test_simple():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test01.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "global" in tree


def test_simple_inclusion():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=TEST_DIR.joinpath("inputs/"))

    with open(TEST_DIR.joinpath("inputs/test02.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert "force_eval" in tree
