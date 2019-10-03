import json
import pytest

from . import TEST_DIR
from cp2k_input_tools.generator import CP2KInputGenerator
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML


def test_simple():
    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test01.json"), "r") as fhandle:
        tree = json.load(fhandle)

    lines = list(cp2k_generator.line_iter(tree))
    assert any("&GLOBAL" in line for line in lines)


def test_simplified_input():
    ryaml = pytest.importorskip("ruamel.yaml")
    yaml = ryaml.YAML()

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/NaCl-BS.simplified.yaml"), "r") as fhandle:
        tree = yaml.load(fhandle)

    lines = list(cp2k_generator.line_iter(tree))
    with open(TEST_DIR.joinpath("inputs/NaCl-BS.simplified.inp"), "r") as fhandle:
        assert lines == [l.strip("\n") for l in fhandle.readlines()]
