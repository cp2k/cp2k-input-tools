import pathlib
import json

from cp2k_input_tools.generator import CP2KInputGenerator
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML

TEST_DIR = pathlib.Path(__file__).resolve().parent


def test_simple():
    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/test01.json"), "r") as fhandle:
        tree = json.load(fhandle)

    lines = list(cp2k_generator.line_iter(tree))
    print(lines)
    assert any("&GLOBAL" in line for line in lines)
