from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML


def test_var_substition():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/NaCl.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["a"] == [0.0, 2.8595, 2.8595]
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["b"] == [2.8595, 0.0, 2.8595]
