import io

import pytest

from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML
from cp2k_input_tools.parser_errors import PreprocessorError


def test_var_substition():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/NaCl.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert isinstance(tree, dict)
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["a"] == (5.64123539364476, 0.0, 0.0)
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["b"] == (0.0, 5.64123539364476, 0.0)
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["c"] == (0.0, 0.0, 5.64123539364476)

    # make sure @IF DO_CELLOPT case evaluated to false:
    assert tree["+global"]["run_type"] == "energy_force"
    assert "+motion" not in tree

    # make sure @IF $DO_KPOINTS evaluated to false
    assert "+kpoints" in tree["+force_eval"][0]["+dft"]


def test_undefined_var():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("""${undef}""")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "undefined variable 'undef'" in excinfo.value.args[0]


def test_if_without_endif():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("""@IF 1\n""")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "conditional block not closed at end of file" in excinfo.value.args[0]


def test_endif_without_if():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("""@ENDIF""")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "found @ENDIF without a previous @IF" in excinfo.value.args[0]


def test_nested_if():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("""@IF 1\n@IF 0\n@ENDIF\n@ENDIF""")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "nested @IF are not allowed" in excinfo.value.args[0]


def test_endif_garbage():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("""@IF 1\n@ENDIF foo""")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "garbage found after @ENDIF" in excinfo.value.args[0]


def test_unknown_preprocessor():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("""@FOOBAR foo""")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "unknown preprocessor directive found" in excinfo.value.args[0]


def test_include_missing():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("@INCLUDE")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "@INCLUDE requires exactly one argument" in excinfo.value.args[0]


def test_include_empty():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("@INCLUDE ''")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "@INCLUDE requires exactly one argument" in excinfo.value.args[0]


def test_include_multiple():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("@INCLUDE 'foo' 'bar'")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "@INCLUDE requires exactly one argument" in excinfo.value.args[0]


def test_include_undefined_var():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("@INCLUDE $foo")

    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "undefined variable" in excinfo.value.args[0]


def test_var_default_val():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/default_var_val.inp"), "r") as fhandle:
        tree_with = cp2k_parser.parse(fhandle)
        fhandle.seek(0)
        tree_without = cp2k_parser.parse(fhandle, initial_variable_values={"HP": 0})

    assert "+kpoints" in tree_with["+force_eval"][0]["+dft"]
    assert "+kpoints" not in tree_without["+force_eval"][0]["+dft"]


def test_invalid_var_name():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO("@SET 1bar 1")
    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)
    assert "invalid variable name" in excinfo.value.args[0]

    fhandle = io.StringIO("${1foo-bar}")
    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)
    assert "invalid variable name" in excinfo.value.args[0]

    fhandle = io.StringIO("@SET CP2K-DATA ./cp2k-data")
    with pytest.raises(PreprocessorError) as excinfo:
        cp2k_parser.parse(fhandle)
    assert "invalid variable name" in excinfo.value.args[0]


def test_xctype():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=[TEST_DIR.joinpath("inputs/")])

    with open(TEST_DIR.joinpath("inputs/He_PBE.inp"), "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    assert tree
    assert tree["+force_eval"][0]["+dft"]["+xc"]["+xc_functional"]["_"] == "PBE"


def test_xctype_not_found():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/He_PBE.inp"), "r") as fhandle:
        with pytest.raises(PreprocessorError) as _:
            cp2k_parser.parse(fhandle)
