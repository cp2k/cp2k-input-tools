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
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["a"] == [0.0, 2.8595, 2.8595]
    assert tree["+force_eval"][0]["+subsys"]["+cell"]["b"] == [2.8595, 0.0, 2.8595]


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
