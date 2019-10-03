import pytest

from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML
from cp2k_input_tools.parser_errors import InvalidParameterError, PreprocessorError
from cp2k_input_tools.tokenizer import UnterminatedStringError


def test_error_invalid_number_of_parameters():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/error_nvar.inp"), "r") as fhandle:
        with pytest.raises(InvalidParameterError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert "invalid values for keyword: A" in excinfo.value.args[0]
    assert excinfo.value.args[1]["linenr"] == 40  # Python starts counting at 0
    assert isinstance(excinfo.value.__cause__, InvalidParameterError)
    assert "keyword expects exactly 3 values, 2 were given" in excinfo.value.__cause__.args[0]


def test_unterminated_string():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/unterminated_string.inp"), "r") as fhandle:
        with pytest.raises(UnterminatedStringError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert excinfo.value.args[1]["linenr"] == 13  # Python starts counting at 0


def test_undefined_preprocessor_var():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/preprocesser_undefined_var.inp"), "r") as fhandle:
        with pytest.raises(PreprocessorError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert "undefined variable 'HP'" in excinfo.value.args[0]
    assert excinfo.value.args[1]["linenr"] == 29
