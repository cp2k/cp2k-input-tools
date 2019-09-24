import pytest

from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML
from cp2k_input_tools.parser_errors import InvalidParameterError


def test_error_invalid_number_of_parameters():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/error_nvar.inp"), "r") as fhandle:
        with pytest.raises(InvalidParameterError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert "invalid values for keyword: A" in excinfo.value.args[0]
    assert excinfo.value.args[1]["linenr"] == 40
    assert isinstance(excinfo.value.__cause__, InvalidParameterError)
    assert "keyword expects exactly 3 values, 2 were given" in excinfo.value.__cause__.args[0]
