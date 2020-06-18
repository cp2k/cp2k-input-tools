import pytest

from . import TEST_DIR

from cp2k_input_tools.parser_errors import InvalidNameError


def test_incomplete(parser):
    with open(TEST_DIR.joinpath("inputs/NaCl-incomplete.inp"), "r") as fhandle:
        with pytest.raises(InvalidNameError) as excinfo:
            parser.parse(fhandle)

    ctx = excinfo.value.args[1]
    section = ctx["section"]
    assert section.name == "SCF"
    assert next(n for n in section.keyword_names if n.startswith("SCF_GUE")) == "SCF_GUESS"
