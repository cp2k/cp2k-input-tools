import pytest

from cp2k_input_tools.parser_errors import InvalidNameError

from . import TEST_DIR


def test_incomplete(parser):
    with open(TEST_DIR.joinpath("inputs/NaCl-incomplete.inp"), "r") as fhandle:
        with pytest.raises(InvalidNameError) as excinfo:
            parser.parse(fhandle)

    ctx = excinfo.value.args[1]
    assert ctx.section.name == "SCF"
    assert next(n for n in ctx.section.keyword_names if n.startswith("SCF_GUE")) == "SCF_GUESS"
