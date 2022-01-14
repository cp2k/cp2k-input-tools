import io

import pytest

from cp2k_input_tools import DEFAULT_CP2K_INPUT_XML
from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.parser_errors import (
    InvalidNameError,
    InvalidParameterError,
    InvalidSectionError,
    PreprocessorError,
    SectionMismatchError,
)
from cp2k_input_tools.tokenizer import UnterminatedStringError

from . import TEST_DIR


def test_error_invalid_number_of_parameters():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/error_nvar.inp"), "r") as fhandle:
        with pytest.raises(InvalidParameterError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert "invalid values for keyword: A" in excinfo.value.args[0]
    assert excinfo.value.args[1].linenr == 41
    assert isinstance(excinfo.value.__cause__, InvalidParameterError)
    assert "keyword expects exactly 3 values, 2 were given" in excinfo.value.__cause__.args[0]


def test_unterminated_string():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/unterminated_string.inp"), "r") as fhandle:
        with pytest.raises(UnterminatedStringError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert excinfo.value.args[1].linenr == 14


def test_undefined_preprocessor_var():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(TEST_DIR.joinpath("inputs/preprocesser_undefined_var.inp"), "r") as fhandle:
        with pytest.raises(PreprocessorError) as excinfo:
            cp2k_parser.parse(fhandle)

    assert "undefined variable 'HP'" in excinfo.value.args[0]
    assert excinfo.value.args[1].linenr == 30


def test_multiple_defined_non_repeating_section():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO(
        """
        &GLOBAL
        &END GLOBAL
        &GLOBAL
        &END GLOBAL
        """
    )

    with pytest.raises(InvalidNameError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "the section 'GLOBAL' can not be defined multiple times" in excinfo.value.args[0]


def test_missing_section_end():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO(
        """
        &GLOBAL
        ! &END GLOBAL
        &FORCE_EVAL
        &END FORCE_EVAL
        """
    )

    with pytest.raises(InvalidSectionError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "invalid section" in excinfo.value.args[0]

    fhandle = io.StringIO(
        """
        &GLOBAL
        &END GLOBAL
        &FORCE_EVAL
        """
    )

    with pytest.raises(SectionMismatchError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "not closed" in excinfo.value.args[0]


def test_section_end_mismatch():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO(
        """
        &GLOBAL
        &END GLOBI
        &FORCE_EVAL
        &END FORCE_EVAL
        """
    )

    with pytest.raises(SectionMismatchError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "could not match open section" in excinfo.value.args[0]


def test_section_parameter_error():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO(
        """
        &GLOBAL invalidparam
        &END GLOBAL
        """
    )

    with pytest.raises(InvalidParameterError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "section parameters given for non-parametrized section" in excinfo.value.args[0]


def test_invalid_keyword():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle = io.StringIO(
        """
        &FORCE_EVAL
           &SUBSYS
              BASIS_SET TZVPd-MOLOPT-SR-GTH
           &END SUBSYS
        &END FORCE_EVAL
        """
    )

    with pytest.raises(InvalidNameError) as excinfo:
        cp2k_parser.parse(fhandle)

    assert "invalid keyword" in excinfo.value.args[0]


def test_internal_cp2k_unit():
    """
    Ensure that values for keywords with a default internal_cp2k unit are accepted unless there is an explicit unit.
    The first one is a reproducer of https://github.com/cp2k/cp2k-input-tools/issues/54
    """

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    fhandle_no_extra_unit = io.StringIO(
        """
        &FORCE_EVAL
          METHOD  FIST
          &MM
            &FORCEFIELD
              IGNORE_MISSING_CRITICAL_PARAMS  T
              &BOND
                ATOMS O H
                KIND  HARMONIC
                K      4.7265512325474252E-01
                R0     1.9124028464802707E+00
              &END BOND
              &BEND
                ATOMS H O H
                KIND  HARMONIC
                K     1.2095435014802065E-01
                THETA0     1.9764108449583786E+00
              &END BEND
            &END FORCEFIELD
          &END MM
        &END FORCE_EVAL
        """
    )

    cp2k_parser.parse(fhandle_no_extra_unit)

    fhandle_extra_unit = io.StringIO(
        """
        &FORCE_EVAL
          METHOD  FIST
          &MM
            &FORCEFIELD
              IGNORE_MISSING_CRITICAL_PARAMS  T
              &BOND
                ATOMS O H
                KIND  HARMONIC
                K      [hartree] 4.7265512325474252E-01
                R0     1.9124028464802707E+00
              &END BOND
              &BEND
                ATOMS H O H
                KIND  HARMONIC
                K     [hartree] 1.2095435014802065E-01
                THETA0     1.9764108449583786E+00
              &END BEND
            &END FORCEFIELD
          &END MM
        &END FORCE_EVAL
        """
    )

    with pytest.raises(InvalidParameterError) as excinfo:
        cp2k_parser.parse(fhandle_extra_unit)

    assert "invalid values" in excinfo.value.args[0]
