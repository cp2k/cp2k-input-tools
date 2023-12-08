import pathlib
import tempfile

import pytest

from cp2k_input_tools.pseudopotentials import (
    PseudopotentialData,
    PseudopotentialDataLocal,
)

from . import TEST_DIR

INPUTS_DIR = TEST_DIR / "inputs"


def test_single_pseudo_import():
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        pseudo = PseudopotentialData.from_lines([line for line in fhandle])

    assert pseudo.element == "Cl"
    assert pseudo.n_el == [2, 5]
    assert len(pseudo.local.coefficients) == 1
    assert len(pseudo.non_local) == 2
    assert pseudo.non_local[0].nproj == 2
    assert pseudo.non_local[1].nproj == 1


def test_single_pseudo_model_validate_aliased():
    """Check that for local/non_local data we can also use the abbrev 'coeffs' for loading"""
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        pseudo = PseudopotentialData.from_lines([line for line in fhandle])

    assert pseudo.local.coefficients
    asdict = pseudo.model_dump()

    asdict["local"]["coeffs"] = asdict["local"].pop("coefficients")
    reloaded = PseudopotentialData.model_validate(asdict)

    assert reloaded == pseudo


def test_single_pseudo_model_validate_nlcc_empty_default():
    """Check that for local/non_local data we can also use the abbrev 'coeffs' for loading"""
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        pseudo = PseudopotentialData.from_lines([line for line in fhandle])

    assert pseudo.nlcc is not None
    asdict = pseudo.model_dump()
    asdict.pop("nlcc")
    reloaded = PseudopotentialData.model_validate(asdict)
    assert reloaded.nlcc == []


def test_single_pseudo_from_dict_deprecated():
    """Check that from_dict still works as intented"""
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        pseudo = PseudopotentialData.from_lines([line for line in fhandle])

    asdict = pseudo.model_dump()
    with pytest.deprecated_call():
        reloaded = PseudopotentialData.from_dict(asdict)

    assert reloaded == pseudo


def test_single_pseudo_from_dict_type_map_deprecated():
    """Check that from_dict still works as intented with the type_map and its likely only use case"""
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        pseudo = PseudopotentialData.from_lines([line for line in fhandle])

    asdict = pseudo.model_dump()
    asdict["local"]["coeffs"] = asdict["local"].pop("coefficients")

    def rename(data):
        data["coefficients"] = data.pop("coeffs")
        return data

    with pytest.deprecated_call():
        reloaded = PseudopotentialData.from_dict(asdict, type_hooks={PseudopotentialDataLocal: rename})

    assert reloaded == pseudo


def test_single_pseudo_roundtrip():
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        lines = [line.rstrip() for line in fhandle]
        pseudo = PseudopotentialData.from_lines(lines)
        assert list(pseudo.cp2k_format_line_iter()) == lines


def test_datafile_lint(script_runner):
    """check that reformatting a formatted file leaves it as is"""
    pseudofile = INPUTS_DIR / "POTENTIAL.formatted"
    ret = script_runner.run(["cp2k-datafile-lint", "pseudo", str(pseudofile)])

    assert ret.stderr == ""
    assert ret.success
    assert ret.stdout == pseudofile.read_text()


def test_datafile_lint_inplace(script_runner):
    """check that reformatting a formatted file inplace leaves it as is"""
    pseudofile_orig = INPUTS_DIR / "POTENTIAL.formatted"
    content = pseudofile_orig.read_text()

    with tempfile.TemporaryDirectory() as tmpdir:
        pseudofile = pathlib.Path(tmpdir).joinpath("pseudos")
        pseudofile.write_text(content)

        ret = script_runner.run(["cp2k-datafile-lint", "--inplace", "pseudo", str(pseudofile)])

        assert ret.stderr == ""
        assert ret.success
        assert pseudofile.read_text() == content
