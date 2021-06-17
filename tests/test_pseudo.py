import pathlib
import tempfile

from cp2k_input_tools.pseudopotentials import PseudopotentialData

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


def test_single_pseudo_roundtrip():
    with (TEST_DIR / "inputs" / "GTH_POTENTIALS.Cl").open() as fhandle:
        lines = [line.rstrip() for line in fhandle]
        pseudo = PseudopotentialData.from_lines(lines)
        assert list(pseudo.cp2k_format_line_iter()) == lines


def test_datafile_lint(script_runner):
    """check that reformatting a formatted file leaves it as is"""
    pseudofile = INPUTS_DIR / "POTENTIAL.formatted"
    ret = script_runner.run("cp2k-datafile-lint", "pseudo", str(pseudofile))

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

        ret = script_runner.run("cp2k-datafile-lint", "--inplace", "pseudo", str(pseudofile))

        assert ret.stderr == ""
        assert ret.success
        assert pseudofile.read_text() == content
