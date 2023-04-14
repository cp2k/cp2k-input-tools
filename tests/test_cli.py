import json
import pathlib
from tempfile import TemporaryDirectory

import pytest

from . import TEST_DIR

INPUTS_DIR = TEST_DIR / "inputs"


def test_fromcp2k_json_simple(script_runner):
    ret = script_runner.run("fromcp2k", str(INPUTS_DIR / "test01.inp"))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "global" in tree

    with (INPUTS_DIR / "test01.json").open() as fhandle:
        # validate output against previously generated tree (to avoid regressions)
        assert json.load(fhandle) == tree


def test_fromcp2k_json_canonical(script_runner):
    ret = script_runner.run("fromcp2k", "-c", str(INPUTS_DIR / "test01.inp"))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "+global" in tree


def test_fromcp2k_json_trafo_lower(script_runner):
    ret = script_runner.run("fromcp2k", "--trafo=lower", str(INPUTS_DIR / "test01.inp"))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert "a" in tree["force_eval"]["subsys"]["cell"]  # single letter keys would be capitalized in the "auto" trafo


def test_fromcp2k_json_trafo_upper(script_runner):
    ret = script_runner.run("fromcp2k", "--trafo=upper", str(INPUTS_DIR / "test01.inp"))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert "GLOBAL" in tree  # keys with more than 3 chars would be lowercase in the "auto" trafo


def test_fromcp2k_yaml_simple(script_runner):
    ryaml = pytest.importorskip("ruamel.yaml")
    yaml = ryaml.YAML()

    ret = script_runner.run("fromcp2k", "-f", "yaml", str(INPUTS_DIR / "test01.inp"))
    assert ret.success
    assert ret.stderr == ""

    tree = yaml.load(ret.stdout)

    assert isinstance(tree, dict)
    assert "global" in tree

    with (INPUTS_DIR / "test01.yaml").open() as fhandle:
        # validate output against previously generated tree (to avoid regressions)
        assert yaml.load(fhandle) == tree


def test_fromcp2k_with_external_var(script_runner):
    ret = script_runner.run("fromcp2k", "-E", "HP=1", str(INPUTS_DIR / "invalid_without_set_var.inp"))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "force_eval" in tree
    assert "DFT" in tree["force_eval"]
    assert "kpoints" in tree["force_eval"]["DFT"]

    ret = script_runner.run("fromcp2k", "-E", "HP=0", str(INPUTS_DIR / "invalid_without_set_var.inp"))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "force_eval" in tree
    assert "DFT" in tree["force_eval"]
    assert "kpoints" not in tree["force_eval"]["DFT"]


def test_fromcp2k_aiida_calc(script_runner):
    ret = script_runner.run("fromcp2k", "--format", "aiida-cp2k-calc", str(INPUTS_DIR / "test01.inp"))

    assert ret.success
    assert ret.stderr == ""

    assert '"METHOD": "QUICKSTEP"' in ret.stdout
    assert "builder = cp2k_code.get_builder" in ret.stdout


def test_fromcp2k_stdin(script_runner):
    with (INPUTS_DIR / "test01.inp").open() as stdin:
        ret = script_runner.run("fromcp2k", stdin=stdin)

    assert ret.success
    assert ret.stderr == ""
    tree = json.loads(ret.stdout)

    with (INPUTS_DIR / "test01.json").open() as fhandle:
        # validate output against previously generated tree (to avoid regressions)
        assert json.load(fhandle) == tree


def test_tocp2k_json(script_runner):
    ret = script_runner.run("tocp2k", str(INPUTS_DIR / "test01.json"))

    assert ret.success
    assert ret.stderr == ""

    assert "&GLOBAL" in ret.stdout


def test_tocp2k_json_stdin(script_runner):
    with (INPUTS_DIR / "test01.json").open() as stdin:
        ret = script_runner.run("tocp2k", stdin=stdin)

    assert ret.success
    assert ret.stderr == ""

    assert "&GLOBAL" in ret.stdout


def test_tocp2k_yaml(script_runner):
    pytest.importorskip("ruamel.yaml")

    ret = script_runner.run("tocp2k", "-y", str(INPUTS_DIR / "NaCl-BS.yaml"))

    assert ret.success
    assert ret.stderr == ""

    assert "&GLOBAL" in ret.stdout


def test_cp2klint(script_runner):
    ret = script_runner.run("cp2klint", str(INPUTS_DIR / "test01.inp"))

    assert ret.success
    assert ret.stderr == ""


def test_cp2klint_error(script_runner):
    ret = script_runner.run("cp2klint", str(INPUTS_DIR / "error_nvar.inp"))

    assert not ret.success
    assert "Syntax error" in ret.stdout
    assert "invalid values for keyword: A" in ret.stdout


def test_cp2klint_error_context(script_runner):
    ret = script_runner.run("cp2klint", str(INPUTS_DIR / "unterminated_var.inp"))

    assert not ret.success
    assert "Syntax error: unterminated variable" in ret.stdout
    assert "line   36: @IF ${HP\n               ~~~~^" in ret.stdout


def test_cp2klint_invalid_set_arg(script_runner):
    ret = script_runner.run("cp2klint", "--set", "missing-equal-sign", str(INPUTS_DIR / "test01.inp"))

    assert not ret.success
    assert "Error: Invalid value for '-E' / '--set'" in ret.stderr


def test_cp2klint_invalid_without_set_var(script_runner):
    ret = script_runner.run("cp2klint", str(INPUTS_DIR / "invalid_without_set_var.inp"))

    assert not ret.success
    assert "Syntax error: undefined variable 'HP'" in ret.stdout


def test_cp2klint_invalid_without_set_var_defined(script_runner):
    ret = script_runner.run("cp2klint", "--set", "HP=1", str(INPUTS_DIR / "invalid_without_set_var.inp"))

    assert ret.success


def test_cp2kgen_simplified(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run("cp2kgen", str(INPUTS_DIR / "NaCl.inp"), "force_eval/dft/mgrid/cutoff=[800,900,1000]", cwd=cwd)

        assert ret.stderr == ""
        assert ret.success

        cwd = pathlib.Path(cwd)
        for cutoff in [800, 900, 1000]:
            assert (cwd / f"NaCl-cutoff_{cutoff}.inp").exists()


def test_cp2kgen_canonical(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run(
            "cp2kgen", "-c", str(INPUTS_DIR / "NaCl.inp"), "+force_eval/0/+dft/+mgrid/cutoff=[800,900,1000]", cwd=cwd
        )

        assert ret.stderr == ""
        assert ret.success

        cwd = pathlib.Path(cwd)
        for cutoff in [800, 900, 1000]:
            assert (cwd / f"NaCl-cutoff_{cutoff}.inp").exists()


def test_cp2kgen_simplified_indexed_single_value(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run("cp2kgen", str(INPUTS_DIR / "NaCl.inp"), "force_eval/subsys/cell/a/0=10.0", cwd=cwd)

        assert ret.stderr == ""
        assert ret.success

        cwd = pathlib.Path(cwd)
        assert (cwd / "NaCl-0_10.0.inp").exists()


def test_cp2kgen_invalid_expression(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run("cp2kgen", str(INPUTS_DIR / "NaCl.inp"), "force_eval/subsys/cell/a/0 -> test", cwd=cwd)

        assert "an expression must be of the form" in ret.stderr
        assert not ret.success


def test_cp2kget_simplified(script_runner):
    ret = script_runner.run("cp2kget", str(INPUTS_DIR / "NaCl.inp"), "force_eval/dft/mgrid/cutoff")

    assert ret.stderr == ""
    assert ret.success
    assert "force_eval/dft/mgrid/cutoff: 800.0" in ret.stdout


def test_cp2kget_canonical(script_runner):
    ret = script_runner.run("cp2kget", "-c", str(INPUTS_DIR / "NaCl.inp"), "+force_eval/0/+dft/+mgrid/cutoff")

    assert ret.stderr == ""
    assert ret.success
    assert "+force_eval/0/+dft/+mgrid/cutoff: 800.0" in ret.stdout


def test_cp2kget_simplified_indexed_single_value(script_runner):
    ret = script_runner.run("cp2kget", str(INPUTS_DIR / "NaCl.inp"), "force_eval/subsys/cell/a/0")

    assert ret.stderr == ""
    assert ret.success
    assert "force_eval/subsys/cell/a/0: 5.64123539364476" in ret.stdout


def test_cp2kget_simplified_list_value(script_runner):
    """check that getting a list element gives human readable output"""
    ret = script_runner.run("cp2kget", str(INPUTS_DIR / "NaCl.inp"), "force_eval/subsys/cell/a")

    assert ret.stderr == ""
    assert ret.success
    assert "force_eval/subsys/cell/a: (5.64123539364476, 0.0, 0.0)" in ret.stdout
