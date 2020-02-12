import json
from tempfile import TemporaryDirectory
import pathlib
import pytest

from . import TEST_DIR


def test_fromcp2k_json_simple(script_runner):
    ret = script_runner.run("fromcp2k", str(TEST_DIR.joinpath("inputs/test01.inp")))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "global" in tree

    with open(TEST_DIR.joinpath("inputs/test01.json"), "r") as fhandle:
        # validate output against previously generated tree (to avoid regressions)
        assert json.load(fhandle) == tree


def test_fromcp2k_json_canonical(script_runner):
    ret = script_runner.run("fromcp2k", "-c", str(TEST_DIR.joinpath("inputs/test01.inp")))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "+global" in tree


def test_fromcp2k_json_trafo_lower(script_runner):
    ret = script_runner.run("fromcp2k", "--trafo=lower", str(TEST_DIR.joinpath("inputs/test01.inp")))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert "a" in tree["force_eval"]["subsys"]["cell"]  # single letter keys would be capitalized in the "auto" trafo


def test_fromcp2k_json_trafo_upper(script_runner):
    ret = script_runner.run("fromcp2k", "--trafo=upper", str(TEST_DIR.joinpath("inputs/test01.inp")))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert "GLOBAL" in tree  # keys with more than 3 chars would be lowercase in the "auto" trafo


def test_fromcp2k_yaml_simple(script_runner):
    ryaml = pytest.importorskip("ruamel.yaml")
    yaml = ryaml.YAML()

    ret = script_runner.run("fromcp2k", "-y", str(TEST_DIR.joinpath("inputs/test01.inp")))
    assert ret.success
    assert ret.stderr == ""

    tree = yaml.load(ret.stdout)

    assert isinstance(tree, dict)
    assert "global" in tree

    with open(TEST_DIR.joinpath("inputs/test01.yaml"), "r") as fhandle:
        # validate output against previously generated tree (to avoid regressions)
        assert yaml.load(fhandle) == tree


def test_fromcp2k_with_external_var(script_runner):
    ret = script_runner.run("fromcp2k", "-E", "HP=1", str(TEST_DIR.joinpath("inputs/invalid_without_set_var.inp")))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "force_eval" in tree
    assert "DFT" in tree["force_eval"]
    assert "kpoints" in tree["force_eval"]["DFT"]

    ret = script_runner.run("fromcp2k", "-E", "HP=0", str(TEST_DIR.joinpath("inputs/invalid_without_set_var.inp")))

    assert ret.success
    assert ret.stderr == ""

    tree = json.loads(ret.stdout)

    assert isinstance(tree, dict)
    assert "force_eval" in tree
    assert "DFT" in tree["force_eval"]
    assert "kpoints" not in tree["force_eval"]["DFT"]


def test_tocp2k_json(script_runner):
    ret = script_runner.run("tocp2k", str(TEST_DIR.joinpath("inputs/test01.json")))

    assert ret.success
    assert ret.stderr == ""

    assert "&GLOBAL" in ret.stdout


def test_tocp2k_yaml(script_runner):
    pytest.importorskip("ruamel.yaml")

    ret = script_runner.run("tocp2k", "-y", str(TEST_DIR.joinpath("inputs/NaCl-BS.yaml")))

    assert ret.success
    assert ret.stderr == ""

    assert "&GLOBAL" in ret.stdout


def test_cp2klint(script_runner):
    ret = script_runner.run("cp2klint", str(TEST_DIR.joinpath("inputs/test01.inp")))

    assert ret.success
    assert ret.stderr == ""


def test_cp2klint_error(script_runner):
    ret = script_runner.run("cp2klint", str(TEST_DIR.joinpath("inputs/error_nvar.inp")))

    assert not ret.success
    assert "Syntax error" in ret.stdout
    assert "invalid values for keyword: A" in ret.stdout


def test_cp2klint_error_context(script_runner):
    ret = script_runner.run("cp2klint", str(TEST_DIR.joinpath("inputs/unterminated_var.inp")))

    assert not ret.success
    assert "Syntax error: unterminated variable" in ret.stdout
    assert "line   36: @IF ${HP\n               ~~~~^" in ret.stdout


def test_cp2klint_invalid_set_arg(script_runner):
    ret = script_runner.run("cp2klint", "--set", "missing-equal-sign", str(TEST_DIR.joinpath("inputs/test01.inp")))

    assert not ret.success
    assert "error: argument -E/--set" in ret.stderr


def test_cp2klint_invalid_without_set_var(script_runner):
    ret = script_runner.run("cp2klint", str(TEST_DIR.joinpath("inputs/invalid_without_set_var.inp")))

    assert not ret.success
    assert "Syntax error: undefined variable 'HP'" in ret.stdout


def test_cp2klint_invalid_without_set_var_defined(script_runner):
    ret = script_runner.run("cp2klint", "--set", "HP=1", str(TEST_DIR.joinpath("inputs/invalid_without_set_var.inp")))

    assert ret.success


def test_cp2kgen_simplified(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run(
            "cp2kgen", str(TEST_DIR / "inputs" / "NaCl.inp"), "force_eval/dft/mgrid/cutoff=[800,900,1000]", cwd=cwd
        )

        assert ret.stderr == ""
        assert ret.success

        cwd = pathlib.Path(cwd)
        for cutoff in [800, 900, 1000]:
            assert (cwd / f"NaCl-cutoff_{cutoff}.inp").exists()


def test_cp2kgen_canonical(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run(
            "cp2kgen", "-c", str(TEST_DIR / "inputs" / "NaCl.inp"), "+force_eval/0/+dft/+mgrid/cutoff=[800,900,1000]", cwd=cwd
        )

        assert ret.stderr == ""
        assert ret.success

        cwd = pathlib.Path(cwd)
        for cutoff in [800, 900, 1000]:
            assert (cwd / f"NaCl-cutoff_{cutoff}.inp").exists()


def test_cp2kgen_simplified_indexed_single_value(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run("cp2kgen", str(TEST_DIR / "inputs" / "NaCl.inp"), "force_eval/subsys/cell/a/0=10.0", cwd=cwd)

        assert ret.stderr == ""
        assert ret.success

        cwd = pathlib.Path(cwd)
        assert (cwd / f"NaCl-0_10.0.inp").exists()


def test_cp2kgen_invalid_expression(script_runner):
    with TemporaryDirectory() as cwd:
        ret = script_runner.run("cp2kgen", str(TEST_DIR / "inputs" / "NaCl.inp"), "force_eval/subsys/cell/a/0 -> test", cwd=cwd)

        assert "an expression must be of the form" in ret.stderr
        assert not ret.success


def test_cp2kget_simplified(script_runner):
    ret = script_runner.run("cp2kget", str(TEST_DIR / "inputs" / "NaCl.inp"), "force_eval/dft/mgrid/cutoff")

    assert ret.stderr == ""
    assert ret.success
    assert "force_eval/dft/mgrid/cutoff: 800.0" in ret.stdout


def test_cp2kget_canonical(script_runner):
    ret = script_runner.run("cp2kget", "-c", str(TEST_DIR / "inputs" / "NaCl.inp"), "+force_eval/0/+dft/+mgrid/cutoff")

    assert ret.stderr == ""
    assert ret.success
    assert "+force_eval/0/+dft/+mgrid/cutoff: 800.0" in ret.stdout


def test_cp2kget_simplified_indexed_single_value(script_runner):
    ret = script_runner.run("cp2kget", str(TEST_DIR / "inputs" / "NaCl.inp"), "force_eval/subsys/cell/a/0")

    assert ret.stderr == ""
    assert ret.success
    assert "force_eval/subsys/cell/a/0: 5.64123539364476" in ret.stdout


def test_cp2kget_simplified_list_value(script_runner):
    """check that getting a list element gives human readable output"""
    ret = script_runner.run("cp2kget", str(TEST_DIR / "inputs" / "NaCl.inp"), "force_eval/subsys/cell/a")

    assert ret.stderr == ""
    assert ret.success
    assert "force_eval/subsys/cell/a: 5.64123539364476, 0.0, 0.0" in ret.stdout
