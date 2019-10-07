import json
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
