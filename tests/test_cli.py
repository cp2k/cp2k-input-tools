import json

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


def test_tocp2k_json(script_runner):
    ret = script_runner.run("tocp2k", str(TEST_DIR.joinpath("inputs/test01.json")))

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
