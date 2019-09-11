import io

from . import TEST_DIR
from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserSimplified
from cp2k_input_tools.generator import CP2KInputGenerator
from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML


def test_roundtrip_simplified():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)
    with open(TEST_DIR.joinpath("inputs/test01.inp"), "r") as fhandle:
        ref_tree = cp2k_parser.parse(fhandle)

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)
    fhandle = io.StringIO("\n".join(cp2k_generator.line_iter(ref_tree)))

    # reinitialize parser and generators to clear any internal state they might have
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)
    fhandle.name = "StringIO"  # add a filename (required by parser for context)

    assert cp2k_parser.parse(fhandle) == ref_tree


def test_roundtrip_canonical():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)
    with open(TEST_DIR.joinpath("inputs/test01.inp"), "r") as fhandle:
        ref_tree = cp2k_parser.parse(fhandle)

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)
    fhandle = io.StringIO("\n".join(cp2k_generator.line_iter(ref_tree)))

    # reinitialize parser and generators to clear any internal state they might have
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)
    fhandle.name = "StringIO"  # add a filename (required by parser for context)

    assert cp2k_parser.parse(fhandle) == ref_tree


def test_roundtrip2_simplified():
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)
    with open(TEST_DIR.joinpath("inputs/test04.inp"), "r") as fhandle:
        ref_tree = cp2k_parser.parse(fhandle)

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)
    fhandle = io.StringIO("\n".join(cp2k_generator.line_iter(ref_tree)))

    # reinitialize parser and generators to clear any internal state they might have
    cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML)
    fhandle.name = "StringIO"  # add a filename (required by parser for context)

    assert cp2k_parser.parse(fhandle) == ref_tree


def test_roundtrip2_canonical():
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)
    with open(TEST_DIR.joinpath("inputs/test04.inp"), "r") as fhandle:
        ref_tree = cp2k_parser.parse(fhandle)
        ref_tree["+force_eval"][0]["+subsys"]["+kind"].sort(key=lambda d: d["_"])

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)
    fhandle = io.StringIO("\n".join(cp2k_generator.line_iter(ref_tree)))

    # reinitialize parser and generators to clear any internal state they might have
    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)
    fhandle.name = "StringIO"  # add a filename (required by parser for context)

    tree = cp2k_parser.parse(fhandle)
    tree["+force_eval"][0]["+subsys"]["+kind"].sort(key=lambda d: d["_"])

    assert tree == ref_tree
