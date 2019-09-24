import argparse
import sys
import re

from . import DEFAULT_CP2K_INPUT_XML
from .parser import CP2KInputParser, CP2KInputParserSimplified
from .parser_errors import PreprocessorError, InvalidParameterError
from .tokenizer import TokenizerError
from .generator import CP2KInputGenerator


def cp2klint():
    parser = argparse.ArgumentParser(description="Check the passed CP2K file for syntax errors")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    args = parser.parse_args()

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(args.file, "r") as fhandle:
        try:
            cp2k_parser.parse(fhandle)
        except (PreprocessorError, TokenizerError, InvalidParameterError) as exc:
            ctx = exc.args[1]
            line = ctx["line"].rstrip()

            print(f"Syntax error: {exc.args[0]}, in {ctx['filename']}:")

            if exc.__cause__:
                print(f"              {exc.__cause__}")

            print(f"line {ctx['linenr']:>4}: {line}")

            if ctx["colnr"] is not None:
                count = 0  # number of underline chars after (positiv) or before (negative) the marker if ref_colnr given
                nchars = ctx["colnr"]  # relevant line length

                if ctx["ref_colnr"] is not None:
                    count = ctx["ref_colnr"] - ctx["colnr"]
                    nchars = min(ctx["ref_colnr"], ctx["colnr"])  # correct if ref comes before

                # replace all non-ws chars with spaces:
                # - assuming a monospace font
                # - preserving other whitespace we don't know the width
                underline = re.sub(r"\S", " ", ctx["line"][:nchars])

                if count >= 0:
                    print(f"{str():>9}  {underline}^{str():~>{count}}")
                else:
                    print(f"{str():>9}  {underline}{str():~>{-count}}^")

            if ctx["ref_line"] is not None:
                print("previous definition:")
                print(f"line {str():>4}: {ctx['ref_line'].rstrip()}")

            sys.exit(1)

    print("All done! Happy calculating!")


def _key_trafo(string):
    if len(string) <= 3:
        return string.upper()

    return string.lower()


def _fromcp2k_trafo_arg(value):
    if "auto" in value:
        return _key_trafo
    elif value == "lower":
        return str.lower
    elif value == "upper":
        return str.upper
    else:
        raise argparse.ArgumentTypeError(f"unknown option '{value}'")


def fromcp2k():
    parser = argparse.ArgumentParser(description="Convert CP2K input to JSON (default) or YAML")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    parser.add_argument("-y", "--yaml", action="store_true", help="output yaml instead of json")
    parser.add_argument("-c", "--canonical", action="store_true", help="use the canonical output format")
    parser.add_argument("-b", "--base-dir", type=str, default=".", help="search path used for relative @include's")
    parser.add_argument(
        "-t",
        "--trafo",
        type=_fromcp2k_trafo_arg,
        default="auto",
        help="transformation applied to key and section names (auto, upper, lower)",
    )
    args = parser.parse_args()

    if args.canonical:
        cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=args.trafo)
    else:
        cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=args.trafo)

    with open(args.file, "r") as fhandle:
        tree = cp2k_parser.parse(fhandle)

    if args.yaml:
        import yaml

        print(yaml.dump(tree, indent=2))
    else:
        import json

        print(json.dumps(tree, indent=2))


def tocp2k():
    parser = argparse.ArgumentParser(description="Convert JSON or YAML input to CP2K")
    parser.add_argument("file", metavar="<file>", type=str, help="JSON or YAML input file")
    parser.add_argument("-y", "--yaml", action="store_true")
    args = parser.parse_args()

    with open(args.file, "r") as fhandle:
        if args.yaml:
            import yaml
            from yaml import SafeLoader

            tree = yaml.load(fhandle, Loader=SafeLoader)
        else:
            import json

            tree = json.load(fhandle)

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)

    for line in cp2k_generator.line_iter(tree):
        print(line)
