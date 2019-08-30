#!/usr/bin/env python3
# coding: utf-8

import argparse
import sys
import re
import pathlib

from .parser import CP2KInputParser
from .parser_errors import PreprocessorError
from .tokenizer import TokenizerError
from .generator import CP2KInputGenerator


DEFAULT_CP2K_INPUT_XML = pathlib.Path(__file__).resolve().parent.joinpath("cp2k_input.xml")


def cp2klint():
    parser = argparse.ArgumentParser(description="Check the passed CP2K file for syntax errors")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    args = parser.parse_args()

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    with open(args.file, "r") as fhandle:
        try:
            cp2k_parser.parse(fhandle)
        except (PreprocessorError, TokenizerError) as exc:
            ctx = exc.args[1]
            line = ctx["line"].rstrip()

            print(f"Syntax error: {exc.args[0]} in {ctx['filename']}:")
            print(f"  {ctx['linenr']:>4}: {line}")

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
                    print(f"{str():>6}  {underline}^{str():~>{count}}")
                else:
                    print(f"{str():>6}  {underline}{str():~>{-count}}^")

            if ctx["ref_line"] is not None:
                print("previous definition line:")
                print(f"  {str():>4}: {ctx['ref_line'].rstrip()}")

            sys.exit(1)

    print("All done! Happy calculating!")


def _key_trafo(string):
    if len(string) <= 3:
        return string.upper()

    return string.lower()


def fromcp2k():
    parser = argparse.ArgumentParser(description="Convert CP2K input to JSON (default) or YAML")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    parser.add_argument("-y", "--yaml", action="store_true")
    args = parser.parse_args()

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, key_trafo=_key_trafo)

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
