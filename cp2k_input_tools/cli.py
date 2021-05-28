import argparse
import sys
import re
import itertools
from copy import deepcopy
import pathlib
import logging
import contextlib
from typing import MutableSequence, Mapping

from . import DEFAULT_CP2K_INPUT_XML
from .parser import CP2KInputParser, CP2KInputParserSimplified, CP2KInputParserAiiDA
from .parser_errors import ParserError
from .tokenizer import TokenizerError
from .generator import CP2KInputGenerator


@contextlib.contextmanager
def smart_open(filename=None):
    """A context manager to automatically read from stdin, based on https://stackoverflow.com/a/17603000/1400465"""
    if filename and filename != "-":
        fhandle = open(filename, "r")
    else:
        fhandle = sys.stdin

    try:
        yield fhandle
    finally:
        if fhandle is not sys.stdin:
            fhandle.close()


def _argparse_str2kv(arg):
    try:
        key, value = arg.split("=", maxsplit=1)
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid value '{arg}', must be of the form 'key=value'")

    return key, value


def cp2klint():
    parser = argparse.ArgumentParser(description="Check the passed CP2K file for syntax errors")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    parser.add_argument(
        "-E",
        "--set",
        dest="var_values",
        metavar="key=value",
        default=[],
        type=_argparse_str2kv,
        action="append",
        help="preset the value for a CP2K preprocessor variable",
    )
    args = parser.parse_args()

    cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)
    with open(args.file, "r") as fhandle:
        try:
            cp2k_parser.parse(fhandle, dict(args.var_values))
        except (TokenizerError, ParserError) as exc:
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

                if ctx["colnrs"] is not None:
                    # shift by the number of left-stripped ws
                    # ctx["colnrs"] contains the left shift for each possibly continued line
                    nchars += ctx["colnrs"][0]  # assume no line-continuation for now

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
    parser = argparse.ArgumentParser(description="Convert CP2K input to JSON (default), YAML or an aiida-cp2k run script template")
    parser.add_argument("file", metavar="[<file>]", type=str, help="CP2K input file", nargs="?")
    parser.add_argument(
        "-f", "--format", type=str, default="json", choices=("json", "yaml", "aiida-cp2k-calc"), help="output format"
    )
    parser.add_argument("-c", "--canonical", action="store_true", help="use the canonical output format")
    parser.add_argument("-b", "--base-dir", type=str, default=".", help="search path used for relative @include's")
    parser.add_argument(
        "-t",
        "--trafo",
        type=_fromcp2k_trafo_arg,
        default="auto",
        help="transformation applied to key and section names (auto, upper, lower)",
    )
    parser.add_argument(
        "-E",
        "--set",
        dest="var_values",
        metavar="key=value",
        default=[],
        type=_argparse_str2kv,
        action="append",
        help="preset the value for a CP2K preprocessor variable",
    )
    args = parser.parse_args()

    if args.format == "aiida-cp2k-calc":
        if args.canonical:
            print("The --canonical argument is ignored when generating an aiida-cp2k run script template", file=sys.stderr)
        if args.trafo != _key_trafo:
            print(
                "Any key transformation function other than 'auto' is ignored when generating an aiida-cp2k run script template",
                file=sys.stderr,
            )
        cp2k_parser = CP2KInputParserAiiDA(xmlspec=DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir)
    elif args.canonical:
        cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=args.trafo)
    else:
        cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=args.trafo)

    with smart_open(args.file) as fhandle:
        tree = cp2k_parser.parse(fhandle, dict(args.var_values))

    if args.format == "json":
        import json

        print(json.dumps(tree, indent=2))
    elif args.format == "yaml":
        from ruamel.yaml import YAML

        yaml = YAML()
        yaml.dump(tree, sys.stdout)
    elif args.format == "aiida-cp2k-calc":
        from jinja2 import Environment, PackageLoader

        env = Environment(loader=PackageLoader("cp2k_input_tools", "templates"))
        env.globals.update({"isinstance": isinstance, "Mapping": Mapping, "MutableSequence": MutableSequence})
        env.filters["quoted"] = lambda item: f'"{item}"' if isinstance(item, str) else item
        template = env.get_template("aiida_cp2k_calc.py.j2")
        print(template.render(tree=tree))


def tocp2k():
    parser = argparse.ArgumentParser(description="Convert JSON or YAML input to CP2K")
    parser.add_argument("file", metavar="[<file>]", type=str, help="JSON or YAML input file", nargs="?")
    parser.add_argument("-y", "--yaml", action="store_true")
    args = parser.parse_args()

    with smart_open(args.file) as fhandle:
        if args.yaml:
            from ruamel.yaml import YAML

            yaml = YAML()

            tree = yaml.load(fhandle)
        else:
            import json

            tree = json.load(fhandle)

    cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)

    for line in cp2k_generator.line_iter(tree):
        print(line)


def cp2kgen():
    parser = argparse.ArgumentParser(description="Generates variations of the given CP2K input file")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    parser.add_argument("expressions", metavar="<expression>", type=str, nargs="+", help="Generator expressions")
    parser.add_argument("-b", "--base-dir", type=str, default=".", help="search path used for relative @include's")
    parser.add_argument("-c", "--canonical", action="store_true", help="use the canonical output format")
    parser.add_argument(
        "-E",
        "--set",
        dest="var_values",
        metavar="key=value",
        default=[],
        type=_argparse_str2kv,
        action="append",
        help="preset the value for a CP2K preprocessor variable",
    )
    # parser.add_argument("-o", "--output-pattern", metavar="<output-file-pattern>",
    #                     type=str, help="Pattern to use for generated output files")
    args = parser.parse_args()

    if args.canonical:
        cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=str.lower)
    else:
        cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=str.lower)

    with open(args.file, "r") as fhandle:
        tree = cp2k_parser.parse(fhandle, dict(args.var_values))

    # list of substitutions/transformations to apply
    substitutions = []

    for expression in args.expressions:
        try:
            kpath, value = expression.split("=", maxsplit=1)
        except ValueError:
            raise ValueError("an expression must be of the form 'path/to/key=...'") from None

        if re.match(r"^\[.+\]$", value):
            values = [v.strip() for v in value.strip("[]").split(",")]
            substitutions += [(kpath, values)]
        else:
            substitutions += [(kpath, [value])]

    ifile = pathlib.Path(args.file)
    onameprefix = ifile.stem
    onamesuffix = ifile.suffix

    # first generate a list of list of tuples [ [(key/a, 10), (key/a, 20), ...], [(key/b, 100), ...], ...]
    for substtuple in itertools.product(*[[(k, v) for v in values] for k, values in substitutions]):
        # ... then iterate over the cartesian product
        curr_tree = deepcopy(tree)  # create a full copy of the initial tree
        onameparts = []  # output name parts
        for kpath, value in substtuple:
            ref = curr_tree

            sections = kpath.split("/")
            for section in sections[:-1]:
                if isinstance(ref, list):
                    section = int(section)  # if we encounter a list, convert the respective path element
                ref = ref[section]  # exploit Python using references into dicts/lists

            attr = sections[-1]
            if isinstance(ref, (list, tuple)):
                attr = int(attr)

            if isinstance(ref, tuple):
                # we only get tuples for keywords which can take multiple words, hence this should be safe
                lref = list(ref)
                lref[attr] = value
                ref = tuple(lref)
            else:
                ref[attr] = value

            # take only the attribute name
            onameparts += [f"{attr}_{value}"]

        opath = pathlib.Path(f"{onameprefix}-{'-'.join(onameparts)}{onamesuffix}")
        print(f"Writing '{opath}'...")

        with opath.open("w") as fhandle:
            cp2k_generator = CP2KInputGenerator(DEFAULT_CP2K_INPUT_XML)
            for line in cp2k_generator.line_iter(curr_tree):
                fhandle.write(f"{line}\n")


def cp2kget():
    parser = argparse.ArgumentParser(description="Get values by path from a CP2K input file")
    parser.add_argument("file", metavar="<file>", type=str, help="CP2K input file")
    parser.add_argument("paths", metavar="<path>", type=str, nargs="+", help="Path, ex.: 'force_eval/dft/mgrid/cutoff'")
    parser.add_argument("-b", "--base-dir", type=str, default=".", help="search path used for relative @include's")
    parser.add_argument("-c", "--canonical", action="store_true", help="use the canonical output format")
    parser.add_argument(
        "-E",
        "--set",
        dest="var_values",
        metavar="key=value",
        default=[],
        type=_argparse_str2kv,
        action="append",
        help="preset the value for a CP2K preprocessor variable",
    )
    args = parser.parse_args()

    if args.canonical:
        cp2k_parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=str.lower)
    else:
        cp2k_parser = CP2KInputParserSimplified(DEFAULT_CP2K_INPUT_XML, base_dir=args.base_dir, key_trafo=str.lower)

    with open(args.file, "r") as fhandle:
        tree = cp2k_parser.parse(fhandle, dict(args.var_values))

    def _(val):
        if isinstance(val, list):
            return ", ".join(str(v) for v in val)

        return val

    for path in args.paths:
        sections = path.split("/")
        ref = tree
        for section in sections:
            if isinstance(ref, (list, tuple)):
                section = int(section)  # if we encounter a list, convert the respective path element
            ref = ref[section]  # exploit Python using references into dicts/lists

        print(f"{path}: {_(ref)}")


def cp2k_language_server():
    parser = argparse.ArgumentParser(description="Language Server Protocol (LSP) implementation for CP2K input files")
    parser.add_argument("--tcp", action="store_true", help="use TCP server instead of stdio")
    parser.add_argument("--host", default="127.0.0.1", help="bind to this address")
    parser.add_argument("--port", type=int, default=2087, help="bind to this port")
    parser.add_argument("--debug", help="write a cp2kls.log file", action="store_true")
    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(filename="cp2kls.log", level=logging.DEBUG, filemode="w")

    try:
        import pygls  # noqa: F401
    except ImportError:
        print(
            f"""Could not import the pygls package. You have to install the cp2k-input-tools with the 'lsp' extra:

    pip install cp2k-input-tools[lsp]
        """
        )
        sys.exit(1)

    from .ls import cp2k_server

    if args.tcp:
        cp2k_server.start_tcp(args.host, args.port)
    else:
        cp2k_server.start_io()


def cp2k_datafile_lint():
    parser = argparse.ArgumentParser(description="Linter/Pretty-printer for other CP2K data formats")
    parser.add_argument("format", metavar="<format>", type=str, choices=("basisset", "pseudo"), help="expected format")
    parser.add_argument("file", metavar="[<file>]", type=str, nargs="?", help="the file to lint (otherwise stdin is used)")
    args = parser.parse_args()

    from .basissets import BasisSetData
    from .pseudopotentials import PseudopotentialData

    class_map = {"basisset": BasisSetData, "pseudo": PseudopotentialData}
    has_preceeding_comments = False
    with smart_open(args.file) as fhandle:
        for entry in class_map[args.format].datafile_iter(fhandle, keep_going=False, emit_comments=True):
            if isinstance(entry, str):
                print(entry)
                has_preceeding_comments = True
            else:
                if not has_preceeding_comments:
                    print("#")
                else:
                    has_preceeding_comments = False
                for line in entry.cp2k_format_line_iter():
                    print(line)
