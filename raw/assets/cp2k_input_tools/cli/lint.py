import re
import sys

import click

from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.parser_errors import ParserError
from cp2k_input_tools.tokenizer import TokenizerError

from . import base_dir_option, fhandle_argument, var_values_option


@click.command()
@fhandle_argument
@var_values_option
@base_dir_option
def cp2klint(fhandle, var_values, base_dir):
    """Check the passed CP2K file for syntax errors"""

    cp2k_parser = CP2KInputParser(base_dir=base_dir)

    try:
        cp2k_parser.parse(fhandle, dict(var_values))
    except (TokenizerError, ParserError) as exc:
        ctx = exc.args[1]
        line = ctx.line.rstrip()

        print(f"Syntax error: {exc.args[0]}, in {ctx.filename}:")

        if exc.__cause__:
            print(f"              {exc.__cause__}")

        print(f"line {ctx.linenr:>4}: {line}")

        if ctx.colnr is not None:
            count = 0  # number of underline chars after (positiv) or before (negative) the marker if ref_colnr given
            nchars = ctx.colnr  # relevant line length

            if ctx.ref_colnr is not None:
                count = ctx.ref_colnr - ctx.colnr
                nchars = min(ctx.ref_colnr, ctx.colnr)  # correct if ref comes before

            if ctx.colnrs:
                # shift by the number of left-stripped ws
                # ctx.colnrs contains the left shift for each possibly continued line
                nchars += ctx.colnrs[0]  # assume no line-continuation for now

            # replace all non-ws chars with spaces:
            # - assuming a monospace font
            # - preserving other whitespace we don't know the width
            underline = re.sub(r"\S", " ", ctx.line[:nchars])

            if count >= 0:
                print(f"{str():>9}  {underline}^{str():~>{count}}")
            else:
                print(f"{str():>9}  {underline}{str():~>{-count}}^")

        if ctx.ref_line is not None:
            print("previous definition:")
            print(f"line {str():>4}: {ctx.ref_line.rstrip()}")

        sys.exit(1)

    print("All done! Happy calculating!")
