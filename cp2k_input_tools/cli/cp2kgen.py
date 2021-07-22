import itertools
import pathlib
import re
import sys
from copy import deepcopy

import click

from cp2k_input_tools.generator import CP2KInputGenerator
from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserSimplified

from .. import __version__
from . import base_dir_option, canonical_option, fhandle_argument, var_values_option


@click.command()
@fhandle_argument
@click.argument("expressions", metavar="<expression>", type=str, nargs=-1)
@base_dir_option
@canonical_option
@var_values_option
def cp2kgen(fhandle, expressions, base_dir, canonical, var_values):
    """
    Generates variations of the given CP2K input file

    Examples for generator expressions:

    "force_eval/dft/mgrid/cutoff=[300,400,500,800]", "force_eval/subsys/cell/a/0=10.0"
    """

    if canonical:
        cp2k_parser = CP2KInputParser(base_dir=base_dir, key_trafo=str.lower)
    else:
        cp2k_parser = CP2KInputParserSimplified(base_dir=base_dir, key_trafo=str.lower)

    tree = cp2k_parser.parse(fhandle, dict(var_values))

    # list of substitutions/transformations to apply
    substitutions = []

    for expression in expressions:
        try:
            kpath, value = expression.split("=", maxsplit=1)
        except ValueError:
            raise ValueError("an expression must be of the form 'path/to/key=...'") from None

        if re.match(r"^\[.+\]$", value):
            values = [v.strip() for v in value.strip("[]").split(",")]
            substitutions += [(kpath, values)]
        else:
            substitutions += [(kpath, [value])]

    fpath = pathlib.Path(fhandle.name)
    onameprefix = fpath.stem
    onamesuffix = fpath.suffix

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

        with opath.open("w") as fouthandle:
            fouthandle.write(f"! Generated with the CP2K input tool cp2kgen v{__version__}\n! ")
            fouthandle.write(" \\\n!   ".join(f"'{arg}'" for arg in sys.argv))
            fouthandle.write("\n")
            cp2k_generator = CP2KInputGenerator()
            for line in cp2k_generator.line_iter(curr_tree):
                fouthandle.write(f"{line}\n")
