import functools
import json
import os
import sys
from enum import Enum
from typing import Mapping, MutableSequence

import click

from cp2k_input_tools.parser import (
    CP2KInputParser,
    CP2KInputParserAiiDA,
    CP2KInputParserSimplified,
)

from . import ENV_VAR_FOR_CP2K_INPUT_XML, base_dir_option, canonical_option, fhandle_argument, var_values_option, xml_option


def _key_trafo(string):
    if len(string) <= 3:
        return string.upper()
    return string.lower()


if sys.version_info >= (3, 13):
    from enum import member
else:

    def member(value):
        return value


class Trafos(Enum):
    # see https://stackoverflow.com/a/40486992 need to wrap functions in function objects
    auto = member(functools.partial(_key_trafo))
    lower = member(functools.partial(str.lower))
    upper = member(functools.partial(str.upper))


@click.command()
@fhandle_argument
@click.option(
    "oformat", "-f", "--format", type=click.Choice(("json", "yaml", "aiida-cp2k-calc")), default="json", help="output format"
)
@canonical_option
@base_dir_option
@click.option(
    "-t",
    "--trafo",
    type=click.Choice([t.name for t in Trafos]),
    callback=lambda c, p, v: getattr(Trafos, v) if v else None,
    default="auto",  # see https://github.com/pallets/click/issues/605#issuecomment-847361079
    help="transformation applied to key and section names",
)
@var_values_option
@xml_option
def fromcp2k(fhandle, oformat, canonical, base_dir, trafo, var_values, xml):
    """Convert CP2K input to JSON (default), YAML or an aiida-cp2k run script template"""

    if not xml:
        xml = os.environ.get(ENV_VAR_FOR_CP2K_INPUT_XML)

    if xml:
        print(f"    Using XML definition '{xml}'", file=sys.stderr)

    if oformat == "aiida-cp2k-calc":
        if canonical:
            print("The --canonical argument is ignored when generating an aiida-cp2k run script template", file=sys.stderr)
        if trafo != Trafos.auto:
            print(
                "Any key transformation function other than 'auto' is ignored when generating an aiida-cp2k run script template",
                file=sys.stderr,
            )
        cp2k_parser = CP2KInputParserAiiDA(xmlspec=xml, base_dir=base_dir)
    elif canonical:
        cp2k_parser = CP2KInputParser(xmlspec=xml, base_dir=base_dir, key_trafo=trafo.value)
    else:
        cp2k_parser = CP2KInputParserSimplified(xmlspec=xml, base_dir=base_dir, key_trafo=trafo.value)

    tree = cp2k_parser.parse(fhandle, dict(var_values))

    if oformat == "json":
        print(json.dumps(tree, indent=2))

    elif oformat == "yaml":
        from ruamel.yaml import YAML

        yaml = YAML()
        yaml.dump(tree, sys.stdout)

    elif oformat == "aiida-cp2k-calc":
        from jinja2 import Environment, PackageLoader

        env = Environment(loader=PackageLoader("cp2k_input_tools", "templates"))
        env.globals.update({"isinstance": isinstance, "Mapping": Mapping, "MutableSequence": MutableSequence})
        env.filters["quoted"] = lambda item: f'"{item}"' if isinstance(item, str) else item
        template = env.get_template("aiida_cp2k_calc.py.j2")
        print(template.render(tree=tree))
