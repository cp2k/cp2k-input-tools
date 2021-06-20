
import json
import sys
from typing import Mapping, MutableSequence
from enum import Enum
import functools
import click

from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserAiiDA, CP2KInputParserSimplified


from . import fhandle_argument, canonical_option, base_dir_option, var_values_option


def _key_trafo(string):
    if len(string) <= 3:
        return string.upper()
    return string.lower()


class Trafos(Enum):
    # see https://stackoverflow.com/a/40486992 need to wrap functions in function objects
    auto = functools.partial(_key_trafo)
    lower = functools.partial(str.lower)
    upper = functools.partial(str.upper)


@click.command()
@fhandle_argument
@click.option("oformat", "-f", "--format", type=click.Choice(("json", "yaml", "aiida-cp2k-calc")), default="json", help="output format")
@canonical_option
@base_dir_option
@click.option("-t", "--trafo",
        type=click.Choice([t.name for t in Trafos]), callback=lambda c, p, v: getattr(Trafos, v) if v else None, default="auto",  # see https://github.com/pallets/click/issues/605#issuecomment-847361079
        help="transformation applied to key and section names",
    )
@var_values_option
def fromcp2k(fhandle, oformat, canonical, base_dir, trafo, var_values):
    """Convert CP2K input to JSON (default), YAML or an aiida-cp2k run script template"""

    if oformat == "aiida-cp2k-calc":
        if canonical:
            print("The --canonical argument is ignored when generating an aiida-cp2k run script template", file=sys.stderr)
        if trafo != Trafos.auto:
            print(
                "Any key transformation function other than 'auto' is ignored when generating an aiida-cp2k run script template",
                file=sys.stderr,
            )
        cp2k_parser = CP2KInputParserAiiDA(base_dir=base_dir)
    elif canonical:
        cp2k_parser = CP2KInputParser(base_dir=base_dir, key_trafo=trafo.value)
    else:
        cp2k_parser = CP2KInputParserSimplified(base_dir=base_dir, key_trafo=trafo.value)

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
