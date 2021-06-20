
import json

import click

from cp2k_input_tools.generator import CP2KInputGenerator

from . import fhandle_argument, yaml_option

@click.command()
@fhandle_argument
@yaml_option
def tocp2k(fhandle, yaml):
    """
    Generate a CP2K input file based on JSON (or YAML) file
    """

    if yaml:
        from ruamel.yaml import YAML

        yaml = YAML()

        tree = yaml.load(fhandle)
    else:

        tree = json.load(fhandle)

    cp2k_generator = CP2KInputGenerator()

    for line in cp2k_generator.line_iter(tree):
        print(line)
