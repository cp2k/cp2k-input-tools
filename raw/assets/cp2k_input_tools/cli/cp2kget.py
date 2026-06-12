import click

from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserSimplified

from . import base_dir_option, canonical_option, fhandle_argument, var_values_option


@click.command()
@fhandle_argument
@click.argument("paths", metavar="<path>", type=str, nargs=-1)
@base_dir_option
@var_values_option
@canonical_option
def cp2kget(fhandle, paths, base_dir, var_values, canonical):
    """Get values by path from a CP2K input file

    Examples for paths:

        force_eval/dft/mgrid/cutoff
    """

    if canonical:
        cp2k_parser = CP2KInputParser(base_dir=base_dir, key_trafo=str.lower)
    else:
        cp2k_parser = CP2KInputParserSimplified(base_dir=base_dir, key_trafo=str.lower)

    tree = cp2k_parser.parse(fhandle, dict(var_values))

    def _(val):
        if isinstance(val, list):
            return ", ".join(str(v) for v in val)

        return val

    for path in paths:
        sections = path.split("/")
        ref = tree
        for section in sections:
            if isinstance(ref, (list, tuple)):
                section = int(section)  # if we encounter a list, convert the respective path element
            ref = ref[section]  # exploit Python using references into dicts/lists

        print(f"{path}: {_(ref)}")
