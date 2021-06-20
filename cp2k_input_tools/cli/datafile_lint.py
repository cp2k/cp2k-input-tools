
import pathlib

import click

from cp2k_input_tools.basissets import BasisSetData
from cp2k_input_tools.pseudopotentials import PseudopotentialData

from . import smart_open


@click.command()
@click.argument("iformat", metavar="<format>", type=click.Choice(("basis", "basisset", "basissets", "pseudo", "pseudos", "pseudopotential", "pseudopotentials", "potentials")))
@click.argument("fpath", metavar="[<file to lint>]", type=click.Path(dir_okay=False, allow_dash=True, path_type=pathlib.Path), default="-")
@click.option("-i", "--inplace", is_flag=True, help="replace the original file with the linted/prettified one")
def cp2k_datafile_lint(iformat, fpath, inplace):
    """Linter/Pretty-printer for other CP2K data formats

    Supported formats are:

        basissets (aliases: basis, basisset, basissets)
        potentials (aliases: pseudo, pseudos, pseudopotential, pseudopotentials, potentials)
    """

    if iformat in ("basis", "basisset", "basissets"):
        datafile = BasisSetData
    elif iformat in ("pseudo", "pseudos", "pseudopotential", "pseudopotentials", "potentials"):
        datafile = PseudopotentialData

    swpfile = None

    if inplace:
        assert str(fpath) != "-", "Replacing file content does not work when reading from stdin"
        swpfile = fpath.parent / f".{fpath.name}.swp"

    has_preceeding_comments = False
    with smart_open(fpath, "r") as fhandle:
        with smart_open(swpfile, "x") as fouthandle:
            for entry in datafile.datafile_iter(fhandle, keep_going=False, emit_comments=True):
                if isinstance(entry, str):
                    print(entry, file=fouthandle)
                    has_preceeding_comments = True
                else:
                    if not has_preceeding_comments:  # if there is no comment before the next entry, add at least an empty one
                        print("#", file=fouthandle)

                    for line in entry.cp2k_format_line_iter():
                        print(line, file=fouthandle)

                    has_preceeding_comments = False

    if inplace:
        swpfile.replace(fpath)
