import pathlib

import click

from cp2k_input_tools.basissets import BasisSetData
from cp2k_input_tools.basissets.crystal import BasisSetData as BasisSetDataCrystal
from cp2k_input_tools.pseudopotentials import PseudopotentialData

from . import smart_open


@click.command()
@click.argument(
    "iformat",
    metavar="<format>",
    type=click.Choice(("basis", "basisset", "basissets", "pseudo", "pseudos", "pseudopotential", "pseudopotentials", "potentials")),
)
@click.argument(
    "fpath", metavar="[<file to lint>]", type=click.Path(dir_okay=False, allow_dash=True, path_type=pathlib.Path), default="-"
)
@click.option("-i", "--inplace", is_flag=True, help="replace the original file with the linted/prettified one")
@click.option(
    "--input-basis-format",
    type=click.Choice(("cp2k", "crystal")),
    default="cp2k",
    help="format in which the input basis set is specified",
)
@click.option(
    "--output-basis-format",
    type=click.Choice(("cp2k", "crystal", "nwchem-ecp")),
    default="cp2k",
    help="format in which the basis set should be printed",
)
@click.option(
    "--identifier",
    type=str,
    default="",
    help="override the identifier on output",
)
def cp2k_datafile_lint(iformat, fpath, inplace, input_basis_format, output_basis_format, identifier):
    """Linter/Pretty-printer for other CP2K data formats

    Supported formats are:

        basissets (aliases: basis, basisset, basissets)
        potentials (aliases: pseudo, pseudos, pseudopotential, pseudopotentials, potentials)
    """

    if iformat in ("basis", "basisset", "basissets"):
        if input_basis_format == "cp2k":
            datafile = BasisSetData
            if output_basis_format != "cp2k":
                raise click.UsageError("Basis set format conversion to CRYSTAL is not yet supported")
        elif input_basis_format == "crystal":
            datafile = BasisSetDataCrystal
        else:
            raise click.UsageError("Invalid input basis format")

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
                    if (
                        not has_preceeding_comments and output_basis_format == "cp2k"
                    ):  # if there is no comment before the next entry, add at least an empty one
                        print("#", file=fouthandle)

                    if output_basis_format == "cp2k":
                        if input_basis_format == "crystal":
                            line_iter = entry.cp2k_format_line_iter(identifier)
                        else:
                            line_iter = entry.cp2k_format_line_iter()
                    elif output_basis_format == "crystal":
                        line_iter = entry.crystal_format_line_iter()
                    elif output_basis_format == "nwchem-ecp":
                        line_iter = entry.nwchem_ecp_format_line_iter()
                    else:
                        continue

                    for line in line_iter:
                        print(line, file=fouthandle)

                    has_preceeding_comments = False

    if inplace:
        swpfile.replace(fpath)
