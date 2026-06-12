"""CLI command for CP2K input diagnostics check."""

import sys

import click

from ..diagnostics_api import check_format
from . import base_dir_option, var_values_option, xml_option


@click.command()
@click.argument("file_path", metavar="<file>", type=click.Path(exists=True))
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["json", "text"], case_sensitive=False),
    default="text",
    help="Output format (json or text)",
)
@click.option("--fail-on-error", is_flag=True, help="Exit with non-zero status if errors are found")
@base_dir_option
@var_values_option
@xml_option
def cp2k_check(file_path, output_format, fail_on_error, base_dir, var_values, xml):
    """Check a CP2K input file for diagnostics.

    This command validates a CP2K input file and outputs diagnostics
    from the parser, linter, type-checker, and semantic validator.

    Examples:

        cp2k-lsp check input.inp

        cp2k-lsp check --format=json input.inp

        cp2k-lsp check --fail-on-error input.inp
    """
    try:
        output = check_format(file_path, format=output_format.lower(), base_dir=str(base_dir))
        click.echo(output)

        # Parse output to check for errors if --fail-on-error is set
        if fail_on_error and output_format == "json":
            import json

            data = json.loads(output)
            if data.get("summary", {}).get("errors", 0) > 0:
                sys.exit(1)

    except Exception as e:
        click.echo(f"Error checking file: {e}", err=True)
        sys.exit(1)


if __name__ == "__main__":
    cp2k_check()
