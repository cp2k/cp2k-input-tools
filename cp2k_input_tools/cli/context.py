"""Context Packs CLI command for agent workflows (#57)."""

import json
from pathlib import Path

import click

from cp2k_input_tools.context_packs import get_context_pack


@click.command()
@click.argument("file_path", type=click.Path(exists=True))
@click.option("--line", "-l", type=int, default=0, help="0-based line number (default: 0)")
@click.option("--char", "-c", type=int, default=0, help="0-based character position (default: 0)")
@click.option("--pretty", "-p", is_flag=True, help="Pretty-print JSON output")
def context(file_path, line, char, pretty):
    """Get unified context pack for a CP2K input file.

    Returns cursor context, hover info, completions, and diagnostics as JSON.
    """
    # Read file content
    with open(file_path, "r") as f:
        text = f.read()

    # Convert file path to URI
    uri = Path(file_path).resolve().as_uri()

    # Get context pack
    pack = get_context_pack(text=text, line=line, char=char, uri=uri)

    # Output as JSON
    if pretty:
        click.echo(json.dumps(pack.to_dict(), indent=2))
    else:
        click.echo(pack.to_json())


if __name__ == "__main__":
    context()
