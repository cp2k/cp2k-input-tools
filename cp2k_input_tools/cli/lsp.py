import logging
import sys

import click

try:
    import pygls  # noqa: F401

    from cp2k_input_tools.ls import cp2k_server
except ImportError:
    print(
        """Could not import the pygls package. You have to install the cp2k-input-tools with the 'lsp' extra:

pip install cp2k-input-tools[lsp]
    """
    )
    sys.exit(1)


@click.command()
@click.option("--tcp", is_flag=True, default=False, help="use TCP server instead of stdio")
@click.option("--host", default="127.0.0.1", type=str, help="bind to this address")
@click.option("--port", type=int, default=2087, help="bind to this port")
@click.option("--debug", is_flag=True, default=False, help="write a cp2kls.log file")
def cp2k_language_server(tcp, host, port, debug):
    """Language Server Protocol (LSP) implementation for CP2K input files"""
    if debug:
        logging.basicConfig(filename="cp2kls.log", level=logging.DEBUG, filemode="w")

    if tcp:
        cp2k_server.start_tcp(host, port)
    else:
        cp2k_server.start_io()
