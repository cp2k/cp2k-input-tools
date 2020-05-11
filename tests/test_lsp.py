from pathlib import Path
from time import sleep
import io
import sys

import pytest

from . import TEST_DIR

try:
    from pygls.features import INITIALIZE, TEXT_DOCUMENT_DID_OPEN
    from pygls.types import DidOpenTextDocumentParams, TextDocumentItem, InitializeParams
except ImportError:
    pytest.skip("pygls unavailable", allow_module_level=True)

if hasattr(sys, "pypy_version_info"):
    # the LSP implementation seems to behave completely different on pypy
    pytest.skip("pypy is currently not supported", allow_module_level=True)


CALL_TIMEOUT = 2


def _initialize_server(server):
    server.lsp.bf_initialize(InitializeParams(process_id=1234, root_uri=Path(__file__).parent.as_uri(), capabilities=None))


def test_initialize(client_server):
    """Simple initialization of the LSP server and single request"""
    client, server = client_server
    root_uri = Path(__file__).parent.as_uri()
    process_id = 1234

    response = client.lsp.send_request(INITIALIZE, {"processId": process_id, "rootUri": root_uri, "capabilities": None}).result(
        timeout=CALL_TIMEOUT
    )

    assert server.process_id == process_id
    assert server.workspace.root_uri == root_uri
    assert hasattr(response, "capabilities")


def test_text_document_did_open(client_server):
    """Check that the server opens an input file"""
    client, server = client_server
    _initialize_server(server)

    testpath = TEST_DIR / "inputs" / "test01.inp"
    with testpath.open("r") as fhandle:
        content = fhandle.read()

    client.lsp.notify(TEXT_DOCUMENT_DID_OPEN, DidOpenTextDocumentParams(TextDocumentItem(str(testpath), "cp2k", 1, content)))
    sleep(1)

    assert len(server.lsp.workspace.documents) == 1
    assert "Validating CP2K input..." in client.msg


def test_text_document_did_open_error(client_server):
    """Check that the server opens an input file with a syntax error"""
    client, server = client_server
    _initialize_server(server)

    testpath = TEST_DIR / "inputs" / "unterminated_string.inp"
    with testpath.open("r") as fhandle:
        content = fhandle.read()

    client.lsp.notify(TEXT_DOCUMENT_DID_OPEN, DidOpenTextDocumentParams(TextDocumentItem(str(testpath), "cp2k", 1, content)))
    sleep(1)

    assert len(server.lsp.workspace.documents) == 1
    assert "Validating CP2K input..." in client.msg
    assert "Syntax error: unterminated string detected" in client.diagnostics[0].message


@pytest.mark.script_launch_mode("subprocess")
def test_cli(script_runner):
    """Simply check whether the server reacts to an exist notification"""
    stdin = io.StringIO('Content-Length: 45\r\n\r\n{"method":"exit","jsonrpc":"2.0","params":{}}')

    ret = script_runner.run("cp2k-language-server", stdin=stdin)

    assert ret.stderr == ""
    assert ret.success
