import io
import sys
from time import sleep

import pytest

from . import TEST_DIR

if hasattr(sys, "pypy_version_info"):
    # the LSP implementation seems to behave completely different on pypy
    pytest.skip("pypy is currently not supported", allow_module_level=True)


pygls = pytest.importorskip("pygls")


from pygls.lsp.methods import TEXT_DOCUMENT_DID_OPEN  # noqa: E402
from pygls.lsp.types import DidOpenTextDocumentParams, TextDocumentItem  # noqa: E402

CALL_TIMEOUT = 5


def test_text_document_did_open(client_server):
    """Check that the server opens an input file"""
    client, server = client_server

    testpath = TEST_DIR / "inputs" / "test01.inp"
    with testpath.open("r") as fhandle:
        content = fhandle.read()

    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(testpath), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    assert len(server.lsp.workspace.documents) == 1
    assert "Validating CP2K input..." in client.msgs[0].message
    assert client.diagnostics is not None and not client.diagnostics, "Diagnostics is not empty as expected"


def test_text_document_did_open_error(client_server):
    """Check that the server opens an input file with a syntax error"""
    client, server = client_server

    testpath = TEST_DIR / "inputs" / "unterminated_string.inp"
    with testpath.open("r") as fhandle:
        content = fhandle.read()

    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(testpath), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    assert (
        len(server.lsp.workspace.documents) == 1
    ), f"More than one document open: {', '.join(server.lsp.workspace.documents.keys())}"
    assert "Validating CP2K input..." in client.msgs[0].message
    assert "Syntax error: unterminated string detected" in client.diagnostics[0].message


@pytest.mark.script_launch_mode("subprocess")
def test_cli(script_runner):
    """Simply check whether the server reacts to an exist notification"""
    stdin = io.StringIO('Content-Length: 45\r\n\r\n{"method":"exit","jsonrpc":"2.0","params":{}}')

    ret = script_runner.run("cp2k-language-server", stdin=stdin)

    assert ret.stderr == ""
    assert ret.success
