import asyncio
import os
from threading import Thread

import pytest


CALL_TIMEOUT = 2


@pytest.fixture
def parser():
    from cp2k_input_tools.parser import CP2KInputParser
    from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML

    return CP2KInputParser(DEFAULT_CP2K_INPUT_XML)


# the following is adapted from https://github.com/openlawlibrary/pygls/blob/master/tests/conftest.py
@pytest.fixture
def client_server():
    """A fixture to setup a LSP client/server"""
    from cp2k_input_tools.ls import setup_ls
    from pygls.server import LanguageServer
    from pygls import features
    from pygls.types import LogMessageParams, PublishDiagnosticsParams

    # Client to Server pipe
    csr, csw = os.pipe()

    # Server to client pipe
    scr, scw = os.pipe()

    server = LanguageServer()
    setup_ls(server)

    server_thread = Thread(target=server.start_io, args=(os.fdopen(csr, "rb"), os.fdopen(scw, "wb")))

    server_thread.daemon = True
    server_thread.start()

    # Add thread id to the server (just for testing)
    server.thread_id = server_thread.ident

    # Setup client
    client = LanguageServer(asyncio.new_event_loop())

    # make sure our minimal client can store log messages and diagnostics

    @client.feature(features.WINDOW_LOG_MESSAGE)
    async def client_log_message(ls, params: LogMessageParams):
        ls.msg_type = params.type
        ls.msg = params.message

    @client.feature(features.TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS)
    async def client_handle_diagnostics(ls, params: PublishDiagnosticsParams):
        ls.diagnostics = params.diagnostics

    client_thread = Thread(target=client.start_io, args=(os.fdopen(scr, "rb"), os.fdopen(csw, "wb")))

    client_thread.daemon = True
    client_thread.start()

    yield client, server

    shutdown_response = client.lsp.send_request(features.SHUTDOWN).result(timeout=CALL_TIMEOUT)

    assert shutdown_response is None
    client.lsp.notify(features.EXIT)
