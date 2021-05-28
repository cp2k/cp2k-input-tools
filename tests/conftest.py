import asyncio
import os
from threading import Thread
from time import sleep
from typing import List

import pytest

CALL_TIMEOUT = 3


@pytest.fixture
def parser():
    from cp2k_input_tools.cli import DEFAULT_CP2K_INPUT_XML
    from cp2k_input_tools.parser import CP2KInputParser

    return CP2KInputParser(DEFAULT_CP2K_INPUT_XML)


# the following is adapted from https://github.com/openlawlibrary/pygls/blob/master/tests/conftest.py


def pytest_sessionfinish(session, exitstatus):
    """whole test run finishes."""
    sleep(CALL_TIMEOUT)
    for cs in ClientServer._to_stop:
        cs._stop()


class ClientServer:
    _to_stop: List["ClientServer"] = []

    def __init__(self):
        from pygls.lsp.methods import (
            TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS,
            WINDOW_LOG_MESSAGE,
        )
        from pygls.lsp.types import LogMessageParams, PublishDiagnosticsParams
        from pygls.server import LanguageServer

        from cp2k_input_tools.ls import setup_cp2k_ls_server

        # Client to Server pipe
        csr, csw = os.pipe()
        # Server to client pipe
        scr, scw = os.pipe()

        # Setup Server
        self.server = LanguageServer()
        setup_cp2k_ls_server(self.server)
        self.server_thread = Thread(target=self.server.start_io, args=(os.fdopen(csr, "rb"), os.fdopen(scw, "wb")))
        self.server_thread.daemon = True

        # Setup client
        self.client = LanguageServer(asyncio.new_event_loop())

        # make sure our minimal client can store log messages and diagnostics
        self.client.msgs = []
        self.client.diagnostics = None

        @self.client.feature(WINDOW_LOG_MESSAGE)
        async def client_log_message(ls, params: LogMessageParams):
            ls.msgs.append(params)

        @self.client.feature(TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS)
        async def client_handle_diagnostics(ls, params: PublishDiagnosticsParams):
            ls.diagnostics = params.diagnostics

        self.client_thread = Thread(target=self.client.start_io, args=(os.fdopen(scr, "rb"), os.fdopen(csw, "wb")))
        self.client_thread.daemon = True

    def start(self):
        self.server_thread.start()
        self.server.thread_id = self.server_thread.ident

        self.client_thread.start()

        self.initialize()

    def stop(self):
        ClientServer._to_stop.append(self)

    def _stop(self):
        from pygls.lsp.methods import EXIT, SHUTDOWN

        shutdown_response = self.client.lsp.send_request(SHUTDOWN).result(timeout=CALL_TIMEOUT)
        assert shutdown_response is None
        # Exit the server
        self.client.lsp.notify(EXIT)
        self.server_thread.join()
        # Exit the client
        self.client._stop_event.set()
        try:
            self.client.loop._signal_handlers.clear()  # HACK ?
        except AttributeError:
            pass
        self.client_thread.join()

    def initialize(self):
        from pygls.lsp.methods import INITIALIZE
        from pygls.lsp.types import ClientCapabilities, InitializeParams

        response = self.client.lsp.send_request(
            INITIALIZE, InitializeParams(process_id=12345, root_uri="file://", capabilities=ClientCapabilities())
        ).result(timeout=CALL_TIMEOUT)

        assert "capabilities" in response

    def __iter__(self):
        yield self.client
        yield self.server


@pytest.fixture
def client_server():
    """A fixture to setup a client/server"""

    client_server = ClientServer()
    client_server.start()
    client, server = client_server

    yield client, server

    client_server.stop()
