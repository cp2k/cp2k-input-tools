import asyncio
import concurrent
import os
import threading

import pytest

CALL_TIMEOUT = 3


@pytest.fixture
def parser():
    from cp2k_input_tools import DEFAULT_CP2K_INPUT_XML
    from cp2k_input_tools.parser import CP2KInputParser

    return CP2KInputParser(DEFAULT_CP2K_INPUT_XML)


# the following is adapted from https://github.com/openlawlibrary/pygls/blob/master/tests/conftest.py

RETRIES = 3
CMD_ASYNC = "cmd_async"
CMD_SYNC = "cmd_sync"
CMD_THREAD = "cmd_thread"


def retry_stalled_init_fix_hack():
    if "DISABLE_TIMEOUT" in os.environ:
        return lambda f: f

    def decorator(func):
        def newfn(*args, **kwargs):
            attempt = 0
            while attempt < RETRIES:
                try:
                    return func(*args, **kwargs)
                except concurrent.futures._base.TimeoutError:
                    print("\n\nRetrying timeouted test server init " "%d of %d\n" % (attempt, RETRIES))
                    attempt += 1
            return func(*args, **kwargs)

        return newfn

    return decorator


def setup_ls_features(server):
    # Commands
    @server.command(CMD_ASYNC)
    async def cmd_test3(ls, *args):  # pylint: disable=unused-variable
        return True, threading.get_ident()

    @server.thread()
    @server.command(CMD_THREAD)
    def cmd_test1(ls, *args):  # pylint: disable=unused-variable
        return True, threading.get_ident()

    @server.command(CMD_SYNC)
    def cmd_test2(ls, *args):  # pylint: disable=unused-variable
        return True, threading.get_ident()


class ClientServer:
    from pygls.server import LanguageServer

    def __init__(self, LS=LanguageServer):
        from lsprotocol.types import (
            TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS,
            WINDOW_LOG_MESSAGE,
            LogMessageParams,
            PublishDiagnosticsParams,
        )

        from cp2k_input_tools.ls import setup_cp2k_ls_server

        # Client to Server pipe
        csr, csw = os.pipe()
        # Server to client pipe
        scr, scw = os.pipe()

        # Setup Server
        self.server = LS("server", "v1")
        setup_cp2k_ls_server(self.server)

        self.server_thread = threading.Thread(
            name="Server Thread",
            target=self.server.start_io,
            args=(os.fdopen(csr, "rb"), os.fdopen(scw, "wb")),
        )
        self.server_thread.daemon = True

        # Setup client
        self.client = LS("client", "v1", asyncio.new_event_loop())

        # make sure our minimal client can store log messages and diagnostics
        self.client.msgs = []
        self.client.diagnostics = None

        @self.client.feature(WINDOW_LOG_MESSAGE)
        async def client_log_message(ls, params: LogMessageParams):
            ls.msgs.append(params)

        @self.client.feature(TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS)
        async def client_handle_diagnostics(ls, params: PublishDiagnosticsParams):
            ls.diagnostics = params.diagnostics

        self.client_thread = threading.Thread(
            name="Client Thread",
            target=self.client.start_io,
            args=(os.fdopen(scr, "rb"), os.fdopen(csw, "wb")),
        )
        self.client_thread.daemon = True

    @classmethod
    def decorate(cls):
        return pytest.mark.parametrize("client_server", [cls], indirect=True)

    def start(self):
        self.server_thread.start()
        self.server.thread_id = self.server_thread.ident
        self.client_thread.start()
        self.initialize()

    def stop(self):
        from lsprotocol.types import (
            EXIT,
            SHUTDOWN,
        )

        shutdown_response = self.client.lsp.send_request(SHUTDOWN).result()
        assert shutdown_response is None
        self.client.lsp.notify(EXIT)
        self.server_thread.join()
        self.client._stop_event.set()
        try:
            self.client.loop._signal_handlers.clear()  # HACK ?
        except AttributeError:
            pass
        self.client_thread.join()

    @retry_stalled_init_fix_hack()
    def initialize(self):
        from lsprotocol.types import (
            INITIALIZE,
            ClientCapabilities,
            InitializeParams,
        )

        timeout = None if "DISABLE_TIMEOUT" in os.environ else 1
        response = self.client.lsp.send_request(
            INITIALIZE,
            InitializeParams(process_id=12345, root_uri="file://", capabilities=ClientCapabilities()),
        ).result(timeout=timeout)
        assert response.capabilities is not None

    def __iter__(self):
        yield self.client
        yield self.server


@pytest.fixture(autouse=True)
def client_server(request):
    if hasattr(request, "param"):
        ConfiguredClientServer = request.param
        client_server = ConfiguredClientServer()
    else:
        client_server = ClientServer()
        setup_ls_features(client_server.server)

    client_server.start()
    client, server = client_server

    yield client, server

    client_server.stop()
