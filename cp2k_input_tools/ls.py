from typing import Union

from pygls.lsp.methods import (
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_CLOSE,
    TEXT_DOCUMENT_DID_OPEN,
)
from pygls.lsp.types import (
    Diagnostic,
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    DidOpenTextDocumentParams,
    Position,
    Range,
)
from pygls.server import LanguageServer

from .parser import CP2KInputParser
from .parser_errors import ParserError
from .tokenizer import TokenizerError


def _validate(ls, params: Union[DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams]):
    ls.show_message_log("Validating CP2K input...")

    diagnostics = []

    text_doc = ls.workspace.get_document(params.text_document.uri)
    parser = CP2KInputParser()

    with open(text_doc.path, "r") as fhandle:
        try:
            parser.parse(fhandle)
        except (TokenizerError, ParserError) as exc:
            ctx = exc.args[1]
            line = ctx.line.rstrip()

            msg = f"Syntax error: {exc.args[0]} ({exc.__cause__})"

            linenr = ctx.linenr - 1
            colnr = ctx.colnr

            if colnr is not None:
                count = 0  # number of underline chars after (positiv) or before (negative) the marker if ref_colnr given
                nchars = colnr  # relevant line length

                if ctx.ref_colnr is not None:
                    count = ctx.ref_colnr - ctx.colnr
                    nchars = min(ctx.ref_colnr, ctx.colnr)  # correct if ref comes before

                if ctx.colnrs:
                    # shift by the number of left-stripped ws
                    # ctx.colnrs contains the left shift for each possibly continued line
                    nchars += ctx.colnrs[0]  # assume no line-continuation for now

                # at least do one context
                count = max(1, count)

                erange = Range(
                    start=Position(line=linenr, character=colnr + 1 - count), end=Position(line=linenr, character=colnr + 1)
                )

            else:
                erange = Range(start=Position(line=linenr, character=1), end=Position(line=linenr, character=len(line)))

            diagnostics += [Diagnostic(range=erange, message=msg, source=type(ls).__name__)]

    ls.publish_diagnostics(text_doc.uri, diagnostics)


def setup_cp2k_ls_server(server):
    @server.feature(TEXT_DOCUMENT_DID_CHANGE)
    def did_change(ls, params: DidChangeTextDocumentParams):
        """Text document did change notification."""
        _validate(ls, params)

    @server.feature(TEXT_DOCUMENT_DID_CLOSE)
    def did_close(ls: LanguageServer, params: DidCloseTextDocumentParams):
        """Text document did close notification."""
        pass

    @server.feature(TEXT_DOCUMENT_DID_OPEN)
    async def did_open(ls, params: DidOpenTextDocumentParams):
        """Text document did open notification."""
        _validate(ls, params)


cp2k_server = LanguageServer()
setup_cp2k_ls_server(cp2k_server)
