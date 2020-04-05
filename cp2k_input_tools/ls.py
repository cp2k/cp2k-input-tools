from pygls.features import TEXT_DOCUMENT_DID_CHANGE, TEXT_DOCUMENT_DID_CLOSE, TEXT_DOCUMENT_DID_OPEN
from pygls.server import LanguageServer
from pygls.types import (
    Diagnostic,
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    DidOpenTextDocumentParams,
    Position,
    Range,
)


from . import DEFAULT_CP2K_INPUT_XML
from .parser import CP2KInputParser
from .parser_errors import ParserError
from .tokenizer import TokenizerError


class CP2KLanguageServer(LanguageServer):
    def __init__(self):
        self.parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

        super().__init__()


cp2k_inp_server = CP2KLanguageServer()


def _validate(ls, params):
    ls.show_message_log("Validating CP2K input...")

    diagnostics = []

    text_doc = ls.workspace.get_document(params.textDocument.uri)

    with open(text_doc.path, "r") as fhandle:
        try:
            cp2k_inp_server.parser.parse(fhandle)
        except (TokenizerError, ParserError) as exc:
            ctx = exc.args[1]
            line = ctx["line"].rstrip()

            msg = f"Syntax error: {exc.args[0]}"

            if exc.__cause__:
                msg += f"({exc.__cause__})"

            linenr = ctx["linenr"]
            colnr = ctx["colnr"]

            if colnr is not None:
                count = 0  # number of underline chars after (positiv) or before (negative) the marker if ref_colnr given
                nchars = colnr  # relevant line length

                if ctx["ref_colnr"] is not None:
                    count = ctx["ref_colnr"] - ctx["colnr"]
                    nchars = min(ctx["ref_colnr"], ctx["colnr"])  # correct if ref comes before

                if ctx["colnrs"] is not None:
                    # shift by the number of left-stripped ws
                    # ctx["colnrs"] contains the left shift for each possibly continued line
                    nchars += ctx["colnrs"][0]  # assume no line-continuation for now

                # at least do one context
                count = max(1, count)

                erange = Range(Position(linenr, colnr + 1 - count), Position(linenr, colnr + 1))

            else:
                erange = Range(Position(linenr, 1), Position(linenr, len(line)))

            diagnostics += [Diagnostic(erange, msg, source=type(cp2k_inp_server).__name__, related_information=[])]

    ls.publish_diagnostics(text_doc.uri, diagnostics)


@cp2k_inp_server.feature(TEXT_DOCUMENT_DID_CHANGE)
def did_change(ls, params: DidChangeTextDocumentParams):
    """Text document did change notification."""
    _validate(ls, params)


@cp2k_inp_server.feature(TEXT_DOCUMENT_DID_CLOSE)
def did_close(server: CP2KLanguageServer, params: DidCloseTextDocumentParams):
    """Text document did close notification."""
    server.show_message("Text Document Did Close")


@cp2k_inp_server.feature(TEXT_DOCUMENT_DID_OPEN)
async def did_open(ls, params: DidOpenTextDocumentParams):
    """Text document did open notification."""
    ls.show_message("Text Document Did Open")
    _validate(ls, params)
