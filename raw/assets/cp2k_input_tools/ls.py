from typing import Union

from lsprotocol.types import (
    TEXT_DOCUMENT_COMPLETION,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_CLOSE,
    TEXT_DOCUMENT_DID_OPEN,
    CompletionParams,
    Diagnostic,
    DiagnosticSeverity,
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    DidOpenTextDocumentParams,
    Position,
    Range,
)
from pygls.server import LanguageServer

from .completion import get_completions
from .parser import CP2KInputParserSimplified
from .parser_errors import ParserError
from .tokenizer import TokenizerError
from .validator import validate_semantics


def _validate(ls, params: Union[DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams]):
    ls.show_message_log("Validating CP2K input...")

    diagnostics = []

    text_doc = ls.workspace.get_text_document(params.text_document.uri)
    parser = CP2KInputParserSimplified()

    with open(text_doc.path, "r") as fhandle:
        try:
            tree = parser.parse(fhandle)

            # Syntax validation passed, now do semantic validation
            semantic_diagnostics = validate_semantics(tree)
            for diag in semantic_diagnostics:
                severity = DiagnosticSeverity.Warning
                if diag.severity == "error":
                    severity = DiagnosticSeverity.Error

                line = diag.line - 1 if diag.line > 0 else 0
                diagnostics.append(
                    Diagnostic(
                        range=Range(
                            start=Position(line=line, character=0), end=Position(line=line, character=100)
                        ),
                        message=diag.message,
                        severity=severity,
                        source="cp2k-lsp",
                        code=diag.code,
                    )
                )

        except (TokenizerError, ParserError) as exc:
            ctx = exc.args[1]
            line = ctx.line.rstrip() if ctx.line else ""

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

                # Clamp character values to valid range [0, 2147483647]
                start_char = max(0, colnr + 1 - count)
                end_char = colnr + 1
                erange = Range(
                    start=Position(line=linenr, character=start_char), end=Position(line=linenr, character=end_char)
                )

            else:
                erange = Range(start=Position(line=linenr, character=1), end=Position(line=linenr, character=len(line)))

            diagnostics += [Diagnostic(range=erange, message=msg, source=type(ls).__name__)]

    ls.publish_diagnostics(text_doc.uri, diagnostics)


def completion(ls, params: CompletionParams):
    """Provide completion items for CP2K input files."""
    text_doc = ls.workspace.get_text_document(params.text_document.uri)
    text = text_doc.source

    return get_completions(
        text=text,
        position=params.position,
        uri=params.text_document.uri,
    )


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

    @server.feature(TEXT_DOCUMENT_COMPLETION)
    def did_completion(ls, params: CompletionParams):
        """Completion request handler."""
        return completion(ls, params)


cp2k_server = LanguageServer("cp2k-lsp", "v0.1")
setup_cp2k_ls_server(cp2k_server)
