from typing import Union

from lsprotocol.types import (
    TEXT_DOCUMENT_CODE_ACTION,
    TEXT_DOCUMENT_COMPLETION,
    TEXT_DOCUMENT_DID_CHANGE,
    TEXT_DOCUMENT_DID_CLOSE,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DOCUMENT_SYMBOL,
    TEXT_DOCUMENT_HOVER,
    WORKSPACE_SYMBOL,
    CodeActionParams,
    CompletionParams,
    Diagnostic,
    DiagnosticSeverity,
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    DidOpenTextDocumentParams,
    DocumentSymbolParams,
    HoverParams,
    Position,
    Range,
    SymbolInformation,
    WorkspaceSymbolParams,
)
from pygls.server import LanguageServer

from .code_actions import get_code_actions
from .completion import get_completions
from .hover import get_hover
from .parser import CP2KInputParserSimplified
from .parser_errors import ParserError
from .symbols import get_document_symbols, get_workspace_symbols
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
                        range=Range(start=Position(line=line, character=0), end=Position(line=line, character=100)),
                        message=diag.message,
                        severity=severity,
                        source="cp2k-lsp",
                        code=diag.code,
                    )
                )

        except (TokenizerError, ParserError) as exc:
            # Handle exceptions with varying argument counts
            if len(exc.args) >= 2 and exc.args[1] is not None:
                ctx = exc.args[1]
                line = ctx.line.rstrip() if ctx.line else ""
                msg = f"Syntax error: {exc.args[0]} ({exc.__cause__})"
                linenr = ctx.linenr - 1 if hasattr(ctx, "linenr") and ctx.linenr else 0
                colnr = ctx.colnr if hasattr(ctx, "colnr") else None
            else:
                # Exception without context (e.g., NameRepetitionError)
                line = ""
                msg = f"Syntax error: {exc.args[0]}"
                linenr = 0
                colnr = None

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
                erange = Range(start=Position(line=linenr, character=start_char), end=Position(line=linenr, character=end_char))

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


def hover(ls, params: HoverParams):
    """Provide hover information for CP2K input files."""
    text_doc = ls.workspace.get_text_document(params.text_document.uri)
    text = text_doc.source

    return get_hover(
        text=text,
        position=params.position,
        uri=params.text_document.uri,
    )


def code_actions(ls, params: CodeActionParams):
    """Provide code actions for CP2K input file diagnostics."""
    text_doc = ls.workspace.get_text_document(params.text_document.uri)
    text = text_doc.source

    # Get diagnostics from the context (handle both object and dict)
    if params.context is None:
        diagnostics = []
    elif isinstance(params.context, dict):
        diagnostics = params.context.get("diagnostics", [])
    else:
        diagnostics = params.context.diagnostics or []

    # Collect all code actions from all diagnostics
    all_actions = []
    for diag in diagnostics:
        actions = get_code_actions(
            text=text,
            diagnostic_range=diag.range,
            diagnostic_message=diag.message,
            uri=params.text_document.uri,
        )
        all_actions.extend(actions)

    return all_actions


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

    @server.feature(TEXT_DOCUMENT_HOVER)
    def did_hover(ls, params: HoverParams):
        """Hover request handler."""
        return hover(ls, params)

    @server.feature(TEXT_DOCUMENT_CODE_ACTION)
    def did_code_actions(ls, params: CodeActionParams):
        """Code actions request handler."""
        return code_actions(ls, params)

    @server.feature(TEXT_DOCUMENT_DOCUMENT_SYMBOL)
    def document_symbol(ls, params: DocumentSymbolParams) -> list[SymbolInformation]:
        """Document symbol request (#55).

        Returns hierarchical symbol information for sections and keywords.
        """
        text_doc = ls.workspace.get_text_document(params.text_document.uri)

        try:
            with open(text_doc.path, "r") as fhandle:
                text = fhandle.read()

            symbols = get_document_symbols(text, text_doc.uri)
            return symbols

        except Exception:
            # Return empty list on error
            return []

    @server.feature(WORKSPACE_SYMBOL)
    def workspace_symbol(ls, params: WorkspaceSymbolParams) -> list[SymbolInformation]:
        """Workspace symbol request (#55).

        Searches for symbols matching the query across all open documents.
        """
        # Get all open documents
        all_files = {}
        for doc in ls.workspace.documents.values():
            try:
                with open(doc.path, "r") as fhandle:
                    all_files[doc.uri] = fhandle.read()
            except Exception:
                continue

        # Search symbols
        symbols = get_workspace_symbols(params.query, all_files)
        return symbols


cp2k_server = LanguageServer("cp2k-lsp", "v0.1")
setup_cp2k_ls_server(cp2k_server)
