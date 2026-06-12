"""CP2K Language Server."""

import logging
from typing import Dict, List, Optional

from lsprotocol import types as lsp
from pygls.server import LanguageServer

from cp2k_lsp.features.code_action import CodeActionProvider
from cp2k_lsp.features.completion import CompletionProvider
from cp2k_lsp.features.diagnostics import DiagnosticsProvider
from cp2k_lsp.features.formatting import FormattingProvider
from cp2k_lsp.features.hover import HoverProvider
from cp2k_lsp.parser import CP2KInput, CP2KParser

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cp2k-lsp")


class CP2KLanguageServer(LanguageServer):
    """CP2K Language Server Protocol implementation."""

    CONFIGURATION_SECTION = "cp2k"

    def __init__(self, *args, **kwargs):
        super().__init__("cp2k-lsp", "0.1.0", *args, **kwargs)
        self.parsed_documents: Dict[str, Optional[CP2KInput]] = {}
        self.parser_errors: Dict[str, List] = {}

        # Feature providers
        self.diagnostics = DiagnosticsProvider(self)
        self.completion = CompletionProvider(self)
        self.hover = HoverProvider(self)
        self.formatting = FormattingProvider(self)
        self.code_action = CodeActionProvider(self)

        self._setup_handlers()

    def _setup_handlers(self) -> None:
        """Setup LSP handlers."""

        @self.feature(lsp.TEXT_DOCUMENT_DID_OPEN)
        async def did_open(params: lsp.DidOpenTextDocumentParams):
            self._parse_document(params.text_document.uri)
            await self._publish_diagnostics(params.text_document.uri)

        @self.feature(lsp.TEXT_DOCUMENT_DID_CHANGE)
        async def did_change(params: lsp.DidChangeTextDocumentParams):
            self._parse_document(params.text_document.uri)
            await self._publish_diagnostics(params.text_document.uri)

        @self.feature(lsp.TEXT_DOCUMENT_DID_CLOSE)
        async def did_close(params: lsp.DidCloseTextDocumentParams):
            uri = params.text_document.uri
            if uri in self.parsed_documents:
                del self.parsed_documents[uri]
            if uri in self.parser_errors:
                del self.parser_errors[uri]

        @self.feature(lsp.TEXT_DOCUMENT_COMPLETION)
        def completion(params: lsp.CompletionParams) -> Optional[lsp.CompletionList]:
            return self.completion.provide_completion(params)

        @self.feature(lsp.TEXT_DOCUMENT_HOVER)
        def hover_provider(params: lsp.HoverParams) -> Optional[lsp.Hover]:
            return self.hover.provide_hover(params)

        @self.feature(lsp.TEXT_DOCUMENT_FORMATTING)
        def formatting_provider(params: lsp.DocumentFormattingParams) -> Optional[List[lsp.TextEdit]]:
            return self.formatting.provide_formatting(params)

        @self.feature(lsp.TEXT_DOCUMENT_CODE_ACTION)
        def code_action_provider(params: lsp.CodeActionParams) -> Optional[List[lsp.CodeAction]]:
            return self.code_action.provide_code_actions(params)

    def _parse_document(self, uri: str) -> None:
        """Parse a document and store the AST."""
        try:
            document = self.workspace.get_text_document(uri)
            parser = CP2KParser.parse_text(document.source, uri)
            self.parsed_documents[uri] = parser.ast
            self.parser_errors[uri] = parser.errors
        except Exception as e:
            logger.error(f"Error parsing document {uri}: {e}")
            self.parsed_documents[uri] = None
            self.parser_errors[uri] = []

    async def _publish_diagnostics(self, uri: str) -> None:
        """Publish diagnostics for a document."""
        diagnostics = self.diagnostics.get_diagnostics(uri)
        self.publish_diagnostics(uri, diagnostics)

    def get_ast(self, uri: str) -> Optional[CP2KInput]:
        """Get parsed AST for a document."""
        if uri not in self.parsed_documents:
            self._parse_document(uri)
        return self.parsed_documents.get(uri)

    def get_errors(self, uri: str) -> List:
        """Get parser errors for a document."""
        if uri not in self.parser_errors:
            self._parse_document(uri)
        return self.parser_errors.get(uri, [])


def main():
    """Main entry point."""
    server = CP2KLanguageServer()
    logger.info("Starting CP2K Language Server...")
    server.start_io()


if __name__ == "__main__":
    main()
