"""Diagnostics Provider."""

from typing import List

from lsprotocol import types as lsp


class DiagnosticsProvider:
    """Provides diagnostics for CP2K input files."""

    def __init__(self, server):
        self.server = server

    def get_diagnostics(self, uri: str) -> List[lsp.Diagnostic]:
        """Get diagnostics for a document."""
        diagnostics: List[lsp.Diagnostic] = []

        # Get parser errors
        errors = self.server.get_errors(uri)
        for error in errors:
            diagnostic = lsp.Diagnostic(
                range=lsp.Range(
                    start=lsp.Position(line=error.line - 1, character=error.column - 1),
                    end=lsp.Position(line=error.line - 1, character=error.column + 10),
                ),
                message=str(error),
                severity=lsp.DiagnosticSeverity.Error,
                source="cp2k-lsp",
            )
            diagnostics.append(diagnostic)

        # Get AST validation diagnostics
        ast = self.server.get_ast(uri)
        if ast:
            diagnostics.extend(self._validate_ast(ast))

        return diagnostics

    def _validate_ast(self, ast) -> List[lsp.Diagnostic]:
        """Validate AST and return diagnostics."""
        diagnostics: List[lsp.Diagnostic] = []

        # Check for unclosed sections is done in parser
        # Additional semantic validation can be added here

        return diagnostics
