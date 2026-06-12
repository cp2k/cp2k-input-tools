"""Code Action Provider (Quick Fix)."""

from typing import List, Optional

from lsprotocol import types as lsp


class CodeActionProvider:
    """Provides code actions (Quick Fixes) for CP2K input."""

    def __init__(self, server):
        self.server = server

    def provide_code_actions(self, params: lsp.CodeActionParams) -> Optional[List[lsp.CodeAction]]:
        """Provide code actions for diagnostics."""
        actions = []
        uri = params.text_document.uri

        for diagnostic in params.context.diagnostics:
            action = self._create_quick_fix(diagnostic, uri)
            if action:
                actions.append(action)

        return actions if actions else None

    def _create_quick_fix(self, diagnostic: lsp.Diagnostic, uri: str) -> Optional[lsp.CodeAction]:
        """Create a quick fix for a diagnostic."""
        message = diagnostic.message.lower()

        if "unclosed" in message or "expected" in message:
            return self._fix_unclosed_section(diagnostic, uri)
        elif "mismatch" in message:
            return self._fix_section_mismatch(diagnostic, uri)

        return None

    def _fix_unclosed_section(self, diagnostic: lsp.Diagnostic, uri: str) -> lsp.CodeAction:
        """Fix unclosed section by adding &END."""
        range_ = diagnostic.range

        return lsp.CodeAction(
            title="Add missing &END tag",
            kind=lsp.CodeActionKind.QuickFix,
            diagnostics=[diagnostic],
            edit=lsp.WorkspaceEdit(
                changes={
                    uri: [
                        lsp.TextEdit(
                            range=lsp.Range(
                                start=lsp.Position(line=range_.end.line + 1, character=0),
                                end=lsp.Position(line=range_.end.line + 1, character=0),
                            ),
                            new_text="\u0026END\n",
                        )
                    ]
                }
            ),
        )

    def _fix_section_mismatch(self, diagnostic: lsp.Diagnostic, uri: str) -> lsp.CodeAction:
        """Fix section name mismatch."""
        return lsp.CodeAction(
            title="Fix section name", kind=lsp.CodeActionKind.QuickFix, diagnostics=[diagnostic], is_preferred=True
        )
