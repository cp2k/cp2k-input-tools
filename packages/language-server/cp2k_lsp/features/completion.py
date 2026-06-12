"""Completion Provider."""

from typing import List, Optional

from lsprotocol import types as lsp


class CompletionProvider:
    """Provides completion items for CP2K input."""

    # Common sections
    COMMON_SECTIONS = [
        ("GLOBAL", "Global settings for the calculation"),
        ("FORCE_EVAL", "Force evaluation settings"),
        ("MOTION", "Molecular dynamics and optimization"),
        ("ATOM", "Atomic kind definitions"),
        ("KIND", "Species kind definition"),
        ("DFT", "Density Functional Theory settings"),
        ("QS", "Quickstep settings"),
        ("SCF", "Self-Consistent Field settings"),
        ("XC", "Exchange-Correlation functional"),
        ("PRINT", "Print control section"),
    ]

    # Common keywords
    COMMON_KEYWORDS = [
        ("PROJECT_NAME", "Name of the project"),
        ("RUN_TYPE", "Type of calculation"),
        ("PRINT_LEVEL", "Level of output printing"),
    ]

    # RUN_TYPE enum values
    RUN_TYPES = [
        "ENERGY",
        "ENERGY_FORCE",
        "WAVEFUNCTION_OPTIMIZATION",
        "WFN_OPT",
        "GEO_OPT",
        "GEOMETRY_OPTIMIZATION",
        "MD",
        "MOLECULAR_DYNAMICS",
        "MC",
        "MONTECARLO",
        "SPECTRA",
        "EP",
        "ELECTRONIC_PROPAGATION",
        "DEBUG",
        "BSSE",
        "LINEAR_RESPONSE",
        "LR",
        "NONE",
    ]

    # PRINT_LEVEL enum values
    PRINT_LEVELS = ["SILENT", "LOW", "MEDIUM", "HIGH", "DEBUG"]

    def __init__(self, server):
        self.server = server

    def provide_completion(self, params: lsp.CompletionParams) -> Optional[lsp.CompletionList]:
        """Provide completion items."""
        uri = params.text_document.uri
        position = params.position

        document = self.server.workspace.get_text_document(uri)
        lines = document.lines

        if position.line >= len(lines):
            return None

        line = lines[position.line]
        line_before = line[: position.character]

        items = []

        # Check context
        if line_before.strip().endswith("&"):
            # Section completion
            items = self._get_section_completions()
        elif line_before.strip().endswith("=") or " " in line_before.strip():
            # Value completion
            items = self._get_value_completions(line_before)
        else:
            # Keyword completion
            items = self._get_keyword_completions()

        return lsp.CompletionList(is_incomplete=False, items=items)

    def _get_section_completions(self) -> List[lsp.CompletionItem]:
        """Get section completion items."""
        items = []
        for name, desc in self.COMMON_SECTIONS:
            item = lsp.CompletionItem(
                label=name,
                kind=lsp.CompletionItemKind.Struct,
                detail=desc,
                insert_text=f"{name}\n\u0026END {name}",
                insert_text_format=lsp.InsertTextFormat.PlainText,
            )
            items.append(item)
        return items

    def _get_keyword_completions(self) -> List[lsp.CompletionItem]:
        """Get keyword completion items."""
        items = []
        for name, desc in self.COMMON_KEYWORDS:
            item = lsp.CompletionItem(
                label=name,
                kind=lsp.CompletionItemKind.Property,
                detail=desc,
                insert_text=f"{name} = ",
                insert_text_format=lsp.InsertTextFormat.PlainText,
            )
            items.append(item)
        return items

    def _get_value_completions(self, line: str) -> List[lsp.CompletionItem]:
        """Get value completion items based on context."""
        items = []
        line_upper = line.upper()

        if "RUN_TYPE" in line_upper:
            for rt in self.RUN_TYPES:
                items.append(lsp.CompletionItem(label=rt, kind=lsp.CompletionItemKind.EnumMember, detail=f"Run type: {rt}"))
        elif "PRINT_LEVEL" in line_upper:
            for pl in self.PRINT_LEVELS:
                items.append(lsp.CompletionItem(label=pl, kind=lsp.CompletionItemKind.EnumMember, detail=f"Print level: {pl}"))
        else:
            # Boolean values
            items.extend(
                [
                    lsp.CompletionItem(label=".TRUE.", kind=lsp.CompletionItemKind.Keyword, detail="Boolean true"),
                    lsp.CompletionItem(label=".FALSE.", kind=lsp.CompletionItemKind.Keyword, detail="Boolean false"),
                ]
            )

        return items
