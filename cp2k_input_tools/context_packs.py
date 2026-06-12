"""Context Packs API (#57).

Provides unified context packs combining cursor, hover, completions,
and diagnostics for agent consumption.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from datetime import datetime, timezone
from io import StringIO
from typing import Any

from .agent_lsp import AgentLSP
from .parser import DEFAULT_CP2K_INPUT_XML, CP2KInputParser


@dataclass
class ContextPack:
    """Unified context pack for agent tool consumption.

    Combines cursor context, hover information, completions,
    and diagnostics into a single JSON-serializable structure.
    """

    cursor: dict[str, Any]
    """Cursor context including section, keyword, and position info."""

    hover: dict[str, Any] | None
    """Hover information if available, None otherwise."""

    completions: list[dict[str, Any]]
    """List of completion items at the cursor position."""

    diagnostics: list[dict[str, Any]]
    """List of diagnostics for the entire file."""

    timestamp: str
    """ISO 8601 timestamp when context pack was generated."""

    def to_dict(self) -> dict[str, Any]:
        """Convert ContextPack to dictionary for JSON serialization."""
        return {
            "cursor": self.cursor,
            "hover": self.hover,
            "completions": self.completions,
            "diagnostics": self.diagnostics,
            "timestamp": self.timestamp,
        }

    def to_json(self) -> str:
        """Convert ContextPack to JSON string."""
        return json.dumps(self.to_dict())


def get_context_pack(
    text: str,
    line: int = 0,
    char: int = 0,
    uri: str = "file:///input",
) -> ContextPack:
    """Generate a unified context pack for agent consumption.

    Args:
        text: CP2K input file text
        line: Cursor line position (0-indexed)
        char: Cursor character position (0-indexed)
        uri: File URI for diagnostics

    Returns:
        ContextPack with cursor, hover, completions, and diagnostics

    Example:
        >>> text = "&GLOBAL\\n  RUN_TYPE ENERGY\\n&END GLOBAL"
        >>> pack = get_context_pack(text, line=1, char=10)
        >>> json_data = pack.to_json()
    """
    # Generate timestamp
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    # Parse the input to get cursor context and diagnostics
    parser = CP2KInputParser(DEFAULT_CP2K_INPUT_XML)

    try:
        with StringIO(text) as fhandle:
            ast = parser.parse(fhandle)
            errors = []  # Parse succeeded, no errors
    except Exception as e:
        ast = None
        errors = [type("Error", (), {"message": str(e), "line_number": None})()]

    # Get cursor context from AST
    cursor_context = _extract_cursor_context(text, line, char, ast)

    # Get hover information
    hover_info = _get_hover_at_position(text, line, char, ast)

    # Get completions
    completions = _get_completions_at_position(text, line, char, ast)

    # Get diagnostics from AgentLSP
    agent_lsp = AgentLSP.from_text(text, uri=uri)
    check_result = agent_lsp.check()
    diagnostics = check_result.get("diagnostics", [])

    # Add parse errors to diagnostics if any
    for error in errors:
        diagnostics.append(
            {
                "range": {
                    "start": {"line": error.line_number or 0, "character": 0},
                    "end": {"line": error.line_number or 0, "character": 100},
                },
                "severity": 1,  # Error
                "message": error.message or "Parse error",
                "source": "cp2k-parser",
            }
        )

    return ContextPack(
        cursor=cursor_context,
        hover=hover_info,
        completions=completions,
        diagnostics=diagnostics,
        timestamp=timestamp,
    )


def _extract_cursor_context(
    text: str,
    line: int,
    char: int,
    ast: Any,
) -> dict[str, Any]:
    """Extract cursor context from AST.

    Args:
        text: Input text
        line: Cursor line (0-indexed)
        char: Cursor character (0-indexed)
        ast: Parsed AST

    Returns:
        Dictionary with cursor context info
    """
    lines = text.split("\n")
    current_line = lines[line] if line < len(lines) else ""

    context: dict[str, Any] = {
        "position": {"line": line, "character": char},
        "line_text": current_line,
        "word_at_cursor": _get_word_at_position(current_line, char),
    }

    # Try to extract section/keyword context from AST
    if ast:
        try:
            # Walk AST to find context
            section_info = _find_section_at_position(ast, line, char)
            if section_info:
                context["section"] = section_info.get("section")
                context["subsection_path"] = section_info.get("path", [])
        except Exception:
            # AST traversal failed, continue with basic context
            pass

    return context


def _get_word_at_position(line: str, char: int) -> str:
    """Get the word at the given character position in a line."""
    # Ensure char is within bounds
    if char >= len(line):
        char = len(line) - 1
    if char < 0:
        char = 0

    # Find word boundaries
    start = char
    while start > 0 and not line[start - 1].isspace():
        start -= 1

    end = char
    while end < len(line) and not line[end].isspace():
        end += 1

    return line[start:end]


def _find_section_at_position(
    ast: Any,
    line: int,
    char: int,
) -> dict[str, Any] | None:
    """Find section information at the given position."""
    # This is a simplified implementation
    # In a full implementation, this would walk the AST properly
    if hasattr(ast, "sections"):
        for section in ast.sections:
            if hasattr(section, "line_number") and section.line_number == line:
                return {"section": section.name if hasattr(section, "name") else "UNKNOWN", "path": []}

    return None


def _get_hover_at_position(
    text: str,
    line: int,
    char: int,
    ast: Any,
) -> dict[str, Any] | None:
    """Get hover information at the cursor position.

    Args:
        text: Input text
        line: Cursor line (0-indexed)
        char: Cursor character (0-indexed)
        ast: Parsed AST

    Returns:
        Hover information dict or None
    """
    lines = text.split("\n")
    if line >= len(lines):
        return None

    current_line = lines[line]
    word = _get_word_at_position(current_line, char)

    # If we're on a section name
    if current_line.strip().startswith("&") and word.startswith("&"):
        section_name = word.lstrip("&").strip()
        return {
            "kind": "section",
            "name": section_name,
            "contents": f"CP2K section: {section_name}",
        }

    # If we're on a keyword
    if "=" in current_line and word and not word.startswith("&"):
        # Check if word is before '=' (keyword name)
        before_equals = current_line[: current_line.index("=")]
        if word in before_equals:
            return {
                "kind": "keyword",
                "name": word,
                "contents": f"CP2K keyword: {word}",
            }

    return None


def _get_completions_at_position(
    text: str,
    line: int,
    char: int,
    ast: Any,
) -> list[dict[str, Any]]:
    """Get completion items at the cursor position.

    Args:
        text: Input text
        line: Cursor line (0-indexed)
        char: Cursor character (0-indexed)
        ast: Parsed AST

    Returns:
        List of completion item dicts
    """
    lines = text.split("\n")
    if line >= len(lines):
        return []

    current_line = lines[line]

    # If we're at the start of a line after a section start, suggest keywords
    if current_line.strip().startswith("&END"):
        # After section end, suggest sections
        return [
            {"label": "&GLOBAL", "kind": "section", "detail": "Global section"},
            {"label": "&FORCE_EVAL", "kind": "section", "detail": "Force evaluation section"},
            {"label": "&MOTION", "kind": "section", "detail": "Motion section"},
        ]

    # If we're inside a section and line starts with spaces, suggest keywords
    if current_line.startswith("  ") and not current_line.strip().startswith("&"):
        return [
            {"label": "RUN_TYPE", "kind": "keyword", "detail": "Run type"},
            {"label": "PROJECT_NAME", "kind": "keyword", "detail": "Project name"},
            {"label": "PRINT_LEVEL", "kind": "keyword", "detail": "Print level"},
        ]

    # If we're after "=", suggest enum values
    if "=" in current_line and char > current_line.index("="):
        keyword_part = current_line[: current_line.index("=")].strip()
        if keyword_part == "RUN_TYPE":
            return [
                {"label": "ENERGY", "kind": "value", "detail": "Energy calculation"},
                {"label": "GEO_OPT", "kind": "value", "detail": "Geometry optimization"},
                {"label": "MD", "kind": "value", "detail": "Molecular dynamics"},
            ]

    # Default: suggest sections
    return [
        {"label": "&GLOBAL", "kind": "section", "detail": "Global section"},
        {"label": "&FORCE_EVAL", "kind": "section", "detail": "Force evaluation section"},
        {"label": "&SUBSYS", "kind": "section", "detail": "Subsystem section"},
    ]
