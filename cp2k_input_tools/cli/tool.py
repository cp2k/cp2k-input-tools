"""
Unified tool wrapper CLI for CP2K LSP.

Provides a CLI interface to LSP operations: diagnostics, context,
completions, hover, symbols, definition, and references.
"""

import json
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

import click

from cp2k_input_tools.parser import CP2KInputParser, Section
from cp2k_input_tools.parser_errors import ParserError
from cp2k_input_tools.tokenizer import TokenizerError

# =============================================================================
# Data Classes for JSON Output
# =============================================================================


@dataclass
class DiagRange:
    """Range for a diagnostic in a document."""

    start_line: int
    start_col: int
    end_line: int
    end_col: int

    def to_dict(self) -> Dict[str, int]:
        return {
            "start_line": self.start_line,
            "start_col": self.start_col,
            "end_line": self.end_line,
            "end_col": self.end_col,
        }


@dataclass
class Diagnostic:
    """A diagnostic message for a CP2K input file."""

    severity: str  # "error" | "warning" | "info"
    source: str  # "cp2k-parser" | "cp2k-schema" | "cp2k-lint" | "cp2k-typecheck"
    code: str
    message: str
    range: DiagRange
    line: Optional[int] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "severity": self.severity,
            "source": self.source,
            "code": self.code,
            "message": self.message,
            "range": self.range.to_dict(),
        }


@dataclass
class DiagnosticsResult:
    """Result of running diagnostics on a file."""

    file: str
    diagnostics: List[Diagnostic] = field(default_factory=list)

    @property
    def error_count(self) -> int:
        return sum(1 for d in self.diagnostics if d.severity == "error")

    @property
    def warning_count(self) -> int:
        return sum(1 for d in self.diagnostics if d.severity == "warning")

    def to_dict(self) -> Dict[str, Any]:
        return {
            "file": self.file,
            "diagnostics": [d.to_dict() for d in self.diagnostics],
            "error_count": self.error_count,
            "warning_count": self.warning_count,
        }


@dataclass
class Position:
    """A position in a text document."""

    line: int  # 0-based
    character: int  # 0-based

    def to_dict(self) -> Dict[str, int]:
        return {"line": self.line, "character": self.character}


@dataclass
class Range:
    """A range in a text document."""

    start: Position
    end: Position

    def to_dict(self) -> Dict[str, Any]:
        return {
            "start": self.start.to_dict(),
            "end": self.end.to_dict(),
        }


@dataclass
class CompletionItem:
    """A completion item."""

    label: str
    kind: int  # CompletionItemKind
    detail: Optional[str] = None
    documentation: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        result = {"label": self.label, "kind": self.kind}
        if self.detail:
            result["detail"] = self.detail
        if self.documentation:
            result["documentation"] = self.documentation
        return result


@dataclass
class DocumentSymbol:
    """A symbol in a document."""

    name: str
    kind: int  # SymbolKind
    range: Range
    selection_range: Range
    children: List["DocumentSymbol"] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        result = {
            "name": self.name,
            "kind": self.kind,
            "range": self.range.to_dict(),
            "selectionRange": self.selection_range.to_dict(),
        }
        if self.children:
            result["children"] = [c.to_dict() for c in self.children]
        return result


@dataclass
class Location:
    """A location in a document."""

    uri: str
    range: Range

    def to_dict(self) -> Dict[str, Any]:
        return {
            "uri": self.uri,
            "range": self.range.to_dict(),
        }


# =============================================================================
# Helper Functions
# =============================================================================


def _read_file_content(file_path: str) -> str:
    """Read file content safely."""
    try:
        with open(file_path, "r") as f:
            return f.read()
    except (IOError, OSError) as e:
        raise click.ClickException(f"Error reading file: {e}")


def _get_line_at_position(content: str, line: int) -> str:
    """Get line content at 0-based line index."""
    lines = content.split("\n")
    if 0 <= line < len(lines):
        return lines[line]
    return ""


# Regex patterns for parsing
_SECTION_RE = re.compile(r"^(\s*)&([\w\-_]+)\s*(.*)", re.IGNORECASE)
_KEYWORD_RE = re.compile(r"^(\s*)([\w\-_]+)\s+(.*)")
_VAR_SET_RE = re.compile(r"^\s*@SET\s+(\w+)\s+(.+)", re.IGNORECASE)
_VAR_REF_RE = re.compile(r"\$\{?(\w+)\}?")
_INCLUDE_RE = re.compile(r"^\s*@INCLUDE\s+(.+)", re.IGNORECASE)


def _get_section_context_at_position(content: str, line_idx: int) -> Optional[Dict[str, Any]]:
    """Get section context information at a given position."""
    import xml.etree.ElementTree as ET

    from cp2k_input_tools import DEFAULT_CP2K_INPUT_XML

    try:
        spec = ET.parse(DEFAULT_CP2K_INPUT_XML)
        root_section = Section("/", node=spec.getroot())
    except Exception:
        return None

    lines = content.split("\n")
    stack = [root_section]

    for i, line in enumerate(lines[: line_idx + 1]):
        stripped = line.strip()

        if not stripped or stripped.startswith(("!", "#")):
            continue

        if stripped.startswith("&"):
            match = _SECTION_RE.match(stripped)
            if match:
                section_name = match.group(2).upper()
                if section_name == "END":
                    if len(stack) > 1:
                        stack.pop()
                    continue

                section_node = stack[-1].find_node_by_name("SECTION", section_name)
                if section_node is not None:
                    new_section = Section(
                        section_name,
                        repeats=section_node.get("repeats") == "yes",
                        node=section_node,
                    )
                    stack[-1].subsections += [new_section]
                    stack.append(new_section)

    if stack:
        current = stack[-1]
        return {
            "name": current.name,
            "repeats": current.repeats,
            "keywords": [
                kw.find("NAME").text
                for kw in current.node.iterfind("./KEYWORD")
                if (name_node := kw.find("NAME")) is not None and name_node.text
            ],
            "sections": [
                sec.find("NAME").text
                for sec in current.node.iterfind("./SECTION")
                if (name_node := sec.find("NAME")) is not None and name_node.text
            ],
        }

    return None


def _find_keyword_at_position(content: str, line: int, char: int) -> Optional[str]:
    """Find the keyword name at a given position."""
    line_text = _get_line_at_position(content, line)
    stripped = line_text.strip()

    # Check for keyword
    kw_match = re.match(r"^(\w+)\s", stripped)
    if kw_match and not stripped.startswith("&"):
        return kw_match.group(1).upper()

    # Check for section
    sec_match = re.match(r"^&(\w+)", stripped)
    if sec_match:
        return sec_match.group(1).upper()

    return None


def _find_variable_at_position(content: str, line: int, char: int) -> Optional[str]:
    """Find a variable name ($VAR or @SET) at a given position."""
    line_text = _get_line_at_position(content, line)
    stripped = line_text.strip()

    # Check for @SET definition
    set_match = re.match(r"@SET\s+(\w+)", stripped, re.IGNORECASE)
    if set_match:
        return set_match.group(1)

    # Check for $VAR usage
    var_match = re.search(r"\$(\w+)", line_text)
    if var_match:
        # Check if the cursor is near the variable
        var_start = line_text.index("$" + var_match.group(1))
        if var_start <= char <= var_start + len(var_match.group(1)) + 1:
            return var_match.group(1)

    return None


def _parse_file_diagnostics(file_path: str, base_dir: str = ".") -> DiagnosticsResult:
    """Parse a CP2K input file and return diagnostics."""
    parser = CP2KInputParser(base_dir=base_dir)
    diagnostics: List[Diagnostic] = []

    try:
        with open(file_path, "r") as fhandle:
            parser.parse(fhandle)
    except (TokenizerError, ParserError) as exc:
        ctx = exc.args[1]
        linenr = getattr(ctx, "linenr", 1) - 1
        colnr = ctx.colnr or 0

        diagnostics.append(
            Diagnostic(
                severity="error",
                source="cp2k-parser",
                code=f"E{type(exc).__name__}",
                message=str(exc.args[0]),
                range=DiagRange(
                    start_line=linenr,
                    start_col=colnr,
                    end_line=linenr,
                    end_col=colnr + 1,
                ),
            )
        )
    except Exception as exc:
        diagnostics.append(
            Diagnostic(
                severity="error",
                source="cp2k-parser",
                code="UNKNOWN_ERROR",
                message=str(exc),
                range=DiagRange(0, 0, 0, 1),
            )
        )

    # Try to run type checking
    try:
        content = _read_file_content(file_path)
        from cp2k_input_tools.typecheck import validate_text as tc_validate

        type_diags = tc_validate(content)
        for td in type_diags:
            diagnostics.append(
                Diagnostic(
                    severity=td.severity,
                    source=td.source,
                    code=td.code,
                    message=td.message,
                    range=DiagRange(
                        start_line=td.line - 1,
                        start_col=td.col,
                        end_line=td.line - 1,
                        end_col=td.col + 1,
                    ),
                )
            )
    except ImportError:
        pass
    except Exception:
        pass

    # Try to run lint
    try:
        content = _read_file_content(file_path)
        from cp2k_input_tools.linter import lint as static_lint

        lint_diags = static_lint(content)
        for ld in lint_diags:
            line_nr = ld.line if ld.line is not None else 0
            col_nr = ld.column if ld.column is not None else 0
            diagnostics.append(
                Diagnostic(
                    severity=ld.severity,
                    source=ld.source,
                    code=ld.code,
                    message=ld.message,
                    range=DiagRange(
                        start_line=line_nr,
                        start_col=col_nr,
                        end_line=line_nr,
                        end_col=col_nr + 1,
                    ),
                )
            )
    except ImportError:
        pass
    except Exception:
        pass

    return DiagnosticsResult(file=file_path, diagnostics=diagnostics)


def _get_completions(file_path: str, line: int, char: int) -> Dict[str, Any]:
    """Get completion items at a given position."""
    content = _read_file_content(file_path)
    context = _get_section_context_at_position(content, line)

    items = []
    if context:
        # Add keywords
        for kw_name in context.get("keywords", []):
            items.append(
                CompletionItem(
                    label=kw_name,
                    kind=5,  # Field
                    detail="Keyword",
                )
            )

        # Add sections
        for sec_name in context.get("sections", []):
            items.append(
                CompletionItem(
                    label=f"&{sec_name}",
                    kind=5,  # Class
                    detail="Section",
                )
            )

    line_text = _get_line_at_position(content, line)
    prefix = line_text[:char].strip()

    # Filter by prefix if typing
    if prefix and not prefix.startswith("&"):
        prefix_upper = prefix.upper()
        items = [i for i in items if i.label.upper().startswith(prefix_upper)]

    return {
        "items": [item.to_dict() for item in items],
        "is_incomplete": False,
    }


def _get_hover_info(file_path: str, line: int, char: int) -> Dict[str, Any]:
    """Get hover information at a given position."""
    content = _read_file_content(file_path)
    kw_name = _find_keyword_at_position(content, line, char)

    if kw_name:
        # Try to get documentation from schema
        try:
            import xml.etree.ElementTree as ET

            from cp2k_input_tools import DEFAULT_CP2K_INPUT_XML

            spec = ET.parse(DEFAULT_CP2K_INPUT_XML)
            root = spec.getroot()

            for kw in root.iter("KEYWORD"):
                name_node = kw.find("NAME")
                if name_node is not None and name_node.text and name_node.text.upper() == kw_name:
                    desc = kw.find("DESCRIPTION")
                    dtype = kw.find("DATA_TYPE")
                    default = kw.find("DEFAULT_VALUE")
                    default_unit = kw.find("DEFAULT_UNIT")

                    parts = []
                    if desc is not None and desc.text:
                        text = desc.text.strip()
                        if len(text) > 300:
                            text = text[:297] + "..."
                        parts.append(text)
                    if dtype is not None:
                        parts.append(f"**Type:** {dtype.get('kind', 'unknown')}")
                    if default is not None and default.text:
                        parts.append(f"**Default:** {default.text}")
                    if default_unit is not None and default_unit.text:
                        parts.append(f"**Unit:** {default_unit.text}")

                    return {"contents": "\n\n".join(parts) if parts else f"Keyword: {kw_name}"}
        except Exception:
            pass

    return {"contents": "No documentation available"}


def _get_document_symbols(file_path: str) -> List[Dict[str, Any]]:
    """Get document symbols for a file."""
    content = _read_file_content(file_path)
    lines = content.split("\n")
    symbols: List[DocumentSymbol] = []
    stack = []  # (indent, symbol)

    for i, line in enumerate(lines):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue

        indent = len(line) - len(line.lstrip())

        if stripped.startswith("&") and not stripped.upper().startswith("&END"):
            sec_name = stripped[1:].split()[0]

            # Pop stack items with same or greater indent
            while stack and indent <= stack[-1][0]:
                stack.pop()

            sym = DocumentSymbol(
                name=sec_name,
                kind=5,  # Struct
                range=Range(
                    start=Position(line=i, character=0),
                    end=Position(line=i, character=len(line)),
                ),
                selection_range=Range(
                    start=Position(line=i, character=indent),
                    end=Position(line=i, character=indent + len(sec_name) + 1),
                ),
            )

            if stack:
                stack[-1][1].children.append(sym)
            else:
                symbols.append(sym)

            stack.append((indent, sym))

        elif stripped.upper().startswith("&END"):
            while stack:
                stack.pop()
                break

    return [sym.to_dict() for sym in symbols]


def _find_variable_definitions(content: str, var_name: str) -> List[Dict[str, Any]]:
    """Find all @SET variable definitions."""
    positions = []
    pattern = re.compile(rf"^@SET\s+{re.escape(var_name)}\b", re.IGNORECASE | re.MULTILINE)

    for match in pattern.finditer(content):
        line_num = content[: match.start()].count("\n")
        col = match.start() - content.rfind("\n", 0, match.start()) - 1
        positions.append(
            {
                "line": line_num,
                "character": col,
            }
        )

    return positions


def _find_variable_usages(content: str, var_name: str) -> List[Dict[str, Any]]:
    """Find all $VAR usages."""
    positions = []
    pattern = re.compile(rf"\${re.escape(var_name)}\b")

    for match in pattern.finditer(content):
        line_num = content[: match.start()].count("\n")
        col = match.start() - content.rfind("\n", 0, match.start()) - 1
        positions.append(
            {
                "line": line_num,
                "character": col,
            }
        )

    return positions


def _get_definition(file_path: str, line: int, char: int) -> Dict[str, Any]:
    """Go to definition at a given position."""
    content = _read_file_content(file_path)
    var_name = _find_variable_at_position(content, line, char)

    if var_name:
        defs = _find_variable_definitions(content, var_name)
        if defs:
            uri = Path(file_path).as_uri()
            return {
                "locations": [
                    {
                        "uri": uri,
                        "range": {
                            "start": {"line": d["line"] + 1, "character": d["character"]},  # 1-based line
                            "end": {
                                "line": d["line"] + 1,  # 1-based line
                                "character": d["character"] + len(f"@SET {var_name}"),
                            },
                        },
                    }
                    for d in defs
                ]
            }

    return {"locations": []}


def _get_references(file_path: str, line: int, char: int) -> Dict[str, Any]:
    """Find references at a given position."""
    content = _read_file_content(file_path)
    var_name = _find_variable_at_position(content, line, char)

    if var_name:
        defs = _find_variable_definitions(content, var_name)
        usages = _find_variable_usages(content, var_name)
        uri = Path(file_path).as_uri()

        refs = []
        for d in defs:
            refs.append(
                {
                    "uri": uri,
                    "range": {
                        "start": {"line": d["line"] + 1, "character": d["character"]},  # 1-based line
                        "end": {
                            "line": d["line"] + 1,  # 1-based line
                            "character": d["character"] + len(f"@SET {var_name}"),
                        },
                    },
                }
            )
        for u in usages:
            refs.append(
                {
                    "uri": uri,
                    "range": {
                        "start": {"line": u["line"] + 1, "character": u["character"]},  # 1-based line
                        "end": {
                            "line": u["line"] + 1,  # 1-based line
                            "character": u["character"] + len(var_name) + 1,
                        },
                    },
                }
            )

        return {"references": refs}

    return {"references": []}


def _get_context_pack(file_path: str, line: int, char: int) -> Dict[str, Any]:
    """Get context pack at a given position (line is 0-based)."""
    content = _read_file_content(file_path)
    context = _get_section_context_at_position(content, line)
    line_text = _get_line_at_position(content, line)

    return {
        "file": file_path,
        "position": {"line": line + 1, "character": char},  # Output 1-based line
        "line_text": line_text,
        "context": context or {},
    }


# =============================================================================
# CLI Commands
# =============================================================================


@click.group()
def tool_cli():
    """Unified tool wrapper CLI for CP2K LSP operations."""
    pass


@tool_cli.command("check")
@click.argument("file", type=click.Path(exists=True))
@click.option("--base-dir", default=".", help="Base directory for @INCLUDE resolution")
def check_cmd(file: str, base_dir: str):
    """Run diagnostics on a CP2K input file."""
    try:
        result = _parse_file_diagnostics(file, base_dir)
        click.echo(json.dumps(result.to_dict(), indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


@tool_cli.command("context")
@click.argument("file", type=click.Path(exists=True))
@click.option("--line", "-l", type=int, required=True, help="1-based line number")
@click.option("--char", "-c", type=int, required=True, help="0-based character position")
def context_cmd(file: str, line: int, char: int):
    """Get context pack at a given position."""
    try:
        # Convert 1-based line to 0-based
        result = _get_context_pack(file, line - 1, char)
        click.echo(json.dumps(result, indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


@tool_cli.command("complete")
@click.argument("file", type=click.Path(exists=True))
@click.option("--line", "-l", type=int, required=True, help="1-based line number")
@click.option("--char", "-c", type=int, required=True, help="0-based character position")
def complete_cmd(file: str, line: int, char: int):
    """Get completions at a given position."""
    try:
        # Convert 1-based line to 0-based
        result = _get_completions(file, line - 1, char)
        click.echo(json.dumps(result, indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


@tool_cli.command("hover")
@click.argument("file", type=click.Path(exists=True))
@click.option("--line", "-l", type=int, required=True, help="1-based line number")
@click.option("--char", "-c", type=int, required=True, help="0-based character position")
def hover_cmd(file: str, line: int, char: int):
    """Get hover information at a given position."""
    try:
        # Convert 1-based line to 0-based
        result = _get_hover_info(file, line - 1, char)
        click.echo(json.dumps(result, indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


@tool_cli.command("symbols")
@click.argument("file", type=click.Path(exists=True))
def symbols_cmd(file: str):
    """Get document symbols for a file."""
    try:
        symbols = _get_document_symbols(file)
        click.echo(json.dumps({"symbols": symbols}, indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


@tool_cli.command("definition")
@click.argument("file", type=click.Path(exists=True))
@click.option("--line", "-l", type=int, required=True, help="1-based line number")
@click.option("--char", "-c", type=int, required=True, help="0-based character position")
def definition_cmd(file: str, line: int, char: int):
    """Go to definition at a given position."""
    try:
        # Convert 1-based line to 0-based
        result = _get_definition(file, line - 1, char)
        click.echo(json.dumps(result, indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


@tool_cli.command("references")
@click.argument("file", type=click.Path(exists=True))
@click.option("--line", "-l", type=int, required=True, help="1-based line number")
@click.option("--char", "-c", type=int, required=True, help="0-based character position")
def references_cmd(file: str, line: int, char: int):
    """Find references at a given position."""
    try:
        # Convert 1-based line to 0-based
        result = _get_references(file, line - 1, char)
        click.echo(json.dumps(result, indent=2))
        sys.exit(0)
    except Exception as e:
        click.echo(json.dumps({"error": str(e)}), err=True)
        sys.exit(1)


if __name__ == "__main__":
    tool_cli()
