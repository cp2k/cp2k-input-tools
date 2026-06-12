"""Diagnostics Feedback Loop API for CP2K input validation.

This module provides a unified Python API and CLI for validating CP2K input
files and returning diagnostics in multiple formats.

The API integrates:
- Parser diagnostics (syntax errors)
- Lint diagnostics (style, schema violations)
- Type-check diagnostics (type validation, enum violations)
- Semantic validation diagnostics (physics/chemistry consistency)

Example:
    >>> from cp2k_input_tools.diagnostics_api import check, check_format
    >>> diagnostics = check("input.inp")
    >>> json_output = check_format("input.inp", format="json")
"""

import json
from pathlib import Path
from typing import Any, Dict, List

from .linter import lint
from .parser import CP2KInputParser
from .parser_errors import ParserError
from .rich_diagnostics import (
    DIAGNOSTIC_ENGINE_VERSION,
    agent_check_payload,
    serialize_diagnostics,
)
from .tokenizer import TokenizerError
from .typecheck import validate_text
from .validator import validate


def check(filepath: str, base_dir: str = ".") -> List[Dict[str, Any]]:
    """Check a CP2K input file and return diagnostics.

    This is the main Python API entry point. It parses the file,
    runs all validation checks (parser, lint, type-check, semantic),
    and returns a unified list of diagnostics.

    Args:
        filepath: Path to the CP2K input file
        base_dir: Base directory for resolving @INCLUDE directives

    Returns:
        List of diagnostic dictionaries with keys:
        - range: {"start": {"line", "character"}, "end": {"line", "character"}}
        - message: str
        - severity: str ("error" | "warning" | "information" | "hint")
        - code: str
        - source: str
        - diagnostic_engine: str
        - category: str
        - confidence: float
        - blocking: bool

    Example:
        >>> diagnostics = check("my_input.inp")
        >>> for d in diagnostics:
        ...     if d["severity"] == "error":
        ...         print(f"Error at line {d['range']['start']['line']}: {d['message']}")
    """
    filepath_obj = Path(filepath)

    if not filepath_obj.exists():
        return [
            {
                "diagnostic_engine": DIAGNOSTIC_ENGINE_VERSION,
                "code": "FILE_NOT_FOUND",
                "severity": "error",
                "category": "syntax",
                "confidence": 1.0,
                "source": "cp2k-diagnostics",
                "range": {
                    "start": {"line": 0, "character": 0},
                    "end": {"line": 0, "character": 0},
                },
                "software": "cp2k-lsp",
                "file_type": "cp2k-input",
                "path": filepath,
                "fix_hints": [],
                "blocking": True,
                "message": f"File not found: {filepath}",
            }
        ]

    # Read file content for lint/typecheck
    try:
        text_content = filepath_obj.read_text(encoding="utf-8")
    except Exception:
        text_content = ""

    # Collect all diagnostics
    all_diagnostics = []

    # 1. Parser diagnostics
    parser = CP2KInputParser(base_dir=base_dir)
    try:
        with open(filepath_obj, "r") as fhandle:
            tree = parser.parse(fhandle)
    except (TokenizerError, ParserError) as exc:
        # Convert parser error to diagnostic
        ctx = exc.args[1] if len(exc.args) > 1 else None
        if ctx:
            linenr = getattr(ctx, "linenr", 1) or 1
            colnr = getattr(ctx, "colnr", 0) or 0
            all_diagnostics.append(
                {
                    "diagnostic_engine": DIAGNOSTIC_ENGINE_VERSION,
                    "code": f"PARSER_{type(exc).__name__}",
                    "severity": "error",
                    "category": "syntax",
                    "confidence": 1.0,
                    "source": "cp2k-parser",
                    "range": {
                        "start": {"line": max(linenr - 1, 0), "character": colnr},
                        "end": {"line": max(linenr - 1, 0), "character": max(colnr + 1, 1)},
                    },
                    "software": "cp2k-lsp",
                    "file_type": "cp2k-input",
                    "path": filepath,
                    "fix_hints": [],
                    "blocking": True,
                    "message": str(exc.args[0]),
                }
            )
        else:
            all_diagnostics.append(
                {
                    "diagnostic_engine": DIAGNOSTIC_ENGINE_VERSION,
                    "code": f"PARSER_{type(exc).__name__}",
                    "severity": "error",
                    "category": "syntax",
                    "confidence": 1.0,
                    "source": "cp2k-parser",
                    "range": {
                        "start": {"line": 0, "character": 0},
                        "end": {"line": 0, "character": 0},
                    },
                    "software": "cp2k-lsp",
                    "file_type": "cp2k-input",
                    "path": filepath,
                    "fix_hints": [],
                    "blocking": True,
                    "message": str(exc.args[0]),
                }
            )
        # Parser error means we can't continue with other checks
        return all_diagnostics

    # 2. Lint diagnostics
    try:
        lint_diags = lint(text_content)
        if lint_diags:
            serialized = serialize_diagnostics(
                lint_diags,
                software="cp2k-lsp",
                path=filepath,
                file_type="cp2k-input",
            )
            all_diagnostics.extend(serialized)
    except Exception:
        # Lint failures shouldn't break the entire check
        pass

    # 3. Type-check diagnostics
    try:
        typecheck_diags = validate_text(text_content)
        if typecheck_diags:
            serialized = serialize_diagnostics(
                typecheck_diags,
                software="cp2k-lsp",
                path=filepath,
                file_type="cp2k-input",
            )
            all_diagnostics.extend(serialized)
    except Exception:
        # Type-check failures shouldn't break the entire check
        pass

    # 4. Semantic validation diagnostics
    try:
        validation_result = validate(tree)
        if validation_result and validation_result.diagnostics:
            serialized = serialize_diagnostics(
                validation_result.diagnostics,
                software="cp2k-lsp",
                path=filepath,
                file_type="cp2k-input",
            )
            all_diagnostics.extend(serialized)
    except Exception:
        # Validation failures shouldn't break the entire check
        pass

    return all_diagnostics


def check_format(filepath: str, format: str = "json", base_dir: str = ".") -> str:
    """Check a CP2K input file and return formatted output.

    Args:
        filepath: Path to the CP2K input file
        format: Output format - "json" or "text"
        base_dir: Base directory for resolving @INCLUDE directives

    Returns:
        Formatted string representation of diagnostics

    Raises:
        ValueError: If format is not supported

    Example:
        >>> json_output = check_format("input.inp", format="json")
        >>> text_output = check_format("input.inp", format="text")
    """
    if format not in ("json", "text"):
        raise ValueError(f"Unsupported format: {format}. Use 'json' or 'text'.")

    diagnostics = check(filepath, base_dir=base_dir)

    # Convert to proper URI format
    filepath_obj = Path(filepath).resolve()
    uri = filepath_obj.as_uri()

    if format == "json":
        payload = agent_check_payload(
            software="cp2k-lsp",
            uri=uri,
            operation="check",
            diagnostics=diagnostics,
            path=filepath,
            file_type="cp2k-input",
        )
        return json.dumps(payload, indent=2)

    # Text format
    lines = []
    lines.append(f"CP2K Input Diagnostics: {filepath}")
    lines.append("")

    if not diagnostics:
        lines.append("✓ No diagnostics found")
    else:
        errors = [d for d in diagnostics if d.get("severity") == "error"]
        warnings = [d for d in diagnostics if d.get("severity") == "warning"]

        lines.append(f"Total: {len(diagnostics)} diagnostic(s)")
        lines.append(f"  Errors: {len(errors)}")
        lines.append(f"  Warnings: {len(warnings)}")
        lines.append("")

        for diag in diagnostics:
            severity = diag.get("severity", "information").upper()
            source = diag.get("source", "unknown")
            code = diag.get("code", "unknown")
            message = diag.get("message", "")
            range_info = diag.get("range", {})
            line = range_info.get("start", {}).get("line", 0) + 1
            col = range_info.get("start", {}).get("character", 0)

            lines.append(f"[{severity}] {source}:{code} at line {line}, col {col}")
            lines.append(f"  {message}")

    return "\n".join(lines)
