"""Diagnostic Engine v1 serialization helpers.

The module keeps existing LSP/provider diagnostics untouched and provides a
python-lsp-server-style provider boundary for agent-facing JSON consumers.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import Any, Iterable

DIAGNOSTIC_ENGINE_VERSION = "1.0"
DIAGNOSTIC_CATEGORIES = (
    "syntax",
    "schema",
    "type/value",
    "cross-file reference",
    "semantic consistency",
    "preflight/runtime-risk",
    "style/deprecation",
)

_SEVERITY_LABELS = {
    1: "error",
    2: "warning",
    3: "information",
    4: "hint",
    "1": "error",
    "2": "warning",
    "3": "information",
    "4": "hint",
    "Error": "error",
    "Warning": "warning",
    "Information": "information",
    "Hint": "hint",
    "error": "error",
    "warning": "warning",
    "info": "information",
    "information": "information",
    "hint": "hint",
}


def severity_label(value: Any) -> str:
    """Return a stable lowercase severity label."""
    raw = getattr(value, "value", value)
    return _SEVERITY_LABELS.get(raw, _SEVERITY_LABELS.get(str(raw), "information"))


def infer_category(code: Any = None, message: str = "", source: str = "") -> str:
    """Infer the Diagnostic Engine v1 category from legacy diagnostic fields."""
    text = f"{code or ''} {message} {source}".lower()
    if any(token in text for token in ("syntax", "parse", "parser", "token", "utf-8")):
        return "syntax"
    if any(token in text for token in ("unknown", "keyword", "section", "schema", "required")):
        return "schema"
    if any(token in text for token in ("type", "enum", "value", "integer", "float", "logical")):
        return "type/value"
    if any(token in text for token in ("file", "path", "include", "basis", "pseudo", "potcar", "reference")):
        return "cross-file reference"
    if any(token in text for token in ("deprecated", "style", "format", "indent")):
        return "style/deprecation"
    if any(token in text for token in ("cutoff", "scf", "memory", "parallel", "runtime", "preflight")):
        return "preflight/runtime-risk"
    return "semantic consistency"


def _get_attr_or_item(obj: Any, key: str, default: Any = None) -> Any:
    if isinstance(obj, dict):
        return obj.get(key, default)
    return getattr(obj, key, default)


def _legacy_payload(obj: Any) -> dict[str, Any]:
    if isinstance(obj, dict):
        return dict(obj)
    if is_dataclass(obj):
        return asdict(obj)  # type: ignore[arg-type]
    to_json = getattr(obj, "to_json", None)
    if callable(to_json):
        try:
            value = to_json()
            if isinstance(value, dict):
                return dict(value)
        except TypeError:
            pass
    return {}


def _range_from_legacy(obj: Any, legacy: dict[str, Any]) -> dict[str, dict[str, int]]:
    lsp_range = _get_attr_or_item(obj, "range")
    if lsp_range is not None:
        start = getattr(lsp_range, "start", None)
        end = getattr(lsp_range, "end", None)
        if start is not None and end is not None:
            return {
                "start": {
                    "line": int(getattr(start, "line", 0) or 0),
                    "character": int(getattr(start, "character", 0) or 0),
                },
                "end": {
                    "line": int(getattr(end, "line", 0) or 0),
                    "character": int(getattr(end, "character", 0) or 0),
                },
            }
    raw_range = legacy.get("range")
    if isinstance(raw_range, dict):
        if "start" in raw_range and "end" in raw_range:
            return raw_range
        if {"start_line", "start_col", "end_line", "end_col"} <= set(raw_range):
            return {
                "start": {
                    "line": int(raw_range["start_line"]),
                    "character": int(raw_range["start_col"]),
                },
                "end": {
                    "line": int(raw_range["end_line"]),
                    "character": int(raw_range["end_col"]),
                },
            }
    line = int(legacy.get("line", 1) or 1)
    column = int(legacy.get("column", legacy.get("col", 1)) or 1)
    line0 = max(line - 1, 0)
    col0 = max(column - 1, 0)
    end_col = int(legacy.get("end_col", col0 + 1) or (col0 + 1))
    return {
        "start": {"line": line0, "character": col0},
        "end": {"line": line0, "character": max(end_col, col0 + 1)},
    }


def diagnostic_to_dict(
    diagnostic: Any,
    *,
    software: str,
    path: str = "",
    file_type: str = "",
) -> dict[str, Any]:
    """Convert legacy/dataclass/LSP diagnostics into the rich v1 contract."""
    legacy = _legacy_payload(diagnostic)
    code = legacy.get("code", _get_attr_or_item(diagnostic, "code", "diagnostic"))
    source = legacy.get("source", _get_attr_or_item(diagnostic, "source", f"{software}-lsp"))
    message = legacy.get("message", _get_attr_or_item(diagnostic, "message", ""))
    severity = severity_label(legacy.get("severity", _get_attr_or_item(diagnostic, "severity", None)))
    confidence = float(legacy.get("confidence", 1.0) or 1.0)
    category = legacy.get("category") or infer_category(code, message, source)
    fix_hints = legacy.get("fix_hints")
    if fix_hints is None:
        suggested_fix = legacy.get("suggested_fix")
        fix_hints = [] if suggested_fix is None else [suggested_fix]
    blocking = bool(legacy.get("blocking", severity == "error" and confidence >= 0.8))
    return {
        "diagnostic_engine": DIAGNOSTIC_ENGINE_VERSION,
        "code": str(code or "diagnostic"),
        "severity": severity,
        "category": category,
        "confidence": confidence,
        "source": str(source or f"{software}-lsp"),
        "range": _range_from_legacy(diagnostic, legacy),
        "software": software,
        "file_type": file_type,
        "path": str(legacy.get("file", path)),
        "expected": legacy.get("expected"),
        "actual": legacy.get("actual"),
        "manual_ref": legacy.get("manual_ref"),
        "fix_hints": fix_hints,
        "blocking": blocking,
        "message": str(message or ""),
    }


def serialize_diagnostics(
    diagnostics: Iterable[Any],
    *,
    software: str,
    path: str = "",
    file_type: str = "",
) -> list[dict[str, Any]]:
    """Serialize diagnostics deterministically for agents and snapshots."""
    items = [diagnostic_to_dict(item, software=software, path=path, file_type=file_type) for item in diagnostics]
    return sorted(
        items,
        key=lambda item: (
            item["range"]["start"]["line"],
            item["range"]["start"]["character"],
            item["code"],
            item["message"],
        ),
    )


def agent_check_payload(
    *,
    software: str,
    uri: str,
    operation: str = "check",
    version: str = DIAGNOSTIC_ENGINE_VERSION,
    diagnostics: Iterable[Any] = (),
    path: str = "",
    file_type: str = "",
) -> dict[str, Any]:
    """Build the stable top-level JSON payload returned by *-lsp-tool."""
    items = serialize_diagnostics(
        diagnostics,
        software=software,
        path=path,
        file_type=file_type,
    )
    blocking_count = sum(1 for item in items if item["blocking"])
    return {
        "uri": uri,
        "operation": operation,
        "ok": blocking_count == 0,
        "version": version,
        "software": software,
        "diagnostic_engine": version,
        "diagnostics": items,
        "summary": {
            "count": len(items),
            "blocking": blocking_count,
            "errors": sum(1 for item in items if item["severity"] == "error"),
            "warnings": sum(1 for item in items if item["severity"] == "warning"),
        },
    }
