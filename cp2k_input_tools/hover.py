"""
CP2K input hover provider for LSP.

This module provides semantic hover information for CP2K input files using the
schema index and cursor context resolution.

Features:
- Hover on keyword: type, default, enum values, description
- Hover on section: description, child sections count
- Hover on enum value: parent keyword + value description

TDD: Implementation written to pass tests in tests/test_hover.py
"""

from typing import Optional

from lsprotocol.types import Hover, Position

from .cursor_context import resolve_cursor_context
from .schema_index import CP2KSchemaIndex, get_schema_index


def get_hover(
    text: str,
    position: Position,
    uri: str,
) -> Optional[Hover]:
    """Get hover information for the given cursor position.

    Args:
        text: The full text of the CP2K input file
        position: The cursor position (line, character)
        uri: The file URI

    Returns:
        Hover with content string, or None if no hover info available
    """
    # Resolve cursor context
    ctx = resolve_cursor_context(text, line=position.line, character=position.character, uri=uri)

    # Get schema index
    schema = get_schema_index()

    # Determine what we're hovering over and build appropriate content
    content = None

    # Hover on section start (e.g., &FORCE_EVAL)
    if ctx.is_section_start:
        # section_path already includes the current section
        content = _format_section_hover(schema, ctx.section_path)

    # Hover on keyword (e.g., METHOD)
    elif ctx.current_keyword:
        section_path = ctx.section_path if ctx.section_path else (ctx.current_section,)
        content = _format_keyword_hover(schema, section_path, ctx.current_keyword)

    # Hover on value (e.g., QS for METHOD)
    elif ctx.is_value_position and ctx.current_keyword:
        section_path = ctx.section_path if ctx.section_path else (ctx.current_section,)
        content = _format_value_hover(schema, section_path, ctx.current_keyword, ctx.prefix)

    if not content:
        return None

    return Hover(contents=content)


def _format_section_hover(schema: CP2KSchemaIndex, section_path: tuple) -> Optional[str]:
    """Format hover content for a section."""
    section_spec = schema.get_section(section_path)

    if not section_spec:
        return None

    parts = [f"**{section_spec.name}**"]

    if section_spec.description:
        parts.append(f"\n{section_spec.description}")

    # Add child sections count
    child_sections = schema.get_child_sections(section_path)
    if child_sections:
        parts.append(f"\n\nChild sections: {len(child_sections)}")

    return "\n".join(parts)


def _format_keyword_hover(schema: CP2KSchemaIndex, section_path: tuple, keyword_name: str) -> Optional[str]:
    """Format hover content for a keyword."""
    keyword_spec = schema.get_keyword(section_path, keyword_name)

    if not keyword_spec:
        # Unknown keyword - don't show hover
        return None

    parts = [f"**{keyword_spec.name}**"]

    # Add type and default
    info_parts = []
    if keyword_spec.variable_type:
        info_parts.append(f"Type: {keyword_spec.variable_type}")
    if keyword_spec.default_value:
        info_parts.append(f"Default: {keyword_spec.default_value}")

    if info_parts:
        parts.append(" | ".join(info_parts))

    # Add enum values if present
    if keyword_spec.enumeration_values:
        parts.append(f"\nValid values: {', '.join(keyword_spec.enumeration_values[:10])}")
        if len(keyword_spec.enumeration_values) > 10:
            parts.append(f" ({len(keyword_spec.enumeration_values)} total)")

    # Add description
    if keyword_spec.description:
        parts.append(f"\n{keyword_spec.description}")

    return "\n".join(parts)


def _format_value_hover(schema: CP2KSchemaIndex, section_path: tuple, keyword_name: str, value_prefix: str) -> Optional[str]:
    """Format hover content for a value (enum or logical)."""
    keyword_spec = schema.get_keyword(section_path, keyword_name)

    if not keyword_spec:
        return None

    # Get the actual value from the prefix
    value = value_prefix.strip().upper()

    parts = []

    # Check if it's an enum value
    if keyword_spec.enumeration_values:
        for enum_val in keyword_spec.enumeration_values:
            if enum_val.upper().startswith(value) or value.startswith(enum_val.upper()):
                parts.append(f"**{enum_val}**")
                parts.append(f"Valid value for `{keyword_spec.name}`")
                if keyword_spec.description:
                    parts.append(f"\n{keyword_spec.description}")
                return "\n".join(parts) if parts else None

    # Check if it's a logical value
    elif keyword_spec.variable_type and "logical" in keyword_spec.variable_type.lower():
        if value in ("F", ".FALSE.", "T", ".TRUE."):
            parts.append(f"**{value}**")
            parts.append(f"Logical value for `{keyword_spec.name}`")
            return "\n".join(parts) if parts else None

    return None
