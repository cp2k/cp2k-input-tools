"""
CP2K input completion provider for LSP.

This module provides semantic completion for CP2K input files using the
schema index and cursor context resolution.

Features:
- Section completion (after &)
- Keyword completion (inside sections)
- Enum value completion (for enumerated keywords)
- Logical value completion (for boolean/flag keywords)

TDD: Implementation written to pass tests in tests/test_lsp.py and tests/test_preprocessor_lsp.py
"""

from typing import List, Optional

from lsprotocol.types import (
    CompletionItem,
    CompletionItemKind,
    CompletionList,
    Position,
)

from .cursor_context import CursorContext, resolve_cursor_context
from .schema_index import CP2KSchemaIndex, get_schema_index


def get_completions(
    text: str,
    position: Position,
    uri: str,
) -> Optional[CompletionList]:
    """Get completion items for the given cursor position.

    Args:
        text: The full text of the CP2K input file
        position: The cursor position (line, character)
        uri: The file URI

    Returns:
        CompletionList with appropriate items, or None if no completions available
    """
    # Resolve cursor context
    ctx = resolve_cursor_context(text, line=position.line, character=position.character, uri=uri)

    # Get schema index
    schema = get_schema_index()

    items: List[CompletionItem] = []

    # Section completion (after &)
    if ctx.is_section_start:
        items.extend(_complete_sections(schema, ctx))

    # Keyword completion (inside section)
    elif ctx.current_section and not ctx.is_section_end:
        if ctx.is_value_position:
            # Value completion (enums, logical values)
            items.extend(_complete_values(schema, ctx))
        elif ctx.is_keyword_position:
            # Keyword completion
            items.extend(_complete_keywords(schema, ctx))
        else:
            # Default to keyword completion for empty lines in sections
            items.extend(_complete_keywords(schema, ctx))

    if not items:
        return None

    return CompletionList(
        is_incomplete=False,
        items=items,
    )


def _complete_sections(schema: CP2KSchemaIndex, ctx: CursorContext) -> List[CompletionItem]:
    """Get section completions for the current context.

    Returns child sections of the current section path, or root sections
    if at top level.
    """
    # Get prefix from section name after &
    prefix = ""
    if ctx.is_section_start:
        # Extract section name prefix from cursor line
        line_text = ctx.line if ctx.line else ""
        ampersand_pos = line_text.find("&")
        if ampersand_pos >= 0:
            prefix = line_text[ampersand_pos + 1 : ctx.character].strip().upper()

    # Get available sections
    if ctx.section_path:
        # Get child sections of current section
        section_spec = schema.get_section(ctx.section_path)
        if section_spec:
            child_sections = schema.get_child_sections(ctx.section_path)
        else:
            # Current section not found in schema, return empty
            child_sections = []
    else:
        # Get root sections
        child_sections = schema.get_root_sections()

    # Filter and create completion items
    items = []
    for section_name in child_sections:
        if not prefix or section_name.upper().startswith(prefix):
            section_detail = schema.get_section(ctx.section_path + (section_name,) if ctx.section_path else (section_name,))
            items.append(
                CompletionItem(
                    label=section_name,
                    kind=CompletionItemKind.Module,
                    detail=section_detail.description if section_detail else "",
                    insert_text=section_name,
                )
            )

    return items


def _complete_keywords(schema: CP2KSchemaIndex, ctx: CursorContext) -> List[CompletionItem]:
    """Get keyword completions for the current section."""
    if not ctx.current_section:
        return []

    # Get prefix for filtering
    prefix = ctx.prefix.upper()

    # Get keywords for current section
    section_path = ctx.section_path if ctx.section_path else (ctx.current_section,)
    keywords = schema.get_keywords(section_path)

    items = []
    for keyword_name, keyword_spec in keywords.items():
        if not prefix or keyword_name.upper().startswith(prefix):
            # Build detail text with type and default
            detail_parts = []
            if keyword_spec.variable_type:
                detail_parts.append(keyword_spec.variable_type)
            if keyword_spec.default_value:
                detail_parts.append(f"default: {keyword_spec.default_value}")
            detail = " | ".join(detail_parts) if detail_parts else keyword_spec.description or ""

            items.append(
                CompletionItem(
                    label=keyword_name,
                    kind=CompletionItemKind.Field,
                    detail=detail,
                    documentation=keyword_spec.description,
                    insert_text=keyword_name,
                )
            )

    return items


def _complete_values(schema: CP2KSchemaIndex, ctx: CursorContext) -> List[CompletionItem]:
    """Get value completions for the current keyword."""
    if not ctx.current_keyword:
        return []

    section_path = ctx.section_path if ctx.section_path else (ctx.current_section,)
    keyword_spec = schema.get_keyword(section_path, ctx.current_keyword)

    if not keyword_spec:
        return []

    items = []
    prefix = ctx.prefix.upper()

    # Enum completion
    if keyword_spec.enumeration_values:
        for value in keyword_spec.enumeration_values:
            if not prefix or value.upper().startswith(prefix):
                items.append(
                    CompletionItem(
                        label=value,
                        kind=CompletionItemKind.EnumMember,
                        detail=f"Enum value for {ctx.current_keyword}",
                    )
                )

    # Logical value completion (for boolean/flag keywords)
    elif keyword_spec.variable_type and "logical" in keyword_spec.variable_type.lower():
        for value in ["F", ".FALSE.", "T", ".TRUE."]:
            if not prefix or value.upper().startswith(prefix):
                items.append(
                    CompletionItem(
                        label=value,
                        kind=CompletionItemKind.Value,
                        detail="Logical value",
                    )
                )

    return items
