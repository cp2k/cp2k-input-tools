"""Safe rename functionality for CP2K input files."""

import re
from typing import Optional, Tuple

from lsprotocol.types import Position, Range, TextDocumentEdit, TextEdit, WorkspaceEdit

# =============================================================================
# Token patterns
# =============================================================================

_SECTION_PATTERN = re.compile(r"&(?P<name>[\w\-_]+)\s*(?P<param>.*)")
_KEYWORD_PATTERN = re.compile(r"(?P<name>[\w\-_]+)\s*(?P<value>.*)")
_VARIABLE_PATTERN = re.compile(r"@\{(?P<name>[\w\-_]+)\}")

# =============================================================================
# Context detection
# =============================================================================


def _get_line_context(text: str, line: int) -> Tuple[str, str, str]:
    """Get the line, previous line, and next line for context."""
    lines = text.split("\n")
    current_line = lines[line] if 0 <= line < len(lines) else ""
    prev_line = lines[line - 1] if line > 0 else ""
    next_line = lines[line + 1] if line < len(lines) - 1 else ""
    return current_line, prev_line, next_line


def _is_in_comment(line: str, char_pos: int) -> bool:
    """Check if position is within a comment."""
    # Find comment character
    comment_pos = line.find("#")
    if comment_pos == -1:
        comment_pos = line.find("!")

    return comment_pos != -1 and char_pos >= comment_pos


def _is_section_keyword(line: str, char_pos: int) -> bool:
    """Check if position is on a section keyword."""
    # Match section start pattern
    match = _SECTION_PATTERN.match(line.strip())
    if match:
        name = match.group("name")
        name_start = line.find(name)
        name_end = name_start + len(name)
        return name_start <= char_pos <= name_end
    return False


def _is_section_parameter(line: str, char_pos: int) -> bool:
    """Check if position is on a section parameter."""
    match = _SECTION_PATTERN.match(line.strip())
    if match:
        name = match.group("name")
        param = match.group("param").strip()
        if param:
            # Find parameter position
            name_end = line.find(name) + len(name)
            param_start = line.find(param, name_end)
            if param_start != -1:
                param_end = param_start + len(param)
                return param_start <= char_pos <= param_end
    return False


def _is_keyword_name(line: str, char_pos: int) -> bool:
    """Check if position is on a keyword name."""
    match = _KEYWORD_PATTERN.match(line.strip())
    if match:
        name = match.group("name")
        name_start = line.find(name)
        name_end = name_start + len(name)
        return name_start <= char_pos <= name_end
    return False


def _is_variable(line: str, char_pos: int) -> bool:
    """Check if position is on a variable reference."""
    match = _VARIABLE_PATTERN.search(line)
    if match:
        name = match.group("name")
        # Check if position is within the variable name (excluding @ and {})
        name_start = match.start("name")
        name_end = match.end("name")
        return name_start <= char_pos <= name_end
    return False


def _is_keyword_value(line: str, char_pos: int) -> bool:
    """Check if position is on a keyword value."""
    match = _KEYWORD_PATTERN.match(line.strip())
    if match:
        name = match.group("name")
        value = match.group("value").strip()
        if value:
            name_end = line.find(name) + len(name)
            value_start = line.find(value, name_end)
            if value_start != -1:
                value_end = value_start + len(value)
                return value_start <= char_pos <= value_end
    return False


# =============================================================================
# Public API
# =============================================================================


def can_rename(text: str, position: Position, uri: str) -> bool:
    """
    Check if rename is safe at the given position.

    Args:
        text: Full document text
        position: Position to check
        uri: Document URI

    Returns:
        True if rename is safe, False otherwise
    """
    line_num = position.line
    char_num = position.character

    # Validate position
    lines = text.split("\n")
    if line_num < 0 or line_num >= len(lines):
        return False

    current_line, prev_line, next_line = _get_line_context(text, line_num)

    # Check if in comment
    if _is_in_comment(current_line, char_num):
        return False

    # Check if on section keyword (schema-defined)
    if _is_section_keyword(current_line, char_num):
        return False  # Section keywords are schema-defined

    # Check if on keyword name (schema-defined)
    if _is_keyword_name(current_line, char_num):
        return False  # Keyword names are schema-defined

    # Check if on variable reference BEFORE checking keyword values
    # This prevents variables from being treated as keyword values
    if _is_variable(current_line, char_num):
        return True

    # Check if on keyword value (enum values are schema-defined)
    if _is_keyword_value(current_line, char_num):
        return False  # Keyword values (enums) are schema-defined

    # Section parameters are allowed
    if _is_section_parameter(current_line, char_num):
        return True

    return False


def get_rename_edit(text: str, position: Position, uri: str, new_name: str) -> Optional[WorkspaceEdit]:
    """
    Get workspace edit for safe rename operation.

    Args:
        text: Full document text
        position: Position of the identifier to rename
        uri: Document URI
        new_name: New name for the identifier

    Returns:
        WorkspaceEdit if rename is safe, None otherwise
    """
    if not can_rename(text, position, uri):
        return None

    line_num = position.line
    char_num = position.character

    current_line, prev_line, next_line = _get_line_context(text, line_num)

    # Determine what we're renaming
    if _is_variable(current_line, char_num):
        return _rename_variable(text, position, uri, new_name)

    if _is_section_parameter(current_line, char_num):
        return _rename_section_parameter(text, line_num, char_num, uri, new_name)

    return None


def _rename_section_parameter(text: str, line_num: int, char_num: int, uri: str, new_name: str) -> WorkspaceEdit:
    """Create edit for renaming section parameter."""
    lines = text.split("\n")
    current_line = lines[line_num]

    # Find the parameter in the section declaration
    match = _SECTION_PATTERN.match(current_line.strip())
    if not match:
        return None

    param = match.group("param").strip()
    if not param:
        return None

    # Find parameter position
    name = match.group("name")
    name_end = current_line.find(name) + len(name)
    param_start = current_line.find(param, name_end)
    param_end = param_start + len(param)

    # Validate position is on the parameter
    if not (param_start <= char_num <= param_end):
        return None

    # Create the edit
    edit_range = Range(start=Position(line=line_num, character=param_start), end=Position(line=line_num, character=param_end))

    text_edit = TextEdit(range=edit_range, new_text=new_name)
    document_edit = TextDocumentEdit(text_document=dict(uri=uri, version=None), edits=[text_edit])

    return WorkspaceEdit(document_changes=[document_edit])


def _rename_variable(text: str, position: Position, uri: str, new_name: str) -> WorkspaceEdit:
    """Create edit for renaming variable (currently single-document only)."""
    lines = text.split("\n")
    line_num = position.line
    char_num = position.character
    current_line = lines[line_num]

    # Find variable reference
    match = _VARIABLE_PATTERN.search(current_line)
    if not match:
        return None

    var_name = match.group("name")

    # Validate position is within the variable name
    name_start = match.start("name")
    name_end = match.end("name")
    if not (name_start <= char_num <= name_end):
        return None

    # Find all occurrences of the variable in the document
    edits = []
    var_pattern = re.compile(r"@\{(?P<name>" + re.escape(var_name) + r")\}")

    for line_idx, line in enumerate(lines):
        for match in var_pattern.finditer(line):
            # Skip if in comment
            if _is_in_comment(line, match.start()):
                continue

            # Edit only the variable name (between @{ and })
            edit_range = Range(
                start=Position(line=line_idx, character=match.start("name")),
                end=Position(line=line_idx, character=match.end("name")),
            )
            text_edit = TextEdit(range=edit_range, new_text=new_name)
            edits.append(text_edit)

    if not edits:
        return None

    document_edit = TextDocumentEdit(text_document=dict(uri=uri, version=None), edits=edits)

    return WorkspaceEdit(document_changes=[document_edit])
