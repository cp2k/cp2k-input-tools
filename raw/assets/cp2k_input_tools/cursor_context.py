"""
CP2K input cursor context resolver for LSP autocomplete.

This module provides cursor position context for CP2K input files,
including section path resolution, keyword detection, and prefix extraction.

Key features:
- Stack-based section path resolution
- Tolerant of incomplete documents (unclosed sections, partial edits)
- Works with preprocessor lines (@SET, @IF, @ENDIF)
- Recognizes CP2K's whitespace assignment style: KEYWORD VALUE
- Uses CP2KSchemaIndex for section validation

TDD: Implementation written to pass tests in tests/test_cursor_context.py
"""
import re
from dataclasses import dataclass
from typing import Optional, Tuple


# Regular expressions for CP2K input parsing
_SECTION_START = re.compile(r"^\s*&\s*([A-Za-z_][A-Za-z0-9_-]*)\b")
_SECTION_END = re.compile(r"^\s*&\s*END\b\s*([A-Za-z_][A-Za-z0-9_-]*)?", re.IGNORECASE)
_KEYWORD_LINE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_-]*)\s+(.*)")
_PREPROCESSOR_LINE = re.compile(r"^\s*@")


@dataclass(frozen=True)
class CursorContext:
    """Context information for a cursor position in a CP2K input file.

    Attributes:
        uri: The file URI
        line: 0-based line number of the cursor
        character: 0-based character offset of the cursor
        section_path: Tuple of section names from root to current section
        current_section: Name of the innermost open section at cursor line
        current_keyword: Name of keyword on cursor line (if any)
        is_section_start: True if cursor is after & starting a new section
        is_section_end: True if cursor is on &END line
        is_keyword_position: True if cursor is on a keyword name
        is_value_position: True if cursor is after a keyword expecting a value
        prefix: Text extracted from current value for autocomplete matching
    """

    uri: str
    line: int
    character: int
    section_path: Tuple[str, ...] = ()
    current_section: Optional[str] = None
    current_keyword: Optional[str] = None
    is_section_start: bool = False
    is_section_end: bool = False
    is_keyword_position: bool = False
    is_value_position: bool = False
    prefix: str = ""


def resolve_cursor_context(
    text: str,
    line: int,
    character: int,
    uri: str,
) -> CursorContext:
    """Resolve cursor context for a CP2K input file.

    This function tracks the section stack up to the cursor line and
    determines whether the cursor is at a section start, section end,
    keyword position, or value position.

    Args:
        text: The full text of the CP2K input file
        line: 0-based line number of the cursor
        character: 0-based character offset of the cursor
        uri: The file URI

    Returns:
        CursorContext with all context information populated
    """
    lines = text.splitlines() if text else []
    cursor_line = lines[line] if 0 <= line < len(lines) else ""

    # Track section stack as we process lines
    section_stack: list[str] = []
    current_section: Optional[str] = None
    current_keyword: Optional[str] = None
    is_section_start = False
    is_section_end = False
    is_keyword_position = False
    is_value_position = False
    prefix = ""

    # Process lines up to (and including) the cursor line
    for line_num in range(line + 1):
        if line_num >= len(lines):
            break

        current_line = lines[line_num]

        # Skip preprocessor lines (@SET, @IF, etc.) - they don't affect section stack
        if _PREPROCESSOR_LINE.match(current_line):
            continue

        # Check for section end (&END or &END SECTION_NAME) - check BEFORE section start
        # This ensures "END" isn't treated as a section name
        end_match = _SECTION_END.match(current_line)
        if end_match:
            if line_num == line:
                is_section_end = True
            # Pop the most recent section if stack is not empty
            if section_stack:
                closed_section = section_stack.pop()
                # Verify section name match if provided
                end_section_name = end_match.group(1)
                if end_section_name:
                    if end_section_name.upper() == closed_section:
                        current_section = closed_section
                    # Even if names don't match (tolerant parsing), we pop
                else:
                    current_section = closed_section
            # Update current_section to parent
            current_section = section_stack[-1] if section_stack else None
            continue

        # Check for section start (&SECTION_NAME)
        section_match = _SECTION_START.match(current_line)
        if section_match:
            section_name = section_match.group(1).upper()
            section_stack.append(section_name)
            current_section = section_name
            # Check if cursor is on this line
            if line_num == line:
                cursor_on_line = current_line
                ampersand_pos = cursor_on_line.find("&")
                if ampersand_pos >= 0 and character > ampersand_pos:
                    # Cursor is after &, might be in section name or parameter
                    is_section_start = True
            continue

        # Check for keyword line (KEYWORD VALUE)
        keyword_match = _KEYWORD_LINE.match(current_line)
        if keyword_match and not current_line.strip().startswith("!"):
            keyword_name = keyword_match.group(1).upper()
            keyword_value = keyword_match.group(2)

            # Strip comment from value
            if "!" in keyword_value:
                keyword_value = keyword_value.split("!", 1)[0]

            # Check if cursor is on this line
            if line_num == line:
                current_keyword = keyword_name

                # Determine cursor position relative to keyword
                keyword_start = current_line.find(keyword_name)
                keyword_end = keyword_start + len(keyword_name)

                # Check if cursor is in a comment first
                comment_pos = current_line.find("!")
                in_comment = comment_pos >= 0 and character >= comment_pos

                if in_comment:
                    # Cursor is in comment - not a keyword or value position
                    is_keyword_position = False
                    is_value_position = False
                elif character <= keyword_start:
                    # Cursor is before the keyword
                    is_keyword_position = False
                    is_value_position = False
                elif character <= keyword_end:
                    # Cursor is on the keyword name
                    is_keyword_position = True
                    is_value_position = False
                else:
                    # Cursor is after the keyword (in value or space)
                    is_value_position = True
                    is_keyword_position = False
                    # Extract prefix up to cursor position
                    value_text = current_line[keyword_end:character]
                    # Remove leading whitespace
                    value_text = value_text.lstrip()
                    # Remove trailing whitespace
                    value_text = value_text.rstrip()
                    prefix = value_text

    # Build section path from stack
    section_path = tuple(section_stack)

    return CursorContext(
        uri=uri,
        line=line,
        character=character,
        section_path=section_path,
        current_section=current_section,
        current_keyword=current_keyword,
        is_section_start=is_section_start,
        is_section_end=is_section_end,
        is_keyword_position=is_keyword_position,
        is_value_position=is_value_position,
        prefix=prefix,
    )
