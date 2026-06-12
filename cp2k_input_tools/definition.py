"""Go-to-Definition & References implementation for CP2K LSP (#56).

Provides:
- get_definition(): Navigate to section/keyword definitions
- get_references(): Find all usages of sections/keywords in file
"""

import re
from typing import List, Optional

from lsprotocol.types import Location, Position, Range

# Try to import schema API; fall back to basic functionality if not available
try:
    from cp2k_lsp.agent_api.schema import (
        lookup_keyword_at_path,
        lookup_keyword_schema,
        lookup_section_schema,
    )

    SCHEMA_AVAILABLE = True
except ImportError:
    SCHEMA_AVAILABLE = False


# Pattern to match section definitions (exclude END)
_SECTION_PATTERN = re.compile(r"&(?!END)(?P<name>[\w\-_]+)(?:\s+(?P<param>.*))?", re.IGNORECASE)
# Pattern to match end sections
_END_SECTION_PATTERN = re.compile(r"&END\s*(?:(?P<name>[\w\-_]+))?", re.IGNORECASE)
# Pattern to match keywords (more restrictive - must start with letter, not numbers)
_KEYWORD_PATTERN = re.compile(r"(?P<name>[a-zA-Z][\w\-_]*)\s+(?P<value>.+)")
# Pattern to match variable references
_VARIABLE_PATTERN = re.compile(r"\$(?P<name>\w+)")
# Pattern to match variable definitions
_SET_PATTERN = re.compile(r"@SET\s+(?P<name>\w+)", re.IGNORECASE)


def resolve_cursor_context(text: str, position: Position) -> Optional[dict]:
    """Determine what's under the cursor.

    Returns a dict with:
        - type: 'section', 'end_section', 'keyword', 'variable', 'value', or None
        - name: the section/keyword/variable name
        - line: the line text
        - range: the range of the token
        - section_path: the section context (dot-separated path)
    """
    lines = text.split("\n")

    if position.line >= len(lines):
        return None

    line_text = lines[position.line]

    # Skip empty lines and comments
    if not line_text.strip() or line_text.strip().startswith("!") or line_text.strip().startswith("#"):
        return None

    # Check if cursor is on an end section FIRST (before section check)
    end_match = _END_SECTION_PATTERN.match(line_text.strip())
    if end_match:
        # For "&END" or "&END SECTION_NAME"
        # Calculate the range that covers "END" or "END NAME"
        start_char = line_text.find("&") + 1
        end_text = "END"
        if start_char + len(end_text) <= len(line_text):
            end_char = start_char + len(end_text)

            # Check if cursor is on "END"
            if start_char <= position.character <= end_char:
                name = end_match.group("name") or ""
                return {
                    "type": "end_section",
                    "name": name if name else None,
                    "line": line_text,
                    "range": Range(
                        start=Position(line=position.line, character=start_char),
                        end=Position(line=position.line, character=end_char),
                    ),
                    "section_path": None,
                }

            # Check if cursor is on the section name after "END"
            if name:
                name_upper = name.upper()
                name_start = line_text.upper().find(name_upper, start_char + len(end_text))
                if name_start != -1:
                    name_end = name_start + len(name)
                    if name_start <= position.character <= name_end:
                        return {
                            "type": "end_section",
                            "name": name,
                            "line": line_text,
                            "range": Range(
                                start=Position(line=position.line, character=name_start),
                                end=Position(line=position.line, character=name_end),
                            ),
                            "section_path": None,
                        }

    # Check if cursor is on a section definition
    section_match = _SECTION_PATTERN.match(line_text.strip())
    if section_match:
        name = section_match.group("name")
        # Calculate character position in original line (not stripped)
        start_char = line_text.find("&") + 1
        # Find the section name in the original line
        name_start = line_text.upper().find(name.upper(), start_char)
        if name_start == -1:
            name_start = start_char
        end_char = name_start + len(name)

        if start_char <= position.character <= end_char:
            return {
                "type": "section",
                "name": name,
                "line": line_text,
                "range": Range(
                    start=Position(line=position.line, character=name_start), end=Position(line=position.line, character=end_char)
                ),
                "section_path": None,  # Will be determined by parent context
            }

    # Check if cursor is on a keyword (only if it starts with a letter)
    keyword_match = _KEYWORD_PATTERN.match(line_text.strip())
    if keyword_match:
        name = keyword_match.group("name")
        # Find the keyword name in the original line
        start_char = line_text.upper().find(name.upper())
        if start_char != -1:
            end_char = start_char + len(name)

            # Only return if cursor is on the keyword name, not the value
            if start_char <= position.character <= end_char:
                return {
                    "type": "keyword",
                    "name": name,
                    "line": line_text,
                    "range": Range(
                        start=Position(line=position.line, character=start_char),
                        end=Position(line=position.line, character=end_char),
                    ),
                    "section_path": None,
                }

    # Check if cursor is on a variable reference
    for match in _VARIABLE_PATTERN.finditer(line_text):
        if match.start() <= position.character <= match.end():
            return {
                "type": "variable",
                "name": match.group("name"),
                "line": line_text,
                "range": Range(
                    start=Position(line=position.line, character=match.start()),
                    end=Position(line=position.line, character=match.end()),
                ),
                "section_path": None,
            }

    return None


def get_section_context(text: str, line_number: int) -> str:
    """Get the section context (dot-separated path) for a given line.

    Returns a dot-separated path like "FORCE_EVAL.DFT.SCF".
    """
    lines = text.split("\n")
    context = []

    for i in range(line_number + 1):  # Go up to the target line
        line = lines[i].strip()
        if not line:
            continue

        # Skip comment lines
        if line.startswith("!") or line.startswith("#"):
            continue

        # Check for section start (but not END)
        section_match = _SECTION_PATTERN.match(line)
        if section_match:
            section_name = section_match.group("name").upper()
            context.append(section_name)
            continue

        # Check for section end
        end_match = _END_SECTION_PATTERN.match(line)
        if end_match:
            named_end = end_match.group("name")
            if named_end:
                # Pop until we find the matching section
                target_section = named_end.upper()
                while context and context[-1] != target_section:
                    context.pop()
                if context:
                    context.pop()
            else:
                # Anonymous &END - pop the last section
                if context:
                    context.pop()
            continue

    return ".".join(context)


def find_matching_section_start(text: str, end_line: int, section_name: Optional[str]) -> Optional[int]:
    """Find the line number where a section starts, given its &END line.

    Args:
        text: The full document text
        end_line: Line number (0-indexed) of the &END
        section_name: Optional section name from &END SECTION_NAME

    Returns:
        Line number of the matching &SECTION, or None if not found
    """
    lines = text.split("\n")
    section_stack = []

    for i, line in enumerate(lines):
        line_stripped = line.strip()

        # Skip empty lines and comments
        if not line_stripped or line_stripped.startswith("!") or line_stripped.startswith("#"):
            continue

        # Check for section start
        section_match = _SECTION_PATTERN.match(line_stripped)
        if section_match:
            section_stack.append((i, section_match.group("name").upper()))
            continue

        # Check for section end
        end_match = _END_SECTION_PATTERN.match(line_stripped)
        if end_match:
            named_end = end_match.group("name")

            if i == end_line:
                # This is the &END we're looking for
                if section_name:
                    # Find matching start in stack
                    for start_i, start_name in reversed(section_stack):
                        if start_name == section_name.upper():
                            return start_i
                else:
                    # Anonymous &END - return the last opened section
                    if section_stack:
                        return section_stack[-1][0]
                return None

            # Update stack
            if named_end:
                target_section = named_end.upper()
                while section_stack and section_stack[-1][1] != target_section:
                    section_stack.pop()
                if section_stack:
                    section_stack.pop()
            else:
                if section_stack:
                    section_stack.pop()

    return None


def get_definition(text: str, position: Position, uri: str) -> Optional[Location]:
    """Get the definition location for the token at the given position.

    Args:
        text: The document text
        position: The cursor position (line and character are 0-indexed)
        uri: The document URI

    Returns:
        A Location object pointing to the definition, or None if not found
    """
    context = resolve_cursor_context(text, position)

    if context is None:
        return None

    if context["type"] == "section":
        # Section definitions are in the schema
        if SCHEMA_AVAILABLE:
            section_path = get_section_context(text, position.line)
            if section_path:
                # Check if this is a nested section
                parts = section_path.split(".")
                if len(parts) > 1 and context["name"] in parts[-1]:
                    # It's the current section
                    pass

            # For now, return a location pointing to the section itself
            # In the future, this could point to schema documentation
            return Location(uri=uri, range=context["range"])
        else:
            return Location(uri=uri, range=context["range"])

    elif context["type"] == "end_section":
        # &END should navigate to the matching &SECTION
        if context["name"]:
            # Named &END - find the matching start
            start_line = find_matching_section_start(text, position.line, context["name"])
        else:
            # Anonymous &END - find the last opened section
            start_line = find_matching_section_start(text, position.line, None)

        if start_line is not None:
            # Find the exact range of the section name
            lines = text.split("\n")
            start_line_text = lines[start_line]
            match = _SECTION_PATTERN.match(start_line_text.strip())
            if match:
                section_name = match.group("name")
                start_char = start_line_text.find("&") + 1
                # Find the section name in the original line
                name_start = start_line_text.upper().find(section_name.upper(), start_char)
                if name_start == -1:
                    name_start = start_char
                end_char = name_start + len(section_name)

                return Location(
                    uri=uri,
                    range=Range(
                        start=Position(line=start_line, character=name_start), end=Position(line=start_line, character=end_char)
                    ),
                )

        return None

    elif context["type"] == "keyword":
        # Keyword definitions are in the schema
        # For now, point to the keyword itself
        # In the future, this could open schema documentation
        return Location(uri=uri, range=context["range"])

    elif context["type"] == "variable":
        # Variable definitions start with @SET
        lines = text.split("\n")
        var_name = context["name"]

        for i, line in enumerate(lines):
            set_match = _SET_PATTERN.match(line.strip())
            if set_match and set_match.group("name").upper() == var_name.upper():
                # Found the @SET definition
                # Find the variable name in the original line
                start_char = line.upper().find(var_name.upper())
                if start_char == -1:
                    continue
                end_char = start_char + len(var_name)

                return Location(
                    uri=uri, range=Range(start=Position(line=i, character=start_char), end=Position(line=i, character=end_char))
                )

        return None

    return None


def find_section_references(text: str, section_name: str, uri: str) -> List[Location]:
    """Find all references to a section name in the document.

    Args:
        text: The document text
        section_name: The section name to search for (case-insensitive)
        uri: The document URI

    Returns:
        List of Location objects for each reference
    """
    references = []
    lines = text.split("\n")
    section_name_upper = section_name.upper()

    for i, line in enumerate(lines):
        line_stripped = line.strip()

        # Skip empty lines and comments
        if not line_stripped or line_stripped.startswith("!") or line_stripped.startswith("#"):
            continue

        # Check for section definition
        section_match = _SECTION_PATTERN.match(line_stripped)
        if section_match:
            matched_name = section_match.group("name").upper()
            if matched_name == section_name_upper:
                start_char = line.find("&") + 1
                # Find the section name in the original line
                name_start = line.upper().find(matched_name, start_char)
                if name_start == -1:
                    name_start = start_char
                end_char = name_start + len(matched_name)
                references.append(
                    Location(
                        uri=uri, range=Range(start=Position(line=i, character=name_start), end=Position(line=i, character=end_char))
                    )
                )
            continue

        # Check for section end
        end_match = _END_SECTION_PATTERN.match(line_stripped)
        if end_match:
            named_end = end_match.group("name")
            if named_end and named_end.upper() == section_name_upper:
                # Named &END - find the name in the original line
                start_char = line.upper().find(named_end.upper())
                if start_char != -1:
                    end_char = start_char + len(named_end)
                    references.append(
                        Location(
                            uri=uri,
                            range=Range(start=Position(line=i, character=start_char), end=Position(line=i, character=end_char)),
                        )
                    )
            continue

    return references


def find_keyword_references(text: str, keyword_name: str, uri: str) -> List[Location]:
    """Find all references to a keyword name in the document.

    Args:
        text: The document text
        keyword_name: The keyword name to search for (case-insensitive)
        uri: The document URI

    Returns:
        List of Location objects for each reference
    """
    references = []
    lines = text.split("\n")
    keyword_name_upper = keyword_name.upper()

    for i, line in enumerate(lines):
        line_stripped = line.strip()

        # Skip empty lines and comments
        if not line_stripped or line_stripped.startswith("!") or line_stripped.startswith("#"):
            continue

        # Check for keyword
        keyword_match = _KEYWORD_PATTERN.match(line_stripped)
        if keyword_match:
            matched_name = keyword_match.group("name").upper()
            if matched_name == keyword_name_upper:
                # Find the keyword name in the original line
                start_char = line.upper().find(matched_name)
                if start_char != -1:
                    end_char = start_char + len(matched_name)
                    references.append(
                        Location(
                            uri=uri,
                            range=Range(start=Position(line=i, character=start_char), end=Position(line=i, character=end_char)),
                        )
                    )

    return references


def find_variable_references(text: str, variable_name: str, uri: str) -> List[Location]:
    """Find all references to a variable in the document.

    Args:
        text: The document text
        variable_name: The variable name to search for (case-insensitive)
        uri: The document URI

    Returns:
        List of Location objects for each reference
    """
    references = []
    lines = text.split("\n")
    variable_name_upper = variable_name.upper()

    for i, line in enumerate(lines):
        # Find @SET definitions
        set_match = _SET_PATTERN.match(line.strip())
        if set_match and set_match.group("name").upper() == variable_name_upper:
            start_char = line.upper().find(variable_name_upper)
            if start_char != -1:
                end_char = start_char + len(variable_name)
                references.append(
                    Location(
                        uri=uri, range=Range(start=Position(line=i, character=start_char), end=Position(line=i, character=end_char))
                    )
                )
            continue

        # Find $VAR references
        for match in _VARIABLE_PATTERN.finditer(line):
            if match.group("name").upper() == variable_name_upper:
                references.append(
                    Location(
                        uri=uri,
                        range=Range(start=Position(line=i, character=match.start()), end=Position(line=i, character=match.end())),
                    )
                )

    return references


def get_references(text: str, position: Position, uri: str, context: Optional[dict] = None) -> List[Location]:
    """Find all references to the token at the given position.

    Args:
        text: The document text
        position: The cursor position (line and character are 0-indexed)
        uri: The document URI
        context: Optional context dict (if None, will call resolve_cursor_context)

    Returns:
        List of Location objects for each reference
    """
    if context is None:
        context = resolve_cursor_context(text, position)

    if context is None:
        return []

    if context["type"] == "section":
        return find_section_references(text, context["name"], uri)

    elif context["type"] == "end_section":
        # For &END, find references to the section it ends
        if context["name"]:
            return find_section_references(text, context["name"], uri)
        else:
            # Anonymous &END - try to find which section it ends
            start_line = find_matching_section_start(text, position.line, None)
            if start_line is not None:
                lines = text.split("\n")
                start_line_text = lines[start_line]
                match = _SECTION_PATTERN.match(start_line_text.strip())
                if match:
                    section_name = match.group("name")
                    return find_section_references(text, section_name, uri)
        return []

    elif context["type"] == "keyword":
        return find_keyword_references(text, context["name"], uri)

    elif context["type"] == "variable":
        return find_variable_references(text, context["name"], uri)

    return []
