"""
CP2K Domain Language Description API (#36, #37, #38).

Provides programmatic access to CP2K domain knowledge from the XML schema:
- describe_language() - Overview of CP2K input language structure
- describe_section(section_path) - Get section documentation
- describe_keyword(section_path, keyword_name) - Get keyword documentation
- suggest_next(text, position, uri) - Suggest what can come next
- make_example(section_path) - Generate minimal example for a section

All functions use the schema index (cp2k_input.xml) as the single source of truth.
"""

import re
import xml.etree.ElementTree as ET
from typing import Any, Dict, List, Optional

from . import DEFAULT_CP2K_INPUT_XML

# Regex patterns for parsing CP2K input
_SECTION_RE = re.compile(r"^(\s*)&([\w\-_]+)\s*(.*)", re.IGNORECASE)
_END_RE = re.compile(r"^\s*&END\s+([\w\-_]+)", re.IGNORECASE)
_KEYWORD_RE = re.compile(r"^(\s*)([\w\-_]+)\s+(.*)")

# Cache for parsed schema
_schema_cache: Optional[ET.Element] = None


def _get_schema_root() -> ET.Element:
    """Get the root element of the CP2K input XML schema (cached)."""
    global _schema_cache
    if _schema_cache is None:
        _schema_cache = ET.parse(DEFAULT_CP2K_INPUT_XML).getroot()
    return _schema_cache


def _find_section_node(name: str, parent_node: ET.Element) -> Optional[ET.Element]:
    """Find a section node by name within a parent node."""
    name_upper = name.upper()
    for sec_node in parent_node.iterfind("./SECTION"):
        name_node = sec_node.find("./NAME")
        if name_node is not None and name_node.text and name_node.text.upper() == name_upper:
            return sec_node
    return None


def _find_keyword_node(name: str, section_node: ET.Element) -> Optional[ET.Element]:
    """Find a keyword node by name within a section node."""
    name_upper = name.upper()
    for kw_node in section_node.iterfind("./KEYWORD"):
        name_node = kw_node.find("./NAME")
        if name_node is not None and name_node.text and name_node.text.upper() == name_upper:
            return kw_node
    return None


def _parse_section_path(path: str) -> List[str]:
    """Parse a dot-separated section path into components."""
    if not path:
        return []
    return [p.upper().strip() for p in path.split(".") if p.strip()]


def _find_section_anywhere(name: str, root: ET.Element) -> Optional[ET.Element]:
    """Find a section by name anywhere in the schema tree."""
    name_upper = name.upper()

    def search(node):
        # Check this node's direct sections
        for sec_node in node.iterfind("./SECTION"):
            name_node = sec_node.find("./NAME")
            if name_node is not None and name_node.text and name_node.text.upper() == name_upper:
                return sec_node
            # Recursively search subsections
            result = search(sec_node)
            if result is not None:
                return result
        return None

    return search(root)


def _resolve_section_path(path: str) -> Optional[ET.Element]:
    """Resolve a dot-separated section path to its XML node."""
    components = _parse_section_path(path)
    if not components:
        return None

    root = _get_schema_root()

    # For single component paths, search anywhere in the schema
    if len(components) == 1:
        result = _find_section_anywhere(components[0], root)
        return result

    # For multi-component paths, follow the exact path
    current_node = root
    for component in components:
        section_node = _find_section_node(component, current_node)
        if section_node is None:
            return None
        current_node = section_node

    return current_node


def _get_keyword_type(keyword_node: ET.Element) -> str:
    """Extract the type of a keyword from its DATA_TYPE node."""
    data_type = keyword_node.find("./DATA_TYPE")
    if data_type is not None:
        kind = data_type.get("kind", "")
        # Normalize type names: "keyword" means enum in CP2K schema
        if kind == "keyword":
            return "enum"
        return kind
    return "unknown"


def _get_enum_values(keyword_node: ET.Element) -> List[str]:
    """Extract enum values from a keyword node."""
    values = []
    data_type = keyword_node.find("./DATA_TYPE")
    if data_type is not None:
        for enum_item in data_type.iterfind("./ENUMERATION/ITEM"):
            name_node = enum_item.find("./NAME")
            if name_node is not None and name_node.text:
                values.append(name_node.text)
    # Also check for LOOSE keyword values (alternative representation)
    if not values:
        loose_vals = keyword_node.find("./DATA_TYPE/LOOSE")
        if loose_vals is not None and loose_vals.text:
            values = [v.strip() for v in loose_vals.text.split(",")]
    return values


def _get_default_value(keyword_node: ET.Element) -> Any:
    """Extract the default value from a keyword node."""
    default_node = keyword_node.find("./DEFAULT_VALUE")
    if default_node is not None and default_node.text:
        text = default_node.text.strip()
        # Try to parse as number
        try:
            if "." in text or "e" in text.lower():
                return float(text)
            return int(text)
        except ValueError:
            return text
    return None


def _get_units(keyword_node: ET.Element) -> Optional[str]:
    """Extract units from a keyword node."""
    unit_node = keyword_node.find("./DEFAULT_UNIT")
    if unit_node is not None and unit_node.text:
        return unit_node.text.strip()
    return None


def describe_language() -> Dict[str, Any]:
    """
    Return an overview of the CP2K input language structure.

    Returns:
        Dict with:
        - language_name: "CP2K"
        - version: CP2K version from schema
        - top_level_sections: List of top-level section info dicts
    """
    root = _get_schema_root()

    # Get CP2K version
    version_node = root.find("./CP2K_VERSION")
    version = version_node.text if version_node is not None and version_node.text else "unknown"

    # Get top-level sections
    top_sections = []
    for sec_node in root.iterfind("./SECTION"):
        name_node = sec_node.find("./NAME")
        if name_node is not None and name_node.text:
            desc_node = sec_node.find("./DESCRIPTION")
            top_sections.append(
                {
                    "name": name_node.text.upper(),
                    "description": desc_node.text if desc_node is not None and desc_node.text else "",
                    "repeats": sec_node.get("repeats") == "yes",
                }
            )

    return {
        "language_name": "CP2K",
        "version": version,
        "top_level_sections": top_sections,
    }


def describe_section(section_path: str) -> Optional[Dict[str, Any]]:
    """
    Get documentation for a section.

    Args:
        section_path: Dot-separated section path (e.g., "FORCE_EVAL.DFT")

    Returns:
        Dict with:
        - name: Section name (uppercase)
        - description: Section description
        - keywords: List of keyword info dicts
        - subsections: List of subsection info dicts
        - required: Whether section is required
        - repeats: Whether section can repeat
        - parent: Parent section name (if nested)
        Or None if section not found.
    """
    components = _parse_section_path(section_path)
    if not components:
        return None

    section_name = components[-1]
    parent_path = ".".join(components[:-1]) if len(components) > 1 else None

    # Resolve the section node
    section_node = _resolve_section_path(section_path)
    if section_node is None:
        return None

    # Get section metadata
    name_node = section_node.find("./NAME")
    desc_node = section_node.find("./DESCRIPTION")
    name = name_node.text.upper() if name_node is not None and name_node.text else section_name

    # Extract keywords
    keywords = []
    for kw_node in section_node.iterfind("./KEYWORD"):
        kw_name_node = kw_node.find("./NAME")
        if kw_name_node is not None and kw_name_node.text:
            kw_type = _get_keyword_type(kw_node)
            keywords.append(
                {
                    "name": kw_name_node.text.upper(),
                    "type": kw_type,
                }
            )

    # Extract subsections
    subsections = []
    for sub_node in section_node.iterfind("./SECTION"):
        sub_name_node = sub_node.find("./NAME")
        if sub_name_node is not None and sub_name_node.text:
            subsections.append(
                {
                    "name": sub_name_node.text.upper(),
                    "repeats": sub_node.get("repeats") == "yes",
                }
            )

    return {
        "name": name,
        "description": desc_node.text if desc_node is not None and desc_node.text else "",
        "keywords": keywords,
        "subsections": subsections,
        "required": True,  # Most CP2K sections are required in context
        "repeats": section_node.get("repeats") == "yes",
        "parent": parent_path,
    }


def describe_keyword(section_path: str, keyword_name: str) -> Optional[Dict[str, Any]]:
    """
    Get documentation for a keyword within a section.

    Args:
        section_path: Dot-separated section path (e.g., "FORCE_EVAL.DFT")
        keyword_name: Keyword name

    Returns:
        Dict with:
        - name: Keyword name (uppercase)
        - description: Keyword description
        - type: Keyword type (enum, integer, real, string, etc.)
        - default: Default value
        - enum_values: List of enum values (if type is enum)
        - units: Units (if applicable)
        Or None if keyword not found.
    """
    section_node = _resolve_section_path(section_path)
    if section_node is None:
        return None

    keyword_upper = keyword_name.upper()
    kw_node = _find_keyword_node(keyword_upper, section_node)
    if kw_node is None:
        return None

    # Get keyword metadata
    name_node = kw_node.find("./NAME")
    desc_node = kw_node.find("./DESCRIPTION")
    name = name_node.text.upper() if name_node is not None and name_node.text else keyword_upper

    kw_type = _get_keyword_type(kw_node)
    enum_values = _get_enum_values(kw_node)
    default = _get_default_value(kw_node)
    units = _get_units(kw_node)

    result = {
        "name": name,
        "description": desc_node.text if desc_node is not None and desc_node.text else "",
        "type": kw_type,
        "default": default,
    }

    if enum_values:
        result["enum_values"] = enum_values

    if units:
        result["units"] = units

    return result


def _parse_context(text: str, position: int) -> Dict[str, Any]:
    """Parse the input text to determine current context."""
    lines = text[:position].split("\n") if text else []
    current_line = lines[-1] if lines else ""
    lines_before = lines[:-1] if len(lines) > 1 else []

    # Track section stack
    section_stack: List[str] = []
    last_keyword: Optional[str] = None

    for line in lines_before:
        stripped = line.strip()
        if not stripped or stripped.startswith("!") or stripped.startswith("#"):
            continue

        end_match = _END_RE.match(stripped)
        if end_match:
            if section_stack:
                section_stack.pop()
            continue

        sec_match = _SECTION_RE.match(stripped)
        if sec_match:
            sec_name = sec_match.group(2).upper()
            if sec_name != "END":
                section_stack.append(sec_name)
            continue

        kw_match = _KEYWORD_RE.match(stripped)
        if kw_match:
            last_keyword = kw_match.group(2).upper()

    # Check current line
    current_stripped = current_line.strip()
    current_sec_match = _SECTION_RE.match(current_stripped)
    current_kw_match = _KEYWORD_RE.match(current_stripped)

    # If current line has a keyword, update last_keyword
    if current_kw_match:
        last_keyword = current_kw_match.group(2).upper()

    # Also check if current line has just a keyword name (no value)
    # This handles cases like "RUN_TYPE " where the regex doesn't match
    # because there's nothing after the space
    if not current_kw_match and current_stripped:
        # Check if the line looks like a keyword (word, spaces, end)
        kw_only_re = re.match(r"^([\w\-_]+)\s*$", current_stripped)
        if kw_only_re:
            last_keyword = kw_only_re.group(1).upper()

    return {
        "section_stack": section_stack,
        "last_keyword": last_keyword,
        "current_section_match": current_sec_match,
        "current_keyword_match": current_kw_match,
        "current_line": current_line,
        "current_is_just_keyword": bool(current_kw_match or (current_stripped and re.match(r"^([\w\-_]+)\s*$", current_stripped))),
    }


def suggest_next(text: str, position: int, uri: str) -> Dict[str, Any]:
    """
    Suggest what can come next at the given position.

    Args:
        text: The full input text
        position: Cursor position in the text
        uri: File URI (for context, not currently used)

    Returns:
        Dict with:
        - context: Context information
        - suggestions: List of suggestion dicts with:
          - name: Suggested token name
          - kind: "section", "keyword", or "value"
          - type: Type (for keywords)
          - description: Optional description
    """
    ctx = _parse_context(text, position)
    section_stack = ctx["section_stack"]
    last_keyword = ctx["last_keyword"]

    # Determine current section path
    if section_stack:
        current_path = ".".join(section_stack)
    else:
        current_path = ""

    suggestions = []

    # Get suggestions based on context
    if not current_path:
        # At top level, suggest top-level sections
        root = _get_schema_root()
        for sec_node in root.iterfind("./SECTION"):
            name_node = sec_node.find("./NAME")
            if name_node is not None and name_node.text:
                desc_node = sec_node.find("./DESCRIPTION")
                suggestions.append(
                    {
                        "name": name_node.text.upper(),
                        "kind": "section",
                        "description": desc_node.text if desc_node is not None and desc_node.text else "",
                    }
                )
    else:
        # Inside a section, suggest subsections and keywords
        section_node = _resolve_section_path(current_path)
        if section_node is not None:
            # Suggest subsections
            for sub_node in section_node.iterfind("./SECTION"):
                name_node = sub_node.find("./NAME")
                if name_node is not None and name_node.text:
                    suggestions.append(
                        {
                            "name": name_node.text.upper(),
                            "kind": "section",
                        }
                    )

            # Suggest keywords
            for kw_node in section_node.iterfind("./KEYWORD"):
                name_node = kw_node.find("./NAME")
                if name_node is not None and name_node.text:
                    kw_type = _get_keyword_type(kw_node)
                    suggestion = {
                        "name": name_node.text.upper(),
                        "kind": "keyword",
                        "type": kw_type,
                    }

                    # For enum keywords, suggest values
                    if kw_type == "enum":
                        enum_values = _get_enum_values(kw_node)
                        if enum_values:
                            suggestion["enum_values"] = enum_values

                    suggestions.append(suggestion)

    # Check if we're completing a value for a keyword
    current_line = ctx["current_line"]
    current_stripped = current_line.strip()
    current_kw_match = ctx.get("current_keyword_match")

    # Check if current line ends with a keyword name followed by space or equals
    # e.g., "RUN_TYPE " or "RUN_TYPE="
    # Use current_line (not stripped) to detect trailing space
    line_ends_with_space = current_line.endswith(" ") or current_line.endswith("\t")
    line_ends_with_equals = "=" in current_line or current_stripped.endswith("=")

    if last_keyword and (
        line_ends_with_equals
        or (current_kw_match and current_stripped == current_kw_match.group(2))
        or (
            current_stripped
            and current_stripped.split()[-1].upper() == last_keyword
            and (line_ends_with_space or line_ends_with_equals or ctx.get("current_is_just_keyword", False))
        )
    ):

        # Use the keyword from current line if available, otherwise last_keyword
        if current_kw_match:
            kw_name = current_kw_match.group(2).upper()
        else:
            # Extract from current line
            parts = current_stripped.split()
            if parts:
                kw_name = parts[-1].upper().replace("=", "").strip()
            else:
                kw_name = last_keyword

        if kw_name and current_path:
            kw_info = describe_keyword(current_path, kw_name)
            if kw_info and kw_info.get("enum_values"):
                # Suggest enum values
                value_suggestions = []
                for value in kw_info["enum_values"]:
                    value_suggestions.append(
                        {
                            "name": value,
                            "kind": "value",
                        }
                    )
                if value_suggestions:
                    return {
                        "context": {
                            "current_section": current_path or "root",
                            "awaiting_value_for": kw_name,
                        },
                        "suggestions": value_suggestions,
                    }

    return {
        "context": {
            "current_section": current_path or "root",
        },
        "suggestions": suggestions,
    }


def make_example(section_path: str, visited: Optional[set] = None) -> Optional[Dict[str, Any]]:
    """
    Generate a minimal valid input example for a section.

    Args:
        section_path: Dot-separated section path (e.g., "FORCE_EVAL.DFT")
        visited: Set of already visited sections to avoid recursion

    Returns:
        Dict with:
        - description: Description of the example
        - input: Minimal CP2K input snippet
        Or None if section not found.
    """
    if visited is None:
        visited = set()

    section_info = describe_section(section_path)
    if section_info is None:
        return None

    components = _parse_section_path(section_path)
    if not components:
        return None

    # Avoid infinite recursion
    if section_path.upper() in visited:
        return None
    visited.add(section_path.upper())

    # Generate the example
    lines = []

    # Add parent sections if nested (without recursion)
    if len(components) > 1:
        parent_path = ".".join(components[:-1])
        # Add parent section headers manually
        for comp in components[:-1]:
            lines.append(f"&{comp}")

    # Add current section
    section_name = components[-1]
    indent = "  " * (len(components) - 1)
    lines.append(f"{indent}&{section_name}")

    # Add some keywords with defaults
    # Look for important keywords first, then fallback to first few with defaults
    important_keywords = {"PROJECT_NAME", "RUN_TYPE", "PRINT_LEVEL", "METHOD", "EPS_SCF", "MAX_SCF"}
    keywords_to_add = []

    # First, try to find important keywords with defaults
    for kw in section_info["keywords"]:
        if kw["name"] in important_keywords:
            kw_info = describe_keyword(section_path, kw["name"])
            if kw_info and kw_info.get("default") is not None:
                keywords_to_add.append((kw["name"], kw_info))
                if len(keywords_to_add) >= 2:
                    break

    # If not enough, add other keywords with defaults
    if len(keywords_to_add) < 2:
        for kw in section_info["keywords"]:
            if kw["name"] not in important_keywords:
                kw_info = describe_keyword(section_path, kw["name"])
                if kw_info and kw_info.get("default") is not None:
                    keywords_to_add.append((kw["name"], kw_info))
                    if len(keywords_to_add) >= 2:
                        break

    # Add the selected keywords
    for kw_name, kw_info in keywords_to_add:
        default_val = kw_info["default"]
        if kw_info.get("enum_values"):
            # Use first enum value
            enum_vals = kw_info.get("enum_values", [])
            if enum_vals:
                default_val = enum_vals[0]

        if isinstance(default_val, str):
            lines.append(f"{indent}  {kw_name} {default_val}")
        else:
            lines.append(f"{indent}  {kw_name} {default_val}")

    # Close current section
    lines.append(f"{indent}&END {section_name}")

    # Close parent sections if nested
    if len(components) > 1:
        for comp in reversed(components[:-1]):
            indent = "  " * (components.index(comp))
            lines.append(f"{indent}&END {comp}")

    return {
        "description": f"Minimal example for {section_name} section",
        "input": "\n".join(lines),
    }
