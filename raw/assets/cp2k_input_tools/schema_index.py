"""
CP2K input schema index for fast lookups.

This module provides a fast in-memory index of the CP2K input schema,
parsed once from cp2k_input.xml (27MB) and cached for subsequent lookups.

The index is a lazy-loaded singleton that parses the XML on first access
(~300ms) and then provides O(1) lookups for sections, keywords, and their
properties (types, defaults, enum values, descriptions).

TDD: Implementation written to pass tests in tests/test_schema_index.py
"""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from xml.etree import ElementTree as ET

# Path to the CP2K input XML schema
_SCHEMA_PATH = Path(__file__).parent / "cp2k_input.xml"


@dataclass(frozen=True)
class KeywordSpec:
    """Specification for a CP2K keyword.

    Attributes:
        name: Keyword name
        variable_type: Data type (e.g., "INTEGER", "REAL", "LOGICAL", "STRING")
        default_value: Default value for the keyword
        enumeration_values: List of valid enum values (if enumerated)
        description: Human-readable description
        repeatable: Whether the keyword can appear multiple times
        required: Whether the keyword is required
    """

    name: str
    variable_type: Optional[str] = None
    default_value: Optional[str] = None
    enumeration_values: Tuple[str, ...] = ()
    description: str = ""
    repeatable: bool = False
    required: bool = False


@dataclass(frozen=True)
class SectionSpec:
    """Specification for a CP2K section.

    Attributes:
        name: Section name
        description: Human-readable description
        repeatable: Whether the section can appear multiple times
        required: Whether the section is required
        subsections: Names of direct child sections
        keywords: Names of keywords in this section
    """

    name: str
    description: str = ""
    repeatable: bool = False
    required: bool = False
    subsections: Tuple[str, ...] = ()
    keywords: Tuple[str, ...] = ()


@dataclass
class CP2KSchemaIndex:
    """Index of CP2K input schema for fast lookups.

    This class parses the CP2K input XML schema and provides fast lookups
    for sections, keywords, and their properties.

    The index is organized as a tree of sections, where each section has
    references to its subsections and keywords.
    """

    # Section tree: {(section_name,): SectionSpec, (parent, child): SectionSpec, ...}
    _sections: Dict[Tuple[str, ...], SectionSpec] = field(default_factory=dict)

    # Keyword index: {(section_path, keyword_name): KeywordSpec, ...}
    _keywords: Dict[Tuple[Tuple[str, ...], str], KeywordSpec] = field(default_factory=dict)

    # Child section index: {section_path: [child_section_names, ...], ...}
    _child_sections: Dict[Tuple[str, ...], List[str]] = field(default_factory=dict)

    def get_section(self, path: Tuple[str, ...]) -> Optional[SectionSpec]:
        """Get section specification by path.

        Args:
            path: Tuple of section names from root to target (e.g., ("FORCE_EVAL", "DFT"))

        Returns:
            SectionSpec if found, None otherwise
        """
        return self._sections.get(path)

    def get_keywords(self, section_path: Tuple[str, ...]) -> Dict[str, KeywordSpec]:
        """Get all keywords for a section.

        Args:
            section_path: Tuple of section names from root to target

        Returns:
            Dict mapping keyword names to KeywordSpec objects
        """
        result = {}
        for (path, name), spec in self._keywords.items():
            if path == section_path:
                result[name] = spec
        return result

    def get_keyword(self, section_path: Tuple[str, ...], keyword_name: str) -> Optional[KeywordSpec]:
        """Get a specific keyword specification.

        Args:
            section_path: Tuple of section names from root to target
            keyword_name: Name of the keyword

        Returns:
            KeywordSpec if found, None otherwise
        """
        return self._keywords.get((section_path, keyword_name.upper()))

    def get_child_sections(self, section_path: Tuple[str, ...]) -> List[str]:
        """Get child section names for a section.

        Args:
            section_path: Tuple of section names from root to target

        Returns:
            List of child section names (empty if no children or section not found)
        """
        return self._child_sections.get(section_path, [])

    def get_root_sections(self) -> List[str]:
        """Get all root-level section names.

        Returns:
            List of section names at the root level
        """
        return self._child_sections.get((), [])


# Singleton instance
_schema_index: Optional[CP2KSchemaIndex] = None


def get_schema_index() -> CP2KSchemaIndex:
    """Get the singleton schema index instance.

    Parses the XML schema on first call and caches the result.

    Returns:
        CP2KSchemaIndex instance
    """
    global _schema_index
    if _schema_index is None:
        _schema_index = _parse_schema()
    return _schema_index


def _parse_schema() -> CP2KSchemaIndex:
    """Parse the CP2K input XML schema into an index.

    This function parses the 27MB cp2k_input.xml file and builds
    an in-memory index for fast lookups.

    Returns:
        CP2KSchemaIndex instance
    """
    index = CP2KSchemaIndex()

    tree = ET.parse(_SCHEMA_PATH)
    root = tree.getroot()

    # Build section tree
    _build_sections(root, index, (), root)

    return index


def _build_sections(
    element: ET.Element,
    index: CP2KSchemaIndex,
    parent_path: Tuple[str, ...],
    root: ET.Element,
) -> None:
    """Recursively build the section tree from XML elements.

    Args:
        element: Current XML element
        index: Schema index being built
        parent_path: Path to parent section
        root: Root XML element for looking up references
    """
    # Process each SECTION element
    for section_elem in element.findall("SECTION"):
        # Section name is in a child NAME element, not an attribute
        name_elem = section_elem.find("NAME")
        if name_elem is None or not name_elem.text:
            continue  # Skip sections without names
        name = name_elem.text.upper()
        description = section_elem.findtext("DESCRIPTION") or ""

        # Get subsections
        subsections = []
        for subsection in section_elem.findall("SECTION"):
            subsection_name_elem = subsection.find("NAME")
            if subsection_name_elem is not None and subsection_name_elem.text:
                subsections.append(subsection_name_elem.text.upper())

        # Get keywords
        keywords = []
        for keyword_elem in section_elem.findall("KEYWORD"):
            keyword_name_elem = keyword_elem.find("NAME")
            if keyword_name_elem is not None and keyword_name_elem.text:
                keyword_name = keyword_name_elem.text.upper()
                keywords.append(keyword_name)
                # Parse keyword specification
                spec = _parse_keyword(keyword_elem, keyword_name)
                index._keywords[(parent_path + (name,), keyword_name)] = spec

        # Create section specification
        section_spec = SectionSpec(
            name=name,
            description=description,
            subsections=tuple(subsections),
            keywords=tuple(keywords),
        )
        section_path = parent_path + (name,)
        index._sections[section_path] = section_spec
        index._child_sections.setdefault(parent_path, []).append(name)
        index._child_sections[section_path] = []

        # Recursively process subsections
        _build_sections(section_elem, index, section_path, root)


def _parse_keyword(element: ET.Element, name: str) -> KeywordSpec:
    """Parse a keyword element into a KeywordSpec.

    Args:
        element: XML element for the keyword
        name: Keyword name

    Returns:
        KeywordSpec object
    """
    # Get variable type from DATA_TYPE kind attribute
    data_type_elem = element.find("DATA_TYPE")
    variable_type = None
    if data_type_elem is not None:
        variable_type = data_type_elem.get("kind")

    # Get default value
    default_value = element.findtext("DEFAULT_VALUE")

    # Get enumeration values
    enumeration_values = []
    enum_element = element.find("ENUMERATION")
    if enum_element is not None:
        # XML uses ITEM/NAME for enum values
        for item in enum_element.findall("ITEM"):
            name_elem = item.find("NAME")
            if name_elem is not None and name_elem.text:
                enumeration_values.append(name_elem.text.upper())

    # Get description
    description = element.findtext("DESCRIPTION") or ""

    return KeywordSpec(
        name=name.upper(),
        variable_type=variable_type,
        default_value=default_value,
        enumeration_values=tuple(enumeration_values),
        description=description,
    )
