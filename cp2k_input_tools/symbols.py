"""Document and workspace symbol providers for CP2K LSP (#55).

Provides:
- get_document_symbols() - Extract symbol hierarchy from a single document
- get_workspace_symbols() - Search symbols across all open files
"""

import io
from typing import Dict, List

from lsprotocol.types import (
    Location,
    Position,
    Range,
    SymbolInformation,
    SymbolKind,
)

from .parser import CP2KInputParserSimplified


def get_document_symbols(text: str, uri: str) -> List[SymbolInformation]:
    """Extract document symbols from CP2K input text.

    Args:
        text: The CP2K input file content
        uri: The document URI (for context)

    Returns:
        List of SymbolInformation objects representing sections and keywords
        in hierarchical order (sections first, then nested children)
    """
    if not text or not text.strip():
        return []

    try:
        # Parse the input
        parser = CP2KInputParserSimplified()
        fhandle = io.StringIO(text)
        tree = parser.parse(fhandle)

        # Extract symbols from the parsed tree
        symbols = []
        _extract_symbols_from_tree(tree, "", symbols, uri)
        return symbols

    except Exception:
        # If parsing fails, return empty list rather than crash
        return []


def _extract_symbols_from_tree(
    tree: dict,
    parent_path: str,
    symbols: List[SymbolInformation],
    uri: str,
) -> None:
    """Recursively extract symbols from parsed tree.

    Args:
        tree: The parsed dictionary tree (keys are lowercase)
        parent_path: Uppercase dot-separated path of parent sections
        symbols: Accumulated list of symbols (modified in-place)
        uri: Document URI for Location objects
    """
    for key, value in tree.items():
        if key.startswith("_"):
            # Skip metadata keys (section parameters)
            continue

        # Convert key to uppercase for display
        key_upper = key.upper()
        current_path = f"{parent_path}.{key_upper}" if parent_path else key_upper

        if isinstance(value, dict):
            # This is a section
            _add_section_symbol(key_upper, parent_path, symbols, uri)

            # Recursively process children
            _extract_symbols_from_tree(value, current_path, symbols, uri)

        elif isinstance(value, list):
            # This could be:
            # 1. A list of sections (repeated sections)
            # 2. A keyword with multiple values
            for item in value:
                if isinstance(item, dict):
                    # Repeated section
                    _add_section_symbol(key_upper, parent_path, symbols, uri)
                    _extract_symbols_from_tree(item, current_path, symbols, uri)
                else:
                    # Keyword with multiple values
                    _add_keyword_symbol(key_upper, parent_path, symbols, uri)
                    break  # Only add once
        else:
            # This is a keyword
            _add_keyword_symbol(key_upper, parent_path, symbols, uri)


def _add_section_symbol(
    name: str,
    parent_path: str,
    symbols: List[SymbolInformation],
    uri: str,
) -> None:
    """Add a section symbol to the list.

    Args:
        name: Section name (uppercase)
        parent_path: Uppercase parent section path (empty for top-level)
        symbols: Symbol list to modify
        uri: Document URI for Location
    """
    # Extract parent name from path
    parent_name = ""
    if parent_path:
        parts = parent_path.split(".")
        parent_name = parts[-1] if parts else ""

    # Create a Location with URI and Range
    location = Location(
        uri=uri,
        range=Range(
            start=Position(line=0, character=0),
            end=Position(line=0, character=0),
        ),
    )

    symbol = SymbolInformation(
        name=name,
        kind=SymbolKind.Namespace,
        container_name=parent_name,
        location=location,
    )
    symbols.append(symbol)


def _add_keyword_symbol(
    name: str,
    parent_path: str,
    symbols: List[SymbolInformation],
    uri: str,
) -> None:
    """Add a keyword symbol to the list.

    Args:
        name: Keyword name (uppercase, may have '+' prefix if conflicts with section)
        parent_path: Uppercase parent section path
        symbols: Symbol list to modify
        uri: Document URI for Location
    """
    # Clean up keyword name (remove '+' prefix if present)
    clean_name = name.lstrip("+")

    # Extract parent name from path
    parent_name = ""
    if parent_path:
        parts = parent_path.split(".")
        parent_name = parts[-1] if parts else ""

    # Create a Location with URI and Range
    location = Location(
        uri=uri,
        range=Range(
            start=Position(line=0, character=0),
            end=Position(line=0, character=0),
        ),
    )

    symbol = SymbolInformation(
        name=clean_name,
        kind=SymbolKind.Field,
        container_name=parent_name,
        location=location,
    )
    symbols.append(symbol)


def get_workspace_symbols(
    query: str,
    all_files: Dict[str, str],
) -> List[SymbolInformation]:
    """Search for symbols across all open workspace files.

    Args:
        query: Search string (empty string returns all symbols)
        all_files: Dictionary mapping file URIs to file contents

    Returns:
        List of matching SymbolInformation objects
    """
    results = []
    query_upper = query.upper()

    for file_uri, file_content in all_files.items():
        # Get all symbols from this file
        file_symbols = get_document_symbols(file_content, file_uri)

        # Filter by query if provided
        if query:
            matching_symbols = [sym for sym in file_symbols if query_upper in sym.name.upper()]
            results.extend(matching_symbols)
        else:
            # No query, return all symbols
            results.extend(file_symbols)

    return results
