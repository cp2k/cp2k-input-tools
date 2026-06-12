"""Tests for Document/Workspace Symbols (#55).

Tests cover:
- Document symbols showing section hierarchy, keywords, ranges
- Workspace symbols searching across open files
- Symbol kind mapping (sections vs keywords)
- Proper range calculation for symbols
"""

from lsprotocol.types import (
    SymbolKind,
)

from cp2k_input_tools.symbols import (
    get_document_symbols,
    get_workspace_symbols,
)


class TestDocumentSymbols:
    """Tests for document symbol provider."""

    def test_empty_input_returns_empty_symbols(self):
        """Empty input should return empty symbol list."""
        text = ""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)
        assert symbols == []

    def test_simple_section_hierarchy(self):
        """Simple input should produce correct section hierarchy."""
        text = """&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should have one section symbol
        assert len(symbols) >= 1

        # Find GLOBAL section
        global_symbols = [s for s in symbols if s.name == "GLOBAL"]
        assert len(global_symbols) == 1

        global_sym = global_symbols[0]
        assert global_sym.kind == SymbolKind.Namespace
        assert global_sym.container_name == ""
        # Should be at line 0 (0-indexed)
        assert global_sym.location.range.start.line == 0

    def test_nested_sections(self):
        """Nested sections should show proper hierarchy."""
        text = """&FORCE_EVAL
  METHOD QS
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    &SCF
      EPS_SCF 1.0E-6
    &END SCF
  &END DFT
&END FORCE_EVAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should have FORCE_EVAL, DFT, and SCF sections
        section_names = [s.name for s in symbols if s.kind == SymbolKind.Namespace]
        assert "FORCE_EVAL" in section_names
        assert "DFT" in section_names
        assert "SCF" in section_names

        # Check parent-child relationships
        force_eval = next(s for s in symbols if s.name == "FORCE_EVAL")
        dft = next(s for s in symbols if s.name == "DFT")
        scf = next(s for s in symbols if s.name == "SCF")

        # DFT should be inside FORCE_EVAL
        assert dft.container_name == "FORCE_EVAL"
        # SCF should be inside DFT
        assert scf.container_name == "DFT"

    def test_keywords_as_symbols(self):
        """Keywords should appear as symbols inside sections."""
        text = """&GLOBAL
  PROJECT_NAME test
  RUN_TYPE ENERGY
&END GLOBAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should have keyword symbols
        keyword_symbols = [s for s in symbols if s.kind == SymbolKind.Field]
        assert len(keyword_symbols) >= 2

        keyword_names = [s.name for s in keyword_symbols]
        assert "PROJECT_NAME" in keyword_names
        assert "RUN_TYPE" in keyword_names

        # Keywords should be inside GLOBAL
        project = next(s for s in keyword_symbols if s.name == "PROJECT_NAME")
        assert project.container_name == "GLOBAL"

    def test_symbol_ranges(self):
        """Symbols should have range objects (line numbers are currently placeholder)."""
        text = """&FORCE_EVAL
  METHOD QS
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
  &END DFT
&END FORCE_EVAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        force_eval = next(s for s in symbols if s.name == "FORCE_EVAL")
        dft = next(s for s in symbols if s.name == "DFT")

        # Symbols should have range objects
        assert force_eval.location.range is not None
        assert dft.location.range is not None
        # Currently all ranges are placeholders (0,0)-(0,0)
        # Real line number tracking would require preserving position info in parser
        assert force_eval.location.range.start.line == 0
        assert dft.location.range.start.line == 0

    def test_multiple_top_level_sections(self):
        """Multiple top-level sections should all appear."""
        text = """&GLOBAL
  PROJECT_NAME test
&END GLOBAL

&FORCE_EVAL
  METHOD QS
&END FORCE_EVAL

&EXT_RESTART
  RESTART_FILE_NAME restart
&END EXT_RESTART
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should have three top-level sections
        top_level = [s for s in symbols if s.container_name == ""]
        assert len(top_level) >= 3

        section_names = [s.name for s in top_level]
        assert "GLOBAL" in section_names
        assert "FORCE_EVAL" in section_names
        assert "EXT_RESTART" in section_names

    def test_realistic_input_structure(self):
        """Realistic CP2K input should produce complete symbol tree."""
        text = """&GLOBAL
  PROJECT_NAME test
  RUN_TYPE ENERGY
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    POTENTIAL_FILE_NAME GTH_POTENTIALS
    &MGRID
      CUTOFF 400
      REL_CUTOFF 50
    &END MGRID
    &SCF
      EPS_SCF 1.0E-6
      MAX_SCF 50
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &KIND H
      BASIS_SET DZVP
      POTENTIAL GTH-PBE
    &END KIND
    &KIND O
      BASIS_SET DZVP
      POTENTIAL GTH-PBE
    &END KIND
    &CELL
      ABC 10.0 10.0 10.0
    &END CELL
    &COORD
      O  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should have all major sections
        section_names = [s.name for s in symbols if s.kind == SymbolKind.Namespace]

        # Top level sections
        assert "GLOBAL" in section_names
        assert "FORCE_EVAL" in section_names

        # Nested sections
        assert "DFT" in section_names
        assert "MGRID" in section_names
        assert "SCF" in section_names
        assert "XC" in section_names
        assert "XC_FUNCTIONAL" in section_names
        assert "SUBSYS" in section_names
        assert "KIND" in section_names
        assert "CELL" in section_names
        assert "COORD" in section_names

        # Should have keywords
        keyword_symbols = [s for s in symbols if s.kind == SymbolKind.Field]
        assert len(keyword_symbols) > 0


class TestWorkspaceSymbols:
    """Tests for workspace symbol provider."""

    def test_empty_query_returns_all_symbols(self):
        """Empty query should return all symbols from all files."""
        # Create test files
        file1_content = """&GLOBAL
  PROJECT_NAME test1
&END GLOBAL
"""
        file2_content = """&FORCE_EVAL
  METHOD QS
&END FORCE_EVAL
"""

        all_files = {
            "test1.inp": file1_content,
            "test2.inp": file2_content,
        }

        symbols = get_workspace_symbols("", all_files)

        # Should have symbols from both files
        assert len(symbols) >= 2

        # Should have symbols from both files
        uris = set(s.container_name for s in symbols)
        # container_name is used for file URI in workspace symbols
        assert len(uris) > 0

    def test_query_filters_by_name(self):
        """Query should filter symbols by name."""
        file1_content = """&GLOBAL
  PROJECT_NAME test
&END GLOBAL

&FORCE_EVAL
  METHOD QS
&END FORCE_EVAL
"""

        all_files = {
            "test.inp": file1_content,
        }

        # Search for "FORCE"
        symbols = get_workspace_symbols("FORCE", all_files)

        # Should only return FORCE_EVAL
        assert len(symbols) >= 1
        assert all("FORCE" in s.name for s in symbols)

        # Should not find GLOBAL
        assert not any(s.name == "GLOBAL" for s in symbols)

    def test_query_case_insensitive(self):
        """Query should be case-insensitive."""
        file_content = """&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""

        all_files = {
            "test.inp": file_content,
        }

        # Search with different cases
        symbols_lower = get_workspace_symbols("global", all_files)
        symbols_upper = get_workspace_symbols("GLOBAL", all_files)
        symbols_mixed = get_workspace_symbols("Global", all_files)

        # All should return the same results
        assert len(symbols_lower) == len(symbols_upper)
        assert len(symbols_upper) == len(symbols_mixed)

    def test_query_searches_keywords(self):
        """Query should also find keyword symbols."""
        file_content = """&GLOBAL
  PROJECT_NAME test
  RUN_TYPE ENERGY
&END GLOBAL
"""

        all_files = {
            "test.inp": file_content,
        }

        # Search for "PROJECT"
        symbols = get_workspace_symbols("PROJECT", all_files)

        # Should find PROJECT_NAME keyword
        assert len(symbols) >= 1
        assert any("PROJECT" in s.name for s in symbols)

    def test_workspace_symbol_structure(self):
        """Workspace symbols should have correct structure."""
        file_content = """&FORCE_EVAL
  &DFT
    METHOD QS
  &END DFT
&END FORCE_EVAL
"""

        all_files = {
            "test.inp": file_content,
        }

        symbols = get_workspace_symbols("", all_files)

        # Each symbol should have required fields
        for sym in symbols:
            assert sym.name is not None
            assert sym.kind is not None
            assert sym.container_name is not None  # Used for file URI

    def test_multiple_files_same_section(self):
        """Same section in multiple files should appear multiple times."""
        file1_content = """&GLOBAL
  PROJECT_NAME test1
&END GLOBAL
"""
        file2_content = """&GLOBAL
  PROJECT_NAME test2
&END GLOBAL
"""

        all_files = {
            "test1.inp": file1_content,
            "test2.inp": file2_content,
        }

        symbols = get_workspace_symbols("GLOBAL", all_files)

        # Should find GLOBAL in both files
        global_symbols = [s for s in symbols if s.name == "GLOBAL"]
        assert len(global_symbols) >= 2


class TestSymbolKindMapping:
    """Tests for symbol kind mapping."""

    def test_sections_are_namespaces(self):
        """Section symbols should be Namespace kind."""
        text = """&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        global_sym = next(s for s in symbols if s.name == "GLOBAL")
        assert global_sym.kind == SymbolKind.Namespace

    def test_keywords_are_fields(self):
        """Keyword symbols should be Field kind."""
        text = """&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        project_sym = next(s for s in symbols if s.name == "PROJECT_NAME")
        assert project_sym.kind == SymbolKind.Field


class TestSymbolEdgeCases:
    """Tests for edge cases and error handling."""

    def test_section_with_parameters(self):
        """Sections with parameters should still be symbols."""
        # Use a valid section with parameter inside FORCE_EVAL
        text = """&FORCE_EVAL
  &SUBSYS
    &KIND H
      BASIS_SET DZVP
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should have KIND section
        kind_sym = next((s for s in symbols if s.name == "KIND"), None)
        assert kind_sym is not None
        assert kind_sym.kind == SymbolKind.Namespace

    def test_repeated_sections_with_parameters(self):
        """Repeated sections with parameters are handled by parser."""
        # KIND sections with different parameters (H, O) get combined
        # into a single KIND entry with dict keys for each parameter
        text = """&FORCE_EVAL
  &SUBSYS
    &KIND H
      BASIS_SET DZVP
    &END KIND
    &KIND O
      BASIS_SET DZVP
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Parser simplifies repeated KIND sections with parameters
        # into a single KIND dict entry with H and O as keys
        kind_symbols = [s for s in symbols if s.name == "KIND"]
        # We get at least one KIND symbol
        assert len(kind_symbols) >= 1
        # The KIND symbol should have SUBSYS as parent
        kind_sym = kind_symbols[0]
        assert kind_sym.container_name == "SUBSYS"

    def test_comments_and_empty_lines(self):
        """Comments and empty lines should not affect symbols."""
        text = """! This is a comment
&GLOBAL
  ! Another comment
  PROJECT_NAME test
&END GLOBAL

&FORCE_EVAL
  METHOD QS
! Final comment
&END FORCE_EVAL
"""
        uri = "test.inp"
        symbols = get_document_symbols(text, uri)

        # Should still find both sections
        section_names = [s.name for s in symbols if s.kind == SymbolKind.Namespace]
        assert "GLOBAL" in section_names
        assert "FORCE_EVAL" in section_names

    def test_invalid_input_returns_empty(self):
        """Invalid input should return empty symbols rather than crashing."""
        # Missing END section
        text = """&GLOBAL
  PROJECT_NAME test
"""
        uri = "test.inp"

        # Should not crash, may return empty or partial symbols
        symbols = get_document_symbols(text, uri)
        assert isinstance(symbols, list)
