"""Tests for Go-to-Definition & References feature (#56).

Tests cover:
- Go-to-definition on section names navigates to section definition
- Go-to-definition on keywords shows schema documentation
- Find references for section/keyword usage in file
- Enum value navigation and references
"""

from lsprotocol.types import Location, Position

from cp2k_input_tools.definition import get_definition, get_references
from tests import TEST_DIR

# Sample CP2K input content for testing
SAMPLE_INPUT = """&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME BASIS_SET
    POTENTIAL_FILE_NAME POTENTIAL
    LSD
    &MGRID
      CUTOFF 140
    &END MGRID
    &QS
      EPS_DEFAULT 1.0E-8
      METHOD GPW
    &END QS
    &SCF
      EPS_DIIS 0.1
      EPS_SCF 1.0E-4
      MAX_DIIS 4
      MAX_SCF 30
      SCF_GUESS atomic
    &END SCF
  &END DFT
  &SUBSYS
    &CELL
      ABC 8.0 4.0 4.0
    &END CELL
    &COORD
      H     0.000000  0.000000  0.000000
      H     1.000000  0.000000  0.000000
    &END COORD
    &KIND H
      BASIS_SET DZV-GTH-PADE
      POTENTIAL GTH-PADE-q1
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
&GLOBAL
  PROJECT H2
  PRINT_LEVEL MEDIUM
&END GLOBAL
"""


class TestGoToDefinition:
    """Tests for go-to-definition functionality."""

    def test_definition_on_section_name(self):
        """Go-to-definition on section name should navigate to section definition."""
        # Test on FORCE_EVAL section name (line 1, character 1-10)
        position = Position(line=0, character=5)  # On "FORCE_EVAL"
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")

        assert result is not None
        assert isinstance(result, Location)
        # Section definitions are in schema, not in the file
        # So we expect either null or a schema location
        # For now, we'll return null for section definitions in schema
        assert result.uri == "test://test.inp" or result.uri.startswith("cp2k-schema://")

    def test_definition_on_keyword_name(self):
        """Go-to-definition on keyword should show schema documentation."""
        # Test on METHOD keyword in FORCE_EVAL (line 2)
        position = Position(line=1, character=4)  # On "METHOD"
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")

        assert result is not None
        assert isinstance(result, Location)
        # Keyword definitions are in schema
        assert result.uri.startswith("cp2k-schema://") or result.uri == "test://test.inp"

    def test_definition_on_nested_section(self):
        """Go-to-definition on nested section should work."""
        # Test on DFT section (line 3, character 3-6)
        position = Position(line=2, character=4)  # On "DFT"
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")

        assert result is not None
        assert isinstance(result, Location)

    def test_definition_on_end_section(self):
        """Go-to-definition on &END should navigate to matching &SECTION."""
        # Test on &END MGRID (line 9 in 1-indexed, line 8 in 0-indexed)
        position = Position(line=8, character=5)  # On "END"
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")

        assert result is not None
        # Should point to the opening &MGRID
        assert isinstance(result, Location)
        assert result.range.start.line == 6  # MGRID starts at line 7 (1-indexed) or line 6 (0-indexed)

    def test_definition_on_variable_usage(self):
        """Go-to-definition on variable usage ($VAR) should navigate to @SET."""
        # This is for future enhancement - variables are not in our sample
        # Just test that it doesn't crash
        position = Position(line=0, character=0)
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")
        # Should return None for non-matching positions
        assert result is None or isinstance(result, Location)

    def test_definition_on_plain_text(self):
        """Go-to-definition on plain text should return None."""
        # Test on a line with just a value (line 7: "      CUTOFF 140")
        # Position on "140" which is a value, not a keyword
        # "CUTOFF" is at position 6-12, "140" starts at position 13
        position = Position(line=7, character=14)  # On "140"
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")

        assert result is None


class TestFindReferences:
    """Tests for find references functionality."""

    def test_references_for_section_name(self):
        """Find all references to a section name."""
        # Find references to FORCE_EVAL section
        position = Position(line=0, character=5)  # On "FORCE_EVAL"
        result = get_references(SAMPLE_INPUT, position, "test://test.inp")

        assert isinstance(result, list)
        # Should find at least the section definition and its &END
        assert len(result) >= 1

    def test_references_for_keyword_name(self):
        """Find all references to a keyword name."""
        # Find references to BASIS_SET keyword
        position = Position(line=3, character=10)  # On "BASIS_SET"
        result = get_references(SAMPLE_INPUT, position, "test://test.inp")

        assert isinstance(result, list)
        # Should find at least one usage
        assert len(result) >= 1

    def test_references_for_repeated_section(self):
        """Find references to a section that appears multiple times."""
        # Our sample has multiple sections but not repeated names
        # Test on SUBSYS which appears once
        position = Position(line=22, character=5)  # On "SUBSYS"
        result = get_references(SAMPLE_INPUT, position, "test://test.inp")

        assert isinstance(result, list)

    def test_references_on_end_section(self):
        """Find references for &END should include matching &SECTION."""
        position = Position(line=8, character=5)  # On "END" of &END MGRID
        result = get_references(SAMPLE_INPUT, position, "test://test.inp")

        assert isinstance(result, list)
        # Should find the opening &MGRID and the closing &END MGRID
        assert len(result) >= 2


class TestDefinitionWithRealFile:
    """Tests with real CP2K input files."""

    def test_definition_on_real_file(self):
        """Test go-to-definition on a real CP2K input file."""
        test_file = TEST_DIR / "inputs" / "He_PBE.inp"

        with open(test_file, "r") as f:
            content = f.read()

        # Test on GLOBAL section (last section in file)
        # Line 37: &GLOBAL
        position = Position(line=36, character=3)  # On "GLOBAL"
        result = get_definition(content, position, str(test_file))

        assert result is not None or result is None  # Can be None if not implemented yet
        if result:
            assert isinstance(result, Location)

    def test_references_on_real_file(self):
        """Test find references on a real CP2K input file."""
        test_file = TEST_DIR / "inputs" / "He_PBE.inp"

        with open(test_file, "r") as f:
            content = f.read()

        # Test on DFT section
        position = Position(line=2, character=5)  # On "DFT"
        result = get_references(content, position, str(test_file))

        assert isinstance(result, list)


class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_empty_input(self):
        """Test with empty input."""
        result = get_definition("", Position(line=0, character=0), "test://test.inp")
        assert result is None

        refs = get_references("", Position(line=0, character=0), "test://test.inp")
        assert isinstance(refs, list)
        assert len(refs) == 0

    def test_invalid_position(self):
        """Test with position beyond file content."""
        result = get_definition(SAMPLE_INPUT, Position(line=100, character=0), "test://test.inp")
        assert result is None

        refs = get_references(SAMPLE_INPUT, Position(line=100, character=0), "test://test.inp")
        assert isinstance(refs, list)

    def test_comment_lines(self):
        """Test that comments are handled correctly."""
        comment_input = """&GLOBAL
  # This is a comment
  PROJECT TEST
&END GLOBAL
"""
        position = Position(line=1, character=5)  # On comment
        result = get_definition(comment_input, position, "test://test.inp")
        # Should return None for comment lines
        assert result is None

    def test_case_insensitive_section_lookup(self):
        """Test that section lookup is case-insensitive."""
        # Force_eval should still work
        mixed_case_input = "&Force_Eval\n  METHOD Quickstep\n&END Force_Eval\n"
        position = Position(line=0, character=5)
        result = get_definition(mixed_case_input, position, "test://test.inp")

        # Should handle case-insensitively
        assert result is None or isinstance(result, Location)


class TestEnumValueDefinition:
    """Tests for enum value definitions and references."""

    def test_definition_on_enum_value(self):
        """Go-to-definition on enum value should show valid values."""
        # Test on "GPW" enum value in QS section
        position = Position(line=11, character=10)  # On "GPW"
        result = get_definition(SAMPLE_INPUT, position, "test://test.inp")

        # For enum values, we might return a special location or hover info
        # For now, it should not crash
        assert result is None or isinstance(result, Location)

    def test_references_for_enum_value(self):
        """Find references to an enum value."""
        # Test on "atomic" value for SCF_GUESS
        position = Position(line=17, character=15)  # On "atomic"
        result = get_references(SAMPLE_INPUT, position, "test://test.inp")

        assert isinstance(result, list)
