"""Tests for LSP rename feature: safe rename with validation."""

from cp2k_lsp.parser import CP2KParser
from lsprotocol.types import Position, WorkspaceEdit

# =============================================================================
# Helper
# =============================================================================


def _parse(text: str):
    """Parse text and return (ast, errors)."""
    parser = CP2KParser.parse_text(text)
    return parser.ast, parser.errors


def _find_position(text: str, search_str: str, line_offset: int = 0, char_offset: int = 0) -> Position:
    """Find the position of a string in the text."""
    lines = text.split("\n")
    for line_idx, line in enumerate(lines):
        char_idx = line.find(search_str)
        if char_idx != -1:
            return Position(line=line_idx + line_offset, character=char_idx + char_offset)
    return Position(line=0, character=0)


def _find_position_in_section_param(text: str, section_name: str) -> Position:
    """Find position of section parameter."""
    lines = text.split("\n")
    for line_idx, line in enumerate(lines):
        if section_name in line and "&" in line and "END" not in line:
            # Find the section parameter (after section name)
            import re

            match = re.match(r"&\s*\w+\s+(\S+)", line.strip())
            if match:
                param = match.group(1)
                char_idx = line.find(param)
                if char_idx != -1:
                    return Position(line=line_idx, character=char_idx)
    return Position(line=0, character=0)


# =============================================================================
# Schema keyword rename safety
# =============================================================================


class TestSchemaKeywordRenameSafety:
    """Test that schema-defined keywords cannot be renamed (spec compliance)."""

    def test_reject_schema_keyword_rename(self):
        """Renaming a schema keyword like PROJECT_NAME should be rejected."""
        from cp2k_input_tools.rename import can_rename

        inp = """\
&GLOBAL
  PROJECT_NAME test
  RUN_TYPE ENERGY
&END GLOBAL
"""
        # Try to rename PROJECT_NAME (schema keyword)
        pos = _find_position(inp, "PROJECT_NAME")

        # Should not be able to rename schema keywords
        result = can_rename(inp, pos, "file://test.inp")
        assert result is False, "Should reject renaming schema-defined keywords"

    def test_reject_section_keyword_rename(self):
        """Renaming a section name like GLOBAL should be rejected."""
        from cp2k_input_tools.rename import can_rename

        inp = """\
&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""
        # Try to rename GLOBAL (section keyword)
        pos = _find_position(inp, "GLOBAL")

        # Should not be able to rename section keywords
        result = can_rename(inp, pos, "file://test.inp")
        assert result is False, "Should reject renaming section keywords"

    def test_reject_keyword_value_run_type(self):
        """Renaming a keyword value like ENERGY should be rejected."""
        from cp2k_input_tools.rename import can_rename

        inp = """\
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
"""
        # Try to rename ENERGY (keyword value - enum)
        pos = _find_position(inp, "ENERGY")

        # Should not be able to rename enum values
        result = can_rename(inp, pos, "file://test.inp")
        assert result is False, "Should reject renaming enum values"


# =============================================================================
# Section parameter rename
# =============================================================================


class TestSectionParameterRename:
    """Test renaming of section parameters with validation."""

    def test_section_parameter_rename_works(self):
        """Renaming a section parameter should work when safe."""
        from cp2k_input_tools.rename import can_rename, get_rename_edit

        inp = """\
&FORCE_EVAL
  &SUBSYS
    &KIND H
      BASIS_SET DZVP
      POTENTIAL GTH-PBE
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
"""
        # Try to rename H to He (section parameter)
        pos = _find_position_in_section_param(inp, "KIND")

        # Should be able to rename section parameters
        result = can_rename(inp, pos, "file://test.inp")
        assert result is True, "Should allow renaming section parameters"

        # Get the rename edit
        edit = get_rename_edit(inp, pos, "file://test.inp", "He")
        assert edit is not None, "Should return WorkspaceEdit for section parameter"

    def test_section_parameter_rename_multiple_occurrences(self):
        """Renaming should update all occurrences of the parameter."""
        from cp2k_input_tools.rename import get_rename_edit

        inp = """\
&FORCE_EVAL
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
        # This test would check if renaming H affects both &KIND H occurrences
        # For now, just verify the mechanics work
        pos = _find_position_in_section_param(inp, "KIND")
        edit = get_rename_edit(inp, pos, "file://test.inp", "He")
        assert edit is not None


# =============================================================================
# Variable rename (already supported)
# =============================================================================


class TestVariableRename:
    """Test that variable renaming still works (regression test)."""

    def test_variable_rename_still_works(self):
        """Variable rename should continue to work."""
        from cp2k_input_tools.rename import can_rename, get_rename_edit

        inp = """\
&GLOBAL
  PROJECT_NAME @{my_project}
&END GLOBAL
"""
        # Try to rename variable @my_project
        # Find position inside the variable name (not @)
        lines = inp.split("\n")
        for line_idx, line in enumerate(lines):
            if "my_project" in line:
                char_idx = line.find("my_project")
                pos = Position(line=line_idx, character=char_idx)
                break
        else:
            pos = Position(line=0, character=0)

        # Should be able to rename variables
        result = can_rename(inp, pos, "file://test.inp")
        assert result is True, "Should allow renaming variables"

        # Get the rename edit
        edit = get_rename_edit(inp, pos, "file://test.inp", "new_project")
        assert edit is not None, "Should return WorkspaceEdit for variable"


# =============================================================================
# Edge cases and safety
# =============================================================================


class TestRenameEdgeCases:
    """Test edge cases and safety scenarios."""

    def test_rename_in_comment_rejected(self):
        """Renaming text in comments should be rejected."""
        from cp2k_input_tools.rename import can_rename

        inp = """\
# This is a comment with PROJECT_NAME
&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""
        # Try to rename PROJECT_NAME in comment
        lines = inp.split("\n")
        comment_line = lines[0]
        char_idx = comment_line.find("PROJECT_NAME")
        pos = Position(line=0, character=char_idx)

        result = can_rename(inp, pos, "file://test.inp")
        assert result is False, "Should reject renaming in comments"

    def test_rename_in_empty_section(self):
        """Renaming in empty sections should be handled safely."""
        from cp2k_input_tools.rename import can_rename

        inp = """\
&GLOBAL
&END GLOBAL
"""
        pos = Position(line=1, character=5)

        # Should not crash
        result = can_rename(inp, pos, "file://test.inp")
        # Result depends on what's at the position

    def test_rename_invalid_position(self):
        """Renaming at invalid position should be rejected."""
        from cp2k_input_tools.rename import can_rename

        inp = """\
&GLOBAL
  PROJECT_NAME test
&END GLOBAL
"""
        # Position outside document
        pos = Position(line=100, character=100)

        result = can_rename(inp, pos, "file://test.inp")
        assert result is False, "Should reject invalid position"


# =============================================================================
# Integration tests
# =============================================================================


class TestRenameIntegration:
    """Integration tests for rename functionality."""

    def test_full_rename_workflow(self):
        """Test complete rename workflow from prepare to edit."""
        from cp2k_input_tools.rename import can_rename, get_rename_edit

        inp = """\
&FORCE_EVAL
  &SUBSYS
    &KIND H
      BASIS_SET DZVP
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
"""
        pos = _find_position_in_section_param(inp, "KIND")

        # Step 1: Check if rename is possible
        can_rename_result = can_rename(inp, pos, "file://test.inp")
        assert can_rename_result is True, "Should allow section parameter rename"

        # Step 2: Get the edit
        edit = get_rename_edit(inp, pos, "file://test.inp", "He")
        assert edit is not None, "Should provide WorkspaceEdit"

        # Step 3: Verify edit structure
        assert isinstance(edit, WorkspaceEdit), "Should return WorkspaceEdit"
        if edit.document_changes:
            assert len(edit.document_changes) > 0, "Should have document changes"
