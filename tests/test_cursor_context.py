"""
Tests for CP2K input cursor context resolution.

TDD: Tests written first, implementation to follow.
"""

import pytest

from cp2k_input_tools.cursor_context import (
    resolve_cursor_context,
)


@pytest.mark.unit
class TestCursorContext:
    """Test cursor context resolution for CP2K input files."""

    def test_cursor_inside_force_eval_dft_qs_resolves_section_path(self) -> None:
        """Test that cursor inside FORCE_EVAL/DFT/QS resolves correct section path."""
        text = """&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    &QS
      METHOD GPW
    &END QS
  &END DFT
&END FORCE_EVAL
"""
        ctx = resolve_cursor_context(text, line=4, character=10, uri="test.inp")
        assert ctx.section_path == ("FORCE_EVAL", "DFT", "QS")
        assert ctx.current_section == "QS"

    def test_cursor_after_method_in_qs_resolves_keyword(self) -> None:
        """Test that cursor after METHOD in &QS resolves keyword."""
        text = """&FORCE_EVAL
  &DFT
    &QS
      METHOD GPW
    &END QS
  &END DFT
&END FORCE_EVAL
"""
        # Character 16 is after "METHOD " (in the value position)
        ctx = resolve_cursor_context(text, line=3, character=16, uri="test.inp")
        assert ctx.current_keyword == "METHOD"
        assert ctx.is_value_position is True

    def test_cursor_after_method_g_resolves_prefix(self) -> None:
        """Test that cursor after METHOD G resolves prefix and value position."""
        text = """&QS
  METHOD G
&END QS
"""
        ctx = resolve_cursor_context(text, line=1, character=12, uri="test.inp")
        assert ctx.current_keyword == "METHOD"
        assert ctx.prefix == "G"
        assert ctx.is_value_position is True

    def test_cursor_after_ampersand_at_top_level_is_section_start(self) -> None:
        """Test that cursor after & at top level is section start."""
        text = """&GLOBAL
  PROJECT NaCl
&END GLOBAL
"""
        ctx = resolve_cursor_context(text, line=0, character=2, uri="test.inp")
        assert ctx.is_section_start is True

    def test_cursor_after_end_section_is_section_end(self) -> None:
        """Test that cursor after &END resolves section end."""
        text = """&QS
  METHOD GPW
&END QS
"""
        ctx = resolve_cursor_context(text, line=2, character=7, uri="test.inp")
        assert ctx.is_section_end is True

    def test_works_on_incomplete_documents_unclosed_section(self) -> None:
        """Test that resolution works on incomplete documents."""
        text = """&FORCE_EVAL
  &DFT
    &QS
      METHOD GPW
"""
        ctx = resolve_cursor_context(text, line=3, character=10, uri="test.inp")
        assert ctx.section_path == ("FORCE_EVAL", "DFT", "QS")
        assert ctx.current_section == "QS"

    def test_preprocessor_if_block_no_spurious_diagnostics(self) -> None:
        """Test that preprocessor directives don't break resolution."""
        text = """&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    @IF ${HP-1}
      &KPOINTS
        SCHEME MONKHORST-PACK 8 8 8
      &END KPOINTS
    @ENDIF
  &END DFT
&END FORCE_EVAL
"""
        ctx = resolve_cursor_context(text, line=6, character=10, uri="test.inp")
        assert ctx.section_path == ("FORCE_EVAL", "DFT", "KPOINTS")
        assert ctx.current_section == "KPOINTS"

    def test_set_directive_recognized(self) -> None:
        """Test that @SET directives are recognized."""
        text = """@SET CUTOFF 600
&MGRID
  CUTOFF ${CUTOFF}
&END MGRID
"""
        ctx = resolve_cursor_context(text, line=0, character=5, uri="test.inp")
        # Should not break - just return what we can
        assert ctx.uri == "test.inp"

    def test_cursor_at_keyword_position(self) -> None:
        """Test cursor at keyword position inside section."""
        text = """&QS
  METHOD GPW
  EXTRAPOLATION USE_GUESS
&END QS
"""
        ctx = resolve_cursor_context(text, line=2, character=5, uri="test.inp")
        assert ctx.is_keyword_position is True
        assert ctx.current_keyword == "EXTRAPOLATION"

    def test_cursor_at_section_name(self) -> None:
        """Test cursor at section name after &."""
        text = """&FORCE_EVAL
  &DFT
    &QS
      METHOD GPW
    &END QS
  &END DFT
&END FORCE_EVAL
"""
        ctx = resolve_cursor_context(text, line=1, character=5, uri="test.inp")
        assert ctx.is_section_start is True
        # We're at "DFT" section

    def test_empty_document(self) -> None:
        """Test cursor context on empty document."""
        text = ""
        ctx = resolve_cursor_context(text, line=0, character=0, uri="test.inp")
        assert ctx.section_path == ()
        assert ctx.current_section is None

    def test_nested_sections_correct_path(self) -> None:
        """Test deeply nested section path."""
        text = """&FORCE_EVAL
  &DFT
    &QS
      &SCF
        MAX_SCF 50
      &END SCF
    &END QS
  &END DFT
&END FORCE_EVAL
"""
        ctx = resolve_cursor_context(text, line=4, character=12, uri="test.inp")
        assert ctx.section_path == ("FORCE_EVAL", "DFT", "QS", "SCF")

    def test_repeated_sections_distinguished(self) -> None:
        """Test that repeated sections are handled correctly."""
        text = """&FORCE_EVAL
  &SUBSYS
    &KIND Na
    &END KIND
    &KIND Cl
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
"""
        # First KIND
        ctx1 = resolve_cursor_context(text, line=2, character=10, uri="test.inp")
        assert ctx1.current_section == "KIND"

        # Second KIND (line 4 is inside "&KIND Cl", not the closing &END)
        ctx2 = resolve_cursor_context(text, line=4, character=10, uri="test.inp")
        assert ctx2.current_section == "KIND"
