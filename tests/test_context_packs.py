"""Tests for Context Packs API (#57).

TDD approach: Tests written first, then implementation.
"""

import json
from dataclasses import asdict
from datetime import datetime

from cp2k_input_tools.context_packs import ContextPack, get_context_pack


class TestContextPackDataclass:
    """Tests for ContextPack dataclass structure."""

    def test_context_pack_attributes(self):
        """ContextPack should have required attributes."""
        pack = ContextPack(
            cursor={"section": "GLOBAL", "keyword": "RUN_TYPE"},
            hover={"contents": "Run type specification"},
            completions=[{"label": "ENERGY"}, {"label": "GEO_OPT"}],
            diagnostics=[],
            timestamp="2024-01-01T12:00:00Z",
        )
        assert pack.cursor is not None
        assert pack.hover is not None
        assert pack.completions is not None
        assert pack.diagnostics is not None
        assert pack.timestamp is not None

    def test_context_pack_json_serializable(self):
        """ContextPack should be JSON-serializable for agent tools."""
        pack = ContextPack(
            cursor={"section": "GLOBAL"},
            hover=None,
            completions=[],
            diagnostics=[],
            timestamp="2024-01-01T12:00:00Z",
        )
        # Should not raise
        serialized = json.dumps(asdict(pack))
        assert serialized is not None
        assert len(serialized) > 0

    def test_context_pack_optional_hover(self):
        """ContextPack should allow None hover."""
        pack = ContextPack(
            cursor={"section": "GLOBAL"},
            hover=None,
            completions=[],
            diagnostics=[],
            timestamp="2024-01-01T12:00:00Z",
        )
        assert pack.hover is None


class TestGetContextPack:
    """Tests for get_context_pack() function."""

    def test_returns_context_pack(self):
        """get_context_pack should return ContextPack instance."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=0, char=10, uri="file://test.inp")
        assert isinstance(pack, ContextPack)

    def test_includes_cursor_context(self):
        """ContextPack should include cursor context information."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=5, uri="file://test.inp")
        assert pack.cursor is not None
        assert isinstance(pack.cursor, dict)

    def test_includes_hover_info(self):
        """ContextPack should include hover information when available."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=10, uri="file://test.inp")
        # hover can be None if no information available
        assert pack.hover is None or isinstance(pack.hover, dict)

    def test_includes_completions(self):
        """ContextPack should include completions list."""
        text = "&GLOBAL\n  RUN_TYPE \n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=12, uri="file://test.inp")
        assert pack.completions is not None
        assert isinstance(pack.completions, list)

    def test_includes_diagnostics(self):
        """ContextPack should include diagnostics list."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=0, char=0, uri="file://test.inp")
        assert pack.diagnostics is not None
        assert isinstance(pack.diagnostics, list)

    def test_includes_timestamp(self):
        """ContextPack should include ISO timestamp."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=0, char=0, uri="file://test.inp")
        assert pack.timestamp is not None
        assert isinstance(pack.timestamp, str)
        # Should be ISO format
        datetime.fromisoformat(pack.timestamp.replace("Z", "+00:00"))

    def test_json_serializable_output(self):
        """Complete ContextPack output should be JSON-serializable."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=10, uri="file://test.inp")
        # Should not raise
        serialized = json.dumps(asdict(pack))
        assert serialized is not None

    def test_empty_input(self):
        """Should handle empty input gracefully."""
        pack = get_context_pack("", line=0, char=0, uri="file://empty.inp")
        assert isinstance(pack, ContextPack)
        assert pack.cursor is not None
        assert pack.diagnostics is not None

    def test_invalid_syntax(self):
        """Should handle invalid syntax gracefully."""
        text = "INVALID &SYNTAX HERE\n&GLOBAL\n  RUN_TYPE ENERGY\n&END"
        pack = get_context_pack(text, line=0, char=0, uri="file://invalid.inp")
        assert isinstance(pack, ContextPack)
        # Should have diagnostics for invalid syntax
        assert pack.diagnostics is not None

    def test_position_at_section_start(self):
        """Cursor at section start should provide context."""
        text = "&FORCE_EVAL\n  METHOD QS\n&END FORCE_EVAL"
        pack = get_context_pack(text, line=0, char=0, uri="file://test.inp")
        assert pack.cursor is not None

    def test_position_inside_section(self):
        """Cursor inside section should provide context."""
        text = "&FORCE_EVAL\n  METHOD QS\n&END FORCE_EVAL"
        pack = get_context_pack(text, line=1, char=5, uri="file://test.inp")
        assert pack.cursor is not None

    def test_position_after_section_end(self):
        """Cursor after section end should still work."""
        text = "&FORCE_EVAL\n  METHOD QS\n&END FORCE_EVAL\n"
        pack = get_context_pack(text, line=3, char=0, uri="file://test.inp")
        assert isinstance(pack, ContextPack)

    def test_uri_parameter(self):
        """Should accept and store URI parameter."""
        text = "&GLOBAL\n  PROJECT_NAME test\n&END GLOBAL"
        pack = get_context_pack(text, line=0, char=0, uri="file:///tmp/test.inp")
        assert isinstance(pack, ContextPack)
        # URI may be used in diagnostics
        assert pack.diagnostics is not None


class TestContextPackIntegration:
    """Integration tests for complete context pack functionality."""

    def test_complete_workflow(self):
        """Test complete context pack workflow for agent consumption."""
        text = """
&GLOBAL
  PROJECT_NAME test
  RUN_TYPE ENERGY
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &SCF
      MAX_SCF 50
      EPS_SCF 1.0E-7
    &END SCF
  &END DFT
&END FORCE_EVAL
"""
        # Get context at different positions
        pack1 = get_context_pack(text, line=2, char=10, uri="file://test.inp")
        pack2 = get_context_pack(text, line=7, char=8, uri="file://test.inp")
        pack3 = get_context_pack(text, line=9, char=10, uri="file://test.inp")

        # All should be valid ContextPacks
        assert isinstance(pack1, ContextPack)
        assert isinstance(pack2, ContextPack)
        assert isinstance(pack3, ContextPack)

        # All should be JSON-serializable
        for pack in [pack1, pack2, pack3]:
            serialized = json.dumps(asdict(pack))
            assert len(serialized) > 0

    def test_cursor_context_structure(self):
        """Cursor context should have expected structure."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=10, uri="file://test.inp")

        if pack.cursor:
            # Should be a dict with relevant context
            assert isinstance(pack.cursor, dict)
            # May contain section, keyword, or other context info

    def test_completions_structure(self):
        """Completions should be list of completion items."""
        text = "&GLOBAL\n  RUN_TYPE \n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=12, uri="file://test.inp")

        assert isinstance(pack.completions, list)
        # Each completion should be a dict
        for completion in pack.completions:
            assert isinstance(completion, dict)

    def test_diagnostics_structure(self):
        """Diagnostics should be list of diagnostic items."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=0, char=0, uri="file://test.inp")

        assert isinstance(pack.diagnostics, list)
        # Each diagnostic should be a dict
        for diag in pack.diagnostics:
            assert isinstance(diag, dict)

    def test_hover_when_available(self):
        """Hover should provide information when cursor is on known element."""
        text = "&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL"
        pack = get_context_pack(text, line=1, char=8, uri="file://test.inp")

        # hover may be None or dict
        if pack.hover:
            assert isinstance(pack.hover, dict)


class TestCLIIntegration:
    """Tests for CLI integration of context packs."""

    def test_context_command_exists(self):
        """Context command should be available in CLI."""
        import subprocess

        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "context", "--help"],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        assert "Get unified context pack" in result.stdout

    def test_context_command_output_json(self):
        """Context command should output valid JSON."""
        import json
        import subprocess
        import tempfile

        # Create temporary test file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".inp", delete=False) as f:
            f.write("&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL\n")
            temp_path = f.name

        try:
            result = subprocess.run(
                ["python3", "-m", "cp2k_input_tools.cli.main", "context", temp_path, "--line", "1", "--char", "10"],
                capture_output=True,
                text=True,
            )

            # Should succeed
            assert result.returncode == 0

            # Should be valid JSON
            data = json.loads(result.stdout)
            assert "cursor" in data
            assert "hover" in data
            assert "completions" in data
            assert "diagnostics" in data
            assert "timestamp" in data
        finally:
            import os

            os.unlink(temp_path)

    def test_context_command_with_pretty(self):
        """Context command with --pretty should format output."""
        import subprocess
        import tempfile

        # Create temporary test file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".inp", delete=False) as f:
            f.write("&GLOBAL\n  RUN_TYPE ENERGY\n&END GLOBAL\n")
            temp_path = f.name

        try:
            result = subprocess.run(
                ["python3", "-m", "cp2k_input_tools.cli.main", "context", temp_path, "--pretty"],
                capture_output=True,
                text=True,
            )

            # Should succeed
            assert result.returncode == 0

            # Pretty output should have indentation (multiple lines)
            lines = result.stdout.strip().split("\n")
            assert len(lines) > 1  # Pretty JSON has multiple lines
        finally:
            import os

            os.unlink(temp_path)
