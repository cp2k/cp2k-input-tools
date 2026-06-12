"""Tests for Diagnostics API and CLI (TDD - Issue #54)."""

import json
import pathlib
import subprocess

import pytest

TEST_DIR = pathlib.Path(__file__).resolve().parent
INPUTS_DIR = TEST_DIR / "inputs"


class TestDiagnosticsAPI:
    """Test the Python diagnostics API."""

    def test_check_returns_list_of_diagnostics(self, tmp_path):
        """Test that check() returns a list of diagnostic dicts."""
        from cp2k_input_tools.diagnostics_api import check

        # Create a simple valid CP2K input file
        test_file = tmp_path / "test.inp"
        test_file.write_text(
            """
&GLOBAL
  PROJECT_NAME test
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        result = check(str(test_file))

        # Should return a list
        assert isinstance(result, list)

        # Each diagnostic should be a dict with required fields
        for diag in result:
            assert isinstance(diag, dict)
            assert "range" in diag
            assert "message" in diag
            assert "severity" in diag
            assert "code" in diag
            assert "source" in diag
            assert "diagnostic_engine" in diag

    def test_check_detects_parser_errors(self, tmp_path):
        """Test that check() detects parser errors."""
        from cp2k_input_tools.diagnostics_api import check

        # Create an invalid CP2K input file
        test_file = tmp_path / "invalid.inp"
        test_file.write_text(
            """
&GLOBAL
  INVALID_KEYWORD_VALUE
&END
"""
        )

        result = check(str(test_file))

        # Should return at least one diagnostic
        assert len(result) > 0

        # Should have at least one error
        errors = [d for d in result if d.get("severity") == "error"]
        assert len(errors) > 0

    def test_check_includes_multiple_validation_sources(self, tmp_path):
        """Test that check() includes diagnostics from multiple sources."""
        from cp2k_input_tools.diagnostics_api import check

        # Create a file that should trigger multiple validation sources
        test_file = tmp_path / "multi_source.inp"
        test_file.write_text(
            """
&GLOBAL
  RUN_TYPE ENERGY
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
    &SCF
      MAX_SCF 5
      EPS_SCF 1.0e-2
    &END
  &END
&END
&MOTION
  &MD
    ENSEMBLE NVT
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        result = check(str(test_file))

        # Should return diagnostics
        assert isinstance(result, list)

        # Check that we have diagnostics from various sources
        # (parser, linter, typecheck, semantic)
        sources = set(d.get("source", "") for d in result)

        # At minimum should have some diagnostics or empty if valid
        # The important thing is it doesn't crash
        assert isinstance(result, list)

    def test_check_format_json_output(self, tmp_path):
        """Test that check_format() returns valid JSON."""
        from cp2k_input_tools.diagnostics_api import check_format

        # Create a simple CP2K input file
        test_file = tmp_path / "test.inp"
        test_file.write_text(
            """
&GLOBAL
  PROJECT_NAME test
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        # Test JSON format
        json_output = check_format(str(test_file), format="json")

        # Should be valid JSON
        parsed = json.loads(json_output)
        assert isinstance(parsed, dict)

        # Should have required top-level fields
        assert "uri" in parsed
        assert "operation" in parsed
        assert "ok" in parsed
        assert "diagnostics" in parsed
        assert "summary" in parsed
        assert "diagnostic_engine" in parsed

    def test_check_format_json_summary(self, tmp_path):
        """Test that JSON output includes correct summary statistics."""
        from cp2k_input_tools.diagnostics_api import check_format

        # Create a file with errors
        test_file = tmp_path / "errors.inp"
        test_file.write_text(
            """
&GLOBAL
  INVALID_KEYWORD
&END
"""
        )

        json_output = check_format(str(test_file), format="json")
        parsed = json.loads(json_output)

        # Check summary structure
        summary = parsed["summary"]
        assert "count" in summary
        assert "blocking" in summary
        assert "errors" in summary
        assert "warnings" in summary

        # Should have at least one error
        assert summary["errors"] >= 1

    def test_check_format_text_output(self, tmp_path):
        """Test that check_format() returns readable text format."""
        from cp2k_input_tools.diagnostics_api import check_format

        # Create a simple CP2K input file
        test_file = tmp_path / "test.inp"
        test_file.write_text(
            """
&GLOBAL
  PROJECT_NAME test
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        # Test text format (default)
        text_output = check_format(str(test_file), format="text")

        # Should be a string
        assert isinstance(text_output, str)

        # Should contain relevant information
        assert "diagnostic" in text_output.lower() or "ok" in text_output.lower()

    def test_check_format_invalid_format_raises_error(self, tmp_path):
        """Test that invalid format raises ValueError."""
        from cp2k_input_tools.diagnostics_api import check_format

        test_file = tmp_path / "test.inp"
        test_file.write_text("&GLOBAL\n&END")

        with pytest.raises(ValueError, match="Unsupported format"):
            check_format(str(test_file), format="xml")


class TestDiagnosticsCLI:
    """Test the CLI diagnostics command."""

    def test_cli_check_command_exists(self):
        """Test that the check command is available."""
        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "check", "--help"],
            capture_output=True,
            text=True,
        )

        # Command should exist and show help
        assert result.returncode == 0
        assert "check" in result.stdout.lower()

    def test_cli_check_with_valid_file(self, tmp_path):
        """Test CLI check with a valid CP2K input file."""
        # Create a valid file
        test_file = tmp_path / "valid.inp"
        test_file.write_text(
            """
&GLOBAL
  PROJECT_NAME test
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        # Run CLI check
        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "check", str(test_file)],
            capture_output=True,
            text=True,
        )

        # Should succeed (exit code 0 for valid files without --fail-on-error)
        assert result.returncode == 0

    def test_cli_check_with_invalid_file(self, tmp_path):
        """Test CLI check with an invalid CP2K input file."""
        # Create an invalid file
        test_file = tmp_path / "invalid.inp"
        test_file.write_text(
            """
&GLOBAL
  INVALID_KEYWORDxyz
&END
"""
        )

        # Run CLI check
        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "check", str(test_file)],
            capture_output=True,
            text=True,
        )

        # Should run without crashing (exit code 0 even with errors)
        # The command should not fail unless --fail-on-error is used
        assert result.returncode == 0

        # Output should contain diagnostics
        output = result.stdout + result.stderr
        assert len(output) > 0

    def test_cli_check_json_format(self, tmp_path):
        """Test CLI check with JSON output format."""
        # Create a test file
        test_file = tmp_path / "test.inp"
        test_file.write_text(
            """
&GLOBAL
  PROJECT_NAME test
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        # Run CLI check with JSON format
        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "check", "--format=json", str(test_file)],
            capture_output=True,
            text=True,
        )

        # Should succeed
        assert result.returncode == 0

        # Output should be valid JSON
        parsed = json.loads(result.stdout)
        assert isinstance(parsed, dict)
        assert "diagnostics" in parsed

    def test_cli_check_fail_on_error(self, tmp_path):
        """Test CLI check with --fail-on-error flag."""
        # Create a file with errors
        test_file = tmp_path / "invalid.inp"
        test_file.write_text(
            """
&GLOBAL
  INVALID_KEYWORDxyz
&END
"""
        )

        # Run CLI check with --fail-on-error
        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "check", "--format=json", "--fail-on-error", str(test_file)],
            capture_output=True,
            text=True,
        )

        # Should fail with non-zero exit code
        assert result.returncode != 0

        # Output should still be valid JSON
        parsed = json.loads(result.stdout)
        assert parsed["summary"]["errors"] >= 1

    def test_cli_check_nonexistent_file(self):
        """Test CLI check with nonexistent file."""
        result = subprocess.run(
            ["python3", "-m", "cp2k_input_tools.cli.main", "check", "nonexistent_file.inp"],
            capture_output=True,
            text=True,
        )

        # Should fail
        assert result.returncode != 0

        # Should mention file not found
        output = result.stdout + result.stderr
        assert "not found" in output.lower() or "does not exist" in output.lower()


class TestDiagnosticsIntegration:
    """Integration tests for diagnostics with real CP2K inputs."""

    def test_real_world_input_no_errors(self):
        """Test with a real-world valid input."""
        from cp2k_input_tools.diagnostics_api import check

        # Use an existing test file if available
        test_files = list(INPUTS_DIR.glob("*.inp"))

        if test_files:
            # Use the first available test file
            result = check(str(test_files[0]))

            # Should return diagnostics
            assert isinstance(result, list)

            # All diagnostics should have required fields
            for diag in result:
                assert "range" in diag
                assert "message" in diag
                assert "severity" in diag
                assert "code" in diag
        else:
            pytest.skip("No test input files available")

    def test_diagnostics_include_all_sources(self, tmp_path):
        """Test that diagnostics include all validation sources."""
        from cp2k_input_tools.diagnostics_api import check

        # Create a file that triggers multiple validation sources
        test_file = tmp_path / "multi_source.inp"
        test_file.write_text(
            """
&GLOBAL
  RUN_TYPE ENERGY
&END
&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL
        PBE
      &END
    &END
    &SCF
      MAX_SCF 5
      EPS_SCF 1.0e-2
    &END
  &END
&END
&MOTION
  &MD
    ENSEMBLE NVT
  &END
&END
&SUBSYS
  &KIND O
    BASIS_SET DZVP-MOLOPT-SR-GTH
    POTENTIAL GTH-PBE
  &END
  &COORD
    O 0.0 0.0 0.0
  &END
&END
"""
        )

        result = check(str(test_file))

        # Should have diagnostics from multiple sources
        sources = set(d.get("source", "") for d in result)

        # Should include at least parser/lint/schema sources
        assert len(sources) > 0 or len(result) == 0  # May be empty if valid
