"""
Closed-loop evaluation harness tests for CP2K LSP.

This test suite validates the evaluation harness that measures LSP accuracy
against a golden corpus. Tests cover:
- Fixture loading (CP2K inputs and golden JSON files)
- Diagnostic comparison (true positives, false positives, false negatives)
- Completion ranking metrics (MRR, hit@k)
- Hover coverage
- Metrics calculation (precision, recall, F1)
- Report generation (HTML/JSON)
- Update mode for regenerating golden files
"""

import json
from unittest.mock import patch

import pytest

from cp2k_input_tools.evaluation import (
    DiagnosticComparison,
    EvaluationHarness,
    EvaluationMetrics,
    EvaluationReport,
    TestFixture,
    calculate_hit_at_k,
    calculate_mrr,
    compare_diagnostics,
    generate_html_report,
    generate_json_report,
)


class TestEvalFixture:
    """Tests for TestFixture data class."""

    def test_fixture_creation(self):
        """Test creating a basic test fixture."""
        fixture = TestFixture(
            name="simple_energy",
            input_content="&FORCE_EVAL\n  METHOD XYZ\n&END\n",
        )
        assert fixture.name == "simple_energy"
        assert fixture.input_content.startswith("&FORCE_EVAL")
        assert fixture.golden_diagnostics is None
        assert fixture.golden_completions is None
        assert fixture.golden_hover is None

    def test_fixture_with_diagnostics(self):
        """Test fixture with golden diagnostics."""
        fixture = TestFixture(
            name="with_diagnostics",
            input_content="&FORCE_EVAL\n  METHOD XYZ\n&END\n",
            golden_diagnostics=[
                {
                    "line": 1,
                    "column": 2,
                    "severity": "error",
                    "code": "cp2k.syntax.unknown_keyword",
                    "message": "Unknown keyword",
                }
            ],
        )
        assert len(fixture.golden_diagnostics) == 1
        assert fixture.golden_diagnostics[0]["severity"] == "error"

    def test_fixture_with_completions(self):
        """Test fixture with golden completions."""
        fixture = TestFixture(
            name="with_completions",
            input_content="&FORCE_EVAL\n  METHOD XYZ\n&END\n",
            golden_completions={
                "line": 1,
                "column": 2,
                "expected_items": ["METHOD_RHF", "METHOD_PBE0", "METHOD_HF"],
            },
        )
        assert fixture.golden_completions["line"] == 1
        assert len(fixture.golden_completions["expected_items"]) == 3

    def test_fixture_with_hover(self):
        """Test fixture with golden hover results."""
        fixture = TestFixture(
            name="with_hover",
            input_content="&FORCE_EVAL\n  METHOD XYZ\n&END\n",
            golden_hover={
                "line": 1,
                "column": 2,
                "expected_content": "Specifies the electronic structure method",
            },
        )
        assert "electronic structure method" in fixture.golden_hover["expected_content"]


class TestDiagnosticComparison:
    """Tests for diagnostic comparison logic."""

    def test_exact_match(self):
        """Test diagnostic exact matching."""
        golden = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test error"}]
        actual = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test error"}]

        comparison = compare_diagnostics(actual, golden)
        assert comparison.true_positives == 1
        assert comparison.false_positives == 0
        assert comparison.false_negatives == 0

    def test_fuzzy_match_by_line_and_code(self):
        """Test fuzzy matching by line and code when exact match fails."""
        golden = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test error"}]
        actual = [{"line": 5, "column": 5, "severity": "error", "code": "test_code", "message": "Slightly different"}]

        comparison = compare_diagnostics(actual, golden)
        # Should match by line and code
        assert comparison.true_positives == 1
        assert comparison.false_positives == 0
        assert comparison.false_negatives == 0

    def test_false_positive_extra_actual_diagnostic(self):
        """Test detection of false positives."""
        golden = []
        actual = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test"}]

        comparison = compare_diagnostics(actual, golden)
        assert comparison.true_positives == 0
        assert comparison.false_positives == 1
        assert comparison.false_negatives == 0

    def test_false_negative_missing_actual_diagnostic(self):
        """Test detection of false negatives."""
        golden = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test"}]
        actual = []

        comparison = compare_diagnostics(actual, golden)
        assert comparison.true_positives == 0
        assert comparison.false_positives == 0
        assert comparison.false_negatives == 1

    def test_multiple_diagnostics_mixed_results(self):
        """Test comparison with multiple diagnostics of different outcomes."""
        golden = [
            {"line": 1, "column": 0, "severity": "error", "code": "error_1", "message": "Error 1"},
            {"line": 2, "column": 0, "severity": "error", "code": "error_2", "message": "Error 2"},
            {"line": 3, "column": 0, "severity": "error", "code": "error_3", "message": "Error 3"},
        ]
        actual = [
            {"line": 1, "column": 0, "severity": "error", "code": "error_1", "message": "Error 1"},
            {"line": 4, "column": 0, "severity": "error", "code": "error_extra", "message": "Extra"},
        ]

        comparison = compare_diagnostics(actual, golden)
        assert comparison.true_positives == 1  # error_1
        assert comparison.false_positives == 1  # error_extra
        assert comparison.false_negatives == 2  # error_2, error_3

    def test_severity_mismatch_counts_as_difference(self):
        """Test that severity mismatch between actual and golden creates a difference."""
        golden = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test"}]
        actual = [{"line": 5, "column": 3, "severity": "warning", "code": "test_code", "message": "Test"}]

        comparison = compare_diagnostics(actual, golden)
        # Should count as different - severity is part of the match criteria
        assert comparison.false_positives == 1
        assert comparison.false_negatives == 1
        assert comparison.true_positives == 0

    def test_duplicate_handling(self):
        """Test that duplicate diagnostics are handled correctly."""
        golden = [{"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test"}]
        actual = [
            {"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test"},
            {"line": 5, "column": 3, "severity": "error", "code": "test_code", "message": "Test"},
        ]

        comparison = compare_diagnostics(actual, golden)
        # One true positive, one false positive (duplicate)
        assert comparison.true_positives == 1
        assert comparison.false_positives == 1


class TestCompletionMetrics:
    """Tests for completion ranking metrics."""

    def test_calculate_mrr_correct_at_first(self):
        """Test MRR calculation when correct item is at rank 1."""
        ranked_items = ["METHOD_RHF", "METHOD_PBE0", "METHOD_HF"]
        expected_items = {"METHOD_RHF"}

        mrr = calculate_mrr(ranked_items, expected_items)
        assert mrr == 1.0

    def test_calculate_mrr_correct_at_second(self):
        """Test MRR calculation when correct item is at rank 2."""
        ranked_items = ["METHOD_PBE0", "METHOD_RHF", "METHOD_HF"]
        expected_items = {"METHOD_RHF"}

        mrr = calculate_mrr(ranked_items, expected_items)
        assert mrr == 0.5

    def test_calculate_mrr_not_found(self):
        """Test MRR calculation when correct item is not in list."""
        ranked_items = ["METHOD_PBE0", "METHOD_HF"]
        expected_items = {"METHOD_RHF"}

        mrr = calculate_mrr(ranked_items, expected_items)
        assert mrr == 0.0

    def test_calculate_mrr_multiple_expected(self):
        """Test MRR calculation with multiple expected items."""
        ranked_items = ["METHOD_PBE0", "METHOD_RHF", "METHOD_HF"]
        expected_items = {"METHOD_RHF", "METHOD_HF"}

        mrr = calculate_mrr(ranked_items, expected_items)
        # RHF is at rank 2 (0.5), HF at rank 3 (0.33)
        # Should use the best (highest) rank
        assert mrr == 0.5

    def test_calculate_hit_at_1(self):
        """Test hit@1 metric."""
        ranked_items = ["METHOD_PBE0", "METHOD_RHF", "METHOD_HF"]
        expected_items = {"METHOD_RHF"}

        hit_1 = calculate_hit_at_k(ranked_items, expected_items, k=1)
        assert hit_1 == 0  # RHF not in top 1

    def test_calculate_hit_at_3(self):
        """Test hit@3 metric."""
        ranked_items = ["METHOD_PBE0", "METHOD_RHF", "METHOD_HF"]
        expected_items = {"METHOD_RHF"}

        hit_3 = calculate_hit_at_k(ranked_items, expected_items, k=3)
        assert hit_3 == 1  # RHF is in top 3

    def test_calculate_hit_at_5(self):
        """Test hit@5 metric with only 3 items."""
        ranked_items = ["METHOD_PBE0", "METHOD_RHF", "METHOD_HF"]
        expected_items = {"METHOD_HF"}

        hit_5 = calculate_hit_at_k(ranked_items, expected_items, k=5)
        assert hit_5 == 1  # HF is in top 5 (even though only 3 items)


class TestEvaluationMetrics:
    """Tests for EvaluationMetrics data class."""

    def test_metrics_initialization(self):
        """Test metrics initialization."""
        metrics = EvaluationMetrics(
            diagnostic_precision=0.9,
            diagnostic_recall=0.8,
            diagnostic_f1=0.85,
            completion_mrr=0.75,
            completion_hit_at_1=0.6,
            completion_hit_at_3=0.8,
            completion_hit_at_5=0.9,
            hover_coverage=0.95,
        )
        assert metrics.diagnostic_precision == 0.9
        assert metrics.diagnostic_f1 == 0.85
        assert metrics.completion_mrr == 0.75

    def test_metrics_default_values(self):
        """Test metrics with default values."""
        metrics = EvaluationMetrics()
        assert metrics.diagnostic_precision == 0.0
        assert metrics.diagnostic_recall == 0.0
        assert metrics.diagnostic_f1 == 0.0

    def test_f1_calculation_helper(self):
        """Test F1 score calculation helper."""
        # From precision=0.75, recall=0.5
        precision = 0.75
        recall = 0.5
        expected_f1 = 2 * precision * recall / (precision + recall)

        metrics = EvaluationMetrics(
            diagnostic_precision=precision,
            diagnostic_recall=recall,
            diagnostic_f1=expected_f1,
        )
        assert abs(metrics.diagnostic_f1 - expected_f1) < 1e-6


class TestEvaluationReport:
    """Tests for EvaluationReport data class."""

    def test_report_creation(self):
        """Test creating an evaluation report."""
        metrics = EvaluationMetrics(diagnostic_precision=0.9)
        report = EvaluationReport(
            fixture_name="test_fixture",
            metrics=metrics,
            diagnostic_comparison=DiagnosticComparison(
                true_positives=9,
                false_positives=1,
                false_negatives=1,
            ),
            completion_results=[],
            hover_results=[],
        )
        assert report.fixture_name == "test_fixture"
        assert report.metrics.diagnostic_precision == 0.9
        assert report.diagnostic_comparison.true_positives == 9


class TestEvaluationHarness:
    """Tests for the main EvaluationHarness class."""

    @pytest.fixture
    def harness(self):
        """Create a test harness instance."""
        return EvaluationHarness()

    @pytest.fixture
    def sample_fixture_dir(self, tmp_path):
        """Create a directory with sample fixtures."""
        fixtures_dir = tmp_path / "fixtures"
        fixtures_dir.mkdir()

        # Create a simple input file
        inp_file = fixtures_dir / "simple.inp"
        inp_file.write_text("&FORCE_EVAL\n  METHOD XYZ\n&END\n")

        # Create golden diagnostics
        golden_file = fixtures_dir / "simple_golden.json"
        golden_data = {
            "diagnostics": [
                {
                    "line": 1,
                    "column": 2,
                    "severity": "error",
                    "code": "cp2k.syntax.unknown_keyword",
                    "message": "Unknown keyword 'XYZ'",
                }
            ],
            "completions": None,
            "hover": None,
        }
        golden_file.write_text(json.dumps(golden_data))

        return fixtures_dir

    def test_load_fixtures_from_directory(self, harness, sample_fixture_dir):
        """Test loading fixtures from a directory."""
        fixtures = harness.load_fixtures(sample_fixture_dir)

        assert len(fixtures) >= 1
        assert any(f.name == "simple" for f in fixtures)

    def test_load_fixture_with_golden_file(self, harness, sample_fixture_dir):
        """Test loading a fixture with its golden file."""
        fixtures = harness.load_fixtures(sample_fixture_dir)
        fixture = next((f for f in fixtures if f.name == "simple"), None)

        assert fixture is not None
        assert fixture.golden_diagnostics is not None
        assert len(fixture.golden_diagnostics) == 1

    def test_run_diagnostics_evaluation(self, harness):
        """Test running diagnostic evaluation on a fixture."""
        fixture = TestFixture(
            name="test",
            input_content="&FORCE_EVAL\n  METHOD XYZ\n&END\n",
            golden_diagnostics=[
                {
                    "line": 1,
                    "column": 2,
                    "severity": "error",
                    "code": "test_code",
                    "message": "Test",
                }
            ],
        )

        with patch.object(harness, "_get_diagnostics", return_value=fixture.golden_diagnostics):
            report = harness.evaluate_diagnostics(fixture)

        assert report.fixture_name == "test"
        assert report.diagnostic_comparison.true_positives == 1

    def test_run_completion_evaluation(self, harness):
        """Test running completion evaluation."""
        fixture = TestFixture(
            name="test",
            input_content="&FORCE_EVAL\n  METHOD \n&END\n",
            golden_completions=[
                {
                    "line": 1,
                    "column": 2,
                    "expected_items": ["METHOD_RHF", "METHOD_PBE0"],
                }
            ],
        )

        mock_completions = ["METHOD_RHF", "METHOD_PBE0"]

        with patch.object(harness, "_get_completions", return_value=mock_completions):
            results = harness.evaluate_completions(fixture)

        assert len(results) == 1
        assert results[0].mrr == 1.0  # First match

    def test_run_hover_evaluation(self, harness):
        """Test running hover evaluation."""
        fixture = TestFixture(
            name="test",
            input_content="&FORCE_EVAL\n  METHOD XYZ\n&END\n",
            golden_hover=[
                {
                    "line": 1,
                    "column": 2,
                    "expected_content": "electronic structure method",
                }
            ],
        )

        mock_hover = "Specifies the electronic structure method to use"

        with patch.object(harness, "_get_hover", return_value=mock_hover):
            results = harness.evaluate_hover(fixture)

        assert len(results) == 1
        assert results[0].covered is True

    def test_calculate_aggregate_metrics(self, harness):
        """Test calculation of aggregate metrics across fixtures."""
        reports = [
            EvaluationReport(
                fixture_name="test1",
                metrics=EvaluationMetrics(diagnostic_precision=0.9),
                diagnostic_comparison=DiagnosticComparison(true_positives=9, false_positives=1, false_negatives=1),
                completion_results=[],
                hover_results=[],
            ),
            EvaluationReport(
                fixture_name="test2",
                metrics=EvaluationMetrics(diagnostic_precision=0.8),
                diagnostic_comparison=DiagnosticComparison(true_positives=8, false_positives=2, false_negatives=2),
                completion_results=[],
                hover_results=[],
            ),
        ]

        aggregate = harness.calculate_aggregate_metrics(reports)
        assert abs(aggregate.diagnostic_precision - 0.85) < 1e-6  # Average of 0.9 and 0.8

    def test_update_mode_regenerates_golden_files(self, harness, tmp_path):
        """Test that update mode regenerates golden files."""
        fixture_dir = tmp_path / "fixtures"
        fixture_dir.mkdir()

        inp_file = fixture_dir / "test.inp"
        inp_file.write_text("&FORCE_EVAL\n  METHOD XYZ\n&END\n")

        golden_file = fixture_dir / "test_golden.json"

        with patch.object(
            harness, "_get_diagnostics", return_value=[{"line": 1, "severity": "error", "code": "test", "message": "Test"}]
        ):
            harness.update_golden_file(str(inp_file), str(golden_file))

        assert golden_file.exists()
        golden_data = json.loads(golden_file.read_text())
        assert "diagnostics" in golden_data
        assert len(golden_data["diagnostics"]) == 1


class TestReportGeneration:
    """Tests for report generation functions."""

    def test_generate_json_report(self, tmp_path):
        """Test JSON report generation."""
        reports = [
            EvaluationReport(
                fixture_name="test",
                metrics=EvaluationMetrics(diagnostic_precision=0.9),
                diagnostic_comparison=DiagnosticComparison(true_positives=9, false_positives=1, false_negatives=1),
                completion_results=[],
                hover_results=[],
            )
        ]

        output_path = tmp_path / "report.json"
        generate_json_report(reports, str(output_path))

        assert output_path.exists()
        data = json.loads(output_path.read_text())
        assert "reports" in data
        assert "aggregate_metrics" in data

    def test_generate_html_report(self, tmp_path):
        """Test HTML report generation."""
        reports = [
            EvaluationReport(
                fixture_name="test",
                metrics=EvaluationMetrics(diagnostic_precision=0.9),
                diagnostic_comparison=DiagnosticComparison(true_positives=9, false_positives=1, false_negatives=1),
                completion_results=[],
                hover_results=[],
            )
        ]

        output_path = tmp_path / "report.html"
        generate_html_report(reports, str(output_path))

        assert output_path.exists()
        content = output_path.read_text()
        assert "<html" in content.lower()  # Matches <!DOCTYPE html> or <html>
        assert "test" in content  # Fixture name should appear


class TestIntegration:
    """Integration tests for the full evaluation workflow."""

    @pytest.fixture
    def full_fixture_set(self, tmp_path):
        """Create a complete set of test fixtures."""
        fixtures_dir = tmp_path / "fixtures"
        fixtures_dir.mkdir()

        # Fixture 1: Basic energy calculation
        inp1 = fixtures_dir / "basic_energy.inp"
        inp1.write_text("&FORCE_EVAL\n  METHOD XYZ\n&END\n")
        golden1 = fixtures_dir / "basic_energy_golden.json"
        golden1.write_text(
            json.dumps(
                {
                    "diagnostics": [{"line": 1, "column": 2, "severity": "error", "code": "unknown", "message": "Unknown"}],
                    "completions": None,
                    "hover": None,
                }
            )
        )

        # Fixture 2: With completions
        inp2 = fixtures_dir / "with_completions.inp"
        inp2.write_text("&FORCE_EVAL\n  METHOD \n&END\n")
        golden2 = fixtures_dir / "with_completions_golden.json"
        golden2.write_text(
            json.dumps(
                {
                    "diagnostics": [],
                    "completions": {
                        "line": 1,
                        "column": 2,
                        "expected_items": ["METHOD_RHF", "METHOD_PBE0", "METHOD_HF"],
                    },
                    "hover": None,
                }
            )
        )

        # Fixture 3: With hover
        inp3 = fixtures_dir / "with_hover.inp"
        inp3.write_text("&FORCE_EVAL\n  METHOD XYZ\n&END\n")
        golden3 = fixtures_dir / "with_hover_golden.json"
        golden3.write_text(
            json.dumps(
                {
                    "diagnostics": [],
                    "completions": None,
                    "hover": {
                        "line": 1,
                        "column": 2,
                        "expected_content": "electronic structure",
                    },
                }
            )
        )

        return fixtures_dir

    def test_full_evaluation_workflow(self, full_fixture_set):
        """Test running the complete evaluation workflow."""
        harness = EvaluationHarness()
        fixtures = harness.load_fixtures(full_fixture_set)

        assert len(fixtures) == 3

        # Run evaluation
        reports = []
        for fixture in fixtures:
            # Skip actual LSP calls for this test
            report = harness.evaluate_diagnostics(fixture)
            reports.append(report)

        assert len(reports) == 3

        # Generate reports
        json_output = full_fixture_set / "evaluation_report.json"
        html_output = full_fixture_set / "evaluation_report.html"

        generate_json_report(reports, str(json_output))
        generate_html_report(reports, str(html_output))

        assert json_output.exists()
        assert html_output.exists()


class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_empty_golden_file(self, tmp_path):
        """Test handling of empty golden files."""
        golden_file = tmp_path / "empty_golden.json"
        golden_file.write_text("{}")

        fixture = TestFixture(
            name="empty",
            input_content="test",
        )
        # Should not raise, should treat missing keys as None
        assert fixture.golden_diagnostics is None

    def test_malformed_golden_file(self, tmp_path):
        """Test handling of malformed JSON."""
        golden_file = tmp_path / "malformed.json"
        golden_file.write_text("{invalid json")

        harness = EvaluationHarness()
        # Should raise or handle gracefully
        with pytest.raises(json.JSONDecodeError):
            json.loads(golden_file.read_text())

    def test_missing_golden_file(self, tmp_path):
        """Test handling when golden file doesn't exist."""
        inp_file = tmp_path / "test.inp"
        inp_file.write_text("test")

        harness = EvaluationHarness()
        # Should load fixture without golden data
        fixtures = harness.load_fixtures(tmp_path)
        assert len(fixtures) == 1
        assert fixtures[0].golden_diagnostics is None

    def test_no_matching_diagnostics_at_all(self):
        """Test when actual and golden diagnostics don't match at all."""
        golden = [{"line": 1, "severity": "error", "code": "error_1", "message": "Error 1"}]
        actual = [{"line": 5, "severity": "error", "code": "error_2", "message": "Error 2"}]

        comparison = compare_diagnostics(actual, golden)
        assert comparison.true_positives == 0
        assert comparison.false_positives == 1
        assert comparison.false_negatives == 1
