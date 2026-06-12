"""
Closed-loop evaluation harness for CP2K LSP.

This module provides tools for evaluating LSP accuracy against a golden corpus:
- Loading test fixtures (CP2K inputs + golden JSON files)
- Comparing actual vs golden results (diagnostics, completions, hover)
- Calculating metrics (precision, recall, F1, MRR, hit@k)
- Generating reports (HTML/JSON with trace artifacts)
- Supporting update mode to regenerate golden files
"""

import dataclasses
import json
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

from lsprotocol.types import (
    Position,
)

from . import DEFAULT_CP2K_INPUT_XML
from .linter import lint
from .parser import CP2KInputParser


@dataclass
class TestFixture:
    """A test fixture containing input and golden expected outputs."""

    name: str
    input_content: str
    golden_diagnostics: Optional[List[Dict[str, Any]]] = None
    golden_completions: Optional[List[Dict[str, Any]]] = None
    golden_hover: Optional[List[Dict[str, Any]]] = None


@dataclass
class DiagnosticComparison:
    """Result of comparing actual vs golden diagnostics."""

    true_positives: int = 0  # Correctly reported diagnostics
    false_positives: int = 0  # Reported but not in golden
    false_negatives: int = 0  # In golden but not reported
    matches: List[Dict[str, Any]] = field(default_factory=list)
    extra_actual: List[Dict[str, Any]] = field(default_factory=list)
    missed_golden: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class CompletionResult:
    """Result of completion evaluation."""

    position: Position
    expected_items: Set[str]
    actual_items: List[str]
    mrr: float = 0.0
    hit_at_1: int = 0
    hit_at_3: int = 0
    hit_at_5: int = 0


@dataclass
class HoverResult:
    """Result of hover evaluation."""

    position: Position
    expected_content: str
    actual_content: Optional[str]
    covered: bool = False
    similarity_score: float = 0.0


@dataclass
class EvaluationMetrics:
    """Aggregate metrics for evaluation."""

    diagnostic_precision: float = 0.0
    diagnostic_recall: float = 0.0
    diagnostic_f1: float = 0.0
    completion_mrr: float = 0.0
    completion_hit_at_1: float = 0.0
    completion_hit_at_3: float = 0.0
    completion_hit_at_5: float = 0.0
    hover_coverage: float = 0.0


@dataclass
class EvaluationReport:
    """Report for a single fixture evaluation."""

    fixture_name: str
    metrics: EvaluationMetrics
    diagnostic_comparison: DiagnosticComparison
    completion_results: List[CompletionResult]
    hover_results: List[HoverResult]
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())


class EvaluationHarness:
    """Main evaluation harness for running LSP accuracy tests."""

    def __init__(self, parser_xml_path: Optional[str] = None):
        """Initialize the evaluation harness.

        Args:
            parser_xml_path: Path to CP2K input XML schema. Uses default if None.
        """
        self.parser_xml_path = parser_xml_path or DEFAULT_CP2K_INPUT_XML
        self.parser = CP2KInputParser(self.parser_xml_path)

    def load_fixtures(self, fixtures_dir: Path) -> List[TestFixture]:
        """Load all test fixtures from a directory.

        Args:
            fixtures_dir: Path to fixtures directory containing .inp and *_golden.json files.

        Returns:
            List of loaded TestFixture objects.
        """
        fixtures = []
        fixtures_path = Path(fixtures_dir)

        # Find all .inp files
        inp_files = sorted(fixtures_path.glob("*.inp"))

        for inp_file in inp_files:
            # Derive golden file name
            base_name = inp_file.stem
            golden_file = fixtures_path / f"{base_name}_golden.json"

            # Read input content
            input_content = inp_file.read_text()

            # Load golden data if exists
            golden_data = {}
            if golden_file.exists():
                try:
                    golden_data = json.loads(golden_file.read_text())
                except json.JSONDecodeError as e:
                    print(f"Warning: Failed to parse {golden_file}: {e}")

            fixture = TestFixture(
                name=base_name,
                input_content=input_content,
                golden_diagnostics=golden_data.get("diagnostics"),
                golden_completions=golden_data.get("completions"),
                golden_hover=golden_data.get("hover"),
            )
            fixtures.append(fixture)

        return fixtures

    def evaluate_diagnostics(self, fixture: TestFixture) -> EvaluationReport:
        """Evaluate diagnostics for a fixture.

        Args:
            fixture: Test fixture to evaluate.

        Returns:
            EvaluationReport with diagnostic comparison results.
        """
        # Get actual diagnostics from linter
        actual_diagnostics = self._get_diagnostics(fixture.input_content)

        # Compare with golden
        comparison = compare_diagnostics(
            actual_diagnostics,
            fixture.golden_diagnostics or [],
        )

        # Calculate metrics
        total_actual = len(actual_diagnostics)
        total_golden = len(fixture.golden_diagnostics or [])

        precision = comparison.true_positives / total_actual if total_actual > 0 else 0.0
        recall = comparison.true_positives / total_golden if total_golden > 0 else 0.0
        f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0

        metrics = EvaluationMetrics(
            diagnostic_precision=precision,
            diagnostic_recall=recall,
            diagnostic_f1=f1,
        )

        return EvaluationReport(
            fixture_name=fixture.name,
            metrics=metrics,
            diagnostic_comparison=comparison,
            completion_results=[],
            hover_results=[],
        )

    def evaluate_completions(self, fixture: TestFixture) -> List[CompletionResult]:
        """Evaluate completions for a fixture.

        Args:
            fixture: Test fixture to evaluate.

        Returns:
            List of CompletionResult objects.
        """
        results = []

        golden_completions = fixture.golden_completions or []
        for golden in golden_completions:
            line = golden.get("line", 0)
            column = golden.get("column", 0)
            expected_items = set(golden.get("expected_items", []))

            position = Position(line=line, character=column)

            # Get actual completions
            actual_items = self._get_completions(fixture.input_content, position)

            # Calculate metrics
            mrr = calculate_mrr(actual_items, expected_items)
            hit_1 = calculate_hit_at_k(actual_items, expected_items, k=1)
            hit_3 = calculate_hit_at_k(actual_items, expected_items, k=3)
            hit_5 = calculate_hit_at_k(actual_items, expected_items, k=5)

            results.append(
                CompletionResult(
                    position=position,
                    expected_items=expected_items,
                    actual_items=actual_items,
                    mrr=mrr,
                    hit_at_1=hit_1,
                    hit_at_3=hit_3,
                    hit_at_5=hit_5,
                )
            )

        return results

    def evaluate_hover(self, fixture: TestFixture) -> List[HoverResult]:
        """Evaluate hover for a fixture.

        Args:
            fixture: Test fixture to evaluate.

        Returns:
            List of HoverResult objects.
        """
        results = []

        golden_hover = fixture.golden_hover or []
        for golden in golden_hover:
            line = golden.get("line", 0)
            column = golden.get("column", 0)
            expected_content = golden.get("expected_content", "")

            position = Position(line=line, character=column)

            # Get actual hover
            actual_content = self._get_hover(fixture.input_content, position)

            # Check if covered (expected content appears in actual)
            covered = actual_content is not None and expected_content.lower() in actual_content.lower()

            # Simple similarity score based on keyword overlap
            similarity_score = calculate_text_similarity(expected_content, actual_content or "")

            results.append(
                HoverResult(
                    position=position,
                    expected_content=expected_content,
                    actual_content=actual_content,
                    covered=covered,
                    similarity_score=similarity_score,
                )
            )

        return results

    def evaluate_full(self, fixture: TestFixture) -> EvaluationReport:
        """Run full evaluation (diagnostics, completions, hover) for a fixture.

        Args:
            fixture: Test fixture to evaluate.

        Returns:
            Complete EvaluationReport with all metrics.
        """
        # Evaluate diagnostics
        diagnostic_report = self.evaluate_diagnostics(fixture)

        # Evaluate completions
        completion_results = self.evaluate_completions(fixture)

        # Evaluate hover
        hover_results = self.evaluate_hover(fixture)

        # Calculate completion metrics
        if completion_results:
            avg_mrr = sum(r.mrr for r in completion_results) / len(completion_results)
            avg_hit_1 = sum(r.hit_at_1 for r in completion_results) / len(completion_results)
            avg_hit_3 = sum(r.hit_at_3 for r in completion_results) / len(completion_results)
            avg_hit_5 = sum(r.hit_at_5 for r in completion_results) / len(completion_results)
        else:
            avg_mrr = avg_hit_1 = avg_hit_3 = avg_hit_5 = 0.0

        # Calculate hover coverage
        if hover_results:
            hover_coverage = sum(r.covered for r in hover_results) / len(hover_results)
        else:
            hover_coverage = 0.0

        # Update metrics
        diagnostic_report.metrics.completion_mrr = avg_mrr
        diagnostic_report.metrics.completion_hit_at_1 = avg_hit_1
        diagnostic_report.metrics.completion_hit_at_3 = avg_hit_3
        diagnostic_report.metrics.completion_hit_at_5 = avg_hit_5
        diagnostic_report.metrics.hover_coverage = hover_coverage
        diagnostic_report.completion_results = completion_results
        diagnostic_report.hover_results = hover_results

        return diagnostic_report

    def calculate_aggregate_metrics(self, reports: List[EvaluationReport]) -> EvaluationMetrics:
        """Calculate aggregate metrics across all fixture reports.

        Args:
            reports: List of EvaluationReport objects.

        Returns:
            Aggregate EvaluationMetrics.
        """
        if not reports:
            return EvaluationMetrics()

        num_reports = len(reports)

        # Average diagnostic metrics
        avg_diag_precision = sum(r.metrics.diagnostic_precision for r in reports) / num_reports
        avg_diag_recall = sum(r.metrics.diagnostic_recall for r in reports) / num_reports
        avg_diag_f1 = sum(r.metrics.diagnostic_f1 for r in reports) / num_reports

        # Average completion metrics
        avg_comp_mrr = sum(r.metrics.completion_mrr for r in reports) / num_reports
        avg_comp_hit_1 = sum(r.metrics.completion_hit_at_1 for r in reports) / num_reports
        avg_comp_hit_3 = sum(r.metrics.completion_hit_at_3 for r in reports) / num_reports
        avg_comp_hit_5 = sum(r.metrics.completion_hit_at_5 for r in reports) / num_reports

        # Average hover coverage
        avg_hover_coverage = sum(r.metrics.hover_coverage for r in reports) / num_reports

        return EvaluationMetrics(
            diagnostic_precision=avg_diag_precision,
            diagnostic_recall=avg_diag_recall,
            diagnostic_f1=avg_diag_f1,
            completion_mrr=avg_comp_mrr,
            completion_hit_at_1=avg_comp_hit_1,
            completion_hit_at_3=avg_comp_hit_3,
            completion_hit_at_5=avg_comp_hit_5,
            hover_coverage=avg_hover_coverage,
        )

    def update_golden_file(self, inp_file: str, golden_file: str) -> None:
        """Regenerate golden file with current LSP outputs.

        Args:
            inp_file: Path to input file.
            golden_file: Path to golden JSON file to write.
        """
        inp_path = Path(inp_file)
        golden_path = Path(golden_file)

        # Read input
        input_content = inp_path.read_text()

        # Get current LSP outputs
        diagnostics = self._get_diagnostics(input_content)

        # Create golden data structure
        golden_data = {
            "diagnostics": diagnostics,
            "completions": None,  # Would need position annotations in input
            "hover": None,  # Would need position annotations in input
        }

        # Write golden file
        golden_path.parent.mkdir(parents=True, exist_ok=True)
        golden_path.write_text(json.dumps(golden_data, indent=2))

    def _get_diagnostics(self, content: str) -> List[Dict[str, Any]]:
        """Get diagnostics from linter.

        Args:
            content: CP2K input content.

        Returns:
            List of diagnostic dictionaries.
        """
        # Use the linter module

        diagnostics = lint(content)

        # Convert to dict format
        result = []
        for diag in diagnostics:
            diag_dict = {
                "line": diag.line,
                "column": diag.column,
                "end_line": diag.end_line,
                "end_column": diag.end_column,
                "severity": diag.severity,
                "code": diag.code,
                "message": diag.message,
                "source": diag.source,
            }
            # Remove None values
            diag_dict = {k: v for k, v in diag_dict.items() if v is not None}
            result.append(diag_dict)

        return result

    def _get_completions(self, content: str, position: Position) -> List[str]:
        """Get completion items at position.

        Args:
            content: CP2K input content.
            position: Position for completion.

        Returns:
            List of completion label strings.
        """
        # This would connect to the actual LSP completion handler
        # For now, return empty list - to be implemented with actual LSP
        return []

    def _get_hover(self, content: str, position: Position) -> Optional[str]:
        """Get hover information at position.

        Args:
            content: CP2K input content.
            position: Position for hover.

        Returns:
            Hover content string or None.
        """
        # This would connect to the actual LSP hover handler
        # For now, return None - to be implemented with actual LSP
        return None


def compare_diagnostics(actual: List[Dict[str, Any]], golden: List[Dict[str, Any]]) -> DiagnosticComparison:
    """Compare actual diagnostics against golden expected diagnostics.

    Uses fuzzy matching: first tries exact match, then falls back to
    matching by line and code if exact match fails.

    Args:
        actual: List of actual diagnostic dictionaries.
        golden: List of golden expected diagnostic dictionaries.

    Returns:
        DiagnosticComparison with match statistics.
    """
    comparison = DiagnosticComparison()
    matched_golden_indices = set()
    matched_actual_indices = set()

    # Try exact matches first
    for i, actual_diag in enumerate(actual):
        if i in matched_actual_indices:
            continue

        for j, golden_diag in enumerate(golden):
            if j in matched_golden_indices:
                continue

            if _diagnostics_equal(actual_diag, golden_diag):
                comparison.true_positives += 1
                comparison.matches.append(
                    {
                        "actual": actual_diag,
                        "golden": golden_diag,
                    }
                )
                matched_actual_indices.add(i)
                matched_golden_indices.add(j)
                break

    # Try fuzzy matches for remaining
    for i, actual_diag in enumerate(actual):
        if i in matched_actual_indices:
            continue

        for j, golden_diag in enumerate(golden):
            if j in matched_golden_indices:
                continue

            if _diagnostics_fuzzy_match(actual_diag, golden_diag):
                comparison.true_positives += 1
                comparison.matches.append(
                    {
                        "actual": actual_diag,
                        "golden": golden_diag,
                        "fuzzy": True,
                    }
                )
                matched_actual_indices.add(i)
                matched_golden_indices.add(j)
                break

    # Collect false positives (actual not matched)
    for i, actual_diag in enumerate(actual):
        if i not in matched_actual_indices:
            comparison.false_positives += 1
            comparison.extra_actual.append(actual_diag)

    # Collect false negatives (golden not matched)
    for j, golden_diag in enumerate(golden):
        if j not in matched_golden_indices:
            comparison.false_negatives += 1
            comparison.missed_golden.append(golden_diag)

    return comparison


def _diagnostics_equal(d1: Dict[str, Any], d2: Dict[str, Any]) -> bool:
    """Check if two diagnostics are exactly equal."""
    # Required fields that must match
    required_fields = ["line", "severity", "code"]

    for field in required_fields:
        if d1.get(field) != d2.get(field):
            return False

    # Message should be similar (allow minor variations)
    msg1 = d1.get("message", "")
    msg2 = d2.get("message", "")
    if msg1.lower() != msg2.lower():
        return False

    return True


def _diagnostics_fuzzy_match(d1: Dict[str, Any], d2: Dict[str, Any]) -> bool:
    """Check if two diagnostics match fuzzily (line and code)."""
    # Match by line and code
    return d1.get("line") == d2.get("line") and d1.get("code") == d2.get("code") and d1.get("severity") == d2.get("severity")


def calculate_mrr(ranked_items: List[str], expected_items: Set[str]) -> float:
    """Calculate Mean Reciprocal Rank for completion results.

    Args:
        ranked_items: List of completion items in ranked order.
        expected_items: Set of expected completion items.

    Returns:
        MRR score (0.0 to 1.0).
    """
    if not expected_items:
        return 0.0

    for i, item in enumerate(ranked_items):
        if item in expected_items:
            return 1.0 / (i + 1)

    return 0.0


def calculate_hit_at_k(ranked_items: List[str], expected_items: Set[str], k: int) -> float:
    """Calculate hit@k metric for completion results.

    Args:
        ranked_items: List of completion items in ranked order.
        expected_items: Set of expected completion items.
        k: Rank threshold (e.g., 1, 3, 5).

    Returns:
        1.0 if any expected item is in top-k, 0.0 otherwise.
    """
    if not expected_items:
        return 0.0

    top_k = ranked_items[:k]
    return 1.0 if any(item in expected_items for item in top_k) else 0.0


def calculate_text_similarity(text1: str, text2: str) -> float:
    """Calculate simple text similarity based on word overlap.

    Args:
        text1: First text string.
        text2: Second text string.

    Returns:
        Similarity score from 0.0 to 1.0.
    """
    if not text1 or not text2:
        return 0.0

    words1 = set(text1.lower().split())
    words2 = set(text2.lower().split())

    if not words1 or not words2:
        return 0.0

    intersection = words1.intersection(words2)
    union = words1.union(words2)

    return len(intersection) / len(union) if union else 0.0


def generate_json_report(reports: List[EvaluationReport], output_path: str) -> None:
    """Generate JSON evaluation report.

    Args:
        reports: List of EvaluationReport objects.
        output_path: Path to write JSON report.
    """
    output = Path(output_path)
    output.parent.mkdir(parents=True, exist_ok=True)

    # Calculate aggregate metrics
    harness = EvaluationHarness()
    aggregate = harness.calculate_aggregate_metrics(reports)

    # Convert reports to dict format
    report_data = {
        "timestamp": datetime.now().isoformat(),
        "aggregate_metrics": dataclasses.asdict(aggregate),
        "reports": [],
    }

    for report in reports:
        report_dict = {
            "fixture_name": report.fixture_name,
            "timestamp": report.timestamp,
            "metrics": dataclasses.asdict(report.metrics),
            "diagnostic_comparison": {
                "true_positives": report.diagnostic_comparison.true_positives,
                "false_positives": report.diagnostic_comparison.false_positives,
                "false_negatives": report.diagnostic_comparison.false_negatives,
            },
            "completion_count": len(report.completion_results),
            "hover_count": len(report.hover_results),
        }
        report_data["reports"].append(report_dict)

    output.write_text(json.dumps(report_data, indent=2))


def generate_html_report(reports: List[EvaluationReport], output_path: str) -> None:
    """Generate HTML evaluation report with visualizations.

    Args:
        reports: List of EvaluationReport objects.
        output_path: Path to write HTML report.
    """
    output = Path(output_path)
    output.parent.mkdir(parents=True, exist_ok=True)

    # Calculate aggregate metrics
    harness = EvaluationHarness()
    aggregate = harness.calculate_aggregate_metrics(reports)

    # Generate HTML
    html = _generate_html_content(reports, aggregate)

    output.write_text(html)


def _generate_html_content(reports: List[EvaluationReport], aggregate: EvaluationMetrics) -> str:
    """Generate HTML content for the report.

    Args:
        reports: List of EvaluationReport objects.
        aggregate: Aggregate metrics.

    Returns:
        Complete HTML document as string.
    """
    # Metric cards HTML
    metric_cards = f"""
    <div class="metric-cards">
        <div class="metric-card">
            <h3>Diagnostic Precision</h3>
            <div class="metric-value">{aggregate.diagnostic_precision:.1%}</div>
        </div>
        <div class="metric-card">
            <h3>Diagnostic Recall</h3>
            <div class="metric-value">{aggregate.diagnostic_recall:.1%}</div>
        </div>
        <div class="metric-card">
            <h3>Diagnostic F1</h3>
            <div class="metric-value">{aggregate.diagnostic_f1:.1%}</div>
        </div>
        <div class="metric-card">
            <h3>Completion MRR</h3>
            <div class="metric-value">{aggregate.completion_mrr:.2f}</div>
        </div>
        <div class="metric-card">
            <h3>Hover Coverage</h3>
            <div class="metric-value">{aggregate.hover_coverage:.1%}</div>
        </div>
    </div>
    """

    # Fixture results table
    table_rows = ""
    for report in reports:
        table_rows += f"""
        <tr>
            <td>{report.fixture_name}</td>
            <td>{report.metrics.diagnostic_precision:.1%}</td>
            <td>{report.metrics.diagnostic_recall:.1%}</td>
            <td>{report.metrics.diagnostic_f1:.1%}</td>
            <td>{report.diagnostic_comparison.true_positives}</td>
            <td>{report.diagnostic_comparison.false_positives}</td>
            <td>{report.diagnostic_comparison.false_negatives}</td>
            <td>{report.metrics.completion_mrr:.2f}</td>
            <td>{report.metrics.hover_coverage:.1%}</td>
        </tr>
        """

    table = f"""
    <table class="results-table">
        <thead>
            <tr>
                <th>Fixture</th>
                <th>Precision</th>
                <th>Recall</th>
                <th>F1</th>
                <th>TP</th>
                <th>FP</th>
                <th>FN</th>
                <th>MRR</th>
                <th>Hover</th>
            </tr>
        </thead>
        <tbody>
            {table_rows}
        </tbody>
    </table>
    """

    # Full HTML document
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>CP2K LSP Evaluation Report</title>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            line-height: 1.6;
        }}
        h1 {{
            color: #333;
            border-bottom: 2px solid #007acc;
            padding-bottom: 10px;
        }}
        h2 {{
            color: #555;
            margin-top: 30px;
        }}
        .metric-cards {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin: 30px 0;
        }}
        .metric-card {{
            background: #f5f5f5;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
        }}
        .metric-card h3 {{
            margin: 0 0 10px 0;
            font-size: 14px;
            color: #666;
        }}
        .metric-value {{
            font-size: 32px;
            font-weight: bold;
            color: #007acc;
        }}
        .results-table {{
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }}
        .results-table th,
        .results-table td {{
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #ddd;
        }}
        .results-table th {{
            background: #007acc;
            color: white;
            font-weight: 600;
        }}
        .results-table tr:hover {{
            background: #f5f5f5;
        }}
        .timestamp {{
            color: #999;
            font-size: 14px;
        }}
    </style>
</head>
<body>
    <h1>CP2K LSP Evaluation Report</h1>
    <p class="timestamp">Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>

    <h2>Aggregate Metrics</h2>
    {metric_cards}

    <h2>Fixture Results</h2>
    {table}

</body>
</html>
"""

    return html
