"""CP2K validate CLI command with optional dry-run support.

Parses a CP2K input file, runs semantic validations, and optionally invokes
a CP2K binary for dry-run validation. Maps all output to LSP-style diagnostics.
"""

import json
import subprocess
import sys
from dataclasses import asdict, dataclass, field
from typing import List

import click

from cp2k_input_tools.parser import CP2KInputParser
from cp2k_input_tools.parser_errors import ParserError
from cp2k_input_tools.tokenizer import TokenizerError


@dataclass
class DiagRange:
    start_line: int
    start_col: int
    end_line: int
    end_col: int


@dataclass
class Diagnostic:
    severity: str  # "error" | "warning" | "info"
    source: str  # "cp2k-parser" | "cp2k-schema" | "cp2k-lint" | "cp2k-dryrun"
    code: str
    message: str
    range: DiagRange


@dataclass
class ValidationResult:
    file: str
    diagnostics: List[Diagnostic] = field(default_factory=list)
    parser_valid: bool = False
    cp2k_dryrun_valid: bool = False

    @property
    def error_count(self) -> int:
        return sum(1 for d in self.diagnostics if d.severity == "error")

    @property
    def warning_count(self) -> int:
        return sum(1 for d in self.diagnostics if d.severity == "warning")


def validate_file(file_path: str, base_dir: str = ".") -> ValidationResult:
    """Run semantic validation on a CP2K input file."""
    result = ValidationResult(file=file_path)
    parser = CP2KInputParser(base_dir=base_dir)

    try:
        with open(file_path, "r") as fhandle:
            parser.parse(fhandle)
        result.parser_valid = True
    except (TokenizerError, ParserError) as exc:
        ctx = exc.args[1]
        colnr = ctx.colnr or 0
        result.diagnostics.append(
            Diagnostic(
                severity="error",
                source="cp2k-parser",
                code=f"E{type(exc).__name__}",
                message=f"Parse error: {exc.args[0]}",
                range=DiagRange(
                    start_line=ctx.linenr - 1,
                    start_col=colnr,
                    end_line=ctx.linenr - 1,
                    end_col=colnr + 1,
                ),
            )
        )

    return result


def run_cp2k_dryrun(file_path: str, cp2k_exe: str = "cp2k.psmp", max_procs: int = 1) -> List[Diagnostic]:
    """Run a CP2K dry-run and map output to diagnostics."""
    diagnostics = []
    cmd = [cp2k_exe, "--dry-run", "--input", file_path, "--max-procs", str(max_procs)]

    try:
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=300,
        )
    except FileNotFoundError:
        diagnostics.append(
            Diagnostic(
                severity="warning",
                source="cp2k-dryrun",
                code="W_CP2K_NOT_FOUND",
                message=f"CP2K executable '{cp2k_exe}' not found. Skipping dry-run validation.",
                range=DiagRange(0, 0, 0, 0),
            )
        )
        return diagnostics
    except subprocess.TimeoutExpired:
        diagnostics.append(
            Diagnostic(
                severity="error",
                source="cp2k-dryrun",
                code="E_CP2K_TIMEOUT",
                message="CP2K dry-run timed out after 300 seconds.",
                range=DiagRange(0, 0, 0, 0),
            )
        )
        return diagnostics

    output = proc.stdout + proc.stderr

    if proc.returncode != 0:
        # Parse CP2K error output for line numbers
        import re

        for line in output.split("\n"):
            # Look for line references in CP2K output
            line_match = re.search(r"line\s+(\d+)", line, re.IGNORECASE)
            linenr = int(line_match.group(1)) - 1 if line_match else 0

            severity = "error" if "ERROR" in line.upper() else "warning"
            diagnostics.append(
                Diagnostic(
                    severity=severity,
                    source="cp2k-dryrun",
                    code=f"CP2K_{severity.upper()}",
                    message=line.strip(),
                    range=DiagRange(linenr, 0, linenr, 80),
                )
            )

    return diagnostics


def result_to_json(result: ValidationResult) -> dict:
    """Convert validation result to JSON-serializable dict."""
    return {
        "file": result.file,
        "parser_valid": result.parser_valid,
        "cp2k_dryrun_valid": result.cp2k_dryrun_valid,
        "diagnostics": [asdict(d) for d in result.diagnostics],
        "error_count": result.error_count,
        "warning_count": result.warning_count,
    }


def human_output(result: ValidationResult) -> str:
    """Convert validation result to human-readable output."""
    lines = []
    lines.append(f"File: {result.file}")
    lines.append(f"Parser valid: {'Yes' if result.parser_valid else 'No'}")
    lines.append(f"CP2K dry-run valid: {'Yes' if result.cp2k_dryrun_valid else 'No'}")
    lines.append(f"Errors: {result.error_count}, Warnings: {result.warning_count}")
    lines.append("")

    if result.diagnostics:
        lines.append("Diagnostics:")
        for d in result.diagnostics:
            sev = d.severity.upper()
            lines.append(f"  [{sev}] {d.source}:{d.code} line {d.range.start_line + 1}")
            lines.append(f"    {d.message}")
    else:
        lines.append("All checks passed!")

    return "\n".join(lines)


@click.command()
@click.argument("file_path", type=click.Path(exists=True), metavar="[<file>]")
@click.option("--base-dir", default=".", help="Base directory for @INCLUDE resolution")
@click.option("--cp2k-exe", default="cp2k.psmp", help="CP2K executable for dry-run")
@click.option("--dry-run", "do_dryrun", is_flag=True, help="Run CP2K dry-run validation")
@click.option("--max-procs", default=1, help="Max processes for CP2K dry-run")
@click.option("--json", "as_json", is_flag=True, help="Output as JSON")
@click.option("--fail-on-error", is_flag=True, help="Exit non-zero if errors found")
def cp2k_validate(file_path, base_dir, cp2k_exe, do_dryrun, max_procs, as_json, fail_on_error):
    """Validate a CP2K input file with optional dry-run."""
    result = validate_file(file_path, base_dir)

    if do_dryrun:
        dryrun_diags = run_cp2k_dryrun(file_path, cp2k_exe, max_procs)
        result.diagnostics.extend(dryrun_diags)
        result.cp2k_dryrun_valid = len([d for d in dryrun_diags if d.severity == "error"]) == 0

    if as_json:
        click.echo(json.dumps(result_to_json(result), indent=2))
    else:
        click.echo(human_output(result))

    if fail_on_error and result.error_count > 0:
        sys.exit(1)


if __name__ == "__main__":
    cp2k_validate()
