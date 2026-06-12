"""
CP2K log file parser for runtime diagnostics.

Parses CP2K output files to extract runtime information and warnings.
"""

import re
from dataclasses import dataclass
from typing import List, Optional
from pathlib import Path


@dataclass
class LogDiagnostic:
    """A diagnostic from CP2K log output."""
    rule_id: str
    message: str
    line_number: int
    severity: str = "error"
    hint: Optional[str] = None


class SCFConvergenceParser:
    """Parser for SCF convergence information in CP2K logs."""

    # Patterns for SCF convergence
    SCF_CONVERGED = re.compile(
        r"SCF run converged\s+in\s+\d+\s+iterations",
        re.IGNORECASE
    )
    SCF_NOT_CONVERGED = re.compile(
        r"SCF run NOT converged",
        re.IGNORECASE
    )
    SCF_ITERATIONS = re.compile(
        r"SCF run converged\s+in\s+(\d+)\s+iterations",
        re.IGNORECASE
    )
    MAX_SCF_EXCEEDED = re.compile(
        r"WARNING: SCF run not converged after maximum number of iterations",
        re.IGNORECASE
    )

    def __init__(self):
        self.diagnostics: List[LogDiagnostic] = []
        self.max_scf_iterations: Optional[int] = None
        self.converged = False

    def parse(self, content: str) -> List[LogDiagnostic]:
        """Parse log content and return diagnostics."""
        self.diagnostics = []
        lines = content.split('\n')

        for line_num, line in enumerate(lines, start=1):
            self._check_scf_convergence(line, line_num)

        return self.diagnostics

    def _check_scf_convergence(self, line: str, line_num: int):
        """Check line for SCF convergence issues."""
        # Check for maximum exceeded first (more specific pattern)
        if self.MAX_SCF_EXCEEDED.search(line):
            self.diagnostics.append(LogDiagnostic(
                rule_id="cp2k.log.scf_not_converged",
                message="SCF reached maximum iterations without convergence.",
                line_number=line_num,
                severity="error",
                hint="Increase MAX_SCF or tighten convergence criteria (EPS_SCF)."
            ))
        elif self.SCF_NOT_CONVERGED.search(line):
            self.diagnostics.append(LogDiagnostic(
                rule_id="cp2k.log.scf_not_converged",
                message="SCF calculation did not converge. Review convergence criteria, initial guess, or system setup.",
                line_number=line_num,
                severity="error",
                hint="Consider tightening ADAPT or MAX_SCF, improving initial guess (RESTART), or checking system geometry."
            ))


def parse_log_file(log_path: str) -> List[LogDiagnostic]:
    """
    Parse a CP2K log file and extract diagnostics.

    Args:
        log_path: Path to the CP2K output log file

    Returns:
        List of LogDiagnostic objects
    """
    log_file = Path(log_path)
    if not log_file.exists():
        return []

    try:
        content = log_file.read_text()
    except Exception:
        return []

    parser = SCFConvergenceParser()
    return parser.parse(content)


def parse_log_content(content: str) -> List[LogDiagnostic]:
    """
    Parse CP2K log content string and extract diagnostics.

    Args:
        content: CP2K log file content as string

    Returns:
        List of LogDiagnostic objects
    """
    parser = SCFConvergenceParser()
    return parser.parse(content)
