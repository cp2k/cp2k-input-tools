"""
CP2K schema-aware static lint rules for LSP diagnostics.

Provides lint checks that go beyond syntax and semantic validation:
- Keyword misspell detection (fuzzy match against schema)
- Invalid section nesting checks
- Duplicate keyword/section detection
- Configuration smell warnings (low cutoff, few SCF iterations, etc.)
- Missing section END detection
- Unknown enum value detection
- File reference validation (basis sets, potentials)
"""

import difflib
import pathlib
import re
import xml.etree.ElementTree as ET
from collections import Counter
from dataclasses import dataclass, field
from functools import lru_cache
from typing import Dict, List, Optional, Set, Tuple

from . import DEFAULT_CP2K_INPUT_XML
from .validator import Diagnostic

# Lint rule codes
RULE_MISSPELLED_KEYWORD = "lint/misspelled-keyword"
RULE_INVALID_NESTING = "cp2k.syntax.invalid_nesting"
RULE_DUPLICATE_KEYWORD = "lint/duplicate-keyword"
RULE_DUPLICATE_SECTION = "cp2k.schema.unsafe_duplicate_section"
RULE_LOW_CUTOFF = "cp2k.dft.cutoff_low"
RULE_LOW_REL_CUTOFF = "cp2k.dft.rel_cutoff_low"
RULE_FEW_SCF = "lint/few-scf-iterations"
RULE_LOOSE_SCF_EPS = "lint/loose-scf-eps"
RULE_SHORT_TIMESTEP = "lint/short-timestep"
RULE_LONG_TIMESTEP = "lint/long-timestep"
RULE_LOW_TEMP = "lint/low-electronic-temp"
RULE_MAX_SCF_TOO_LOW = "lint/max-scf-too-low"
RULE_GEO_OPT_MAX_ITER_LOW = "lint/geo-opt-max-iter-low"
RULE_MISSING_END = "cp2k.syntax.missing_end"
RULE_UNKNOWN_ENUM = "cp2k.schema.unknown_enum"
RULE_MISSING_BASIS = "cp2k.files.missing_basis"
RULE_MISSING_POTENTIAL = "cp2k.files.missing_potential"

# Short forms accepted by existing fixtures/parser behavior but not listed as
# explicit XML aliases in the bundled schema.
ACCEPTED_KEYWORD_ALIASES = {
    "COORD_FILE",
}

# Sections whose child records are data rows rather than ordinary keywords.
DATA_RECORD_SECTIONS = {
    "COORD",
}

# Common CP2K sections that are expected to appear multiple times under one parent.
REPEATABLE_SECTIONS = {
    "FORCE_EVAL",
    "KIND",
    "PRINT",
}

# Section parent-child validity map (common sections)
VALID_SECTION_PARENTS = {
    "GLOBAL": {"/"},
    "FORCE_EVAL": {"/"},
    "MOTION": {"/"},
    "DFT": {"FORCE_EVAL"},
    "SUBSYS": {"FORCE_EVAL"},
    "QS": {"DFT"},
    "MGRID": {"DFT"},
    "SCF": {"DFT"},
    "XC": {"DFT"},
    "XC_FUNCTIONAL": {"XC"},
    "POISSON": {"DFT"},
    "KPOINTS": {"DFT"},
    "PRINT": {"DFT", "MOTION", "FORCE_EVAL", "GLOBAL", "XC"},
    "CELL": {"SUBSYS"},
    "COORD": {"SUBSYS"},
    "TOPOLOGY": {"SUBSYS"},
    "KIND": {"SUBSYS"},
    "GEO_OPT": {"MOTION"},
    "MD": {"MOTION"},
    "CELL_OPT": {"MOTION"},
    "EACH": {"PRINT"},
    "OT": {"SCF"},
    "DIAGONALIZATION": {"SCF"},
    "SMEAR": {"SCF"},
    "MIXING": {"SCF"},
    "BASIS_SET_FILE_NAME": {"FORCE_EVAL"},
    "POTENTIAL_FILE_NAME": {"FORCE_EVAL"},
}

# Common configuration smell thresholds
LOW_CUTOFF_THRESHOLD = 200  # Ry - very low, likely mistake
LOW_REL_CUTOFF_THRESHOLD = 30  # Ry - very low
FEW_SCF_THRESHOLD = 10  # iterations - very few
LOOSE_SCF_EPS = 1.0e-4  # too loose for production
SHORT_TIMESTEP_FS = 0.01  # fs - suspiciously short
LONG_TIMESTEP_FS = 5.0  # fs - suspiciously long for most MD
LOW_ELECTRONIC_TEMP_K = 10  # K - very low
GEO_OPT_MAX_ITER_LOW = 5  # very few iterations for geometry optimization


@dataclass
class SectionNode:
    """Represents a parsed section with its context."""

    name: str
    keywords: List[Tuple[str, str, int]] = field(default_factory=list)  # (name, value, line)
    subsections: List["SectionNode"] = field(default_factory=list)
    parent: Optional["SectionNode"] = None
    line: int = 0


@lru_cache(maxsize=1)
def _get_all_schema_keywords() -> Set[str]:
    """Extract all valid keyword names from the XML schema."""
    keywords = set(ACCEPTED_KEYWORD_ALIASES)
    try:
        tree = ET.parse(DEFAULT_CP2K_INPUT_XML)
        root = tree.getroot()
        for kw_node in root.iter("KEYWORD"):
            for name_node in kw_node.findall("./NAME"):
                if name_node.text:
                    keywords.add(name_node.text.upper())
    except Exception:
        pass
    return keywords


@lru_cache(maxsize=1)
def _get_all_schema_sections() -> Set[str]:
    """Extract all valid section names from the XML schema."""
    sections = set()
    try:
        tree = ET.parse(DEFAULT_CP2K_INPUT_XML)
        root = tree.getroot()
        for sec_node in root.iter("SECTION"):
            for name_node in sec_node.findall("./NAME"):
                if name_node.text:
                    sections.add(name_node.text.upper())
    except Exception:
        pass
    return sections


def _parse_sections(text: str) -> List[SectionNode]:
    """Parse CP2K input text into a section tree."""
    lines = text.split("\n")
    root = SectionNode(name="/", line=0)
    stack = [root]

    section_re = re.compile(r"^(\s*)&(\w[\w\-_]*)\s*(.*)", re.IGNORECASE)
    end_re = re.compile(r"^(\s*)&END\s+(\w[\w\-_]*)", re.IGNORECASE)
    keyword_re = re.compile(r"^(\s*)(\w[\w\-_]*)\s+(.*)")

    for i, line in enumerate(lines):
        stripped = line.strip()

        # Skip empty lines and comments
        if not stripped or stripped.startswith("!") or stripped.startswith("#"):
            continue

        # Skip preprocessor directives
        if stripped.startswith("@"):
            continue

        # Check for section end
        end_match = end_re.match(line)
        if end_match:
            end_name = end_match.group(2).upper()
            # Pop the matching section from stack
            for j in range(len(stack) - 1, 0, -1):
                if stack[j].name == end_name:
                    stack = stack[:j]
                    break
            continue

        # Check for section start
        sec_match = section_re.match(line)
        if sec_match:
            sec_name = sec_match.group(2).upper()
            if sec_name == "END":
                continue
            new_node = SectionNode(name=sec_name, line=i)
            new_node.parent = stack[-1]
            stack[-1].subsections.append(new_node)
            stack.append(new_node)
            continue

        # Check for keyword
        kw_match = keyword_re.match(line)
        if kw_match and stack:
            kw_name = kw_match.group(2).upper()
            kw_value = kw_match.group(3).strip()
            stack[-1].keywords.append((kw_name, kw_value, i))

    return root.subsections


def _fuzzy_match(keyword: str, valid_keywords: Set[str], threshold: float = 0.7) -> Optional[str]:
    """Find the closest matching valid keyword using fuzzy matching."""
    matches = difflib.get_close_matches(keyword, list(valid_keywords), n=1, cutoff=threshold)
    return matches[0] if matches else None


def lint_keywords_misspelled(text: str, valid_keywords: Set[str]) -> List[Diagnostic]:
    """Detect potentially misspelled keywords by fuzzy matching against schema."""
    diagnostics = []
    lines = text.split("\n")
    section_re = re.compile(r"^(\s*)&(\w[\w\-_]*)\s*", re.IGNORECASE)
    end_re = re.compile(r"^(\s*)&END\s+(\w[\w\-_]*)", re.IGNORECASE)
    keyword_re = re.compile(r"^(\s*)(\w[\w\-_]*)\s+(.*)")

    for i, line in enumerate(lines):
        stripped = line.strip()
        if not stripped or stripped.startswith("!") or stripped.startswith("#") or stripped.startswith("@"):
            continue

        if section_re.match(line) or end_re.match(line):
            continue

        kw_match = keyword_re.match(line)
        if kw_match:
            kw_name = kw_match.group(2).upper()
            if kw_name not in valid_keywords:
                suggestion = _fuzzy_match(kw_name, valid_keywords)
                if suggestion:
                    diagnostics.append(
                        Diagnostic(
                            severity="warning",
                            source="cp2k-lint",
                            code=RULE_MISSPELLED_KEYWORD,
                            message=f"Unknown keyword '{kw_name}'. Did you mean '{suggestion}'?",
                            line=i,
                            column=kw_match.start(2),
                        )
                    )
    return diagnostics


def lint_invalid_nesting(text: str, valid_sections: Set[str]) -> List[Diagnostic]:
    """Detect sections placed under invalid parent sections."""
    diagnostics = []
    sections = _parse_sections(text)

    def check_section(node: SectionNode, parent_name: str):
        valid_parents = VALID_SECTION_PARENTS.get(node.name)
        if valid_parents is not None and parent_name not in valid_parents:
            diagnostics.append(
                Diagnostic(
                    severity="error",
                    source="cp2k-lint",
                    code=RULE_INVALID_NESTING,
                    message=f"Section '&{node.name}' is not valid under '&{parent_name}'. "
                    f"Expected one of: {', '.join(sorted(valid_parents))}",
                    line=node.line,
                )
            )
        for sub in node.subsections:
            check_section(sub, node.name)

    for section in sections:
        check_section(section, "/")

    return diagnostics


def lint_duplicates(text: str) -> List[Diagnostic]:
    """Detect duplicate keywords or non-repeating sections."""
    diagnostics = []
    sections = _parse_sections(text)

    def check_duplicates(node: SectionNode):
        # Check for duplicate keywords
        if node.name not in DATA_RECORD_SECTIONS:
            kw_names = [kw[0] for kw in node.keywords]
            kw_counts = Counter(kw_names)
            for kw_name, count in kw_counts.items():
                if count > 1:
                    # Find the line of the second occurrence
                    lines = [kw[2] for kw in node.keywords if kw[0] == kw_name]
                    diagnostics.append(
                        Diagnostic(
                            severity="warning",
                            source="cp2k-lint",
                            code=RULE_DUPLICATE_KEYWORD,
                            message=f"Keyword '{kw_name}' appears {count} times in section '&{node.name}'. "
                            f"This may be unintended.",
                            line=lines[1] if len(lines) > 1 else lines[0],
                        )
                    )

        # Check for duplicate non-repeating sections
        sec_names = [sub.name for sub in node.subsections]
        sec_counts = Counter(sec_names)
        for sec_name, count in sec_counts.items():
            if count > 1 and sec_name not in REPEATABLE_SECTIONS:
                # Find the line of the second occurrence
                lines = [sub.line for sub in node.subsections if sub.name == sec_name]
                diagnostics.append(
                    Diagnostic(
                        severity="warning",
                        source="cp2k-lint",
                        code=RULE_DUPLICATE_SECTION,
                        message=f"Section '&{sec_name}' appears {count} times under '&{node.name}'. " f"This may be unintended.",
                        line=lines[1] if len(lines) > 1 else lines[0],
                    )
                )

        for sub in node.subsections:
            check_duplicates(sub)

    for section in sections:
        check_duplicates(section)

    return diagnostics


def lint_config_smells(text: str) -> List[Diagnostic]:
    """Detect configuration smells like very low cutoff, few SCF iterations, etc."""
    diagnostics = []
    lines = text.split("\n")
    keyword_re = re.compile(r"^(\s*)(\w[\w\-_]*)\s+(.*)", re.IGNORECASE)

    current_section_path = []  # Track current section context
    section_re = re.compile(r"^(\s*)&(\w[\w\-_]*)\s*", re.IGNORECASE)
    end_re = re.compile(r"^(\s*)&END\s+(\w[\w\-_]*)", re.IGNORECASE)

    for i, line in enumerate(lines):
        stripped = line.strip()
        if not stripped or stripped.startswith("!") or stripped.startswith("#") or stripped.startswith("@"):
            continue

        sec_match = section_re.match(line)
        if sec_match:
            sec_name = sec_match.group(2).upper()
            if sec_name != "END":
                current_section_path.append(sec_name)
            continue

        end_match = end_re.match(line)
        if end_match:
            end_name = end_match.group(2).upper()
            while current_section_path and current_section_path[-1] != end_name:
                current_section_path.pop()
            if current_section_path and current_section_path[-1] == end_name:
                current_section_path.pop()
            continue

        kw_match = keyword_re.match(line)
        if kw_match:
            kw_name = kw_match.group(2).upper()
            kw_value = kw_match.group(3).strip()

            # Check CUTOFF
            if kw_name == "CUTOFF":
                try:
                    # Extract numeric value, ignoring unit
                    val_str = re.sub(r"[^\d.eE+-]", "", kw_value.split()[0]) if kw_value.split() else kw_value
                    cutoff_val = float(val_str)
                    if cutoff_val < LOW_CUTOFF_THRESHOLD:
                        diagnostics.append(
                            Diagnostic(
                                severity="warning",
                                source="cp2k-lint",
                                code=RULE_LOW_CUTOFF,
                                message=f"CUTOFF {cutoff_val} Ry is very low. Consider ≥ {LOW_CUTOFF_THRESHOLD} Ry for reasonable accuracy.",
                                line=i,
                            )
                        )
                except (ValueError, IndexError):
                    pass

            # Check REL_CUTOFF
            if kw_name == "REL_CUTOFF":
                try:
                    val_str = re.sub(r"[^\d.eE+-]", "", kw_value.split()[0]) if kw_value.split() else kw_value
                    rel_cutoff_val = float(val_str)
                    if rel_cutoff_val < LOW_REL_CUTOFF_THRESHOLD:
                        diagnostics.append(
                            Diagnostic(
                                severity="warning",
                                source="cp2k-lint",
                                code=RULE_LOW_REL_CUTOFF,
                                message=f"REL_CUTOFF {rel_cutoff_val} Ry is very low. Consider ≥ {LOW_REL_CUTOFF_THRESHOLD} Ry.",
                                line=i,
                            )
                        )
                except (ValueError, IndexError):
                    pass

            # Check MAX_SCF
            if kw_name == "MAX_SCF":
                try:
                    max_scf = int(kw_value.split()[0]) if kw_value.split() else int(kw_value)
                    if max_scf < FEW_SCF_THRESHOLD:
                        diagnostics.append(
                            Diagnostic(
                                severity="warning",
                                source="cp2k-lint",
                                code=RULE_MAX_SCF_TOO_LOW,
                                message=f"MAX_SCF {max_scf} is very low. SCF may not converge. Consider ≥ {FEW_SCF_THRESHOLD}.",
                                line=i,
                            )
                        )
                except (ValueError, IndexError):
                    pass

            # Check EPS_SCF
            if kw_name == "EPS_SCF":
                try:
                    eps_str = kw_value.split()[0] if kw_value.split() else kw_value
                    eps_val = float(eps_str)
                    if eps_val > LOOSE_SCF_EPS:
                        diagnostics.append(
                            Diagnostic(
                                severity="warning",
                                source="cp2k-lint",
                                code=RULE_LOOSE_SCF_EPS,
                                message=f"EPS_SCF {eps_val} is very loose. Consider ≤ 1.0e-6 for production runs.",
                                line=i,
                            )
                        )
                except (ValueError, IndexError):
                    pass

            # Check TIMESTEP
            if kw_name == "TIMESTEP":
                try:
                    # TIMESTEP can have units like [fs]
                    parts = kw_value.split()
                    if parts:
                        val_str = re.sub(r"[^\d.eE+-]", "", parts[0])
                        timestep_val = float(val_str)
                        if 0 < timestep_val < SHORT_TIMESTEP_FS:
                            diagnostics.append(
                                Diagnostic(
                                    severity="warning",
                                    source="cp2k-lint",
                                    code=RULE_SHORT_TIMESTEP,
                                    message=f"TIMESTEP {timestep_val} fs is very short. Is this intentional?",
                                    line=i,
                                )
                            )
                        elif timestep_val > LONG_TIMESTEP_FS:
                            diagnostics.append(
                                Diagnostic(
                                    severity="warning",
                                    source="cp2k-lint",
                                    code=RULE_LONG_TIMESTEP,
                                    message=f"TIMESTEP {timestep_val} fs is very long. Most MD simulations use 0.5-2.0 fs.",
                                    line=i,
                                )
                            )
                except (ValueError, IndexError):
                    pass

            # Check MAX_ITER in GEO_OPT
            if kw_name == "MAX_ITER" and "GEO_OPT" in current_section_path:
                try:
                    max_iter = int(kw_value.split()[0]) if kw_value.split() else int(kw_value)
                    if max_iter < GEO_OPT_MAX_ITER_LOW:
                        diagnostics.append(
                            Diagnostic(
                                severity="warning",
                                source="cp2k-lint",
                                code=RULE_GEO_OPT_MAX_ITER_LOW,
                                message=f"GEO_OPT MAX_ITER {max_iter} is very low. Geometry optimization may not converge.",
                                line=i,
                            )
                        )
                except (ValueError, IndexError):
                    pass

    return diagnostics


def lint_missing_end(text: str) -> List[Diagnostic]:
    """Detect sections with missing &END."""
    diagnostics = []
    lines = text.split("\n")
    section_re = re.compile(r"^(\s*)&(\w[\w\-_]*)\s*(.*)", re.IGNORECASE)
    end_re = re.compile(r"^(\s*)&END\s*(.*)", re.IGNORECASE)

    section_stack = []  # Stack of (section_name, line_number)

    for i, line in enumerate(lines):
        stripped = line.strip()

        # Skip empty lines and comments
        if not stripped or stripped.startswith("!") or stripped.startswith("#") or stripped.startswith("@"):
            continue

        # Check for section end
        end_match = end_re.match(line)
        if end_match:
            end_name = end_match.group(2).strip().upper()
            # Pop matching section from stack
            if section_stack:
                if end_name and end_name != section_stack[-1][0]:
                    # Mismatched END - parser will catch this, but we can note it
                    pass
                section_stack.pop()
            continue

        # Check for section start
        sec_match = section_re.match(line)
        if sec_match:
            sec_name = sec_match.group(2).upper()
            if sec_name == "END":
                continue
            section_stack.append((sec_name, i))
            continue

    # Report any sections left on the stack (missing END)
    for sec_name, line_num in section_stack:
        diagnostics.append(
            Diagnostic(
                severity="error",
                source="cp2k-syntax",
                code=RULE_MISSING_END,
                message=f"Section '&{sec_name}' is missing &END{sec_name}.",
                line=line_num,
            )
        )

    return diagnostics


def lint_unknown_enum(text: str) -> List[Diagnostic]:
    """Detect keyword values that don't match schema-defined enum values."""
    diagnostics = []
    lines = text.split("\n")
    keyword_re = re.compile(r"^(\s*)(\w[\w\-_]*)\s+(.*)")

    # Get enum values from schema
    enum_values_by_keyword = _get_enum_values_from_schema()

    current_section_path = []
    section_re = re.compile(r"^(\s*)&(\w[\w\-_]*)\s*", re.IGNORECASE)
    end_re = re.compile(r"^(\s*)&END\s+(\w[\w\-_]*)", re.IGNORECASE)

    for i, line in enumerate(lines):
        stripped = line.strip()

        if not stripped or stripped.startswith("!") or stripped.startswith("#") or stripped.startswith("@"):
            continue

        # Track section nesting
        sec_match = section_re.match(line)
        if sec_match:
            sec_name = sec_match.group(2).upper()
            if sec_name != "END":
                current_section_path.append(sec_name)
            continue

        end_match = end_re.match(line)
        if end_match:
            end_name = end_match.group(2).upper()
            while current_section_path and current_section_path[-1] != end_name:
                current_section_path.pop()
            if current_section_path and current_section_path[-1] == end_name:
                current_section_path.pop()
            continue

        # Check keyword enum values
        kw_match = keyword_re.match(line)
        if kw_match:
            kw_name = kw_match.group(2).upper()
            kw_value = kw_match.group(3).strip()

            # Build the full section path for lookup
            section_path = "/".join(current_section_path)
            lookup_key = f"{section_path}/{kw_name}".upper()

            if lookup_key in enum_values_by_keyword:
                allowed_values = list(enum_values_by_keyword[lookup_key])
                # Split value to handle lists and units
                value_parts = kw_value.split()[0] if kw_value.split() else kw_value
                value_upper = value_parts.upper()

                if value_upper not in allowed_values:
                    # Get a few suggestions
                    suggestions = difflib.get_close_matches(value_upper, allowed_values, n=2, cutoff=0.6)
                    suggest_text = f" Did you mean: {', '.join(suggestions)}?" if suggestions else ""
                    diagnostics.append(
                        Diagnostic(
                            severity="error",
                            source="cp2k-schema",
                            code=RULE_UNKNOWN_ENUM,
                            message=f"Unknown enum value '{value_parts}' for keyword '{kw_name}'. "
                            f"Allowed: {', '.join(allowed_values[:5])}{'...' if len(allowed_values) > 5 else ''}.{suggest_text}",
                            line=i,
                            column=kw_match.start(2),
                        )
                    )

    return diagnostics


def lint_missing_files(text: str, base_dir: str = ".") -> List[Diagnostic]:
    """Detect references to basis set and potential files that don't exist."""
    diagnostics = []
    lines = text.split("\n")
    keyword_re = re.compile(r"^(\s*)(\w[\w\-_]*)\s+(.*)")

    current_section_path = []
    section_re = re.compile(r"^(\s*)&(\w[\w\-_]*)\s*", re.IGNORECASE)
    end_re = re.compile(r"^(\s*)&END\s+(\w[\w\-_]*)", re.IGNORECASE)

    for i, line in enumerate(lines):
        stripped = line.strip()

        if not stripped or stripped.startswith("!") or stripped.startswith("#") or stripped.startswith("@"):
            continue

        # Track section nesting
        sec_match = section_re.match(line)
        if sec_match:
            sec_name = sec_match.group(2).upper()
            if sec_name != "END":
                current_section_path.append(sec_name)
            continue

        end_match = end_re.match(line)
        if end_match:
            end_name = end_match.group(2).upper()
            while current_section_path and current_section_path[-1] != end_name:
                current_section_path.pop()
            if current_section_path and current_section_path[-1] == end_name:
                current_section_path.pop()
            continue

        # Check file references
        kw_match = keyword_re.match(line)
        if kw_match:
            kw_name = kw_match.group(2).upper()
            kw_value = kw_match.group(3).strip().strip("'\"")

            # Check BASIS_SET_FILE_NAME
            if kw_name == "BASIS_SET_FILE_NAME" and "SUBSYS" in current_section_path:
                file_path = pathlib.Path(base_dir) / kw_value
                if not file_path.exists() and not _is_builtin_basis(kw_value):
                    diagnostics.append(
                        Diagnostic(
                            severity="error",
                            source="cp2k-files",
                            code=RULE_MISSING_BASIS,
                            message=f"Basis set file not found: '{kw_value}'",
                            line=i,
                        )
                    )

            # Check POTENTIAL_FILE_NAME
            if kw_name == "POTENTIAL_FILE_NAME" and "SUBSYS" in current_section_path:
                file_path = pathlib.Path(base_dir) / kw_value
                if not file_path.exists() and not _is_builtin_potential(kw_value):
                    diagnostics.append(
                        Diagnostic(
                            severity="error",
                            source="cp2k-files",
                            code=RULE_MISSING_POTENTIAL,
                            message=f"Potential file not found: '{kw_value}'",
                            line=i,
                        )
                    )

    return diagnostics


def _is_builtin_basis(filename: str) -> bool:
    """Check if this is a known built-in basis set name."""
    # Common built-in basis set names
    builtin_prefixes = {
        "BASIS_MOLOPT",
        "BASIS_ADMM",
        "BASIS_SET",
        "GTH",
        "DZVP",
        "TZVP",
        "QZVP",
        "SZV",
        "DOUBLE_ZETA",
        "TRIPLE_ZETA",
    }
    fname_upper = filename.upper()
    return any(fname_upper.startswith(p) for p in builtin_prefixes)


def _is_builtin_potential(filename: str) -> bool:
    """Check if this is a known built-in potential name."""
    # Common built-in potential names
    builtin_prefixes = {
        "GTH_POTENTIALS",
        "POTENTIAL",
        "ALL_POTENTIALS",
        "PADE_POTENTIALS",
        "NLCC_POTENTIALS",
    }
    fname_upper = filename.upper()
    return any(fname_upper.startswith(p) for p in builtin_prefixes)


@lru_cache(maxsize=1)
def _get_enum_values_from_schema() -> Dict[str, Set[str]]:
    """Extract enum values from the XML schema for validation."""
    enum_values = {}
    try:
        tree = ET.parse(DEFAULT_CP2K_INPUT_XML)
        root = tree.getroot()

        # Walk through all sections and keywords
        for section in root.iter("SECTION"):
            section_name_el = section.find("./NAME")
            if section_name_el is None or not section_name_el.text:
                continue
            section_name = section_name_el.text.upper()

            # Process keywords in this section
            for kw in section.iterfind(".//KEYWORD"):
                kw_name_el = kw.find("./NAME")
                if kw_name_el is None or not kw_name_el.text:
                    continue
                kw_name = kw_name_el.text.upper()

                # Check for ENUMERATION data type
                dt = kw.find("./DATA_TYPE")
                if dt is not None and dt.get("kind") == "keyword":
                    enum_el = dt.find("./ENUMERATION")
                    if enum_el is not None:
                        values = set()
                        # CP2K schema uses ITEM for enum values
                        for item_el in enum_el.findall("./ITEM"):
                            name_el = item_el.find("./NAME")
                            if name_el is not None and name_el.text:
                                values.add(name_el.text.upper())

                        if values:
                            # Store with full path for lookup
                            lookup_key = f"{section_name}/{kw_name}"
                            enum_values[lookup_key] = values
    except Exception:
        pass
    return enum_values


def lint(text: str) -> List[Diagnostic]:
    """Run all lint checks on a CP2K input text and return diagnostics."""
    all_diagnostics = []

    # Get schema data
    valid_keywords = _get_all_schema_keywords()

    # Run lint rules
    all_diagnostics.extend(lint_keywords_misspelled(text, valid_keywords))
    all_diagnostics.extend(lint_invalid_nesting(text, set()))
    all_diagnostics.extend(lint_duplicates(text))
    all_diagnostics.extend(lint_config_smells(text))
    all_diagnostics.extend(lint_missing_end(text))
    all_diagnostics.extend(lint_unknown_enum(text))
    all_diagnostics.extend(lint_missing_files(text))

    return all_diagnostics
