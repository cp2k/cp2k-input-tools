"""CP2K Section definitions."""

from dataclasses import dataclass, field
from typing import Dict, List, Optional


@dataclass
class SectionInfo:
    """Information about a CP2K section."""

    name: str
    description: str = ""
    keywords: List[str] = field(default_factory=list)
    subsections: List[str] = field(default_factory=list)
    required: bool = False
    repeats: bool = False


# CP2K Section definitions
CP2K_SECTIONS: Dict[str, SectionInfo] = {
    # Root sections
    "GLOBAL": SectionInfo(
        name="GLOBAL",
        description="Global settings for the calculation",
        keywords=["PROJECT_NAME", "RUN_TYPE", "PRINT_LEVEL", "WALLTIME"],
        subsections=["DBCSR", "FM", "FM_DIAG_SETTINGS", "PREFERRED_FFT_LIBRARY", "PRINT"],
        required=True,
    ),
    "FORCE_EVAL": SectionInfo(
        name="FORCE_EVAL",
        description="Force evaluation settings",
        keywords=["METHOD"],
        subsections=["DFT", "SUBSYS", "PRINT", "PROPERTIES"],
        repeats=True,
    ),
    "MOTION": SectionInfo(
        name="MOTION",
        description="Molecular dynamics and geometry optimization",
        keywords=["MD_ENSEMBLE"],
        subsections=["MD", "GEO_OPT", "CELL_OPT", "BAND", "MC", "DRIVER", "PRINT"],
    ),
    # DFT sections
    "DFT": SectionInfo(
        name="DFT",
        description="Density Functional Theory settings",
        keywords=["BASIS_SET_FILE_NAME", "POTENTIAL_FILE_NAME", "CHARGE", "MULTIPLICITY"],
        subsections=["QS", "SCF", "XC", "POISSON", "PRINT", "OPTIMIZE_BASIS", "OPTIMIZE_INPUT"],
    ),
    "QS": SectionInfo(
        name="QS",
        description="Quickstep settings",
        keywords=["METHOD", "EPS_DEFAULT", "EXTRAPOLATION", "EXTRAPOLATION_ORDER"],
        subsections=["OPTIMIZE_BASIS", "DENSITY_FITTING", "DISTRIBUTION"],
    ),
    "SCF": SectionInfo(
        name="SCF",
        description="Self-Consistent Field settings",
        keywords=["SCF_GUESS", "EPS_SCF", "MAX_SCF", "ADDED_MOS"],
        subsections=["DIAGONALIZATION", "MIXING", "SMEAR", "PRINT", "OT"],
    ),
    "XC": SectionInfo(
        name="XC",
        description="Exchange-Correlation functional",
        keywords=["XC_FUNCTIONAL"],
        subsections=["XC_FUNCTIONAL", "XC_GRID", "VDW_POTENTIAL"],
    ),
    "SUBSYS": SectionInfo(
        name="SUBSYS", description="System definition", keywords=[], subsections=["CELL", "COORD", "KIND", "TOPOLOGY"]
    ),
    "KIND": SectionInfo(
        name="KIND",
        description="Atomic kind definition",
        keywords=["ELEMENT", "BASIS_SET", "POTENTIAL", "MAGNETIZATION"],
        repeats=True,
    ),
    "CELL": SectionInfo(
        name="CELL", description="Unit cell definition", keywords=["A", "B", "C", "ALPHA", "BETA", "GAMMA", "PERIODIC", "SYMMETRY"]
    ),
    "COORD": SectionInfo(name="COORD", description="Atomic coordinates", keywords=["UNIT", "SCALED"]),
    # MD sections
    "MD": SectionInfo(
        name="MD",
        description="Molecular dynamics settings",
        keywords=["ENSEMBLE", "STEPS", "TIMESTEP", "TEMPERATURE"],
        subsections=["THERMOSTAT", "BAROSTAT", "PRINT", "AVERAGES", "SHELL"],
    ),
    "GEO_OPT": SectionInfo(
        name="GEO_OPT",
        description="Geometry optimization settings",
        keywords=["TYPE", "MAX_ITER", "MAX_DR", "RMS_DR", "MAX_FORCE", "RMS_FORCE"],
        subsections=["CG", "BFGS", "LBFGS", "PRINT"],
    ),
    "PRINT": SectionInfo(
        name="PRINT",
        description="Print control section",
        keywords=[],
        subsections=["PROGRAM_RUN_INFO", "FORCES_ON", "STRESS_ON", "TRAJECTORY"],
        repeats=True,
    ),
}


def get_section_info(name: str) -> Optional[SectionInfo]:
    """Get information about a section."""
    return CP2K_SECTIONS.get(name.upper())


def get_valid_subsections(section_name: str) -> List[str]:
    """Get valid subsections for a section."""
    info = get_section_info(section_name)
    return info.subsections if info else []


def get_valid_keywords(section_name: str) -> List[str]:
    """Get valid keywords for a section."""
    info = get_section_info(section_name)
    return info.keywords if info else []
