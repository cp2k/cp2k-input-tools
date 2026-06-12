"""CP2K Keyword definitions."""

from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional


class KeywordType(Enum):
    """Keyword value types."""

    STRING = "string"
    INTEGER = "integer"
    REAL = "real"
    BOOLEAN = "boolean"
    ENUM = "enum"
    ARRAY = "array"
    FILE = "file"


@dataclass
class KeywordInfo:
    """Information about a CP2K keyword."""

    name: str
    description: str = ""
    keyword_type: KeywordType = KeywordType.STRING
    default: Any = None
    required: bool = False
    enum_values: Optional[List[str]] = None
    units: Optional[List[str]] = None


# CP2K Keyword definitions
CP2K_KEYWORDS: Dict[str, KeywordInfo] = {
    # Global keywords
    "PROJECT_NAME": KeywordInfo(
        name="PROJECT_NAME",
        description="Name of the project (basename for output files)",
        keyword_type=KeywordType.STRING,
        default="PROJECT",
    ),
    "RUN_TYPE": KeywordInfo(
        name="RUN_TYPE",
        description="Type of calculation to perform",
        keyword_type=KeywordType.ENUM,
        default="ENERGY",
        enum_values=["ENERGY", "ENERGY_FORCE", "GEO_OPT", "MD", "MC", "BSSE", "DEBUG", "NONE"],
    ),
    "PRINT_LEVEL": KeywordInfo(
        name="PRINT_LEVEL",
        description="Verbosity level of output",
        keyword_type=KeywordType.ENUM,
        default="MEDIUM",
        enum_values=["SILENT", "LOW", "MEDIUM", "HIGH", "DEBUG"],
    ),
    "WALLTIME": KeywordInfo(
        name="WALLTIME",
        description="Maximum walltime for the calculation",
        keyword_type=KeywordType.REAL,
        default=None,
        units=["s", "m", "h"],
    ),
    # DFT keywords
    "CHARGE": KeywordInfo(name="CHARGE", description="Total charge of the system", keyword_type=KeywordType.INTEGER, default=0),
    "MULTIPLICITY": KeywordInfo(
        name="MULTIPLICITY", description="Spin multiplicity (2S+1)", keyword_type=KeywordType.INTEGER, default=1
    ),
    "BASIS_SET_FILE_NAME": KeywordInfo(
        name="BASIS_SET_FILE_NAME",
        description="File containing basis set definitions",
        keyword_type=KeywordType.FILE,
        default="BASIS_SET",
    ),
    "POTENTIAL_FILE_NAME": KeywordInfo(
        name="POTENTIAL_FILE_NAME",
        description="File containing pseudopotential definitions",
        keyword_type=KeywordType.FILE,
        default="POTENTIAL",
    ),
    # QS keywords
    "METHOD": KeywordInfo(
        name="METHOD",
        description="Electronic structure method",
        keyword_type=KeywordType.ENUM,
        default="GPW",
        enum_values=["GPW", "GAPW", "GTO", "STO", "OCE"],
    ),
    "EPS_DEFAULT": KeywordInfo(
        name="EPS_DEFAULT",
        description="Default threshold for various screening operations",
        keyword_type=KeywordType.REAL,
        default=1.0e-10,
    ),
    "EXTRAPOLATION": KeywordInfo(
        name="EXTRAPOLATION",
        description="Wavefunction extrapolation method for MD/optimization",
        keyword_type=KeywordType.ENUM,
        default="ASPC",
        enum_values=["USE_GUESS", "USE_PREV_P", "USE_PREV_RHO", "ASPC"],
    ),
    # SCF keywords
    "SCF_GUESS": KeywordInfo(
        name="SCF_GUESS",
        description="Initial guess for wavefunction",
        keyword_type=KeywordType.ENUM,
        default="ATOMIC",
        enum_values=["ATOMIC", "CORE", "RANDOM", "RESTART", "SPARSE"],
    ),
    "EPS_SCF": KeywordInfo(name="EPS_SCF", description="SCF convergence threshold", keyword_type=KeywordType.REAL, default=1.0e-7),
    "MAX_SCF": KeywordInfo(
        name="MAX_SCF", description="Maximum number of SCF iterations", keyword_type=KeywordType.INTEGER, default=50
    ),
    "ADDED_MOS": KeywordInfo(
        name="ADDED_MOS", description="Number of additional unoccupied MOs", keyword_type=KeywordType.INTEGER, default=0
    ),
    # MD keywords
    "ENSEMBLE": KeywordInfo(
        name="ENSEMBLE",
        description="Statistical ensemble for MD",
        keyword_type=KeywordType.ENUM,
        default="NVE",
        enum_values=["NVE", "NVT", "NPT_I", "NPT_F", "LANGEVIN"],
    ),
    "STEPS": KeywordInfo(name="STEPS", description="Number of MD steps", keyword_type=KeywordType.INTEGER, default=1000),
    "TIMESTEP": KeywordInfo(name="TIMESTEP", description="MD timestep", keyword_type=KeywordType.REAL, default=1.0, units=["fs"]),
    "TEMPERATURE": KeywordInfo(
        name="TEMPERATURE",
        description="Target temperature for thermostat",
        keyword_type=KeywordType.REAL,
        default=300.0,
        units=["K"],
    ),
    # KIND keywords
    "ELEMENT": KeywordInfo(name="ELEMENT", description="Chemical element symbol", keyword_type=KeywordType.STRING, required=True),
    "BASIS_SET": KeywordInfo(
        name="BASIS_SET", description="Basis set for this kind", keyword_type=KeywordType.STRING, required=True
    ),
    "POTENTIAL": KeywordInfo(name="POTENTIAL", description="Pseudopotential for this kind", keyword_type=KeywordType.STRING),
    "MAGNETIZATION": KeywordInfo(
        name="MAGNETIZATION", description="Initial magnetic moment", keyword_type=KeywordType.REAL, default=0.0
    ),
}


def get_keyword_info(name: str) -> Optional[KeywordInfo]:
    """Get information about a keyword."""
    return CP2K_KEYWORDS.get(name.upper())


def get_enum_values(name: str) -> List[str]:
    """Get enum values for a keyword."""
    info = get_keyword_info(name)
    return info.enum_values if info and info.enum_values else []
