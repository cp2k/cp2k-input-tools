"""Semantic validation for CP2K input files.

This module provides physics/chemistry-aware validation that goes beyond
syntax checking to detect semantically incorrect but syntactically valid inputs.

Supports CP2K versions: 2022.x, 2023.x, 2024.1, 2024.2
"""

from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass
class SemanticDiagnostic:
    """A semantic validation diagnostic."""

    line: int
    message: str
    severity: str  # "error" or "warning"
    code: str  # Error code for programmatic handling
    section: Optional[str] = None


# Version-specific removed/deprecated keywords
REMOVED_KEYWORDS = {
    # CP2K 2023.x removed
    "SINGLE_PRECISION_MATRICES": {
        "since": "2023.1",
        "message": "SINGLE_PRECISION_MATRICES 已在 CP2K 2023.x 中移除。\n" "建议：移除此关键字，精度控制现在通过其他参数实现。",
    },
    "BROYDEN_MIXING_NEW": {
        "since": "2024.1",
        "message": "BROYDEN_MIXING_NEW 已在 CP2K 2024.x 中移除。\n" "建议：使用 BROYDEN_MIXING 或其他混合方法。",
    },
    "KP_RI_EXTENSION_FACTOR": {
        "since": "2024.1",
        "message": "KP_RI_EXTENSION_FACTOR 已在 CP2K 2024.x 中移除。\n" "建议：使用新的 K 点 RI 参数配置。",
    },
}

DEPRECATED_KEYWORDS = {
    # CP2K 2024.x deprecated
    "QUIP": {
        "since": "2024.1",
        "message": "QUIP 支持已在 CP2K 2024.x 中标记为废弃，将在未来版本移除。\n"
        "建议：迁移到其他机器学习势方法如 DeePMD 或 NequIP。",
    },
    "PEXSI": {
        "since": "2024.1",
        "message": "PEXSI 支持已在 CP2K 2024.x 中标记为废弃。\n" "建议：考虑使用其他电子结构方法。",
    },
}


class CP2KSemanticValidator:
    """Validates CP2K input files for semantic correctness."""

    # Target CP2K version for validation
    TARGET_VERSION = "2024.1"

    # RUN_TYPE to MOTION section mapping
    # Format: RUN_TYPE -> (required_sections, forbidden_sections, description)
    RUN_TYPE_MOTION_MAP = {
        "ENERGY": {
            "required": set(),
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT", "BAND", "MC"},
            "description": "静态单点能计算",
        },
        "ENERGY_FORCE": {
            "required": set(),
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT", "BAND", "MC"},
            "description": "计算能量和力",
        },
        "GEO_OPT": {
            "required": {"GEO_OPT"},
            "forbidden": {"MD", "CELL_OPT", "BAND"},
            "description": "几何优化",
        },
        "MD": {
            "required": {"MD"},
            "forbidden": {"GEO_OPT", "CELL_OPT", "BAND"},
            "description": "分子动力学",
        },
        "CELL_OPT": {
            "required": {"CELL_OPT"},
            "forbidden": {"GEO_OPT", "MD", "BAND"},
            "description": "晶胞优化",
        },
        "BAND": {
            "required": {"BAND"},
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT"},
            "description": "能带计算",
        },
        "MC": {
            "required": {"MC"},
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT", "BAND"},
            "description": "蒙特卡洛",
        },
        "VIBRATIONAL_ANALYSIS": {
            "required": set(),  # Uses VIBRATIONAL_ANALYSIS top-level section
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT"},
            "description": "振动分析",
        },
        "LR": {
            "required": set(),
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT"},
            "description": "线性响应",
        },
        "SPECTRA": {
            "required": set(),
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT"},
            "description": "光谱计算",
        },
        "NONE": {
            "required": set(),
            "forbidden": {"GEO_OPT", "MD", "CELL_OPT", "BAND", "MC"},
            "description": "无任务",
        },
    }

    # FORCE_EVAL METHOD compatibility
    METHOD_SECTION_MAP = {
        "QS": {"required": {"DFT"}, "forbidden": {"FIST", "MM"}, "description": "QuickStep DFT"},
        "QUICKSTEP": {"required": {"DFT"}, "forbidden": {"FIST", "MM"}, "description": "QuickStep DFT"},
        "FIST": {"required": set(), "forbidden": {"DFT"}, "description": "分子力学"},  # FIST uses MM subsection
        "QMMM": {"required": set(), "forbidden": set(), "description": "QM/MM"},
        "EIP": {"required": set(), "forbidden": {"DFT", "FIST"}, "description": "经验势"},
        "NNP": {"required": {"NNP"}, "forbidden": {"DFT"}, "description": "神经网络势"},
        "MIXED": {"required": set(), "forbidden": set(), "description": "混合方法"},
    }

    # SCF solver conflicts
    SCF_SOLVER_SECTIONS = {"OT", "DIAGONALIZATION"}

    # Element to atomic number (common elements)
    ELEMENT_Z = {
        "H": 1,
        "He": 2,
        "Li": 3,
        "Be": 4,
        "B": 5,
        "C": 6,
        "N": 7,
        "O": 8,
        "F": 9,
        "Ne": 10,
        "Na": 11,
        "Mg": 12,
        "Al": 13,
        "Si": 14,
        "P": 15,
        "S": 16,
        "Cl": 17,
        "Ar": 18,
        "K": 19,
        "Ca": 20,
        "Sc": 21,
        "Ti": 22,
        "V": 23,
        "Cr": 24,
        "Mn": 25,
        "Fe": 26,
        "Co": 27,
        "Ni": 28,
        "Cu": 29,
        "Zn": 30,
        "Ga": 31,
        "Ge": 32,
        "As": 33,
        "Se": 34,
        "Br": 35,
        "Kr": 36,
        "Rb": 37,
        "Sr": 38,
        "Y": 39,
        "Zr": 40,
        "Nb": 41,
        "Mo": 42,
        "Tc": 43,
        "Ru": 44,
        "Rh": 45,
        "Pd": 46,
        "Ag": 47,
        "Cd": 48,
        "In": 49,
        "Sn": 50,
        "Sb": 51,
        "Te": 52,
        "I": 53,
        "Xe": 54,
        "Cs": 55,
        "Ba": 56,
        "La": 57,
        "Ce": 58,
        "Pr": 59,
        "Nd": 60,
        "Pm": 61,
        "Sm": 62,
        "Eu": 63,
        "Gd": 64,
        "Tb": 65,
        "Dy": 66,
        "Ho": 67,
        "Er": 68,
        "Tm": 69,
        "Yb": 70,
        "Lu": 71,
        "Hf": 72,
        "Ta": 73,
        "W": 74,
        "Re": 75,
        "Os": 76,
        "Ir": 77,
        "Pt": 78,
        "Au": 79,
        "Hg": 80,
        "Tl": 81,
        "Pb": 82,
        "Bi": 83,
        "Po": 84,
        "At": 85,
        "Rn": 86,
        "Fr": 87,
        "Ra": 88,
        "Ac": 89,
        "Th": 90,
        "Pa": 91,
        "U": 92,
    }

    def __init__(self):
        self.diagnostics: List[SemanticDiagnostic] = []

    def validate(self, tree: Any) -> List[SemanticDiagnostic]:
        """Validate a parsed CP2K input tree.

        Args:
            tree: The parsed CP2K input tree (nested dict from parser).

        Returns:
            List of semantic diagnostics.
        """
        self.diagnostics = []

        # Get GLOBAL section
        global_section = tree.get("global", {})
        if isinstance(global_section, list):
            global_section = global_section[0] if global_section else {}

        # Get RUN_TYPE
        run_type = self._get_keyword_value(global_section, "run_type", "ENERGY_FORCE").upper()

        # Get MOTION section
        motion_section = tree.get("motion", {})
        if isinstance(motion_section, list):
            motion_section = motion_section[0] if motion_section else {}

        # Get FORCE_EVAL section
        force_eval_sections = tree.get("force_eval", [])
        if not isinstance(force_eval_sections, list):
            force_eval_sections = [force_eval_sections] if force_eval_sections else []

        # 1. Validate RUN_TYPE vs MOTION sections
        self._validate_run_type_motion(run_type, motion_section, global_section)

        # 2. Validate FORCE_EVAL METHOD compatibility
        for fe in force_eval_sections:
            self._validate_force_eval_method(fe)

        # 3. Validate electronic structure (for each FORCE_EVAL)
        for fe in force_eval_sections:
            self._validate_electronic_structure(fe)

        # 4. Validate SCF solver conflicts
        for fe in force_eval_sections:
            dft = fe.get("dft", {})
            if isinstance(dft, list):
                dft = dft[0] if dft else {}
            self._validate_scf_solvers(dft)

        # 5. Validate cutoff energy
        for fe in force_eval_sections:
            self._validate_cutoff(fe)

        # 6. Validate removed/deprecated keywords (version-aware)
        self._validate_removed_keywords(tree)

        # 7. Validate SCF parameters
        for fe in force_eval_sections:
            dft = fe.get("dft", {})
            if isinstance(dft, list):
                dft = dft[0] if dft else {}
            scf = dft.get("scf", {})
            if isinstance(scf, list):
                scf = scf[0] if scf else {}
            self._validate_scf_params(scf)

        # 8. Validate coordinate section
        for fe in force_eval_sections:
            subsys = fe.get("subsys", {})
            if isinstance(subsys, list):
                subsys = subsys[0] if subsys else {}
            coord = subsys.get("coord", {})
            if isinstance(coord, list):
                coord = coord[0] if coord else {}
            self._validate_coordinates(coord)

        # 9. Validate XC functional
        for fe in force_eval_sections:
            dft = fe.get("dft", {})
            if isinstance(dft, list):
                dft = dft[0] if dft else {}
            self._validate_xc_functional(dft)

        return self.diagnostics

    def _get_keyword_value(self, section: Dict, keyword: str, default: str = "") -> str:
        """Get keyword value from a section dict."""
        value = section.get(keyword, default)
        if isinstance(value, dict):
            return value.get("_", default)
        return str(value) if value else default

    def _get_section_line(self, section: Dict, default: int = 1) -> int:
        """Try to get the line number of a section."""
        return section.get("_line", default)

    def _validate_run_type_motion(self, run_type: str, motion_section: Dict, global_section: Dict):
        """Validate RUN_TYPE consistency with MOTION subsections."""
        if run_type not in self.RUN_TYPE_MOTION_MAP:
            # Unknown RUN_TYPE, skip validation
            return

        rules = self.RUN_TYPE_MOTION_MAP[run_type]
        required: set[str] = set(rules["required"])  # type: ignore[assignment]
        forbidden: set[str] = set(rules["forbidden"])  # type: ignore[assignment]

        # Get present MOTION subsections
        present_motion_sections = set()
        for name in motion_section.keys():
            if name.startswith("_"):
                continue
            present_motion_sections.add(name.upper())

        # Check for forbidden sections
        forbidden_present = present_motion_sections & forbidden
        if forbidden_present:
            line = self._get_section_line(motion_section)
            section_name = next(iter(forbidden_present))

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"`RUN_TYPE={run_type}` ({rules['description']}) 与 `&MOTION / &{section_name}` 截面矛盾。\n"
                    f"  - 当前设置：RUN_TYPE={run_type}\n"
                    f"  - 检测到：&{section_name} 截面\n"
                    f"建议：\n"
                    f"  - 若需{'几何优化' if section_name == 'GEO_OPT' else '分子动力学' if section_name == 'MD' else section_name}"
                    f"，请将 RUN_TYPE 改为 `{section_name}`\n"
                    f"  - 若需静态计算，请删除 &MOTION / &{section_name} 截面",
                    severity="error",
                    code="RUN_TYPE_MOTION_MISMATCH",
                    section=f"MOTION/{section_name}",
                )
            )

        # Check for missing required sections (warning only)
        missing_required = required - present_motion_sections
        if missing_required:
            line = self._get_section_line(global_section)
            section_name = next(iter(missing_required))

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"`RUN_TYPE={run_type}` 通常需要 `&MOTION / &{section_name}` 截面。\n"
                    f"  - 当前设置：RUN_TYPE={run_type} ({rules['description']})\n"
                    f"  - 缺少：&{section_name} 截面\n"
                    f"建议：添加 `&MOTION / &{section_name}` 截面或检查 RUN_TYPE 设置是否正确",
                    severity="warning",
                    code="MISSING_MOTION_SECTION",
                    section="MOTION",
                )
            )

    def _validate_force_eval_method(self, force_eval: Dict):
        """Validate FORCE_EVAL METHOD compatibility."""
        method = self._get_keyword_value(force_eval, "method", "QS").upper()

        if method not in self.METHOD_SECTION_MAP:
            return

        rules = self.METHOD_SECTION_MAP[method]
        required: set[str] = set(rules["required"])  # type: ignore[assignment]
        forbidden: set[str] = set(rules["forbidden"])  # type: ignore[assignment]

        # Get present sections in FORCE_EVAL
        present_sections = set()
        for name in force_eval.keys():
            if name.startswith("_"):
                continue
            present_sections.add(name.upper())

        # Check for required sections
        missing_required = required - present_sections
        if missing_required:
            line = self._get_section_line(force_eval)
            section_name = next(iter(missing_required))

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"`METHOD={method}` ({rules['description']}) 需要 `&{section_name}` 截面。\n"
                    f"  - 当前设置：METHOD={method}\n"
                    f"  - 缺少：&{section_name} 截面",
                    severity="error",
                    code="MISSING_REQUIRED_SECTION",
                    section=f"FORCE_EVAL/{section_name}",
                )
            )

        # Check for forbidden sections
        forbidden_present = present_sections & forbidden
        if forbidden_present:
            line = self._get_section_line(force_eval)
            section_name = next(iter(forbidden_present))

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"`METHOD={method}` 与 `&{section_name}` 截面不兼容。\n"
                    f"  - 当前设置：METHOD={method} ({rules['description']})\n"
                    f"  - 不兼容：&{section_name} 截面",
                    severity="error",
                    code="METHOD_SECTION_INCOMPAT",
                    section=f"FORCE_EVAL/{section_name}",
                )
            )

    def _validate_electronic_structure(self, force_eval: Dict):
        """Validate electronic structure settings (charge, multiplicity, UKS)."""
        dft = force_eval.get("dft", {})
        if isinstance(dft, list):
            dft = dft[0] if dft else {}

        if not dft:
            return  # Not a DFT calculation

        # Get charge and multiplicity
        charge_str = self._get_keyword_value(dft, "charge", "0")
        mult_str = self._get_keyword_value(dft, "multiplicity", "0")
        uks = self._get_keyword_value(dft, "uks", "false").lower() in ("true", "yes", "1")

        try:
            charge = int(charge_str)
            multiplicity = int(mult_str)
        except ValueError:
            return  # Invalid values, skip

        # Get coordinates to count electrons
        subsys = force_eval.get("subsys", {})
        if isinstance(subsys, list):
            subsys = subsys[0] if subsys else {}

        coord = subsys.get("coord", {})
        if isinstance(coord, list):
            coord = coord[0] if coord else {}

        if not coord:
            return  # No coordinates, skip

        # Count electrons from coordinates
        total_electrons = self._count_electrons(coord)

        if total_electrons == 0:
            return  # Could not determine

        # Adjust for charge
        total_electrons -= charge

        # Validate multiplicity vs electron count
        # odd multiplicity (1, 3, 5...) -> even electrons
        # even multiplicity (2, 4, 6...) -> odd electrons
        electrons_mod_2 = total_electrons % 2
        mult_mod_2 = multiplicity % 2

        # If both are even or both are odd -> mismatch
        if multiplicity > 0 and electrons_mod_2 == mult_mod_2:
            line = self._get_section_line(dft)

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"电子数 ({total_electrons}) 与多重态 (MULTIPLICITY={multiplicity}) 不一致。\n"
                    f"  - 当前电子数：{total_electrons} ({'偶数' if electrons_mod_2 == 0 else '奇数'})\n"
                    f"  - 多重态 {multiplicity} 需要{'偶数' if mult_mod_2 == 1 else '奇数'}电子\n"
                    f"建议：检查 CHARGE、MULTIPLICITY 或坐标设置",
                    severity="error",
                    code="ELECTRON_MULT_MISMATCH",
                    section="FORCE_EVAL/DFT",
                )
            )

        # Validate UKS with multiplicity > 1
        if multiplicity > 1 and not uks:
            line = self._get_section_line(dft)

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"MULTIPLICITY={multiplicity} (> 1) 需要 UKS=TRUE。\n"
                    f"  - 多重态 {multiplicity} 意味着开壳层体系\n"
                    f"  - 当前 UKS=FALSE (闭壳层)\n"
                    f"建议：添加 `UKS TRUE` 或设置 `MULTIPLICITY 1`",
                    severity="error",
                    code="MULTIPLICITY_UKS_MISMATCH",
                    section="FORCE_EVAL/DFT",
                )
            )

    def _count_electrons(self, coord_section: Dict) -> int:
        """Count electrons from COORD section."""
        total = 0

        # Get coordinate lines (default keyword *)
        coord_lines = coord_section.get("*", [])
        if isinstance(coord_lines, str):
            coord_lines = [coord_lines]

        for line in coord_lines:
            if isinstance(line, str):
                parts = line.split()
                if parts:
                    element = parts[0].capitalize()
                    z = self.ELEMENT_Z.get(element, 0)
                    total += z

        return total

    def _validate_scf_solvers(self, dft: Dict):
        """Validate SCF solver conflicts."""
        scf = dft.get("scf", {})
        if isinstance(scf, list):
            scf = scf[0] if scf else {}

        if not scf:
            return

        present_solvers = []
        for solver in self.SCF_SOLVER_SECTIONS:
            if solver.lower() in [k.lower() for k in scf.keys()]:
                present_solvers.append(solver)

        if len(present_solvers) > 1:
            line = self._get_section_line(scf)

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"不能同时使用多种 SCF 求解器。\n"
                    f"  - 检测到：{', '.join(f'&{s}' for s in present_solvers)}\n"
                    f"  - OT (优化传递) 和 DIAGONALIZATION 是互斥的\n"
                    f"建议：只保留一种 SCF 求解器",
                    severity="error",
                    code="SCF_SOLVER_CONFLICT",
                    section="FORCE_EVAL/DFT/SCF",
                )
            )

    def _validate_cutoff(self, force_eval: Dict):
        """Validate cutoff energy."""
        dft = force_eval.get("dft", {})
        if isinstance(dft, list):
            dft = dft[0] if dft else {}

        mgrid = dft.get("mgrid", {})
        if isinstance(mgrid, list):
            mgrid = mgrid[0] if mgrid else {}

        if not mgrid:
            return

        cutoff_str = self._get_keyword_value(mgrid, "cutoff", "0")

        try:
            cutoff = float(cutoff_str)
        except ValueError:
            return

        # Warn if cutoff is too low (typically < 300 Ry for production)
        if 0 < cutoff < 300:
            line = self._get_section_line(mgrid)

            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message=f"截断能过低可能导致结果不准确。\n"
                    f"  - 当前 CUTOFF：{cutoff} Ry\n"
                    f"  - 建议值：≥ 300 Ry (生产计算)\n"
                    f"  - 高精度：≥ 600 Ry\n"
                    f"注意：截断能过低会导致基组不完整，能量和力计算误差增大",
                    severity="warning",
                    code="LOW_CUTOFF",
                    section="FORCE_EVAL/DFT/MGRID",
                )
            )

    def _validate_removed_keywords(self, tree: Dict):
        """Validate removed/deprecated keywords based on CP2K version."""
        # Recursively search for removed keywords
        self._check_keywords_recursive(tree, [])

    def _check_keywords_recursive(self, section: Dict, path: List[str]):
        """Recursively check for removed/deprecated keywords."""
        if not isinstance(section, dict):
            return

        for key, value in section.items():
            if key.startswith("_"):
                continue

            key_upper = key.upper()

            # Check for removed keywords
            if key_upper in REMOVED_KEYWORDS:
                info = REMOVED_KEYWORDS[key_upper]
                line = self._get_section_line(section)
                section_path = "/".join(path) if path else "ROOT"

                self.diagnostics.append(
                    SemanticDiagnostic(
                        line=line,
                        message=f"关键字 `{key}` 已在 CP2K {info['since']} 中移除。\n"
                        f"{info['message']}\n"
                        f"当前 LSP 支持版本：{self.TARGET_VERSION}",
                        severity="error",
                        code="REMOVED_KEYWORD",
                        section=section_path,
                    )
                )

            # Check for deprecated keywords
            if key_upper in DEPRECATED_KEYWORDS:
                info = DEPRECATED_KEYWORDS[key_upper]
                line = self._get_section_line(section)
                section_path = "/".join(path) if path else "ROOT"

                self.diagnostics.append(
                    SemanticDiagnostic(
                        line=line,
                        message=f"关键字 `{key}` 在 CP2K {info['since']} 中已废弃。\n" f"{info['message']}",
                        severity="warning",
                        code="DEPRECATED_KEYWORD",
                        section=section_path,
                    )
                )

            # Recurse into subsections
            if isinstance(value, dict):
                self._check_keywords_recursive(value, path + [key_upper])
            elif isinstance(value, list):
                for item in value:
                    if isinstance(item, dict):
                        self._check_keywords_recursive(item, path + [key_upper])

    def _validate_scf_params(self, scf: Dict):
        """Validate SCF parameters for common issues."""
        if not scf:
            return

        # Check MAX_SCF
        max_scf_str = self._get_keyword_value(scf, "max_scf", "50")
        try:
            max_scf = int(max_scf_str)
            if max_scf < 20:
                line = self._get_section_line(scf)
                self.diagnostics.append(
                    SemanticDiagnostic(
                        line=line,
                        message=f"MAX_SCF 设置过少可能导致 SCF 不收敛。\n"
                        f"  - 当前值：{max_scf}\n"
                        f"  - 建议值：≥ 20（默认 50）\n"
                        f"建议：增加 MAX_SCF 或检查初始猜测设置",
                        severity="warning",
                        code="LOW_MAX_SCF",
                        section="FORCE_EVAL/DFT/SCF",
                    )
                )
        except ValueError:
            pass

        # Check EPS_SCF
        eps_scf_str = self._get_keyword_value(scf, "eps_scf", "1.0E-5")
        try:
            eps_scf = float(eps_scf_str)
            if eps_scf > 1.0e-5:
                line = self._get_section_line(scf)
                self.diagnostics.append(
                    SemanticDiagnostic(
                        line=line,
                        message=f"EPS_SCF 设置过松可能导致能量精度不足。\n"
                        f"  - 当前值：{eps_scf_str}\n"
                        f"  - 建议值：≤ 1.0E-5（默认 1.0E-5）\n"
                        f"建议：减小 EPS_SCF 以提高精度",
                        severity="warning",
                        code="LOOSE_EPS_SCF",
                        section="FORCE_EVAL/DFT/SCF",
                    )
                )
        except ValueError:
            pass

        # Check for IGNORE_CONVERGENCE_FAILURE
        ignore = self._get_keyword_value(scf, "ignore_convergence_failure", "false")
        if ignore.lower() in ("true", "yes", "1"):
            line = self._get_section_line(scf)
            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message="IGNORE_CONVERGENCE_FAILURE 已启用。\n"
                    "  - SCF 不收敛时程序将继续运行\n"
                    "  - 结果可能不可靠\n"
                    "建议：仅在调试时使用，生产计算请确保 SCF 收敛",
                    severity="warning",
                    code="IGNORE_SCF_FAILURE",
                    section="FORCE_EVAL/DFT/SCF",
                )
            )

    def _validate_coordinates(self, coord: Dict):
        """Validate coordinate section."""
        if not coord:
            return

        coord_lines = coord.get("*", [])
        if isinstance(coord_lines, str):
            coord_lines = [coord_lines]

        line_num = self._get_section_line(coord)
        unknown_elements = []

        for line in coord_lines:
            if isinstance(line, str):
                parts = line.split()
                if parts:
                    element = parts[0].capitalize()
                    # Check if valid element
                    if element not in self.ELEMENT_Z:
                        unknown_elements.append(element)

        if unknown_elements:
            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line_num,
                    message=f"检测到未知元素符号。\n"
                    f"  - 未知元素：{', '.join(set(unknown_elements))}\n"
                    f"  - 支持的元素：H-U (1-92)\n"
                    f"建议：检查坐标中的元素符号是否正确",
                    severity="error",
                    code="UNKNOWN_ELEMENT",
                    section="FORCE_EVAL/SUBSYS/COORD",
                )
            )

    def _validate_xc_functional(self, dft: Dict):
        """Validate XC functional configuration."""
        xc = dft.get("xc", {})
        if isinstance(xc, list):
            xc = xc[0] if xc else {}

        # If no XC section at all, skip (QS method requires it but that's caught elsewhere)
        # Note: {} means XC section exists but is empty - we should check it
        if xc is None:
            return

        # Get xc_functional section
        xc_functional = xc.get("xc_functional")

        # If xc_functional key doesn't exist (empty XC section or missing XC_FUNCTIONAL)
        if xc_functional is None:
            line = self._get_section_line(xc) if xc else 1
            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message="未指定 XC 泛函。\n"
                    "  - &XC 截面存在但未定义 &XC_FUNCTIONAL\n"
                    "建议：添加泛函定义如 `&XC_FUNCTIONAL PBE`",
                    severity="warning",
                    code="NO_XC_FUNCTIONAL",
                    section="FORCE_EVAL/DFT/XC",
                )
            )
            return

        if isinstance(xc_functional, list):
            xc_functional = xc_functional[0] if xc_functional else {}

        # Check if xc_functional is empty
        has_functional = False

        # Check for shortcut form like XC_FUNCTIONAL PBE
        if xc_functional and xc_functional.get("_"):
            has_functional = True
        else:
            # Check for subsection form like &PBE, &B3LYP
            for key in xc_functional.keys():
                if not key.startswith("_"):
                    has_functional = True
                    break

        if not has_functional:
            line = self._get_section_line(xc_functional) if xc_functional else self._get_section_line(xc)
            self.diagnostics.append(
                SemanticDiagnostic(
                    line=line,
                    message="未指定 XC 泛函。\n"
                    "  - &XC_FUNCTIONAL 截面为空或未定义\n"
                    "建议：添加泛函定义如 `&XC_FUNCTIONAL PBE` 或 `&PBE` 子截面",
                    severity="warning",
                    code="NO_XC_FUNCTIONAL",
                    section="FORCE_EVAL/DFT/XC/XC_FUNCTIONAL",
                )
            )


def validate_semantics(tree: Any) -> List[SemanticDiagnostic]:
    """Convenience function to validate semantics."""
    validator = CP2KSemanticValidator()
    return validator.validate(tree)
