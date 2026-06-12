"""Tests for semantic validation of CP2K input files."""

from cp2k_input_tools.parser import CP2KInputParserSimplified
from cp2k_input_tools.validator import validate_semantics


class TestRunTypeMotionValidation:
    """Test RUN_TYPE vs MOTION section validation."""

    def test_energy_with_motion_geo_opt_is_error(self):
        """RUN_TYPE=ENERGY with GEO_OPT section should raise error."""
        content = """
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL

&MOTION
  &GEO_OPT
    MAX_ITER 100
  &END GEO_OPT
&END MOTION
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "RUN_TYPE_MOTION_MISMATCH" in codes

        error = next(d for d in diagnostics if d.code == "RUN_TYPE_MOTION_MISMATCH")
        assert error.severity == "error"
        assert "ENERGY" in error.message
        assert "GEO_OPT" in error.message

    def test_energy_with_motion_md_is_error(self):
        """RUN_TYPE=ENERGY with MD section should raise error."""
        content = """
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
  &END DFT
&END FORCE_EVAL

&MOTION
  &MD
    STEPS 1000
  &END MD
&END MOTION
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "RUN_TYPE_MOTION_MISMATCH" in codes

    def test_geo_opt_without_geo_opt_section_is_warning(self):
        """RUN_TYPE=GEO_OPT without GEO_OPT section should warn."""
        content = """
&GLOBAL
  RUN_TYPE GEO_OPT
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
  &END DFT
&END FORCE_EVAL

&MOTION
  &MD
    STEPS 1000
  &END MD
&END MOTION
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        # Should have both RUN_TYPE_MOTION_MISMATCH and MISSING_MOTION_SECTION
        codes = [d.code for d in diagnostics]
        assert "RUN_TYPE_MOTION_MISMATCH" in codes  # MD is forbidden for GEO_OPT
        assert "MISSING_MOTION_SECTION" in codes  # GEO_OPT section is missing

    def test_geo_opt_with_geo_opt_section_is_valid(self):
        """RUN_TYPE=GEO_OPT with GEO_OPT section should be valid."""
        content = """
&GLOBAL
  RUN_TYPE GEO_OPT
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL

&MOTION
  &GEO_OPT
    MAX_ITER 100
  &END GEO_OPT
&END MOTION
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        # Filter to error-level diagnostics only
        errors = [d for d in diagnostics if d.severity == "error"]
        assert len(errors) == 0

    def test_md_with_md_section_is_valid(self):
        """RUN_TYPE=MD with MD section should be valid."""
        content = """
&GLOBAL
  RUN_TYPE MD
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
  &END DFT
&END FORCE_EVAL

&MOTION
  &MD
    STEPS 1000
  &END MD
&END MOTION
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        errors = [d for d in diagnostics if d.severity == "error"]
        assert len(errors) == 0

    def test_static_calculation_is_valid(self):
        """RUN_TYPE=ENERGY without MOTION section should be valid."""
        content = """
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        errors = [d for d in diagnostics if d.severity == "error"]
        assert len(errors) == 0


class TestForceEvalMethodValidation:
    """Test FORCE_EVAL METHOD validation."""

    def test_qs_method_requires_dft(self):
        """METHOD=QS should require DFT section."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "MISSING_REQUIRED_SECTION" in codes

        error = next(d for d in diagnostics if d.code == "MISSING_REQUIRED_SECTION")
        assert "DFT" in error.message

    def test_qs_method_with_fist_is_error(self):
        """METHOD=QS with FIST/MM is incompatible - use FIST method instead."""
        # FIST section is not a valid subsection in CP2K schema
        # Instead, test that QS with MM subsection (if valid) would be flagged
        # For now, we test that QS method requires DFT section
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD FIST
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "METHOD_SECTION_INCOMPAT" in codes


class TestElectronicStructureValidation:
    """Test electronic structure validation."""

    def test_multiplicity_uks_mismatch(self):
        """MULTIPLICITY > 1 without UKS should raise error."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    MULTIPLICITY 2
    UKS FALSE
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "MULTIPLICITY_UKS_MISMATCH" in codes

    def test_multiplicity_with_uks_is_valid(self):
        """MULTIPLICITY > 1 with UKS should be valid."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    MULTIPLICITY 2
    UKS TRUE
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "MULTIPLICITY_UKS_MISMATCH" not in codes

    def test_electron_multiplicity_mismatch(self):
        """Electron count inconsistent with multiplicity should raise error."""
        # H atom has 1 electron, singlet (MULT=1) needs even electrons
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    CHARGE 0
    MULTIPLICITY 1
    UKS FALSE
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      H  0.0  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "ELECTRON_MULT_MISMATCH" in codes

    def test_electron_multiplicity_valid(self):
        """Consistent electron count and multiplicity should be valid."""
        # H2O has 10 electrons, singlet needs even - valid
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    CHARGE 0
    MULTIPLICITY 1
    UKS FALSE
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "ELECTRON_MULT_MISMATCH" not in codes


class TestSCFSolverValidation:
    """Test SCF solver validation."""

    def test_ot_and_diagonalization_conflict(self):
        """OT and DIAGONALIZATION together should raise error."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &SCF
      &OT
      &END OT
      &DIAGONALIZATION
      &END DIAGONALIZATION
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "SCF_SOLVER_CONFLICT" in codes

    def test_ot_only_is_valid(self):
        """OT alone should be valid."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &SCF
      &OT
      &END OT
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "SCF_SOLVER_CONFLICT" not in codes


class TestCutoffValidation:
    """Test cutoff energy validation."""

    def test_low_cutoff_warning(self):
        """Low cutoff should raise warning."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &MGRID
      CUTOFF 50
    &END MGRID
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "LOW_CUTOFF" in codes

        diag = next(d for d in diagnostics if d.code == "LOW_CUTOFF")
        assert diag.severity == "warning"

    def test_high_cutoff_is_valid(self):
        """High cutoff should not raise warning."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &MGRID
      CUTOFF 500
    &END MGRID
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "LOW_CUTOFF" not in codes


class TestIntegration:
    """Integration tests for complex scenarios."""

    def test_realistic_dft_calculation(self):
        """A realistic DFT calculation should have no errors."""
        content = """
&GLOBAL
  PROJECT H2O
  RUN_TYPE ENERGY_FORCE
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    CHARGE 0
    MULTIPLICITY 1
    &MGRID
      CUTOFF 500
      REL_CUTOFF 50
    &END MGRID
    &SCF
      &OT
      &END OT
    &END SCF
    &XC
      &XC_FUNCTIONAL B3LYP
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.000000  0.000000  0.117489
      H  0.000000  0.757210 -0.469957
      H  0.000000 -0.757210 -0.469957
    &END COORD
    &KIND O
      BASIS_SET TZVP
      POTENTIAL GTH-BLYP
    &END KIND
    &KIND H
      BASIS_SET TZVP
      POTENTIAL GTH-BLYP
    &END KIND
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        errors = [d for d in diagnostics if d.severity == "error"]
        assert len(errors) == 0

    def test_realistic_md_calculation(self):
        """A realistic MD calculation should have no errors."""
        content = """
&GLOBAL
  PROJECT H2O_MD
  RUN_TYPE MD
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &MGRID
      CUTOFF 400
    &END MGRID
    &SCF
      &OT
      &END OT
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL

&MOTION
  &MD
    ENSEMBLE NVT
    STEPS 1000
    TIMESTEP 0.5
    TEMPERATURE 300
  &END MD
&END MOTION
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        errors = [d for d in diagnostics if d.severity == "error"]
        assert len(errors) == 0


class TestRemovedKeywords:
    """Test removed/deprecated keywords validation."""

    # Note: Many removed keywords are caught at parser level
    # Semantic validation catches keywords that are valid in syntax but deprecated

    def test_removed_keyword_detection(self):
        """Removed keywords should be detected if they pass parser."""
        # This test documents that removed keywords like SINGLE_PRECISION_MATRICES
        # are caught at the parser level, not semantic level
        # The semantic validator adds version-specific messages
        pass


class TestSCFParamsValidation:
    """Test SCF parameters validation."""

    def test_low_max_scf_warning(self):
        """Low MAX_SCF should raise warning."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &SCF
      MAX_SCF 10
      &OT
      &END OT
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "LOW_MAX_SCF" in codes

        diag = next(d for d in diagnostics if d.code == "LOW_MAX_SCF")
        assert diag.severity == "warning"

    def test_loose_eps_scf_warning(self):
        """Loose EPS_SCF should raise warning."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &SCF
      EPS_SCF 1.0E-3
      &OT
      &END OT
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "LOOSE_EPS_SCF" in codes

    def test_ignore_convergence_failure_warning(self):
        """IGNORE_CONVERGENCE_FAILURE should raise warning."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &SCF
      IGNORE_CONVERGENCE_FAILURE T
      &OT
      &END OT
    &END SCF
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "IGNORE_SCF_FAILURE" in codes


class TestCoordinateValidation:
    """Test coordinate section validation."""

    def test_unknown_element_error(self):
        """Unknown element symbol should raise error."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      O  0.0  0.0  0.0
      X  1.0  0.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "UNKNOWN_ELEMENT" in codes

        error = next(d for d in diagnostics if d.code == "UNKNOWN_ELEMENT")
        assert "X" in error.message
        assert error.severity == "error"

    def test_valid_elements_no_error(self):
        """Valid elements should not raise error."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
  &SUBSYS
    &COORD
      C  0.0  0.0  0.0
      H  0.9  0.0  0.0
      H -0.9  0.0  0.0
      O  0.0  1.0  0.0
      N  0.0  2.0  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "UNKNOWN_ELEMENT" not in codes


class TestXCFunctionalValidation:
    """Test XC functional validation."""

    def test_no_xc_functional_warning(self):
        """Missing XC functional should raise warning."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "NO_XC_FUNCTIONAL" in codes

    def test_xc_functional_present_is_valid(self):
        """XC functional present should be valid."""
        content = """
&GLOBAL
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC
  &END DFT
&END FORCE_EVAL
"""
        parser = CP2KInputParserSimplified()
        tree = parser.parse(content.split("\n"))
        diagnostics = validate_semantics(tree)

        codes = [d.code for d in diagnostics]
        assert "NO_XC_FUNCTIONAL" not in codes
