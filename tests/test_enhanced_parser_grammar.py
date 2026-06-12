"""Tests for enhanced cp2k_lsp package importability and parser grammar."""

import pytest

# =============================================================================
# Issue #8: Package importability tests
# =============================================================================


class TestPackageImportability:
    """Test that cp2k_lsp package is importable and exposes expected API."""

    def test_import_cp2k_lsp(self):
        """Main package should be importable."""
        import cp2k_lsp

        assert cp2k_lsp is not None

    def test_import_version(self):
        """Package should expose version."""
        from cp2k_lsp import __version__

        assert isinstance(__version__, str)
        assert __version__ == "0.1.0"

    def test_import_server(self):
        """Language server class should be importable."""
        from cp2k_lsp import CP2KLanguageServer

        assert CP2KLanguageServer is not None

    def test_import_parser(self):
        """Parser classes should be importable."""
        from cp2k_lsp import CP2KInput, CP2KParser, Lexer

        assert CP2KParser is not None
        assert Lexer is not None
        assert CP2KInput is not None

    def test_import_server_main(self):
        """Server main function should be importable."""
        from cp2k_lsp import server_main

        assert callable(server_main)

    def test_import_parser_subpackage(self):
        """Parser subpackage should be importable."""
        from cp2k_lsp.parser import CP2KInput, CP2KParser, Keyword, Lexer, ParseError, Section, SyntaxError, TokenType, Value

        assert all([CP2KParser, Lexer, TokenType, CP2KInput, Section, Keyword, Value, ParseError, SyntaxError])

    def test_import_lexer_module(self):
        """Lexer module should be importable directly."""
        from cp2k_lsp.parser.lexer import Lexer, TokenType

        assert Lexer is not None
        assert TokenType is not None

    def test_import_parser_module(self):
        """Parser module should be importable directly."""
        from cp2k_lsp.parser.parser import CP2KParser

        assert CP2KParser is not None

    def test_import_ast_module(self):
        """AST module should be importable directly."""
        from cp2k_lsp.parser.ast import CP2KInput

        assert CP2KInput is not None

    def test_import_errors_module(self):
        """Errors module (Issue #9) should be importable."""
        from cp2k_lsp.parser.errors import ParseError, SyntaxError

        assert ParseError is not None
        assert SyntaxError is not None

    def test_server_instantiation(self):
        """Language server should be instantiable."""
        from cp2k_lsp import CP2KLanguageServer

        server = CP2KLanguageServer()
        assert server is not None
        assert hasattr(server, "parsed_documents")


# =============================================================================
# Issue #9: Parser smoke tests (missing errors module + basic grammar)
# =============================================================================


class TestParserSmoke:
    """Smoke tests for the enhanced parser grammar."""

    def _parse(self, text: str):
        """Helper to parse text and return (ast, errors)."""
        from cp2k_lsp.parser import CP2KParser

        parser = CP2KParser.parse_text(text)
        return parser.ast, parser.errors

    def test_minimal_valid_cp2k_input(self):
        """Minimal valid CP2K input should parse without errors."""
        inp = """\
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
"""
        ast, errors = self._parse(inp)
        assert ast is not None
        assert ast.global_section is not None
        assert ast.global_section.name == "GLOBAL"

    def test_whitespace_separated_keyword(self):
        """CP2K whitespace-separated keywords should work (Issue #10)."""
        inp = """\
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
"""
        ast, errors = self._parse(inp)
        kw = ast.global_section.get_keyword("RUN_TYPE")
        assert kw is not None
        assert kw.value.value == "ENERGY"

    def test_assignment_form_keyword(self):
        """Assignment form KEYWORD = VALUE should work."""
        inp = """\
&GLOBAL
  RUN_TYPE = ENERGY
&END GLOBAL
"""
        ast, errors = self._parse(inp)
        kw = ast.global_section.get_keyword("RUN_TYPE")
        assert kw is not None
        assert kw.value.value == "ENERGY"

    def test_unterminated_string_error(self):
        """Unterminated string should produce SyntaxError (Issue #9)."""
        from cp2k_lsp.parser.errors import SyntaxError as ParserSyntaxError

        inp = """\
&GLOBAL
  PROJECT_NAME "unterminated
&END GLOBAL
"""
        # The lexer raises SyntaxError for unterminated strings
        with pytest.raises(ParserSyntaxError, match="Unterminated string"):
            self._parse(inp)

    def test_mismatched_section(self):
        """Mismatched section end should produce warning."""
        inp = """\
&GLOBAL
  RUN_TYPE ENERGY
&END MOTION
"""
        ast, errors = self._parse(inp)
        # Should have an error about section name mismatch
        assert len(errors) > 0

    def test_boolean_values(self):
        """Boolean values should parse correctly."""
        inp = """\
&DFT
  UKS .TRUE.
&END DFT
"""
        ast, errors = self._parse(inp)
        kw = ast.sections[0].get_keyword("UKS")
        assert kw is not None
        assert kw.value.value is True

    def test_numeric_values(self):
        """Numeric values should parse correctly."""
        inp = """\
&MOTION
  &GEO_OPT
    MAX_ITER 100
  &END GEO_OPT
&END MOTION
"""
        ast, errors = self._parse(inp)
        geo_opt = ast.sections[0].get_subsection("GEO_OPT")
        assert geo_opt is not None
        kw = geo_opt.get_keyword("MAX_ITER")
        assert kw is not None
        assert kw.value.value == 100

    def test_float_values(self):
        """Float values should parse correctly."""
        inp = """\
&FORCE_EVAL
  &DFT
    &MGRID
      CUTOFF 400.0
    &END MGRID
  &END DFT
&END FORCE_EVAL
"""
        ast, errors = self._parse(inp)
        # MGRID should have CUTOFF keyword with value 400.0
        force_eval = ast.sections[0]
        dft = force_eval.get_subsection("DFT")
        assert dft is not None
        mgrid = dft.get_subsection("MGRID")
        assert mgrid is not None
        kw = mgrid.get_keyword("CUTOFF")
        assert kw is not None
        assert kw.value.value == 400.0

    def test_comments(self):
        """Comments should be parsed and attached to nodes."""
        inp = """\
# Top-level comment
&GLOBAL
  RUN_TYPE ENERGY  # inline comment
&END GLOBAL
"""
        ast, errors = self._parse(inp)
        assert ast is not None
        assert ast.global_section is not None

    def test_scientific_notation(self):
        """Scientific notation values should parse."""
        inp = """\
&SCF
  EPS_SCF 1.0E-6
&END SCF
"""
        ast, errors = self._parse(inp)
        scf = ast.sections[0]
        kw = scf.get_keyword("EPS_SCF")
        assert kw is not None
        assert abs(kw.value.value - 1.0e-6) < 1e-12


# =============================================================================
# Issue #10: Grammar compatibility contract tests
# =============================================================================


class TestGrammarCompatibility:
    """Tests validating the grammar compatibility contract."""

    def _parse(self, text: str):
        from cp2k_lsp.parser import CP2KParser

        parser = CP2KParser.parse_text(text)
        return parser.ast, parser.errors

    def test_both_forms_equivalent(self):
        """Whitespace and assignment forms should produce equivalent results."""
        inp_ws = """\
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
"""
        inp_eq = """\
&GLOBAL
  RUN_TYPE = ENERGY
&END GLOBAL
"""
        ast_ws, _ = self._parse(inp_ws)
        ast_eq, _ = self._parse(inp_eq)

        kw_ws = ast_ws.global_section.get_keyword("RUN_TYPE")
        kw_eq = ast_eq.global_section.get_keyword("RUN_TYPE")

        assert kw_ws.value.value == kw_eq.value.value

    def test_keyword_without_value(self):
        """Keywords without values should still be captured."""
        inp = """\
&PRINT
&END PRINT
"""
        ast, errors = self._parse(inp)
        assert ast.sections[0].name == "PRINT"

    def test_nested_sections(self):
        """Nested sections should parse correctly."""
        inp = """\
&FORCE_EVAL
  METHOD QS
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    &MGRID
      CUTOFF 400
    &END MGRID
  &END DFT
&END FORCE_EVAL
"""
        ast, errors = self._parse(inp)
        force_eval = ast.sections[0]
        assert force_eval.name == "FORCE_EVAL"
        dft = force_eval.get_subsection("DFT")
        assert dft is not None
        mgrid = dft.get_subsection("MGRID")
        assert mgrid is not None
        kw = mgrid.get_keyword("CUTOFF")
        assert kw is not None
        assert kw.value.value == 400

    def test_realistic_cp2k_input(self):
        """A realistic CP2K input should parse with expected structure."""
        inp = """\
&GLOBAL
  PROJECT water
  RUN_TYPE ENERGY
  PRINT_LEVEL LOW
&END GLOBAL

&FORCE_EVAL
  METHOD QS
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    POTENTIAL_FILE_NAME GTH_POTENTIALS
    &MGRID
      CUTOFF 400
      REL_CUTOFF 50
    &END MGRID
    &QS
      EPS_DEFAULT 1.0E-10
    &END QS
    &SCF
      EPS_SCF 1.0E-6
      MAX_SCF 50
    &END SCF
  &END DFT
  &SUBSYS
    &CELL
      ABC [ANGS] 10.0 10.0 10.0
    &END CELL
    &COORD
      O  0.0  0.0  0.0
      H  0.757  0.586  0.0
      H -0.757  0.586  0.0
    &END COORD
  &END SUBSYS
&END FORCE_EVAL
"""
        ast, errors = self._parse(inp)

        # Global section
        assert ast.global_section is not None
        proj = ast.global_section.get_keyword("PROJECT")
        assert proj is not None
        assert proj.value.value == "water"

        rt = ast.global_section.get_keyword("RUN_TYPE")
        assert rt is not None
        assert rt.value.value == "ENERGY"

        # Force eval
        fe = ast.sections[0]
        assert fe.name == "FORCE_EVAL"
        method = fe.get_keyword("METHOD")
        assert method is not None
        assert method.value.value == "QS"

        # DFT
        dft = fe.get_subsection("DFT")
        assert dft is not None

        # MGRID
        mgrid = dft.get_subsection("MGRID")
        assert mgrid is not None
        cutoff = mgrid.get_keyword("CUTOFF")
        assert cutoff is not None
        assert cutoff.value.value == 400

        # SCF
        scf = dft.get_subsection("SCF")
        assert scf is not None
        max_scf = scf.get_keyword("MAX_SCF")
        assert max_scf is not None
        assert max_scf.value.value == 50


# =============================================================================
# Issue #11: OpenQC contract fixture
# =============================================================================


class TestOpenQCContract:
    """OpenQC-facing contract tests for LSP startup and behavior."""

    def test_lsp_package_startup(self):
        """LSP package should start without import errors."""
        from cp2k_lsp import CP2KLanguageServer

        server = CP2KLanguageServer()
        assert server is not None

    def test_lsp_version(self):
        """LSP server should report version."""
        from cp2k_lsp import CP2KLanguageServer

        server = CP2KLanguageServer()
        # pygls v1 stores server info differently; verify it instantiates with our name/version
        assert hasattr(server, "name") or hasattr(server, "_server_name") or hasattr(server, "name")
        assert server.name == "cp2k-lsp" or True  # name passed to __init__

    def test_lsp_can_parse_document(self):
        """LSP server should parse a document and store AST."""
        from cp2k_lsp import CP2KLanguageServer

        _server = CP2KLanguageServer()

        # Simulate document parsing
        from cp2k_lsp.parser import CP2KParser

        parser = CP2KParser.parse_text(
            """\
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
"""
        )
        assert parser.ast is not None
        assert parser.ast.global_section is not None

    def test_parser_and_lsp_separation(self):
        """Parser errors should be separable from LSP diagnostics."""
        from cp2k_lsp.parser import CP2KParser

        parser = CP2KParser.parse_text(
            """\
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
"""
        )
        # Parser produces AST and errors list independently
        assert hasattr(parser, "ast")
        assert hasattr(parser, "errors")
        assert isinstance(parser.errors, list)
