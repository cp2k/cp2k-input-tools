"""TDD tests for domain language description API (#36, #37, #38)."""

import json

from cp2k_input_tools.domain_language import (
    describe_keyword,
    describe_language,
    describe_section,
    make_example,
    suggest_next,
)

# ============================================================================
# #36 – Domain language description API
# ============================================================================


class TestDescribeLanguage:
    """Tests for describe_language (issue #36)."""

    def test_describe_language_returns_structure(self):
        """describe_language should return the overall language structure."""
        result = describe_language()
        assert result is not None
        assert "top_level_sections" in result
        assert isinstance(result["top_level_sections"], list)

    def test_describe_language_has_global_section(self):
        """GLOBAL should be a top-level section."""
        result = describe_language()
        section_names = [s["name"] for s in result["top_level_sections"]]
        assert "GLOBAL" in section_names
        assert "FORCE_EVAL" in section_names
        assert "MOTION" in section_names

    def test_describe_language_has_general_info(self):
        """Should include general language information."""
        result = describe_language()
        assert "language_name" in result
        assert result["language_name"] == "CP2K"
        assert "version" in result

    def test_describe_language_json_serializable(self):
        """Result should be JSON serializable for agent APIs."""
        result = describe_language()
        json.dumps(result)  # Should not raise


class TestDescribeSection:
    """Tests for describe_section (issue #37)."""

    def test_describe_global_section(self):
        """Should describe GLOBAL section with metadata."""
        result = describe_section("GLOBAL")
        assert result is not None
        assert result["name"] == "GLOBAL"
        assert "description" in result
        assert "keywords" in result
        assert "subsections" in result
        assert isinstance(result["keywords"], list)
        assert isinstance(result["subsections"], list)

    def test_describe_global_keywords(self):
        """Should list keywords available in GLOBAL."""
        result = describe_section("GLOBAL")
        kw_names = [k["name"] for k in result["keywords"]]
        assert "PROJECT_NAME" in kw_names
        assert "RUN_TYPE" in kw_names
        assert "PRINT_LEVEL" in kw_names

    def test_describe_section_properties(self):
        """Should include section properties like required/repeats."""
        result = describe_section("GLOBAL")
        assert result["required"] is True
        assert result["repeats"] is False

    def test_describe_force_eval_section(self):
        """FORCE_EVAL should be repeatable and have subsections."""
        result = describe_section("FORCE_EVAL")
        assert result["repeats"] is True
        sub_names = [s["name"] for s in result["subsections"]]
        assert "DFT" in sub_names
        assert "SUBSYS" in sub_names

    def test_describe_nested_section(self):
        """Should describe nested sections like DFT."""
        result = describe_section("FORCE_EVAL.DFT")
        assert result is not None
        assert result["name"] == "DFT"
        assert "SCF" in [s["name"] for s in result["subsections"]]

    def test_describe_section_unknown(self):
        """Unknown section should return None."""
        assert describe_section("NONEXISTENT") is None

    def test_describe_section_case_insensitive(self):
        """Should be case-insensitive."""
        result = describe_section("global")
        assert result is not None
        assert result["name"] == "GLOBAL"

    def test_describe_section_with_parent_info(self):
        """Should include parent information."""
        result = describe_section("FORCE_EVAL.DFT")
        assert result is not None
        assert "parent" in result
        assert result["parent"] == "FORCE_EVAL"

    def test_describe_section_json_serializable(self):
        """Should be JSON serializable."""
        result = describe_section("DFT")
        json.dumps(result)


class TestDescribeKeyword:
    """Tests for describe_keyword (issue #37)."""

    def test_describe_enum_keyword(self):
        """Should describe enum keyword with values."""
        result = describe_keyword("GLOBAL", "RUN_TYPE")
        assert result is not None
        assert result["name"] == "RUN_TYPE"
        assert result["type"] == "enum"
        assert "enum_values" in result
        assert "ENERGY" in result["enum_values"]
        assert "MD" in result["enum_values"]
        assert result["default"] == "ENERGY_FORCE"  # Actual default from schema

    def test_describe_integer_keyword(self):
        """Should describe integer keyword."""
        result = describe_keyword("FORCE_EVAL.DFT.SCF", "MAX_SCF")
        assert result is not None
        assert result["type"] == "integer"
        assert result["default"] == 50

    def test_describe_real_keyword(self):
        """Should describe real keyword."""
        result = describe_keyword("FORCE_EVAL.DFT.SCF", "EPS_SCF")
        assert result is not None
        assert result["type"] == "real"
        assert result["default"] == 1.0e-5  # Default from schema

    def test_describe_keyword_with_units(self):
        """Should include units information."""
        result = describe_keyword("MOTION.MD", "TIMESTEP")
        assert result is not None
        assert "units" in result
        assert result["units"] is not None

    def test_describe_keyword_at_global_level(self):
        """Should describe global-level keywords."""
        result = describe_keyword("GLOBAL", "PROJECT_NAME")
        assert result is not None
        assert result["type"] == "word"  # CP2K uses "word" type for PROJECT_NAME
        assert result["default"] == "PROJECT"

    def test_describe_keyword_description(self):
        """Should include keyword description."""
        result = describe_keyword("GLOBAL", "RUN_TYPE")
        assert result is not None
        assert "description" in result
        assert len(result["description"]) > 0

    def test_describe_unknown_keyword(self):
        """Unknown keyword should return None."""
        assert describe_keyword("GLOBAL", "NONEXISTENT") is None

    def test_describe_keyword_unknown_section(self):
        """Unknown section path should return None."""
        assert describe_keyword("NONEXISTENT.SECTION", "RUN_TYPE") is None

    def test_describe_keyword_case_insensitive(self):
        """Should be case-insensitive."""
        result = describe_keyword("global", "run_type")
        assert result is not None
        assert result["name"] == "RUN_TYPE"

    def test_describe_keyword_json_serializable(self):
        """Should be JSON serializable."""
        result = describe_keyword("GLOBAL", "RUN_TYPE")
        json.dumps(result)


# ============================================================================
# #38 – Next-token suggestions and examples
# ============================================================================


class TestSuggestNext:
    """Tests for suggest_next (issue #38)."""

    def test_suggest_next_empty_input(self):
        """Empty input should suggest top-level sections."""
        result = suggest_next("", 0, "test.inp")
        assert result is not None
        assert "context" in result
        assert "suggestions" in result
        assert "GLOBAL" in [s["name"] for s in result["suggestions"]]

    def test_suggest_next_after_global_section(self):
        """After GLOBAL section, suggest FORCE_EVAL."""
        text = "&GLOBAL\n  PROJECT_NAME test\n  RUN_TYPE ENERGY\n&END GLOBAL\n"
        result = suggest_next(text, len(text), "test.inp")
        assert result is not None
        assert "FORCE_EVAL" in [s["name"] for s in result["suggestions"]]

    def test_suggest_next_inside_global(self):
        """Inside GLOBAL, suggest keywords."""
        text = "&GLOBAL\n  PROJECT_NAME test\n"
        result = suggest_next(text, len(text), "test.inp")
        assert result is not None
        assert "RUN_TYPE" in [s["name"] for s in result["suggestions"]]

    def test_suggest_next_inside_dft_scf(self):
        """Inside DFT/SCF, suggest SCF keywords."""
        text = "&FORCE_EVAL\n  &DFT\n    &SCF\n"
        result = suggest_next(text, len(text), "test.inp")
        assert result is not None
        assert "EPS_SCF" in [s["name"] for s in result["suggestions"]]
        assert "MAX_SCF" in [s["name"] for s in result["suggestions"]]

    def test_suggest_next_keyword_awaiting_value(self):
        """When keyword expects value, suggest enum values."""
        text = "&GLOBAL\n  RUN_TYPE "
        result = suggest_next(text, len(text), "test.inp")
        assert result is not None
        # Should suggest enum values
        suggestions = result["suggestions"]
        value_suggestions = [s for s in suggestions if s.get("kind") == "value"]
        assert len(value_suggestions) > 0
        assert "ENERGY" in [s["name"] for s in value_suggestions]

    def test_suggest_next_includes_type_info(self):
        """Suggestions should include type information."""
        text = "&FORCE_EVAL\n  &DFT\n    &SCF\n"
        result = suggest_next(text, len(text), "test.inp")
        assert result is not None
        for suggestion in result["suggestions"]:
            assert "kind" in suggestion  # "section" or "keyword" or "value"
            if suggestion["kind"] == "keyword":
                assert "type" in suggestion

    def test_suggest_next_context_description(self):
        """Should describe the current context."""
        text = "&GLOBAL\n  PROJECT_NAME test\n"
        result = suggest_next(text, len(text), "test.inp")
        assert result is not None
        assert "current_section" in result["context"]

    def test_suggest_next_json_serializable(self):
        """Should be JSON serializable."""
        result = suggest_next("&GLOBAL\n  ", 10, "test.inp")
        json.dumps(result)


class TestMakeExample:
    """Tests for make_example (issue #38)."""

    def test_make_example_global(self):
        """Should generate minimal GLOBAL section example."""
        result = make_example("GLOBAL")
        assert result is not None
        assert "input" in result
        assert "&GLOBAL" in result["input"]
        assert "&END" in result["input"]
        # Should include at least one keyword (preferably PROJECT_NAME or RUN_TYPE)
        assert "PROJECT_NAME" in result["input"] or "RUN_TYPE" in result["input"] or "PRINT_LEVEL" in result["input"]

    def test_make_example_force_eval(self):
        """Should generate FORCE_EVAL section example."""
        result = make_example("FORCE_EVAL")
        assert result is not None
        assert "&FORCE_EVAL" in result["input"]
        assert "METHOD" in result["input"] or "&DFT" in result["input"]

    def test_make_example_dft(self):
        """Should generate DFT section example."""
        result = make_example("DFT")
        assert result is not None
        assert "&DFT" in result["input"]
        assert "&END DFT" in result["input"]

    def test_make_example_scf(self):
        """Should generate SCF section example."""
        result = make_example("SCF")
        assert result is not None
        assert "&SCF" in result["input"]
        assert "EPS_SCF" in result["input"] or "MAX_SCF" in result["input"]

    def test_make_example_nested_path(self):
        """Should handle nested section paths."""
        result = make_example("FORCE_EVAL.DFT")
        assert result is not None
        assert "&DFT" in result["input"]
        # Should include some SCF or QS reference

    def test_make_example_unknown_section(self):
        """Unknown section should return None."""
        assert make_example("NONEXISTENT") is None

    def test_make_example_includes_description(self):
        """Should include description of the example."""
        result = make_example("GLOBAL")
        assert result is not None
        assert "description" in result
        assert len(result["description"]) > 0

    def test_make_example_is_parseable(self):
        """Generated example should be parseable."""
        import io

        from cp2k_input_tools.parser import CP2KInputParser

        result = make_example("GLOBAL")
        assert result is not None

        parser = CP2KInputParser()
        # Should not raise an exception
        with io.StringIO(result["input"]) as fhandle:
            tree = parser.parse(fhandle)
        assert isinstance(tree, dict)

    def test_make_example_json_serializable(self):
        """Should be JSON serializable."""
        result = make_example("GLOBAL")
        json.dumps(result)


# ============================================================================
# Integration tests
# ============================================================================


class TestDomainLanguageIntegration:
    """Integration tests for domain language API."""

    def test_complete_workflow(self):
        """Test complete workflow: language -> section -> keyword -> example."""
        # 1. Get language overview
        lang = describe_language()
        assert lang is not None
        assert "FORCE_EVAL" in [s["name"] for s in lang["top_level_sections"]]

        # 2. Get section details
        section = describe_section("FORCE_EVAL")
        assert section is not None
        assert "DFT" in [s["name"] for s in section["subsections"]]

        # 3. Get keyword details in subsection (use a real keyword, not QS which is a section)
        keyword = describe_keyword("FORCE_EVAL.DFT", "CHARGE")
        assert keyword is not None
        assert "type" in keyword

        # 4. Generate example
        example = make_example("FORCE_EVAL")
        assert example is not None
        assert "input" in example

    def test_schema_index_drives_all_queries(self):
        """All functions should rely on schema index, not hardcoded lists."""
        # These should work for any valid section/keyword from the schema
        all_sections = describe_language()["top_level_sections"]
        for section_info in all_sections[:3]:  # Check first 3
            section_name = section_info["name"]
            section = describe_section(section_name)
            assert section is not None
            assert "keywords" in section
            assert "subsections" in section

    def test_api_consistency(self):
        """All functions should handle case insensitivity consistently."""
        # All these should return equivalent results
        r1 = describe_section("GLOBAL")
        r2 = describe_section("global")
        r3 = describe_section("Global")

        assert r1["name"] == r2["name"] == r3["name"] == "GLOBAL"
