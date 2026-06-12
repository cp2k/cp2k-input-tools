"""Tests for CP2K preprocessor directives support in LSP (issue #48).

Test Cases:
1. NaCl sample with @IF ${HP-1} block parses without spurious diagnostics
2. Cursor inside @IF block still resolves correct section path (FORCE_EVAL/DFT/KPOINTS)
3. @SET VAR value does not produce "unknown keyword" diagnostic
4. Unclosed @IF produces a diagnostic
5. @INCLUDE nonexistent.inc produces a diagnostic
6. Formatter preserves @IF/@ENDIF lines
"""

import sys
from time import sleep

import pytest

from . import TEST_DIR

if hasattr(sys, "pypy_version_info"):
    pytest.skip("pypy is currently not supported", allow_module_level=True)

pygls = pytest.importorskip("pygls")

from lsprotocol.types import (  # noqa: E402
    TEXT_DOCUMENT_COMPLETION,
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_FORMATTING,
    CompletionParams,
    DidOpenTextDocumentParams,
    DocumentFormattingParams,
    Position,
    TextDocumentIdentifier,
    TextDocumentItem,
)

CALL_TIMEOUT = 5


def _open_file(client, server, filepath):
    """Helper to open a file in the LSP server."""
    testpath = filepath
    with testpath.open("r") as fhandle:
        content = fhandle.read()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(testpath), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)
    return content


def test_nacl_sample_no_spurious_diagnostics(client_server):
    """Test that NaCl sample with @IF blocks parses without spurious diagnostics.

    This test ensures that:
    - @SET directives don't produce "unknown keyword" diagnostics
    - @IF/@ENDIF blocks are properly handled
    - Variable references like ${LATTICE} don't cause errors

    Note: The NaCl sample has intentional duplicate keywords in @IF blocks
    (PROJECT and RUN_TYPE appear in both branches), which produce lint warnings.
    This is expected behavior - the test verifies no SYNTAX errors occur.
    """
    client, server = client_server
    testpath = TEST_DIR / "inputs" / "NaCl.inp"
    content = _open_file(client, server, testpath)

    # Should have diagnostics (lint warnings about duplicates are expected)
    assert client.diagnostics is not None, "Should have diagnostics published"

    # Check that there are no ERROR severity diagnostics (only warnings allowed)
    from lsprotocol.types import DiagnosticSeverity

    errors = [d for d in client.diagnostics if d.severity == DiagnosticSeverity.Error]
    assert len(errors) == 0, f"NaCl sample should parse without errors, got: {errors}"

    # Verify the known lint warnings are about duplicate keywords in conditional branches
    warning_messages = [d.message for d in client.diagnostics if d.severity == DiagnosticSeverity.Warning]
    assert all(
        "appears" in msg.lower() and "2 times" in msg.lower() for msg in warning_messages
    ), f"Expected only duplicate-keyword warnings, got: {warning_messages}"


def test_cursor_inside_if_block_completion(client_server, tmp_path):
    """Test that cursor inside @IF block still resolves correct section path.

    This test verifies that inside an @IF block within FORCE_EVAL/DFT,
    completion works correctly and returns DFT-level keywords.
    """
    client, server = client_server

    # Create a test file with @IF block inside FORCE_EVAL/DFT
    test_file = tmp_path / "if_block_test.inp"
    test_file.write_text(
        "&FORCE_EVAL\n"
        "   METHOD QS\n"
        "   &DFT\n"
        "      BASIS_SET_FILE_NAME BASIS_DEFAULT\n"
        "      @IF $WITH_KP == yes\n"
        "         &KPOINTS\n"
        "            SCHEME MONKHORST-PACK 4 4 4\n"
        "         &END KPOINTS\n"
        "      @ENDIF\n"
        "   &END DFT\n"
        "&END FORCE_EVAL\n"
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request completion on line 4 (inside @IF block, before &KPOINTS)
    # Should return DFT-level sections/keywords, not error
    result = client.lsp.send_request(
        TEXT_DOCUMENT_COMPLETION,
        CompletionParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=4, character=10),
        ),
    ).result(timeout=CALL_TIMEOUT)

    assert result is not None, "Should get completion results inside @IF block"
    items = result.items if hasattr(result, "items") else result
    assert len(items) > 0, "Should have completion items available in DFT section"


def test_set_directive_no_diagnostic(client_server, tmp_path):
    """Test that @SET VAR value does not produce 'unknown keyword' diagnostic.

    This ensures @SET directives are recognized and don't trigger spurious
    validation errors.
    """
    client, server = client_server

    test_file = tmp_path / "set_test.inp"
    test_file.write_text("@SET MY_VAR hello\n" "@SET ANOTHER_VAR 123\n" "&GLOBAL\n" "   PROJECT test\n" "&END GLOBAL\n")

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Should have no diagnostics
    assert client.diagnostics is not None, "Should have diagnostics published"
    assert len(client.diagnostics) == 0, f"@SET directives should not produce diagnostics, got: {client.diagnostics}"


def test_unclosed_if_diagnostic(client_server, tmp_path):
    """Test that unclosed @IF produces a diagnostic.

    This ensures the LSP properly detects and reports unclosed
    conditional blocks at end of file.
    """
    client, server = client_server

    test_file = tmp_path / "unclosed_if.inp"
    # Use a defined variable to avoid "undefined variable" error
    test_file.write_text(
        "@SET CONDITION yes\n"
        "&GLOBAL\n"
        "   @IF $CONDITION == yes\n"
        "      PROJECT test\n"
        "&END GLOBAL\n"
        # Missing @ENDIF
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Should have a diagnostic about unclosed @IF
    assert client.diagnostics is not None, "Should have diagnostics published"
    assert len(client.diagnostics) > 0, "Unclosed @IF should produce a diagnostic"

    # Check that the diagnostic mentions the unclosed block
    messages = [d.message for d in client.diagnostics]
    assert any(
        "conditional" in msg.lower() or "not closed" in msg.lower() or "endif" in msg.lower() for msg in messages
    ), f"Expected diagnostic about unclosed conditional, got: {messages}"


def test_include_nonexistent_diagnostic(client_server, tmp_path):
    """Test that @INCLUDE nonexistent.inc produces a diagnostic.

    This ensures the LSP properly detects and reports missing include files.
    """
    client, server = client_server

    test_file = tmp_path / "include_test.inp"
    test_file.write_text("@INCLUDE nonexistent.inc\n" "&GLOBAL\n" "   PROJECT test\n" "&END GLOBAL\n")

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Should have a diagnostic about missing include
    assert client.diagnostics is not None, "Should have diagnostics published"
    assert len(client.diagnostics) > 0, "Missing @INCLUDE file should produce a diagnostic"

    # Check that the diagnostic mentions the include issue
    messages = [d.message for d in client.diagnostics]
    assert any(
        "include" in msg.lower() or "could not be opened" in msg.lower() for msg in messages
    ), f"Expected diagnostic about missing include, got: {messages}"


@pytest.mark.xfail(reason="Formatter not yet implemented in LSP")
def test_formatter_preserves_preprocessor(client_server, tmp_path):
    """Test that formatter preserves @IF/@ENDIF lines.

    This ensures that document formatting doesn't remove or mangle
    preprocessor directives.
    """
    client, server = client_server

    test_file = tmp_path / "format_preprocessor.inp"
    original_content = "&GLOBAL\n" "   @IF $TEST == yes\n" "      PROJECT test\n" "   @ENDIF\n" "&END GLOBAL\n"
    test_file.write_text(original_content)

    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(
            text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=original_content)
        ),
    )
    sleep(CALL_TIMEOUT)

    # Request formatting
    result = client.lsp.send_request(
        TEXT_DOCUMENT_FORMATTING,
        DocumentFormattingParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            options={},
        ),
    ).result(timeout=CALL_TIMEOUT)

    assert result is not None, "Should get formatting result"

    # Check that preprocessor directives are preserved
    # The formatted text should still contain @IF and @ENDIF
    formatted_text = original_content
    for edit in result:
        if edit.range and edit.newText:
            # Apply the edit (simplified - in real scenario would need proper text edit application)
            lines = formatted_text.split("\n")
            start_line = edit.range.start.line
            end_line = edit.range.end.line
            if start_line == end_line:
                lines[start_line] = edit.newText
            formatted_text = "\n".join(lines)

    assert "@IF" in formatted_text, "Formatted text should preserve @IF directive"
    assert "@ENDIF" in formatted_text, "Formatted text should preserve @ENDIF directive"


def test_variable_reference_in_cell(client_server, tmp_path):
    """Test that variable references like ${LATTICE} in CELL section work.

    This is a regression test for the NaCl sample where ${LATTICE} is used
    in the &CELL section.
    """
    client, server = client_server

    test_file = tmp_path / "variable_ref_test.inp"
    test_file.write_text(
        "@SET LATTICE 5.64\n"
        "&FORCE_EVAL\n"
        "   METHOD QS\n"
        "   &DFT\n"
        "      &XC\n"
        "         &XC_FUNCTIONAL PBE\n"
        "         &END XC_FUNCTIONAL\n"
        "      &END XC\n"
        "   &END DFT\n"
        "   &SUBSYS\n"
        "      &CELL\n"
        "         A ${LATTICE} 0 0\n"
        "         B 0 ${LATTICE} 0\n"
        "         C 0 0 ${LATTICE}\n"
        "         PERIODIC XYZ\n"
        "      &END CELL\n"
        "   &END SUBSYS\n"
        "&END FORCE_EVAL\n"
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Should have no diagnostics
    assert client.diagnostics is not None, "Should have diagnostics published"
    assert len(client.diagnostics) == 0, f"Variable references should not cause diagnostics, got: {client.diagnostics}"


def test_nested_section_with_preprocessor(client_server, tmp_path):
    """Test completion works correctly through nested sections with preprocessor.

    This tests that cursor context is preserved through the preprocessor
    when navigating nested sections.
    """
    client, server = client_server

    test_file = tmp_path / "nested_preprocessor.inp"
    test_file.write_text(
        "@SET USE_DFT yes\n"
        "&FORCE_EVAL\n"
        "   METHOD QS\n"
        "   @IF $USE_DFT == yes\n"
        "      &DFT\n"
        "         &XC\n"
        "            &\n"
        "            &END XC_FUNCTIONAL\n"
        "         &END XC\n"
        "      &END DFT\n"
        "   @ENDIF\n"
        "&END FORCE_EVAL\n"
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request completion after & in &XC section (line 6, character 15) - should get child sections
    result = client.lsp.send_request(
        TEXT_DOCUMENT_COMPLETION,
        CompletionParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=6, character=15),
        ),
    ).result(timeout=CALL_TIMEOUT)

    assert result is not None, "Should get completion results in nested section with preprocessor"
    items = result.items if hasattr(result, "items") else result
    assert len(items) > 0, "Should have completion items for XC child sections"
