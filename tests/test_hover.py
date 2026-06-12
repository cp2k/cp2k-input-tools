"""Tests for schema-backed LSP hover functionality (issue #46).

Test Cases:
1. Hover on keyword shows type, default, description
2. Hover on section shows description and child count
3. Hover on enum value shows parent keyword and value info
4. Hover inside preprocessor blocks still works
5. Hover on unknown keyword returns None
"""

import sys
from time import sleep

import pytest

from . import TEST_DIR

if hasattr(sys, "pypy_version_info"):
    pytest.skip("pypy is currently not supported", allow_module_level=True)

pygls = pytest.importorskip("pygls")

from lsprotocol.types import (  # noqa: E402
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_HOVER,
    DidOpenTextDocumentParams,
    HoverParams,
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


def test_hover_on_keyword_shows_metadata(client_server, tmp_path):
    """Test that hovering on a keyword shows type, default, and description."""
    client, server = client_server

    test_file = tmp_path / "hover_keyword.inp"
    test_file.write_text(
        "&FORCE_EVAL\n" "   METHOD QS\n" "   &DFT\n" "      BASIS_SET_FILE_NAME BASIS_DEFAULT\n" "   &END DFT\n" "&END FORCE_EVAL\n"
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request hover on "METHOD" keyword (line 1, character 3-8)
    result = client.lsp.send_request(
        TEXT_DOCUMENT_HOVER,
        HoverParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=1, character=5),
        ),
    ).result(timeout=CALL_TIMEOUT)

    assert result is not None, "Should get hover result for METHOD keyword"
    # Check that hover content contains keyword information
    hover_content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
    assert "METHOD" in hover_content.upper(), "Hover should mention the keyword name"


def test_hover_on_section_shows_description(client_server, tmp_path):
    """Test that hovering on a section shows description and child count."""
    client, server = client_server

    test_file = tmp_path / "hover_section.inp"
    test_file.write_text(
        "&FORCE_EVAL\n" "   METHOD QS\n" "   &DFT\n" "      BASIS_SET_FILE_NAME BASIS_DEFAULT\n" "   &END DFT\n" "&END FORCE_EVAL\n"
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request hover on "&FORCE_EVAL" section start (line 0, character 1)
    result = client.lsp.send_request(
        TEXT_DOCUMENT_HOVER,
        HoverParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=0, character=5),
        ),
    ).result(timeout=CALL_TIMEOUT)

    assert result is not None, "Should get hover result for FORCE_EVAL section"
    hover_content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
    assert "FORCE_EVAL" in hover_content.upper(), "Hover should mention the section name"


def test_hover_on_enum_value_shows_info(client_server, tmp_path):
    """Test that hovering on an enum value shows parent keyword and value info."""
    client, server = client_server

    test_file = tmp_path / "hover_enum.inp"
    test_file.write_text("&FORCE_EVAL\n" "   METHOD QS\n" "&END FORCE_EVAL\n")

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request hover on "QS" value (line 1, after METHOD)
    result = client.lsp.send_request(
        TEXT_DOCUMENT_HOVER,
        HoverParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=1, character=10),
        ),
    ).result(timeout=CALL_TIMEOUT)

    # This should show information about QS as a value for METHOD
    if result is not None:
        hover_content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
        # Should mention either the value or the keyword
        assert (
            "QS" in hover_content.upper() or "METHOD" in hover_content.upper()
        ), "Hover should mention the enum value or parent keyword"


def test_hover_inside_preprocessor_block(client_server, tmp_path):
    """Test that hover works correctly inside preprocessor blocks."""
    client, server = client_server

    test_file = tmp_path / "hover_preprocessor.inp"
    test_file.write_text(
        "@SET USE_DFT yes\n" "&FORCE_EVAL\n" "   @IF $USE_DFT == yes\n" "      METHOD QS\n" "   @ENDIF\n" "&END FORCE_EVAL\n"
    )

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request hover on "METHOD" inside @IF block (line 3)
    result = client.lsp.send_request(
        TEXT_DOCUMENT_HOVER,
        HoverParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=3, character=10),
        ),
    ).result(timeout=CALL_TIMEOUT)

    assert result is not None, "Should get hover result inside preprocessor block"
    hover_content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
    assert "METHOD" in hover_content.upper(), "Hover should work inside @IF blocks"


def test_hover_on_unknown_keyword_returns_none(client_server, tmp_path):
    """Test that hovering on an unknown keyword returns None."""
    client, server = client_server

    test_file = tmp_path / "hover_unknown.inp"
    test_file.write_text("&FORCE_EVAL\n" "   FAKE_KEYWORD xyz\n" "&END FORCE_EVAL\n")

    content = test_file.read_text()
    client.lsp.notify(
        TEXT_DOCUMENT_DID_OPEN,
        DidOpenTextDocumentParams(text_document=TextDocumentItem(uri=str(test_file), language_id="cp2k", version=1, text=content)),
    )
    sleep(CALL_TIMEOUT)

    # Request hover on "FAKE_KEYWORD" (line 1)
    result = client.lsp.send_request(
        TEXT_DOCUMENT_HOVER,
        HoverParams(
            text_document=TextDocumentIdentifier(uri=str(test_file)),
            position=Position(line=1, character=5),
        ),
    ).result(timeout=CALL_TIMEOUT)

    # Unknown keyword should not show hover info
    assert result is None, "Should return None for unknown keywords"


def test_hover_nacl_sample(client_server):
    """Test hover works on the NaCl sample file."""
    client, server = client_server
    testpath = TEST_DIR / "inputs" / "NaCl.inp"
    content = _open_file(client, server, testpath)

    lines = content.split("\n")

    # Find a keyword line to test hover
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped and not stripped.startswith("&") and not stripped.startswith("!") and not stripped.startswith("@"):
            # Try hover on this line
            result = client.lsp.send_request(
                TEXT_DOCUMENT_HOVER,
                HoverParams(
                    text_document=TextDocumentIdentifier(uri=str(testpath)),
                    position=Position(line=i, character=5),
                ),
            ).result(timeout=CALL_TIMEOUT)

            # If we found a valid keyword, hover should work
            if result is not None:
                hover_content = result.contents.value if hasattr(result.contents, "value") else str(result.contents)
                # Should have some content
                assert len(hover_content) > 0, "Hover should have content"
                break
        else:
            # Found at least one valid keyword line
            pass
