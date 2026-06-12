"""Error types for the enhanced CP2K parser."""

from typing import Optional


class ParseError(Exception):
    """Base parse error with location information."""

    def __init__(self, message: str, line: int, column: int, source: Optional[str] = None):
        super().__init__(message)
        self.message = message
        self.line = line
        self.column = column
        self.source = source

    def __str__(self) -> str:
        loc = f"{self.source}:{self.line}:{self.column}" if self.source else f"line {self.line}:{self.column}"
        return f"{loc}: {self.message}"


class SyntaxError(ParseError):
    """Syntax error during lexical or syntactic analysis."""

    def __init__(
        self,
        message: str,
        line: int,
        column: int,
        source: Optional[str] = None,
        expected: Optional[str] = None,
        found: Optional[str] = None,
    ):
        super().__init__(message, line, column, source)
        self.expected = expected
        self.found = found

    def __str__(self) -> str:
        detail = ""
        if self.expected and self.found:
            detail = f" (expected {self.expected}, found {self.found})"
        return f"{super().__str__()}{detail}"


class LexerError(ParseError):
    """Error during lexing."""

    pass


class SemanticError(ParseError):
    """Semantic error (e.g., invalid section combination)."""

    pass
