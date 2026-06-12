"""CP2K Input File Lexer."""

from dataclasses import dataclass
from enum import Enum, auto
from typing import List, Optional


class TokenType(Enum):
    """Token types for CP2K input."""

    SECTION_START = auto()
    SECTION_END = auto()
    KEYWORD = auto()
    VALUE = auto()
    ASSIGN = auto()
    COMMENT = auto()
    EOL = auto()
    EOF = auto()
    STRING = auto()
    NUMBER = auto()
    BOOLEAN = auto()
    UNIT = auto()


@dataclass
class Token:
    """Token representation."""

    type: TokenType
    value: str
    line: int
    column: int
    raw: Optional[str] = None


class Lexer:
    """Lexer for CP2K input files."""

    def __init__(self, text: str, source: Optional[str] = None):
        self.text = text
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []

    def peek(self, offset: int = 0) -> str:
        pos = self.pos + offset
        if pos >= len(self.text):
            return "\0"
        return self.text[pos]

    def advance(self) -> str:
        if self.pos >= len(self.text):
            return "\0"
        char = self.text[self.pos]
        self.pos += 1
        if char == "\n":
            self.line += 1
            self.column = 1
        else:
            self.column += 1
        return char

    def skip_whitespace(self) -> None:
        while self.peek() in " \t\r":
            self.advance()

    def read_string(self) -> Token:
        start_line = self.line
        start_col = self.column
        quote = self.peek()
        self.advance()
        value = ""
        while self.peek() not in (quote, "\0", "\n"):
            value += self.advance()
        if self.peek() != quote:
            from cp2k_lsp.parser.errors import SyntaxError

            raise SyntaxError("Unterminated string", start_line, start_col, self.source)
        self.advance()
        return Token(TokenType.STRING, value, start_line, start_col)

    def read_number(self) -> Token:
        start_line = self.line
        start_col = self.column
        value = ""
        if self.peek() == "-":
            value += self.advance()
        while self.peek().isdigit():
            value += self.advance()
        if self.peek() == ".":
            value += self.advance()
            while self.peek().isdigit():
                value += self.advance()
        if self.peek() in "eE":
            value += self.advance()
            if self.peek() in "+-":
                value += self.advance()
            while self.peek().isdigit():
                value += self.advance()
        return Token(TokenType.NUMBER, value, start_line, start_col)

    def read_keyword(self) -> Token:
        start_line = self.line
        start_col = self.column
        value = ""
        while self.peek().isalnum() or self.peek() in "_-":
            value += self.advance()
        return Token(TokenType.KEYWORD, value, start_line, start_col)

    def read_comment(self) -> Token:
        start_line = self.line
        start_col = self.column
        value = ""
        while self.peek() not in "\n\0":
            value += self.advance()
        return Token(TokenType.COMMENT, value, start_line, start_col)

    def tokenize(self) -> List[Token]:
        while self.pos < len(self.text):
            self.skip_whitespace()
            char = self.peek()

            if char == "\0":
                break
            elif char == "\n":
                self.tokens.append(Token(TokenType.EOL, "\n", self.line, self.column))
                self.advance()
            elif char in ('"', "'"):
                self.tokens.append(self.read_string())
            elif char.isdigit() or (char == "-" and self.peek(1).isdigit()):
                self.tokens.append(self.read_number())
            elif char == "=":
                self.tokens.append(Token(TokenType.ASSIGN, "=", self.line, self.column))
                self.advance()
            elif char == "&":
                start_line = self.line
                start_col = self.column
                self.advance()
                if self.text[self.pos : self.pos + 3].upper() == "END":
                    name = ""
                    while self.peek().isalnum() or self.peek() == "_":
                        name += self.advance()
                    self.tokens.append(Token(TokenType.SECTION_END, name, start_line, start_col))
                else:
                    name = ""
                    while self.peek().isalnum() or self.peek() == "_":
                        name += self.advance()
                    self.tokens.append(Token(TokenType.SECTION_START, name, start_line, start_col))
            elif char == ".":
                start_line = self.line
                start_col = self.column
                # Simple boolean parsing
                if self.text[self.pos : self.pos + 6].upper() == ".TRUE.":
                    for _ in range(6):
                        self.advance()
                    self.tokens.append(Token(TokenType.BOOLEAN, ".TRUE.", start_line, start_col))
                elif self.text[self.pos : self.pos + 7].upper() == ".FALSE.":
                    for _ in range(7):
                        self.advance()
                    self.tokens.append(Token(TokenType.BOOLEAN, ".FALSE.", start_line, start_col))
                else:
                    self.advance()
            elif char in "!#":
                self.tokens.append(self.read_comment())
            elif char.isalpha() or char == "_":
                self.tokens.append(self.read_keyword())
            else:
                self.advance()

        self.tokens.append(Token(TokenType.EOF, "", self.line, self.column))
        return self.tokens
