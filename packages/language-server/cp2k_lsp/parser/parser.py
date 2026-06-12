"""CP2K Input Parser."""

from typing import List, Optional

from cp2k_lsp.parser.ast import Comment, CP2KInput, Keyword, Section, Value, ValueType
from cp2k_lsp.parser.errors import ParseError, SyntaxError
from cp2k_lsp.parser.lexer import Lexer, Token, TokenType


class CP2KParser:
    """Parser for CP2K input files."""

    def __init__(self, tokens: List[Token], source: Optional[str] = None):
        self.tokens = tokens
        self.source = source
        self.pos = 0
        self.errors: List[ParseError] = []
        self.ast: Optional[CP2KInput] = None

    def current(self) -> Token:
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return self.tokens[-1]

    def peek(self, offset: int = 0) -> Token:
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return self.tokens[-1]

    def advance(self) -> Token:
        token = self.current()
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
        return token

    def expect(self, token_type: TokenType) -> Token:
        token = self.current()
        if token.type != token_type:
            self.errors.append(
                SyntaxError(
                    f"Expected {token_type.name}",
                    token.line,
                    token.column,
                    self.source,
                    expected=token_type.name,
                    found=token.type.name,
                )
            )
        return self.advance()

    def match(self, *types: TokenType) -> bool:
        return self.current().type in types

    def skip_eol_and_comments(self) -> List[Comment]:
        comments = []
        while self.match(TokenType.EOL, TokenType.COMMENT):
            if self.match(TokenType.COMMENT):
                token = self.advance()
                comments.append(Comment(text=token.value, line=token.line, column=token.column))
            else:
                self.advance()
        return comments

    def parse(self) -> CP2KInput:
        """Parse tokens into AST."""
        root = CP2KInput(line=1, column=1)
        root.comments = self.skip_eol_and_comments()

        while not self.match(TokenType.EOF):
            if self.match(TokenType.SECTION_START):
                section = self.parse_section()
                if section.name.upper() == "GLOBAL":
                    root.global_section = section
                else:
                    root.sections.append(section)
            elif self.match(TokenType.COMMENT):
                token = self.advance()
                root.comments.append(Comment(text=token.value, line=token.line, column=token.column))
            elif self.match(TokenType.EOL):
                self.advance()
            else:
                # Unexpected token - try to recover
                self.errors.append(
                    SyntaxError(
                        f"Unexpected token: {self.current().value}", self.current().line, self.current().column, self.source
                    )
                )
                self.advance()

            root.comments.extend(self.skip_eol_and_comments())

        # Check for unclosed sections
        if self.current().type != TokenType.EOF:
            self.errors.append(
                SyntaxError(
                    "Unexpected end of file - possible unclosed section", self.current().line, self.current().column, self.source
                )
            )

        return root

    def parse_section(self) -> Section:
        """Parse a section."""
        start_token = self.expect(TokenType.SECTION_START)
        section = Section(name=start_token.value, line=start_token.line, column=start_token.column)

        section.comments = self.skip_eol_and_comments()

        while not self.match(TokenType.SECTION_END, TokenType.EOF):
            if self.match(TokenType.SECTION_START):
                # Nested subsection
                subsection = self.parse_section()
                section.subsections.append(subsection)
            elif self.match(TokenType.KEYWORD):
                keyword = self.parse_keyword()
                if keyword:
                    section.keywords.append(keyword)
            elif self.match(TokenType.COMMENT):
                token = self.advance()
                section.comments.append(Comment(text=token.value, line=token.line, column=token.column))
            elif self.match(TokenType.EOL):
                self.advance()
            elif self.match(TokenType.EOF):
                self.errors.append(
                    SyntaxError(f"Unclosed section &{section.name}", start_token.line, start_token.column, self.source)
                )
                break
            else:
                self.errors.append(
                    SyntaxError(
                        f"Unexpected token in section &{section.name}: {self.current().value}",
                        self.current().line,
                        self.current().column,
                        self.source,
                    )
                )
                self.advance()

        # Expect end of section
        if self.match(TokenType.SECTION_END):
            end_token = self.advance()
            end_name = end_token.value[3:] if end_token.value.upper().startswith("END") else ""
            if end_name and end_name.upper() != section.name.upper():
                self.errors.append(
                    SyntaxError(
                        f"Section name mismatch: &{section.name} closed with &{end_token.value}",
                        end_token.line,
                        end_token.column,
                        self.source,
                    )
                )

        return section

    def parse_keyword(self) -> Optional[Keyword]:
        """Parse a keyword assignment.

        Supports both CP2K grammar forms:
        - KEYWORD = VALUE  (explicit assignment)
        - KEYWORD VALUE    (whitespace-separated, CP2K-native)
        """
        name_token = self.expect(TokenType.KEYWORD)

        self.skip_eol_and_comments()

        if not self.match(TokenType.ASSIGN):
            # Check for whitespace-separated value (CP2K grammar)
            # In CP2K, keywords can be followed directly by a value token
            # without an equals sign: e.g., "RUN_TYPE ENERGY"
            if self.match(TokenType.STRING, TokenType.NUMBER, TokenType.BOOLEAN, TokenType.KEYWORD, TokenType.UNIT):
                value = self.parse_value()
                return Keyword(name=name_token.value, value=value, line=name_token.line, column=name_token.column)
            # Keyword without value (boolean flag or section parameter)
            return Keyword(name=name_token.value, line=name_token.line, column=name_token.column)

        self.expect(TokenType.ASSIGN)
        self.skip_eol_and_comments()

        value = self.parse_value()
        return Keyword(name=name_token.value, value=value, line=name_token.line, column=name_token.column)

    def parse_value(self) -> Value:
        """Parse a value."""
        token = self.current()

        if self.match(TokenType.BOOLEAN):
            self.advance()
            return Value(value=token.value.upper() == ".TRUE.", value_type=ValueType.BOOLEAN, line=token.line, column=token.column)
        elif self.match(TokenType.NUMBER):
            self.advance()
            num_str = token.value
            if "." in num_str or "e" in num_str.lower():
                value = float(num_str)
            else:
                value = int(num_str)
            return Value(value=value, value_type=ValueType.NUMBER, line=token.line, column=token.column)
        elif self.match(TokenType.STRING):
            self.advance()
            return Value(value=token.value, value_type=ValueType.STRING, line=token.line, column=token.column)
        elif self.match(TokenType.KEYWORD):
            # Keyword as value (e.g., RUN_TYPE GEO_OPT)
            self.advance()
            return Value(value=token.value, value_type=ValueType.STRING, line=token.line, column=token.column)
        elif self.match(TokenType.UNIT):
            self.advance()
            return Value(value=token.value, value_type=ValueType.UNIT, line=token.line, column=token.column)
        else:
            # Try to read raw value
            self.advance()
            return Value(value=token.value, value_type=ValueType.STRING, line=token.line, column=token.column)

    @classmethod
    def parse_text(cls, text: str, source: Optional[str] = None) -> "CP2KParser":
        """Parse text and return parser instance with AST."""
        lexer = Lexer(text, source)
        tokens = lexer.tokenize()
        parser = cls(tokens, source)
        parser.ast = parser.parse()
        return parser
