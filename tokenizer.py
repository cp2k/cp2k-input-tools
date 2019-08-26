#!/usr/bin/env python3
# coding: utf-8

import re

import transitions


class TokenizerError(Exception):
    pass


class UnterminatedStringError(TokenizerError):
    pass


class InvalidTokenCharError(TokenizerError):
    pass


class PreprocessorError(TokenizerError):
    pass


class CP2KInputTokenizer(transitions.Machine):
    def begin_basic_token(self, _, colnr):
        self._current_token_start = colnr

    def end_basic_token(self, _, colnr):
        # the end idx follows the python-default of specifying ranges,
        # since this is triggered on the character after, using idx is correct
        self._tokens += [(self._current_token_start, colnr)]

    def begin_string_token(self, content, colnr):
        self._current_token_start = colnr
        self._tracking_quote_char = content[colnr]

    def end_string_token(self, content, colnr):
        # this is trigger ON the encounter of the string token, while the
        # end of the basic token is determined by the character that follows
        self._tokens += [(self._current_token_start, colnr + 1)]

    def unterminated_string(self, _, colnr):
        raise UnterminatedStringError(
            f"unterminated string detected, starting at {self._current_token_start}"
        )

    def invalid_token_char(self, _, colnr):
        raise InvalidTokenCharError(
            f"invalid character detected at index {colnr} (token started in {self._current_token_start})"
        )

    def is_not_escaped(self, content, colnr):
        if colnr > 0:
            # possible to do: account for multiple escapes
            return content[colnr - 1] != "\\"

        return True

    def is_matching_quote(self, content, colnr):
        return self._tracking_quote_char == content[colnr]

    def __init__(self, varstack=None):
        super().__init__(
            self,
            initial="lookout",
            states=[
                transitions.State(name="lookout"),
                transitions.State(
                    name="basic_token",
                    on_enter=["begin_basic_token"],
                    on_exit=["end_basic_token"],
                ),
                transitions.State(
                    name="string_token",
                    on_enter=["begin_string_token"],
                    on_exit=["end_string_token"],
                ),
                transitions.State(
                    name="comment",
                    on_enter=["begin_basic_token"],
                    on_exit=["end_basic_token"],
                ),
            ],
            transitions=[
                # start parsing a token:
                {"trigger": "token_char", "source": "lookout", "dest": "basic_token"},
                # ... unless we're already parsing a token or inside a string or comment
                {
                    "trigger": "token_char",
                    "source": ["basic_token", "string_token", "comment"],
                    "dest": None,
                },
                # '/" initiate strings
                {"trigger": "quote_char", "source": "lookout", "dest": "string_token"},
                {
                    "trigger": "quote_char",
                    "source": "string_token",
                    "dest": "lookout",
                    "conditions": ["is_not_escaped", "is_matching_quote"],
                },
                # a '!' initiates a comment (and terminates a token if necessary)
                {
                    "trigger": "comment_char",
                    "source": ["lookout", "basic_token"],
                    "dest": "comment",
                },
                # ... unless inside a single or double quoted string, where it is consumed:
                {"trigger": "comment_char", "source": "string_token", "dest": None},
                # whitespace terminates a basic token
                {"trigger": "ws_char", "source": "basic_token", "dest": "lookout"},
                # ... and is consumed in all other cases
                {
                    "trigger": "ws_char",
                    "source": ["lookout", "string_token", "comment"],
                    "dest": None,
                },
                # single/double quotes are not allowed in a basic token:
                {
                    "trigger": "quote_char",
                    "source": "basic_token",
                    "before": "invalid_token_char",
                    "dest": None,
                },
                {
                    "trigger": "nl_char",
                    "source": ["basic_token", "comment"],
                    "dest": "lookout",
                },
                {"trigger": "nl_char", "source": "lookout", "dest": None},
                {
                    "trigger": "nl_char",
                    "source": "string_token",
                    "before": "unterminated_string",
                    "dest": None,
                },
            ],
        )

        self._tracking_quote_char = None
        self._current_token_start = 0
        self._tokens = []
        self._varstack = {}
        # conditional blocks can not be nested and not spawn multiple files
        self._conditional_block = None

        if varstack:
            self._varstack = varstack

    def _resolve_variables(self, line):
        var_start = 0
        var_end = 0

        # the following algorithm is from CP2Ks cp_parser_inpp_methods.F to reproduce its behavior :(

        # first replace all "${...}"  with no nesting, meaning that ${foo${bar}} means foo$bar is the key
        while True:
            var_start = line.find("${", var_end+1)
            if var_start < 0:
                break

            var_end = line.find("}", var_start+2)
            if var_end < 0:
                raise PreprocessorError("unterminated variable found")

            key = line[var_start+2:var_end]  # without ${ and }
            try:
                value = self._varstack[key.upper()]
            except KeyError:
                raise PreprocessorError(f"undefined variable '{key}'") from None

            line = f"{line[:var_start]}{value}{line[var_end+1:]}"

        var_start = 0
        var_end = 0

        while True:
            var_start = line.find("$", var_end+1)
            if var_start < 0:
                break

            # if not found we get a -1, which is the last char in the string
            var_end = line.find(" ", var_start+1)
            if var_end < 0:
                # -1 would be the last entry, but in a range it is without the specified entry
                var_end = len(line)

            key = line[var_start+1:var_end]
            try:
                value = self._varstack[key.upper()]
            except KeyError:
                raise PreprocessorError(f"undefined variable '{key}'") from None

            line = f"{line[:var_start]}{value}{line[var_end+1:]}"

        return line

    def _parse_preprocessor(self, line):
        # TODO: regex do not cover all corner cases (like trailing quotes)

        conditional_match = re.match(
            r"\s*@(?P<stmt>IF|ENDIF)\s*(?P<cond>.*)", line, flags=re.IGNORECASE
        )
        if conditional_match:
            stmt = conditional_match.group("stmt")
            condition = conditional_match.group("cond").strip()

            if stmt.upper() == "ENDIF":
                if self._conditional_block is None:
                    raise PreprocessorError("found @ENDIF without a previous @IF")
                # TODO: do not ignore garbage after the @ENDIF

                self._conditional_block = None
            else:
                if self._conditional_block is not None:
                    raise PreprocessorError("nested @IF detected")

                # resolve any variables inside the condition
                condition = self._resolve_variables(condition)

                # prefix-whitespace are consumed in the regex, suffix with the strip() above
                if not condition or condition == "0":
                    self._conditional_block = False
                elif "==" in condition:
                    lhs, rhs = [s.strip() for s in condition.split("==", maxsplit=1)]
                    self._conditional_block = lhs == rhs
                elif "/=" in condition:
                    lhs, rhs = [s.strip() for s in condition.split("/=", maxsplit=1)]
                    self._conditional_block = lhs != rhs
                else:
                    self._conditional_block = True

            return

        if self._conditional_block == False:
            # ignore all other preprocessor directives if we are in a conditional block
            # and the condition was false
            return

        set_match = re.match(
            r"\s*@SET\s+(?P<var>\w+)\s+(?P<value>.+)", line, flags=re.IGNORECASE
        )
        if set_match:
            # resolve other variables in the definition first
            value = self._resolve_variables(set_match.group("value"))
            self._varstack[set_match.group("var").upper()] = value
            return

        include_match = re.match(
            r"\s*@INCLUDE\s+(?P<file>('[^']+')|(\"[^']+\")|[^'\"].*)",
            line,
            flags=re.IGNORECASE,
        )
        if include_match:
            # strip quotes and resolve variables for the filename
            filename = self._resolve_variables(include_match.group("file").strip("'\""))

            with open(filename, "r") as included_handle:
                included_token_iter = CP2KInputTokenizer(
                    self._varstack
                )  # here we start sharing the variable stack
                # the tokens from here can be yielded directly since the nested tokenizer has the variable stack
                # and with the reference we ensure that we get ours updated as well (intended side-effect)
                yield from included_token_iter.token_iter(included_handle)

            return

        raise PreprocessorError(f"unknown preprocessor directive found: {line}")

    def token_iter(self, fhandle):
        char_map = {
            " ": self.ws_char,
            "\t": self.ws_char,
            "!": self.comment_char,
            "#": self.comment_char,
            "'": self.quote_char,
            '"': self.quote_char,
            "\n": self.nl_char,
        }

        for linenr, line in enumerate(fhandle):
            if re.match(r"\s*@", line):
                # the preprocessor can yield it's own tokens (in case of an include)
                # but also change our state (_varstack, _conditional_block)
                yield from self._parse_preprocessor(line)
                # ... in any way, this line will not be seen by the rest of the state machine
                continue

            # TODO: here we are losing the original line which is bad for error reporting
            line = self._resolve_variables(line)

            # None == True/False always evaluates to False, so the following
            # is never true if we are NOT in a conditional block
            # If we are in a conditional block and the condition was False, ignore this line.
            if self._conditional_block == False:
                continue

            for colnr, char in enumerate(line):
                char_map.get(char, self.token_char)(line, colnr)
                # TODO: variable substitution could introduce new line endings,
                #       which could lump tokens from different lines together

            self.nl_char(line, len(line))
            if self._tokens:
                yield tuple(line[s:e] for s, e in self._tokens)
                self._tokens = []

        if self._conditional_block is not None:
            raise PreprocessorError(f"conditional block not closed at end of file")
