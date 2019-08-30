#!/usr/bin/env python3
# coding: utf-8

import collections

import transitions


class TokenizerError(Exception):
    pass


class UnterminatedStringError(TokenizerError):
    pass


class InvalidTokenCharError(TokenizerError):
    pass


def Context(**kwargs):
    return collections.defaultdict(lambda: None, **kwargs)


Token = collections.namedtuple("Token", ["string", "ctx"])


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
        raise UnterminatedStringError(f"unterminated string detected", Context(colnr=colnr, ref_colnr=self._current_token_start))

    def invalid_token_char(self, content, colnr):
        raise InvalidTokenCharError(f"invalid keyword character found", Context(colnr=colnr, ref_colnr=self._current_token_start))

    def is_not_escaped(self, content, colnr):
        if colnr > 0:
            # possible to do: account for multiple escapes
            return content[colnr - 1] != "\\"

        return True

    @property
    def tokens(self):
        return self._tokens

    def is_matching_quote(self, content, colnr):
        return self._tracking_quote_char == content[colnr]

    def __init__(self):
        super().__init__(
            self,
            initial="lookout",
            states=[
                transitions.State(name="lookout"),
                transitions.State(name="basic_token", on_enter=["begin_basic_token"], on_exit=["end_basic_token"]),
                transitions.State(name="string_token", on_enter=["begin_string_token"], on_exit=["end_string_token"]),
                transitions.State(name="comment", on_enter=["begin_basic_token"], on_exit=["end_basic_token"]),
            ],
            transitions=[
                # start parsing a token:
                {"trigger": "token_char", "source": "lookout", "dest": "basic_token"},
                # ... unless we're already parsing a token or inside a string or comment
                {"trigger": "token_char", "source": ["basic_token", "string_token", "comment"], "dest": None},
                # '/" initiate strings
                {"trigger": "quote_char", "source": "lookout", "dest": "string_token"},
                {
                    "trigger": "quote_char",
                    "source": "string_token",
                    "dest": "lookout",
                    "conditions": ["is_not_escaped", "is_matching_quote"],
                },
                # a '!' initiates a comment (and terminates a token if necessary)
                {"trigger": "comment_char", "source": ["lookout", "basic_token"], "dest": "comment"},
                # ... unless inside a single or double quoted string, where it is consumed:
                {"trigger": "comment_char", "source": "string_token", "dest": None},
                # whitespace terminates a basic token
                {"trigger": "ws_char", "source": "basic_token", "dest": "lookout"},
                # ... and is consumed in all other cases
                {"trigger": "ws_char", "source": ["lookout", "string_token", "comment"], "dest": None},
                # single/double quotes are not allowed in a basic token:
                {"trigger": "quote_char", "source": "basic_token", "before": "invalid_token_char", "dest": None},
                {"trigger": "nl_char", "source": ["basic_token", "comment"], "dest": "lookout"},
                {"trigger": "nl_char", "source": "lookout", "dest": None},
                {"trigger": "nl_char", "source": "string_token", "before": "unterminated_string", "dest": None},
            ],
        )

        self._tracking_quote_char = None
        self._current_token_start = 0
        self._tokens = []


def tokenize(string):
    tokenizer = CP2KInputTokenizer()

    char_map = {
        " ": tokenizer.ws_char,
        "\t": tokenizer.ws_char,
        "!": tokenizer.comment_char,
        "#": tokenizer.comment_char,
        "'": tokenizer.quote_char,
        '"': tokenizer.quote_char,
    }

    for colnr, char in enumerate(string):
        char_map.get(char, tokenizer.token_char)(string, colnr)

    tokenizer.nl_char(string, len(string))

    return tuple(string[s:e] for s, e in tokenizer.tokens)
