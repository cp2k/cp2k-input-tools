#!/usr/bin/env python3
# coding: utf-8

import re
import xml.etree.ElementTree as ET

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
        while True:
            # TODO: integrate this in the state machine (probably as a sub-HSM), because
            #       with regexes we don't get nested bracketing right
            match = re.search(r"\$(?P<var>\w+\b|{.+})", line)
            if not match:
                break

            var = match.group("var")
            if var[0] == "{":
                # resolve nested variables recursively, the result will be a variable name
                varname = self._resolve_variables(var.strip("{}")).upper()
            else:
                varname = var.upper()

            line = re.sub(f"\${var}", self._varstack[varname], line)

        return line

    def _parse_preprocessor(self, line):
        # TODO: regex do not cover all corner cases (like trailing quotes)

        conditional_match =  re.match(r"\s*@(?P<stmt>IF|ENDIF)\s*(?P<cond>.*)", line, flags=re.IGNORECASE)
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

        set_match = re.match(r"\s*@SET\s+(?P<var>\w+)\s+(?P<value>.+)", line, flags=re.IGNORECASE)
        if set_match:
            self._varstack[set_match.group("var").upper()] = set_match.group("value")
            return

        include_match =  re.match(r"\s*@INCLUDE\s+(?P<file>('[^']+')|(\"[^']+\")|[^'\"].*)", line, flags=re.IGNORECASE)
        if include_match:
            with open(include_match.group("file").strip("'\""), "r") as included_handle:
                included_token_iter = CP2KInputTokenizer(self._varstack)  # here we start sharing the variable stack
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

            # TODO: here we are losing the original line which is bad for error reporting
            # TODO: this is potentially a waste of resources since we might be in a conditional section
            #       but the other preprocessing part comes later and depends on variables being resolved
            line = self._resolve_variables(line)

            if re.match(r"\s*@", line):
                # the preprocessor can yield it's own tokens (in case of an include)
                # but also change our state (_varstack, _conditional_block)
                yield from self._parse_preprocessor(line)
                # ... in any way, this line will not be seen by the rest of the state machine
                continue

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


def parse_dt_kw(element):
    return {
        "type": "keyword",
        "n_var": int(element.find("./N_VAR").text),
        "items": [e.text for e in element.iterfind(".//NAME")],
    }


TYPE_PARSERS = {
    "integer": lambda e: {"type": "integer", "n_var": int(e.find("./N_VAR").text)},
    "keyword": parse_dt_kw,
    "logical": lambda e: {"type": "logical", "n_var": int(e.find("./N_VAR").text)},
    "real": lambda e: {"type": "real", "n_var": int(e.find("./N_VAR").text)},
    "string": lambda e: {"type": "string", "n_var": int(e.find("./N_VAR").text)},
    "word": lambda e: {"type": "word", "n_var": int(e.find("./N_VAR").text)},
}


class ParserError(Exception):
    pass


class InvalidNameError(ParserError):
    pass


class SectionMismatchError(ParserError):
    pass


class InvalidParameterError(ParserError):
    pass


def bool_kw_converter(string):
    string = string.upper()

    if string in ("0", "F", ".F.", "FALSE", ".FALSE.", "N", "NO", "OFF"):
        return False

    if string in ("1", "T", ".T.", "TRUE", ".TRUE.", "Y", "YES", "ON"):
        return True

    raise InvalidParameterError(f"invalid value given for a boolean: '{string}'")


def string_kw_converter(string):
    return string.strip("'\"")


FORTRAN_REAL = re.compile(r"(\d*\.\d+)[dD]([-+]?\d+)")


def real_kw_converter(string):
    return float(FORTRAN_REAL.sub(r"\1e\2", string))


KW_VALUE_CONVERTERS = {
    "keyword": string_kw_converter,
    "logical": bool_kw_converter,
    "integer": int,
    "real": real_kw_converter,
    "word": string_kw_converter,
    "string": string_kw_converter,
}


def parse_tokens(element, tokens, key=None):
    if not key:
        # do not use tokens.pop(0) here since it is a reference and it might still be needed
        key = tokens[0]
        tokens = tokens[1:]

    if key not in [e.text for e in element.iterfind("./NAME")]:
        raise InvalidNameError(f"invalid key name '{key}'")

    dt = element.find("./DATA_TYPE")
    dt_kind = dt.get("kind")

    type_info = TYPE_PARSERS[dt_kind](dt)
    kw_type = type_info["type"]
    value_converter = KW_VALUE_CONVERTERS[kw_type]

    default_value = None
    try:
        default_value = element.find("./DEFAULT_VALUE").text
        default_value = value_converter(default_value)
    except AttributeError:
        pass

    lone_keyword_value = None
    try:
        lone_keyword_value = element.find("./LONE_KEYWORD_VALUE").text
        lone_keyword_value = value_converter(lone_keyword_value)
    except AttributeError:
        pass

    if not tokens:
        if not lone_keyword_value:
            raise InvalidParameterError(
                f"the keyword '{key}' expects at least one value"
            )

        tokens = [lone_keyword_value]

    default_unit = None
    try:
        default_unit = element.find("./DEFAULT_UNIT").text
    except AttributeError:
        pass
    current_unit = default_unit

    values = []

    for token in tokens:
        if token.startswith("["):
            if not default_unit:
                raise InvalidParameterError(
                    f"unit specified for value in keyword '{key}', but no default unit available"
                )
            current_unit = token.strip("[]")
            continue

        value = value_converter(token)

        assert (
            current_unit == default_unit
        ), f"unit conversion not (yet) implemented (keyword: '{key}')"

        values += [value]

    if not values:
        raise InvalidParameterError(
            f"the keyword '{key}' expects at least one value, only a unit spec was given"
        )

    if (type_info["n_var"] > 0) and (type_info["n_var"] != len(values)):
        raise InvalidParameterError(
            f"the keyword '{key}' expects exactly {type_info['n_var']} values, {len(values)} were given"
        )

    # simplify the value if only one is given/requested
    if len(values) == 1:
        values = values[0]

    return {
        "name": element.find("./NAME[@type='default']").text,
        "aliases": [e.text for e in element.findall("./NAME[@type='alias']")],
        "repeats": True if element.get("repeats") == "yes" else False,
        "default_value": default_value,
        "lone_keyword_value": lone_keyword_value,
        "type": type_info,
        "values": values,
    }


class CP2KInputParser:
    def __init__(self, xmlspec):

        self._tokenizer = CP2KInputTokenizer()
        self._parse_tree = ET.parse(xmlspec)
        self._nodes = [self._parse_tree.getroot()]
        self._tree = {}
        self._treerefs = [self._tree]

    def _parse_section(self, tokens):
        section_name = tokens.pop(0)[1:].upper()

        if section_name == "END":
            if tokens and tokens[0].upper() not in [
                e.text for e in self._nodes[-1].iterfind("./NAME")
            ]:
                raise SectionMismatchError()

            if len(tokens) > 1:
                raise ParserError("garbage at section end")

            self._nodes.pop()
            self._treerefs.pop()
            return

        # check all section nodes for matching names or aliases
        for sec in self._nodes[-1].iterfind("./SECTION"):
            if section_name not in [e.text for e in sec.iterfind("./NAME")]:
                continue

            self._nodes += [
                sec
            ]  # add the current XML section node to the stack of nodes

            repeats = True if sec.get("repeats") == "yes" else False

            if repeats:
                if section_name not in self._treerefs[-1]:
                    self._treerefs[-1][section_name] = []

                self._treerefs[-1][section_name] += [{}]
                self._treerefs += [self._treerefs[-1][section_name][-1]]
            else:
                if section_name in self._treerefs[-1]:
                    raise InvalidNameError(
                        f"the section '{section_name}' can only be mentioned once"
                    )

                self._treerefs[-1][section_name] = {}
                self._treerefs += [self._treerefs[-1][section_name]]

            # check whether we got a parameter for the section and validate it
            param = sec.find("./SECTION_PARAMETERS")
            if param:  # validate the section parameter like a kw datatype
                # there is no way we get a second section parameter, assign directly
                self._treerefs[-1]["_"] = parse_tokens(
                    param, tokens, "SECTION_PARAMETERS"
                )["values"]

            break
        else:
            raise RuntimeError(f"invalid section '{section_name}'")

    def _parse_keyword(self, tokens):
        token_name = tokens[0].upper()
        default_kw = None

        for kw in self._nodes[-1].iterfind("./KEYWORD"):
            try:
                data = parse_tokens(kw, tokens)

                if data["repeats"]:
                    if data["name"] not in self._treerefs[-1]:
                        self._treerefs[-1][data["name"]] = []

                    self._treerefs[-1][data["name"]] += [data["values"]]
                else:
                    if data["name"] in self._treerefs[-1]:
                        raise InvalidNameError(
                            f"the keyword '{token_name}' can only be mentioned once"
                        )

                    self._treerefs[-1][data["name"]] = data["values"]

            except InvalidNameError:
                # but let's see whether we found the DEFAULT_KEYWORD (could be used later)
                if kw.find("./NAME[@type='default']").text == "DEFAULT_KEYWORD":
                    default_kw = kw

                continue

            break

        else:
            # no match so far, and if we didn't find a default keyword, then that's it
            if not default_kw:
                raise RuntimeError(f"invalid keyword '{token_name}'")

            # if there is a default keyword, parse the data with that
            data = parse_tokens(default_kw_node, tokens, "DEFAULT_KEYWORD")

            if data["repeats"]:
                if "*" not in self._treerefs[-1]:
                    self._treerefs[-1]["*"] = []
                self._treerefs[-1]["*"] += [data["values"]]
            else:
                if "*" in self._treerefs[-1]:
                    raise InvalidNameError(
                        f"the default keyword in section '...' can only be used once"
                    )
                self._treerefs[-1]["*"] = data["values"]


    def parse(self, fhandle):
        for tokens in self._tokenizer.token_iter(fhandle):
            # filter all comments:
            tokens = [t for t in tokens if t[0] not in "!#"]
            if not tokens:
                continue  # go to next if the comment was the only token

            if tokens[0].startswith("&"):
                self._parse_section(tokens)
            else:
                self._parse_keyword(tokens)

        return self._tree


parser = CP2KInputParser("cp2k_input.xml")
# parser.parse("    \t FOO \t  ' \\' \"  ! baz'  BAR   .13  ! comment  y\n abc")
with open("test02.inp", "r") as fhandle:
    tree = parser.parse(fhandle)
    import json

    print(json.dumps(tree, indent=2))
