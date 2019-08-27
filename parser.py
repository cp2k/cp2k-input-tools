#!/usr/bin/env python3
# coding: utf-8

import re
import xml.etree.ElementTree as ET

from tokenizer import CP2KInputTokenizer


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


class NameRepetitionError(ParserError):
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
        key = tokens[0].string
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
        if token.string.startswith("["):
            if not default_unit:
                raise InvalidParameterError(
                    f"unit specified for value in keyword '{key}', but no default unit available"
                )
            current_unit = token.string.strip("[]")
            continue

        value = value_converter(token.string)

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
        section_token = tokens.pop(0)
        section_name = section_token.string[1:].upper()

        if section_name == "END":
            if tokens and tokens[0].string.upper() not in [
                e.text for e in self._nodes[-1].iterfind("./NAME")
            ]:
                raise SectionMismatchError(
                    "could not match open section with name:", tokens[0]
                )

            if len(tokens) > 1:
                raise ParserError("garbage at section end:", tokens[1:])

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

            if section_name not in self._treerefs[-1]:
                self._treerefs[-1][section_name] = {}
                self._treerefs += [self._treerefs[-1][section_name]]

            elif repeats:
                if isinstance(self._treerefs[-1][section_name], list):
                    self._treerefs[-1][section_name] += [{}]
                else:
                    self._treerefs[-1][section_name] = [self._treerefs[-1][section_name], {}]

                self._treerefs += [self._treerefs[-1][section_name][-1]]

            else:
                raise InvalidNameError(
                    f"the section '{section_name}' can not be defined multiple times:",
                    section_token,
                )

            # check whether we got a parameter for the section and validate it
            param = sec.find("./SECTION_PARAMETERS")
            if param:  # validate the section parameter like a kw datatype
                # there is no way we get a second section parameter, assign directly
                self._treerefs[-1]["_"] = parse_tokens(
                    param, tokens, "SECTION_PARAMETERS"
                )["values"]

            break
        else:
            raise RuntimeError(f"invalid section '{section_name}'", section_token)

    def _parse_keyword(self, tokens):
        keyword_token = tokens[0]
        keyword_name = keyword_token.string.upper()
        default_kw = None

        for kw in self._nodes[-1].iterfind("./KEYWORD"):
            try:
                data = parse_tokens(kw, tokens)

                if data["name"] not in self._treerefs[-1]:
                    # even if it is a repeating element, store it as a single value first
                    self._treerefs[-1][data["name"]] = data["values"]

                # if the keyword already exists and is a repeating element
                elif data["repeats"]:
                    if isinstance(self._treerefs[-1][data["name"]], list):
                        # ... and already a list, simply append
                        self._treerefs[-1][data["name"]] += [data["values"]]
                    else:
                        # ... otherwise turn it into a list now
                        self._treerefs[-1][data["name"]] = [self._treerefs[-1][data["name"]], data["values"]]

                else:
                    raise NameRepetitionError(
                        f"the keyword '{keyword_name}' can only be mentioned once",
                        keyword_token,
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
                raise InvalidNameError(
                    f"invalid keyword '{keyword_name}'", keyword_token
                )

            # if there is a default keyword, parse the data with that
            data = parse_tokens(default_kw_node, tokens, "DEFAULT_KEYWORD")

            if "*" not in self._treerefs[-1]:
                self._treerefs[-1]["*"] = data["values"]

            elif data["repeats"]:
                if isinstance(self._treerefs[-1]["*"], list):
                    self._treerefs[-1]["*"] += [data["values"]]
                else:
                    self._treerefs[-1]["*"] = data[self._treerefs[-1]["*"], data["values"]]

            else:
                raise NameRepetitionError(
                    f"the default keyword in section '...' can only be used once",
                    keyword_token,
                )

    def parse(self, fhandle):
        for tokens in self._tokenizer.token_iter(fhandle):
            # filter all comments:
            tokens = [t for t in tokens if t.string[0] not in "!#"]
            if not tokens:
                continue  # go to next if the comment was the only token

            if tokens[0].string.startswith("&"):
                self._parse_section(tokens)
            else:
                self._parse_keyword(tokens)

        return self._tree
