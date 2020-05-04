import collections
import re
import pathlib
from fractions import Fraction
from dataclasses import dataclass
from typing import Any
import xml.etree.ElementTree as ET

import pint

from .parser_errors import InvalidParameterError
from .tokenizer import tokenize, COMMENT_CHARS


UREG = pint.UnitRegistry()
UREG.load_definitions(str(pathlib.Path(__file__).resolve().parent.joinpath("pint_units.txt")))


def kw_converter_bool(string):
    string = string.upper()

    if string in ("0", "F", ".F.", "FALSE", ".FALSE.", "N", "NO", "OFF"):
        return False

    if string in ("1", "T", ".T.", "TRUE", ".TRUE.", "Y", "YES", "ON"):
        return True

    raise InvalidParameterError(f"invalid value given for a boolean: '{string}'")


def kw_converter_str(string):
    return string.strip("'\"")


FORTRAN_REAL = re.compile(r"(\d*\.\d+)[dD]([-+]?\d+)")


def kw_converter_float(string):
    """convert a given string to a Python float

    :param string: string with the float, can be in Fortran scientific notation or as fraction
    """
    string = FORTRAN_REAL.sub(r"\1e\2", string)

    if "/" in string:
        return float(Fraction(string))

    return float(string)


def kw_converter_keyword(string, allowed_values):
    string = string.upper()

    if string in allowed_values:
        return string

    raise InvalidParameterError(f"invalid keyword '{string}'")


KW_VALUE_CONVERTERS = {
    "logical": kw_converter_bool,
    "integer": int,
    "real": kw_converter_float,
    "word": kw_converter_str,
    "string": kw_converter_str,
}

KWDataType = collections.namedtuple("KWDataType", ["type", "n_var", "parser"])


def get_datatype(kw_node):
    dt = kw_node.find("./DATA_TYPE")
    kind = dt.get("kind")

    if kind == "keyword":
        # the keywords parser needs the list of valid keywords for verification
        valid_keywords = [e.text for e in dt.iterfind(".//NAME")]
        parser = lambda v: kw_converter_keyword(v, valid_keywords)  # noqa
    else:
        parser = KW_VALUE_CONVERTERS[kind]

    return KWDataType(kind, int(dt.find("./N_VAR").text), parser)


@dataclass
class Keyword:
    name: str
    values: Any
    repeats: bool
    node: ET.Element

    @staticmethod
    def from_string(kw_node, vstring, key_trafo=str):
        datatype = get_datatype(kw_node)

        # for a string datatype, no tokenization shall be done
        if datatype.type == "string":
            if vstring.startswith(("'", '"')):
                # if the content is actually a string, employ the tokenizer do correctly determine the end of it
                tokens = tokenize(vstring)
            else:
                tokens = [vstring.rstrip()]  # strip trailing whitespace
            # in case of no value, this will lead to an empty string, which is intentional
            # since we can't distinguish between an empty string and a lone keyword for a string datatype
            # -> inline comments for strings which are not escaped will be treated as part of the string, reproducing CP2K-behaviour
        else:
            tokens = tokenize(vstring)

        lone_keyword_value = None
        try:
            lone_keyword_value = tokenize(kw_node.find("./LONE_KEYWORD_VALUE").text)
        except AttributeError:
            pass

        if not tokens:
            if not lone_keyword_value:
                raise InvalidParameterError("keyword expects at least one value")

            tokens = lone_keyword_value

        default_unit = None
        try:
            default_unit = kw_node.find("./DEFAULT_UNIT").text
        except AttributeError:
            pass

        if default_unit:
            default_unit = UREG.parse_expression(default_unit)

        current_unit = default_unit

        values = []

        for token in tokens:
            if token.startswith("["):
                if not default_unit:
                    raise InvalidParameterError("unit specified for value in keyword, but no default unit available")
                current_unit = UREG.parse_expression(token.strip("[]"))
                continue

            if token.startswith(COMMENT_CHARS):
                assert token == tokens[-1], "found inline comment which is not the last token"
                continue  # ignore inline comments

            value = datatype.parser(token)

            if datatype.type == "keyword":
                # keywords are also matched case insensitive, apply the same rules as for the keys
                value = key_trafo(value)

            if current_unit != default_unit:
                # interpret the given value in the specified unit, convert it and get the raw value
                value = (value * current_unit).to(default_unit).magnitude

            values += [value]

        if not values:
            raise InvalidParameterError("keyword expects at least one value, only a unit spec was given")

        if (datatype.n_var > 0) and (datatype.n_var != len(values)):
            raise InvalidParameterError(f"keyword expects exactly {datatype.n_var} values, {len(values)} were given")

        # simplify the value if only one is given/requested
        if len(values) == 1:
            values = values[0]

        key_name = kw_node.find("./NAME[@type='default']").text
        if key_name == "DEFAULT_KEYWORD":
            key_name = "*"

        return Keyword(key_name, values, True if kw_node.get("repeats") == "yes" else False, kw_node)
