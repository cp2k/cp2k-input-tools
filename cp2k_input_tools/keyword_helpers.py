import collections
import re

from .parser_errors import InvalidParameterError
from .tokenizer import tokenize


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
    return float(FORTRAN_REAL.sub(r"\1e\2", string))


def kw_converter_keyword(string, allowed_values):
    string = string.upper()

    if string in allowed_values:
        return string

    raise InvalidParameterError(f"invalid keyword")


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
        valid_keywords = [e.text for e in kw_node.iterfind(".//NAME")]
        parser = lambda v: kw_converter_keyword(v, valid_keywords)
    else:
        parser = KW_VALUE_CONVERTERS[kind]

    return KWDataType(kind, int(dt.find("./N_VAR").text), parser)


Keyword = collections.namedtuple("Keyword", ["name", "repeats", "values"])


def parse_keyword(kw_node, vstring, key_trafo=str):
    datatype = get_datatype(kw_node)

    # for a string datatype, no tokenization shall be done
    if datatype.type == "string":
        tokens = [vstring.rstrip()]  # strip trailing whitespace
        # in case of no value, this will lead to an empty string, which is intentional
        # since we can't distinguish between an empty string and a lone keyword for a string datatype
    else:
        tokens = tokenize(vstring)

    lone_keyword_value = None
    try:
        lone_keyword_value = tokenize(kw_node.find("./LONE_KEYWORD_VALUE").text)
    except AttributeError:
        pass

    if not tokens:
        if not lone_keyword_value:
            raise InvalidParameterError(
                f"the keyword '{key}' expects at least one value"
            )

        tokens = lone_keyword_value

    default_unit = None
    try:
        default_unit = kw_node.find("./DEFAULT_UNIT").text
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

        value = datatype.parser(token)

        if datatype.type == "keyword":
            # keywords are also matched case insensitive, apply the same rules as for the keys
            value = key_trafo(value)

        assert (
            current_unit == default_unit
        ), f"unit conversion not (yet) implemented (keyword: '{key}')"

        values += [value]

    if not values:
        raise InvalidParameterError(
            f"the keyword '{key}' expects at least one value, only a unit spec was given"
        )

    if (datatype.n_var > 0) and (datatype.n_var != len(values)):
        raise InvalidParameterError(
            f"the keyword '{key}' expects exactly {type_info['n_var']} values, {len(values)} were given"
        )

    # simplify the value if only one is given/requested
    if len(values) == 1:
        values = values[0]

    key_name = kw_node.find("./NAME[@type='default']").text
    if key_name == "DEFAULT_KEYWORD":
        key_name = "*"

    return Keyword(
        key_trafo(key_name),
        True if kw_node.get("repeats") == "yes" else False,
        values,
    )
