import re
import collections
import xml.etree.ElementTree as ET
import pathlib
import itertools

from . import DEFAULT_CP2K_INPUT_XML
from .tokenizer import Context, TokenizerError, COMMENT_CHARS, tokenize
from .lineiterator import MultiFileLineIterator
from .keyword_helpers import parse_keyword
from .parser_errors import (
    InvalidNameError,
    InvalidSectionError,
    NameRepetitionError,
    SectionMismatchError,
    PreprocessorError,
    InvalidParameterError,
)


def _find_node_by_name(parent, tag, name):
    """check all specified nodes for matching names or aliases in the NAME tag"""

    for node in parent.iterfind(f"./{tag}"):
        if name.upper() in [e.text for e in node.iterfind("./NAME")]:
            return node

    return None


_Variable = collections.namedtuple("Variable", ["value", "ctx"])
_ConditionalBlock = collections.namedtuple("ConditionalBlock", ["condition", "ctx"])


_SECTION_MATCH = re.compile(r"&(?P<name>[\w\-_]+)\s*(?P<param>.*)")
_KEYWORD_MATCH = re.compile(r"(?P<name>[\w\-_]+)\s*(?P<value>.*)")

_CONDITIONAL_MATCH = re.compile(r"\s*@(?P<stmt>IF|ENDIF)\s*(?P<cond>.*)", flags=re.IGNORECASE)
_SET_MATCH = re.compile(r"\s*@SET\s+(?P<var>\w+)\s+(?P<value>.+)", flags=re.IGNORECASE)
_INCLUDE_MATCH = re.compile(r"\s*(?P<complete>@INCLUDE\b\s*(?P<file>.*))", flags=re.IGNORECASE)

_VALID_VAR_NAME_MATCH = re.compile(r"[a-z_]\w*", flags=re.IGNORECASE | re.ASCII)


class CP2KInputParser:
    def __init__(self, xmlspec=DEFAULT_CP2K_INPUT_XML, base_dir=".", key_trafo=str.lower):
        """
        The CP2K input parser.

        :param xmlspec: Path to the `cp2k_input.xml` file generated with `cp2k --xml`
        :param base_dir: The base directory to be used for resolving `@include` directives
        :param key_trafo: A function object used for mangling key names, must treat input case-insensitive
        """

        # schema:
        self._parse_tree = ET.parse(xmlspec)
        self._nodes = [self._parse_tree.getroot()]

        # datatree being generated:
        self._tree = {}
        self._treerefs = [self._tree]
        self._key_trafo = key_trafo

        # file handling:
        self._lineiter = MultiFileLineIterator()

        # preprocessor state:
        self._varstack = {}
        self._conditional_block = None
        self._base_inc_dir = pathlib.Path(base_dir)

    def _add_tree_section(self, section_key, repeats):
        # CP2K uses the same names for keywords and sections (in the same section)
        # if the keyword is already present but not as a section (or list of sections),
        # prefix the section name to resolve the ambiguity in the output format
        section_key = f"+{section_key}"

        if section_key not in self._treerefs[-1]:
            # if we encounter this section the first time, simply add it

            if repeats:
                self._treerefs[-1][section_key] = [{}]
                self._treerefs += [self._treerefs[-1][section_key][-1]]
            else:
                self._treerefs[-1][section_key] = {}
                self._treerefs += [self._treerefs[-1][section_key]]

        elif repeats:
            self._treerefs[-1][section_key] += [{}]
            self._treerefs += [self._treerefs[-1][section_key][-1]]

        else:
            # TODO: the user possibly specified an alias, but here we only return the matching key
            raise InvalidNameError(f"the section '{section_key}' can not be defined multiple times")

    def _parse_as_section(self, entry):
        match = _SECTION_MATCH.match(entry.line)

        section_name = match.group("name").upper()
        section_key = self._key_trafo(section_name)
        section_param = match.group("param")

        if section_name == "END":
            section_param = section_param.rstrip()

            if section_param and section_param.upper() not in [e.text for e in self._nodes[-1].iterfind("./NAME")]:
                raise SectionMismatchError("could not match open section with name: {section_param}", Context())

            # if the END param was a match or none was specified, go a level up
            self._nodes.pop()
            self._treerefs.pop()
            return

        # check all section nodes for matching names or aliases
        section_node = _find_node_by_name(self._nodes[-1], "SECTION", section_name)

        if not section_node:
            raise InvalidSectionError(f"invalid section '{section_name}'", Context())

        self._nodes += [section_node]  # add the current XML section node to the stack of nodes
        repeats = True if section_node.get("repeats") == "yes" else False

        self._add_tree_section(section_key, repeats)

        # check whether we got a parameter for the section and validate it
        if section_param and not section_param.startswith(COMMENT_CHARS):
            param_node = section_node.find("./SECTION_PARAMETERS")
            if param_node:  # validate the section parameter like a kw datatype
                # there is no way we get a second section parameter, assign directly
                self._treerefs[-1]["_"] = parse_keyword(param_node, section_param).values
            else:
                raise InvalidParameterError(
                    f"section parameters given for non-parametrized section '{section_name}': {section_param}", Context()
                )

    def _add_tree_keyword(self, kw):
        if kw.name not in self._treerefs[-1]:
            if kw.repeats:
                self._treerefs[-1][kw.name] = [kw.values]
            else:
                self._treerefs[-1][kw.name] = kw.values

        elif kw.repeats:  # if the keyword already exists and is a repeating element
            # ... and is already a list, simply append
            self._treerefs[-1][kw.name] += [kw.values]

        else:
            # TODO: improve error message
            raise NameRepetitionError(f"the keyword '{kw.name}' can only be mentioned once")

    def _parse_as_keyword(self, entry):
        match = _KEYWORD_MATCH.match(entry.line)

        kw_name = match.group("name").upper()
        kw_value = match.group("value")

        kw_node = _find_node_by_name(self._nodes[-1], "KEYWORD", kw_name)

        # if no keyword with the given name has been found, check for a default keyword for this section
        if not kw_node:
            kw_node = _find_node_by_name(self._nodes[-1], "DEFAULT_KEYWORD", "DEFAULT_KEYWORD")
            if kw_node:  # for default keywords, the whole line is the value
                kw_value = entry.line

        if not kw_node:
            raise InvalidNameError("invalid keyword specified and no default keyword for this section")

        try:
            kw = parse_keyword(kw_node, kw_value, self._key_trafo)
        except InvalidParameterError as exc:
            raise InvalidParameterError(f"invalid values for keyword: {match.group('name')}", Context()) from exc

        self._add_tree_keyword(kw)

    def _resolve_variables(self, line):
        var_start = 0
        var_end = 0

        ctx = Context(line=line)

        # the following algorithm is from CP2Ks cp_parser_inpp_methods.F to reproduce its behavior :(

        # first replace all "${...}"  with no nesting, meaning that ${foo${bar}} means foo$bar is the key
        while True:
            var_start = line.find("${")
            if var_start < 0:
                break

            var_end = line.find("}", var_start + 2)
            if var_end < 0:
                ctx["colnr"] = len(line)
                ctx["ref_colnr"] = var_start
                raise PreprocessorError(f"unterminated variable", ctx)

            ctx["colnr"] = var_start
            ctx["ref_colnr"] = var_end

            key = line[var_start + 2 : var_end]  # without ${ and }
            value = None

            try:
                # see whether we got a default value and unpack
                key, value = key.split("-", maxsplit=1)
            except ValueError:
                pass

            if not _VALID_VAR_NAME_MATCH.match(key):
                raise PreprocessorError(f"invalid variable name '{key}'", ctx) from None

            try:
                value = self._varstack[key.upper()].value
            except KeyError:
                if value is None:
                    raise PreprocessorError(f"undefined variable '{key}' (and no default given)", ctx) from None

            line = f"{line[:var_start]}{value}{line[var_end+1:]}"

        var_start = 0
        var_end = 0

        while True:
            var_start = line.find("$")
            if var_start < 0:
                break

            var_end = line.find(" ", var_start + 1)
            if var_end < 0:
                # -1 would be the last entry, but in a range it is without the specified entry
                var_end = len(line.rstrip())

            ctx["colnr"] = var_start
            ctx["ref_colnr"] = var_end - 1

            key = line[var_start + 1 : var_end]

            if not _VALID_VAR_NAME_MATCH.match(key):
                raise PreprocessorError(f"invalid variable name '{key}'", ctx) from None

            try:
                value = self._varstack[key.upper()].value
            except KeyError:
                raise PreprocessorError(f"undefined variable '{key}'", ctx) from None

            line = f"{line[:var_start]}{value}{line[var_end:]}"

        return line

    def _parse_preprocessor_instruction(self, line):
        conditional_match = _CONDITIONAL_MATCH.match(line)

        ctx = Context(line=line)

        if conditional_match:
            stmt = conditional_match.group("stmt")
            condition = conditional_match.group("cond").strip()

            if stmt.upper() == "ENDIF":
                if self._conditional_block is None:
                    raise PreprocessorError("found @ENDIF without a previous @IF", ctx)

                # check for garbage which is not a comment, note: we're stricter than CP2K here
                if condition and not condition.startswith(COMMENT_CHARS):
                    ctx["colnr"] = conditional_match.start("cond")
                    ctx["ref_colnr"] = conditional_match.end("cond")
                    raise PreprocessorError("garbage found after @ENDIF", ctx)

                self._conditional_block = None
            else:
                if self._conditional_block is not None:
                    ctx["ref_line"] = self._conditional_block.ctx["line"]
                    raise PreprocessorError("nested @IF are not allowed", ctx)

                # resolve any variables inside the condition
                try:
                    condition = self._resolve_variables(condition)
                except PreprocessorError as exc:
                    exc.args[1]["colnr"] += conditional_match.start("cond")
                    exc.args[1]["ref_colnr"] += conditional_match.start("cond")
                    raise

                # prefix-whitespace are consumed in the regex, suffix with the strip() above
                if not condition or condition == "0":
                    self._conditional_block = _ConditionalBlock(False, ctx)
                elif "==" in condition:
                    lhs, rhs = [s.strip() for s in condition.split("==", maxsplit=1)]
                    self._conditional_block = _ConditionalBlock(lhs == rhs, ctx)
                elif "/=" in condition:
                    lhs, rhs = [s.strip() for s in condition.split("/=", maxsplit=1)]
                    self._conditional_block = _ConditionalBlock(lhs != rhs, ctx)
                else:
                    self._conditional_block = _ConditionalBlock(True, ctx)

            return

        if self._conditional_block and not self._conditional_block.condition:
            return

        set_match = _SET_MATCH.match(line)
        if set_match:
            # resolve other variables in the definition first
            key = set_match.group("var")
            value = self._resolve_variables(set_match.group("value"))

            if not _VALID_VAR_NAME_MATCH.match(key):
                raise PreprocessorError(f"invalid variable name '{key}'", ctx) from None

            self._varstack[key.upper()] = _Variable(value, ctx)
            return

        include_match = _INCLUDE_MATCH.match(line)
        if include_match:
            # resolve variables first
            try:
                filename = self._resolve_variables(include_match.group("file"))
            except PreprocessorError as exc:
                exc.args[1]["colnr"] += include_match.start("file")  # shift colnr
                exc.args[1]["ref_colnr"] += include_match.start("file")
                raise

            if filename.startswith(("'", '"')):
                try:
                    tokens = tokenize(filename)  # use the tokenizer to detect unterminated quotes
                except TokenizerError as exc:
                    exc.args[1]["colnr"] += include_match.start("file")  # shift colnr
                    exc.args[1]["ref_colnr"] += include_match.start("file")
                    raise

                if len(tokens) != 1:
                    raise PreprocessorError(
                        "@INCLUDE requires exactly one argument",
                        Context(colnr=include_match.start("complete"), ref_colnr=include_match.end("complete")),
                    )

                filename = tokens[0].strip("'\"")

            if not filename:
                raise PreprocessorError(
                    "@INCLUDE requires exactly one argument",
                    Context(colnr=include_match.start("complete"), ref_colnr=include_match.end("complete")),
                )

            # if the filename is an absolute path, joinpath uses that one
            fhandle = open(self._base_inc_dir.joinpath(filename.strip("'\"")), "r")
            # the _lineiter takes over the handle and closes it at EOF
            self._lineiter.add_file(fhandle)

            return

        raise PreprocessorError(f"unknown preprocessor directive found", ctx)

    def parse(self, fhandle, initial_variable_values=None):
        """Parse a CP2K input file
        :param fhandle: An open file handle. Included files will be opened/closed transparently.
        :param initial_variable_values: optional dictionary with preprocessor variable names and their initial values
        :return: A nested dictionary, the parsed option "tree"
        """

        if initial_variable_values:
            self._varstack.update({k.upper(): _Variable(v, Context()) for k, v in initial_variable_values.items()})

        self._lineiter.add_file(fhandle)

        try:
            for entry in self._lineiter.lines():
                # ignore empty lines and comments:
                if not entry.line or entry.line.startswith(COMMENT_CHARS):
                    continue

                if entry.line.startswith("@"):
                    self._parse_preprocessor_instruction(entry.line)
                    continue

                # ignore everything in a disable @IF/@ENDIF block
                if self._conditional_block and not self._conditional_block.condition:
                    continue

                entry = entry._replace(line=self._resolve_variables(entry.line))

                if entry.line.startswith("&"):
                    self._parse_as_section(entry)
                    continue

                self._parse_as_keyword(entry)

            if self._conditional_block is not None:
                raise PreprocessorError(
                    f"conditional block not closed at end of file", Context(ref_line=self._conditional_block.ctx["line"])
                )

        except (PreprocessorError, TokenizerError, InvalidParameterError, InvalidSectionError) as exc:
            exc.args[1]["filename"] = entry.fname
            exc.args[1]["linenr"] = entry.linenr
            exc.args[1]["line"] = entry.line
            exc.args[1]["colnrs"] = entry.colnrs
            raise

        return self._tree


class CP2KInputParserSimplified(CP2KInputParser):
    """Implement structured output simplification. Rules #1 and #2 are applied on the fly, rule #3 is done in the cleanup"""

    def _add_tree_keyword(self, kw):
        # if there is already a section with the same name as this key
        if (kw.name in self._treerefs[-1]) and (
            isinstance(self._treerefs[-1][kw.name], dict)
            or (isinstance(self._treerefs[-1][kw.name], list) and isinstance(self._treerefs[-1][kw.name][0], dict))
        ):
            # prefix that sections key with a '+' (see also the similar thing in the sections above)
            self._treerefs[-1][f"+{kw.name}"] = self._treerefs[-1].pop(kw.name)

        if kw.name not in self._treerefs[-1]:
            # even if it is a repeating element, store it as a single value first
            self._treerefs[-1][kw.name] = kw.values

        elif kw.repeats:  # if the keyword already exists and is a repeating element
            if isinstance(self._treerefs[-1][kw.name], list):
                # ... and is already a list, simply append
                self._treerefs[-1][kw.name] += [kw.values]
            else:
                # ... otherwise turn it into a list now
                self._treerefs[-1][kw.name] = [self._treerefs[-1][kw.name], kw.values]

        else:
            # TODO: improve error message
            raise NameRepetitionError(f"the keyword '{kw.name}' can only be mentioned once")

    def _add_tree_section(self, section_key, repeats):
        if (section_key in self._treerefs[-1]) and not (
            isinstance(self._treerefs[-1][section_key], dict)
            or (isinstance(self._treerefs[-1][section_key], list) and isinstance(self._treerefs[-1][section_key][0], dict))
        ):
            # prefix sections using the '+' allows for unquoted section names in YAML
            section_key = f"+{section_key}"

        if section_key not in self._treerefs[-1]:
            # if we encounter this section the first time, simply add it
            self._treerefs[-1][section_key] = {}
            self._treerefs += [self._treerefs[-1][section_key]]

        elif repeats:
            # if we already have it AND it is in fact a repeating section
            if isinstance(self._treerefs[-1][section_key], list):
                # if the entry is already a list, then simply add a new empty dict for this section
                self._treerefs[-1][section_key] += [{}]
            else:
                # if the entry is not yet a list, convert it to one
                self._treerefs[-1][section_key] = [self._treerefs[-1][section_key], {}]

            # the next entry in the stack shall be our newly created section
            self._treerefs += [self._treerefs[-1][section_key][-1]]

        else:
            raise InvalidNameError(f"the section '{section_key}' can not be defined multiple times")

    def parse(self, fhandle, initial_variable_values=None):
        super().parse(fhandle, initial_variable_values)

        # implement Rule #3 of the simplified format, as a post-process step

        treerefs = [self._tree]
        nodes = [
            self._parse_tree.getroot()
        ]  # the XML tree is needed to detect ambiguities when section params match section keywords

        while treerefs:
            tree = treerefs.pop()
            node = nodes.pop()

            for key, value in tree.items():
                section_node = _find_node_by_name(node, "SECTION", key.lstrip("+"))

                if not section_node:
                    # if the given key is a keyword, ignore it
                    continue

                valid_keys = [
                    kw.text
                    for kw in itertools.chain(section_node.iterfind("./KEYWORD/NAME"), section_node.iterfind("./SECTION/NAME"))
                ]

                if isinstance(value, dict):  # found a single section (already simplified according to rule #2)
                    # and one with a default parameter, transform it, but only if the type is a string (never use ints or bools as keys)
                    if "_" in value and isinstance(value["_"], str) and not value["_"].upper() in valid_keys:
                        tree[key] = {value["_"]: {k: v for k, v in value.items() if k != "_"}}
                        treerefs += [
                            tree[key][value["_"]]  # tree[key][value["_"]] is at this point not a valid section name anymore
                        ]
                    else:
                        treerefs += [value]

                    nodes += [section_node]

                elif isinstance(value, list) and isinstance(value[0], dict):  # found a repeated section
                    if (
                        all("_" in d for d in value)  # check if all entries have a section parameter
                        and len(set(d["_"] for d in value)) == len(value)  # check if the given section parameters are unique
                        and not any(
                            str(d["_"]).upper() in valid_keys for d in value
                        )  # and that none of the section parameters collides with a keyword or section name
                    ):
                        tree[key] = {d["_"]: {k: v for k, v in d.items() if k != "_"} for d in value}
                        treerefs += tree[
                            key
                        ].values()  # only add the values (sections) since the key names in here are not valid section names
                        nodes += [section_node] * len(tree[key])
                    else:
                        # otherwise unpack the list
                        treerefs += value
                        nodes += [section_node] * len(value)

                # else: it could still have been a keyword (cases where we have same name of sections and keywords)

        return self._tree
