import re
import xml.etree.ElementTree as ET
import itertools
from typing import List, Union, Any
from dataclasses import dataclass, field
from collections import Counter

from . import DEFAULT_CP2K_INPUT_XML
from .tokenizer import Context, TokenizerError, COMMENT_CHARS
from .keyword_helpers import parse_keyword
from .preprocessor import CP2KPreprocessor
from .parser_errors import InvalidNameError, InvalidSectionError, NameRepetitionError, SectionMismatchError, InvalidParameterError


def _find_node_by_name(parent, tag, name):
    """check all specified nodes for matching names or aliases in the NAME tag"""

    for node in parent.iterfind(f"./{tag}"):
        if name.upper() in [e.text for e in node.iterfind("./NAME")]:
            return node

    return None


_SECTION_MATCH = re.compile(r"&(?P<name>[\w\-_]+)\s*(?P<param>.*)")
_KEYWORD_MATCH = re.compile(r"(?P<name>[\w\-_]+)\s*(?P<value>.*)")


@dataclass
class Keyword:
    name: str
    values: Any
    repeats: bool = False


@dataclass
class Section:
    name: str
    node: ET.Element
    subsections: List["Section"] = field(default_factory=list)
    keywords: List[Keyword] = field(default_factory=list)
    param: Union[int, float, str, bool, None] = None
    repeats: bool = False


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
        self._tree = Section("/", node=self._nodes[0])
        self._treerefs = [self._tree]
        self._key_trafo = key_trafo

        self._base_inc_dir = base_dir

    def _add_tree_section(self, section_key, repeats, node):
        if not repeats and any(s.name == section_key for s in self._treerefs[-1].subsections):
            # TODO: the user possibly specified an alias, but here we only return the matching key
            raise InvalidNameError(f"the section '{section_key}' can not be defined multiple times", Context())

        self._treerefs[-1].subsections += [Section(section_key, repeats=repeats, node=node)]
        self._treerefs += [self._treerefs[-1].subsections[-1]]

    def _parse_as_section(self, line):
        match = _SECTION_MATCH.match(line)

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

        self._add_tree_section(section_key, repeats, section_node)

        # check whether we got a parameter for the section and validate it
        if section_param and not section_param.startswith(COMMENT_CHARS):
            param_node = section_node.find("./SECTION_PARAMETERS")
            if param_node:  # validate the section parameter like a kw datatype
                # there is no way we get a second section parameter, assign directly
                self._treerefs[-1].param = parse_keyword(param_node, section_param).values
            else:
                raise InvalidParameterError(
                    f"section parameters given for non-parametrized section '{section_name}': {section_param}", Context()
                )

    def _add_tree_keyword(self, kw):
        if not kw.repeats and any(k.name == kw.name for k in self._treerefs[-1].keywords):
            # TODO: the user possibly specified an alias, but here we only return the matching key
            raise NameRepetitionError(f"the keyword '{kw.name}' can only be mentioned once")

        self._treerefs[-1].keywords += [Keyword(kw.name, kw.values, repeats=kw.repeats)]

    def _parse_as_keyword(self, line):
        match = _KEYWORD_MATCH.match(line)

        kw_name = match.group("name").upper()
        kw_value = match.group("value")

        kw_node = _find_node_by_name(self._nodes[-1], "KEYWORD", kw_name)

        # if no keyword with the given name has been found, check for a default keyword for this section
        if not kw_node:
            kw_node = _find_node_by_name(self._nodes[-1], "DEFAULT_KEYWORD", "DEFAULT_KEYWORD")
            if kw_node:  # for default keywords, the whole line is the value
                kw_value = line

        if not kw_node:
            raise InvalidNameError(f"invalid keyword '{kw_name}' specified and no default keyword for this section", Context())

        try:
            kw = parse_keyword(kw_node, kw_value, self._key_trafo)
        except InvalidParameterError as exc:
            raise InvalidParameterError(f"invalid values for keyword: {match.group('name')}", Context()) from exc

        self._add_tree_keyword(kw)

    @property
    def nested_dict(self):
        stack = [self._tree]
        tree = {}
        treerefs = [tree]

        while stack:
            currsec = stack.pop(-1)
            treeref = treerefs.pop(-1)

            for section in currsec.subsections:
                if section.repeats:
                    try:
                        treeref[f"+{section.name}"] += [{}]
                    except KeyError:
                        treeref[f"+{section.name}"] = [{}]

                    treerefs += [treeref[f"+{section.name}"][-1]]

                else:
                    treeref[f"+{section.name}"] = {}
                    treerefs += [treeref[f"+{section.name}"]]

                stack += [section]

            for keyword in currsec.keywords:
                if keyword.repeats:
                    try:
                        treeref[keyword.name] += [keyword.values]
                    except KeyError:
                        treeref[keyword.name] = [keyword.values]
                else:
                    treeref[keyword.name] = keyword.values

            if currsec.param is not None:
                treeref["_"] = currsec.param

        return tree

    def parse(self, fhandle, initial_variable_values=None):
        """Parse a CP2K input file
        :param fhandle: An open file handle. Included files will be opened/closed transparently.
        :param initial_variable_values: optional dictionary with preprocessor variable names and their initial values
        :return: A nested dictionary, the parsed option "tree"
        """

        preprocessor = CP2KPreprocessor(fhandle, self._base_inc_dir, initial_variable_values)

        for line in preprocessor:
            try:
                if line.startswith("&"):
                    self._parse_as_section(line)
                    continue

                self._parse_as_keyword(line)

            except (TokenizerError, InvalidParameterError, InvalidSectionError, InvalidNameError) as exc:
                exc.args[1]["filename"] = preprocessor.fname
                exc.args[1]["linenr"] = preprocessor.line_range[1]
                exc.args[1]["colnrs"] = preprocessor.colnrs
                exc.args[1]["line"] = line
                raise

        return self.nested_dict


class CP2KInputParserSimplified(CP2KInputParser):
    """Implement structured output simplification."""

    @property
    def nested_dict(self):
        tree = {}

        stack = [self._tree]
        treerefs = [tree]

        while stack:
            currsec = stack.pop(-1)
            treeref = treerefs.pop(-1)

            for section in currsec.subsections:
                # if the section can be repeated and has a string parameter, we can possible simplify the structure
                if section.repeats:
                    # if the section is not already there, check whether to add as list or as dict with the param as subkey
                    if section.name not in treeref:
                        param_counts = Counter(s.param for s in currsec.subsections if s.name == section.name)
                        valid_keys = [
                            kw.text
                            for kw in itertools.chain(
                                section.node.iterfind("./KEYWORD/NAME"), section.node.iterfind("./SECTION/NAME")
                            )
                        ]
                        # check that the parameters are unique, strings and do not match any keywords or sections valid in that section
                        if all(c == 1 and isinstance(p, str) and p.upper() not in valid_keys for p, c in param_counts.items()):
                            treeref[section.name] = {}
                        else:
                            treeref[section.name] = []

                    if isinstance(treeref[section.name], dict):
                        # if the already present section type is a section, we're using section params as keys
                        treeref[section.name][section.param] = {}
                        treerefs += [treeref[section.name][section.param]]
                    elif not any(s.name == section.name for s in currsec.subsections if s is not section):
                        # if the section would become a list of sections, but this is the only section with that name in
                        # the current level of the parsed tree, remove one level of the list as well
                        treeref[section.name] = {}
                        treerefs += [treeref[section.name]]
                    else:
                        if section.param is not None:
                            treeref[section.name] += [{"_": section.param}]
                        else:
                            treeref[section.name] += [{}]
                        treerefs += [treeref[section.name][-1]]

                else:
                    if section.param is not None:
                        treeref[section.name] = {"_": section.param}
                    else:
                        treeref[section.name] = {}
                    treerefs += [treeref[section.name]]

                stack += [section]

            for keyword in currsec.keywords:
                # if the keyword already exists as a section:
                if (keyword.name in treeref) and (
                    isinstance(treeref[keyword.name], dict)
                    or (isinstance(treeref[keyword.name], list) and isinstance(treeref[keyword.name][0], dict))
                ):
                    # prefix that sections key with a '+'
                    treeref[f"+{keyword.name}"] = treeref.pop(keyword.name)

                if keyword.name in treeref:
                    # NOTE: we don't have to check for mistakenly repeated keywords, that was already done while parsing
                    #       we are therefore not risking to append to a keyword with multiple values
                    if not isinstance(treeref[keyword.name], list):
                        # if the value is not yet a list, make it one
                        treeref[keyword.name] = [keyword.values]

                    treeref[keyword.name] += [keyword.values]
                else:
                    treeref[keyword.name] = keyword.values

        return tree
