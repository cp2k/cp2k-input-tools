import re
import xml.etree.ElementTree as ET
import itertools
from typing import List, Union
from dataclasses import dataclass, field
from collections import Counter
from fractions import Fraction

from . import DEFAULT_CP2K_INPUT_XML
from .tokenizer import Context, TokenizerError, COMMENT_CHARS
from .keyword_helpers import Keyword, UREG
from .preprocessor import CP2KPreprocessor
from .parser_errors import InvalidNameError, InvalidSectionError, NameRepetitionError, SectionMismatchError, InvalidParameterError


_SECTION_MATCH = re.compile(r"&(?P<name>[\w\-_]+)\s*(?P<param>.*)")
_KEYWORD_MATCH = re.compile(r"(?P<name>[\w\-_]+)\s*(?P<value>.*)")


@dataclass
class Section:
    name: str
    node: ET.Element
    subsections: List["Section"] = field(default_factory=list)
    keywords: List[Keyword] = field(default_factory=list)
    param: Union[int, float, str, bool, None] = None
    repeats: bool = False

    def subsections_by_name(self, name):
        yield from (s for s in self.subsections if s.name == name)

    def keywords_by_name(self, name):
        yield from (k for k in self.keywords if k.name == name)

    @property
    def keyword_names(self):
        yield from (n.text for n in self.node.iterfind("./KEYWORD/NAME"))

    @property
    def section_names(self):
        yield from (n.text for n in self.node.iterfind("./SECTION/NAME"))

    def find_node_by_name(self, tag, name):
        """return the node matching the given name or None"""
        for node in self.node.iterfind(f"./{tag}"):
            # ElementTree does not have a parent relationship,
            # hence the double loop
            for sub in node.iterfind("./NAME"):
                if sub.text == name.upper():
                    return node

        return None


class CP2KInputParser:
    def __init__(self, xmlspec=DEFAULT_CP2K_INPUT_XML, base_dir=".", key_trafo=str.lower):
        """
        The CP2K input parser.

        :param xmlspec: Path to the `cp2k_input.xml` file generated with `cp2k --xml`
        :param base_dir: The base directory to be used for resolving `@include` directives
        :param key_trafo: A function object used for mangling key names, must treat input case-insensitive
        """

        # schema:
        self._spec = ET.parse(xmlspec)

        # datatree being generated:
        self._tree = None
        self._treerefs = []  # initializing to empty will make nested_dict return {} if nothing was parsed yet

        self._key_trafo = key_trafo
        self._base_inc_dir = base_dir

    def _add_tree_section(self, section_name, repeats, node):
        if not repeats and any(s.name == section_name for s in self._treerefs[-1].subsections):
            # TODO: the user possibly specified an alias, but here we only return the matching key
            raise InvalidNameError(f"the section '{section_name}' can not be defined multiple times", Context())

        self._treerefs[-1].subsections += [Section(section_name, repeats=repeats, node=node)]
        self._treerefs += [self._treerefs[-1].subsections[-1]]

    def _parse_as_section(self, line):
        match = _SECTION_MATCH.match(line)

        section_name = match.group("name").upper()
        section_param = match.group("param")

        if section_name == "END":
            section_param = section_param.rstrip()

            if section_param and section_param.upper() not in [e.text for e in self._treerefs[-1].node.iterfind("./NAME")]:
                raise SectionMismatchError("could not match open section with name: {section_param}", Context())

            # if the END param was a match or none was specified, go a level up
            self._treerefs.pop()
            return

        # check all section nodes for matching names or aliases
        section_node = self._treerefs[-1].find_node_by_name("SECTION", section_name)
        if section_node is None:
            raise InvalidSectionError(f"invalid section '{section_name}'", Context())

        repeats = True if section_node.get("repeats") == "yes" else False

        self._add_tree_section(section_name, repeats, section_node)

        # check whether we got a parameter for the section and validate it
        if section_param and not section_param.startswith(COMMENT_CHARS):
            param_node = section_node.find("./SECTION_PARAMETERS")
            if param_node:  # validate the section parameter like a kw datatype
                # there is no way we get a second section parameter, assign directly
                self._treerefs[-1].param = Keyword.from_string(param_node, section_param).values
            else:
                raise InvalidParameterError(
                    f"section parameters given for non-parametrized section '{section_name}': {section_param}", Context()
                )

    def _add_tree_keyword(self, kw):
        if not kw.repeats and any(k.name == kw.name for k in self._treerefs[-1].keywords):
            # TODO: the user possibly specified an alias, but here we only return the matching key
            raise NameRepetitionError(f"the keyword '{kw.name}' can only be mentioned once")

        self._treerefs[-1].keywords += [kw]

    def _parse_as_keyword(self, line):
        match = _KEYWORD_MATCH.match(line)

        kw_name = match.group("name").upper()
        kw_value = match.group("value")

        kw_node = self._treerefs[-1].find_node_by_name("KEYWORD", kw_name)

        # if no keyword with the given name has been found, check for a default keyword for this section
        if kw_node is None:
            kw_node = self._treerefs[-1].find_node_by_name("DEFAULT_KEYWORD", "DEFAULT_KEYWORD")
            if kw_node is not None:  # for default keywords, the whole line is the value
                kw_value = line

        if kw_node is None:
            raise InvalidNameError(f"invalid keyword '{kw_name}' specified and no default keyword for this section", Context())

        try:
            kw = Keyword.from_string(kw_node, kw_value, self._key_trafo)  # the key_trafo is needed to mangle keywords
        except InvalidParameterError as exc:
            raise InvalidParameterError(f"invalid values for keyword: {match.group('name')}", Context()) from exc

        self._add_tree_keyword(kw)

    @property
    def nested_dict(self):
        stack = self._treerefs.copy()
        tree = {}
        treerefs = [tree]

        while stack:
            currsec = stack.pop(-1)
            treeref = treerefs.pop(-1)

            for section in currsec.subsections:
                section_name = f"+{self._key_trafo(section.name)}"

                if section.repeats:
                    try:
                        treeref[section_name] += [{}]
                    except KeyError:
                        treeref[section_name] = [{}]

                    treerefs += [treeref[section_name][-1]]

                else:
                    treeref[section_name] = {}
                    treerefs += [treeref[section_name]]

                stack += [section]

            for keyword in currsec.keywords:
                keyword_name = self._key_trafo(keyword.name)

                if keyword.repeats:
                    try:
                        treeref[keyword_name] += [keyword.values]
                    except KeyError:
                        treeref[keyword_name] = [keyword.values]
                else:
                    treeref[keyword_name] = keyword.values

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
        self._tree = Section("/", node=self._spec.getroot())
        self._treerefs = [self._tree]

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
                exc.args[1]["section"] = self._treerefs[-1]
                raise

        if len(self._treerefs) > 1:
            raise SectionMismatchError(
                f"section '{self._treerefs[-1].name}' not closed",
                Context(line=line, section=self._treerefs[-1]),  # preprocessor is not valid anymore at this point
            )

        # returning the nested dictionary representation for convenience
        return self.nested_dict

    def coords(self, force_eval=0):
        """
        Return an iterator to coordinates given in a FORCE_EVAL/SUBSYS/COORD section
        where the coordinates are proper float values and converted to Angstrom if specified in
        a different unit
        """

        try:
            coord = next(
                next(
                    next(
                        itertools.islice(self._tree.subsections_by_name("FORCE_EVAL"), force_eval, None), None
                    ).subsections_by_name("SUBSYS")
                ).subsections_by_name("COORD")
            )
        except AttributeError:
            return

        scaled = next(coord.keywords_by_name("SCALED"), False)
        current_unit = UREG.parse_expression(next(coord.keywords_by_name("UNIT"), "ANGSTROM"), case_sensitive=False)

        for coordline in coord.keywords_by_name("*"):
            # coordinates are a series of strings according to the CP2K schema
            fields = coordline.values.split()

            name = fields[0]
            position = (float(Fraction(f)) for f in fields[1:4])  # positions can be fractions
            molname = fields[4] if len(fields) > 4 else None

            if not scaled and current_unit != UREG.angstrom:
                position = ((p * current_unit).to(UREG.angstrom).magnitude for p in position)

            yield (name, tuple(position), molname)


def _flattened_keyword_parameter(keyword):
    if isinstance(keyword.values, (list, tuple)):
        return " ".join(str(v) for v in keyword.values)
    return keyword.values


class CP2KInputParserSimplified(CP2KInputParser):
    """Implement structured output simplification."""

    def __init__(self, multi_value_unpack=True, repeated_section_unpack=True, level_reduction_blacklist=None, *args, **kwargs):
        super().__init__(*args, **kwargs)

        if multi_value_unpack:
            self._get_value = lambda x: x.values
        else:
            # this is the mode as currently employed by the aiida-cp2k plugin:
            # keywords with multiple arguments are treated as simple strings
            self._get_value = _flattened_keyword_parameter

        self._repeated_section_unpack = repeated_section_unpack
        self._no_lvl_reduction = level_reduction_blacklist if (level_reduction_blacklist is not None) else []

    @property
    def nested_dict(self):
        stack = self._treerefs.copy()
        tree = {}
        treerefs = [tree]

        while stack:
            currsec = stack.pop(-1)
            treeref = treerefs.pop(-1)

            for section in currsec.subsections:
                section_name = self._key_trafo(section.name)

                # if the section can be repeated and has a string parameter, we can possible simplify the structure
                if section.repeats:
                    # if the section is not already there, check whether to add as list or as dict with the param as subkey
                    if section_name not in treeref:
                        param_counts = Counter(s.param for s in currsec.subsections if s.name == section.name)
                        valid_keys = [n for n in itertools.chain(section.keyword_names, section.section_names)]
                        # check that the parameters are unique, strings and do not match any keywords or sections valid in that section
                        if self._repeated_section_unpack and all(
                            c == 1 and isinstance(p, str) and p.upper() not in valid_keys for p, c in param_counts.items()
                        ):
                            treeref[section_name] = {}
                        else:
                            treeref[section_name] = []

                    if isinstance(treeref[section_name], dict):
                        # if the already present section type is a section, we're using section params as keys
                        treeref[section_name][section.param] = {}
                        treerefs += [treeref[section_name][section.param]]
                    elif not any(s.name == section.name for s in currsec.subsections if s is not section) and (
                        section.name not in self._no_lvl_reduction
                    ):
                        # if the section would become a list of sections, but this is the only section with that name in
                        # the current level of the parsed tree, remove one level of the list as well
                        treeref[section_name] = {"_": section.param} if section.param is not None else {}
                        treerefs += [treeref[section_name]]
                    else:
                        treeref[section_name] += [{"_": section.param}] if section.param is not None else [{}]
                        treerefs += [treeref[section_name][-1]]

                else:
                    treeref[section_name] = {"_": section.param} if section.param is not None else {}
                    treerefs += [treeref[section_name]]

                stack += [section]

            for keyword in currsec.keywords:
                keyword_name = self._key_trafo(keyword.name)

                # if the keyword already exists as a section:
                if (keyword_name in treeref) and (
                    isinstance(treeref[keyword_name], dict)
                    or (isinstance(treeref[keyword_name], list) and isinstance(treeref[keyword_name][0], dict))
                ):
                    # prefix that sections key with a '+'
                    treeref[f"+{keyword_name}"] = treeref.pop(keyword_name)

                if keyword_name in treeref:
                    # NOTE: we don't have to check for mistakenly repeated keywords, that was already done while parsing
                    #       we are therefore not risking to append to a keyword with multiple values
                    if not isinstance(treeref[keyword_name], list):
                        # if the value is not yet a list, make it one
                        treeref[keyword_name] = [treeref[keyword_name]]

                    treeref[keyword_name] += [self._get_value(keyword)]
                else:
                    treeref[keyword_name] = self._get_value(keyword)

        return tree


class CP2KInputParserAiiDA(CP2KInputParserSimplified):
    """Implement structured output simplification as expected by aiida-cp2k as input parameter"""

    def __init__(self, *args, **kwargs):
        # aiida-cp2k uses a limited dict-based representation of the CP2K input,
        # and the simplified parser needs to be tweaked:
        # * avoid that something like "BASIS_SET ORB DZVP-MOLOPT-GTH" is unpacked
        #   into {"BASIS_SET": ("ORB", "DZVP-MOLOPT-GTH")}
        # * prevents the unpacking of repeated sections (removing the list)
        # * prevents that "KIND H" is turned into {"KIND": {"H": {"BASIS_SET": ...}}}
        #   but kept as {"KIND": {"_": "H", "BASIS_SET": ...}}} and with the option above
        #   makes it compatible with aiida-cp2k's way of CP2K input representation
        # NOTE: some CP2K input files can not be represented in this form

        super().__init__(
            key_trafo=str.upper,
            multi_value_unpack=False,
            repeated_section_unpack=False,
            level_reduction_blacklist=["KIND"],
            *args,
            **kwargs,
        )
