import collections
import itertools
import re
import xml.etree.ElementTree as ET

from . import DEFAULT_CP2K_INPUT_XML


class GeneratorError(Exception):
    pass


class SectionNotFoundError(Exception):
    pass


class KeywordNotFoundError(Exception):
    pass


class SectionParametersNotFoundError(Exception):
    pass


class InvalidSectionDataError(Exception):
    pass


class InvalidKeywordDataError(Exception):
    pass


class InvalidBooleanDataError(Exception):
    pass


class SimplifiedSectionAmbiguityError(Exception):
    pass


TreeNode = collections.namedtuple("TreeNode", ["name", "dictref", "xmlnode", "indent"])


class CP2KInputGenerator:
    def __init__(self, xmlspec=DEFAULT_CP2K_INPUT_XML, indent_shift=3):
        """Initialize the generator

        :param xmlspec: path to a `cp2k_input.xml` file (default: use the bundled)
        :param indent_shift: Number of spaces to use for indentation of the
                             keywords/subsections of a section
        """
        self._parse_tree = ET.parse(xmlspec)
        self._shift = indent_shift

    def _get_section(self, name, node):
        for section in node.iterfind("./SECTION"):
            if name.upper() in [e.text for e in section.iterfind("./NAME")]:
                break
        else:  # if we come to the end without a break:
            raise SectionNotFoundError()

        return section

    def _get_keyword(self, name, section_node):

        for kw in section_node.iterfind("./KEYWORD"):
            if name.upper() in [e.text for e in kw.iterfind("./NAME")]:
                break
        else:  # if we come to the end without a break:
            raise KeywordNotFoundError(f"keyword {name} not allowed")

        return kw

    def _get_section_parameter(self, section_node):
        # there is only one
        sparam = section_node.find("./SECTION_PARAMETERS")

        if sparam:
            return sparam

        raise SectionParametersNotFoundError("section does not take any parameters")

    def _get_default_keyword(self, section_node):
        # there is only one
        dkw = section_node.find("./DEFAULT_KEYWORD")

        if dkw:
            return dkw

        raise GeneratorError("invalid default keyword")

    def _render_keyword(self, value, kw_node):
        repeats = True if kw_node.get("repeats") == "yes" else False
        dt = kw_node.find("./DATA_TYPE")
        kind = dt.get("kind")
        n_var = int(dt.find("./N_VAR").text)

        def word_renderer(string):
            # words with whitespaces need to be quoted
            if re.search(r"\s", string):
                return f'"{string}"'

            return string

        def bool_renderer(val):
            """the given value may or may not be a boolean, make sure we also accept other datatypes"""

            if str(val).upper() in ("1", "T", ".T.", "TRUE", ".TRUE.", "Y", "YES", "ON"):
                return ".TRUE."

            if str(val).upper() in ("0", "F", ".F.", "FALSE", ".FALSE.", "N", "NO", "OFF"):
                return ".FALSE."

            raise InvalidBooleanDataError(f"the given value '{val}' could not be recognized as a CP2K boolean value")

        TYPE_RENDERERS = {
            "integer": lambda v: str(v),
            "keyword": lambda v: str(v).upper(),
            "logical": bool_renderer,
            "real": lambda v: str(v),
            "string": lambda v: v,
            "word": word_renderer,
        }

        if isinstance(value, (list, tuple)):
            if not repeats and n_var == 1:
                raise InvalidKeywordDataError("got multiple values for non-repeating single-valued keyword")

            nested_list = any(isinstance(v, (list, tuple)) for v in value)

            if repeats and n_var == 1 and nested_list:
                raise InvalidKeywordDataError("got multiple values for repeating single-valued keyword")

            # if any of the values is a list itself we know the outer list to be for the repetion and the inner for the values
            # and we only have to ensure that those entries where the inner list is not a list but a single word are also a list
            if nested_list:
                value = [v if isinstance(v, (list, tuple)) else [v] for v in value]

            elif n_var == 1:
                value = [[v] for v in value]

            else:
                # for the case where none of the values are actually list entries, we give priority to the values, meaning that
                #   "BASIS_SET": ["ORB", "pob-TZVP"]
                # becomes
                #   BASIS_SET ORB pob_TZVP
                # So if one wants to have
                #   BASIS_SET pob-TZVP
                #   BASIS_SET some-added-basis
                # s/he must explicitly provide it as:
                #   "BASIS_SET": [["pob-TZVP"], ["some-added-basis"]]
                # to break ambiguity
                value = [value]

        else:
            # replace by list of lists, because that's the most general thing
            value = [[value]]

        for entry in value:
            yield " ".join(TYPE_RENDERERS[kind](v) for v in entry)

    def _parse_section(self, name, node, path, content, indent):
        repeats = True if node.get("repeats") == "yes" else False

        # if a section contains a list, the only option is that it is a
        if isinstance(content, list):
            if not repeats:
                raise InvalidSectionDataError("multiple entries given for non-repeating section")

            if not all(isinstance(v, dict) for v in content):
                raise InvalidSectionDataError("non-section value given in repeating section")

            return [TreeNode(path + [name], v, node, indent + self._shift) for v in content]

        if not isinstance(content, dict):
            # if the value does not match the one expected for a section
            raise InvalidSectionDataError()

        section_params = None
        try:
            section_params = self._get_section_parameter(node)
        except SectionParametersNotFoundError:
            pass

        # if we have a section taking section parameters, we also accept the format:
        #   kind:
        #     H:
        #       basis_set: ...
        # instead of:
        #   kind:
        #     - _: H
        #       basis_set: ...
        # If all of the values in this dict are dicts themselves we therefore have to look closer,
        # since this could either be a simplified section (like the one above) or a regular one, like:
        #   BS:
        #     alpha:
        #       ...
        #     beta:
        #       ...
        if section_params and all(isinstance(v, dict) for v in content.values()):
            # 1. figure out all the valid keys in simplified and canonical mode
            valid_keys = [kw.text for kw in itertools.chain(node.iterfind("./KEYWORD/NAME"), node.iterfind("./SECTION/NAME"))]
            valid_keys += [f"+{kw.text}" for kw in node.iterfind("./SECTION/NAME")]

            # 2. if all of the specified keys match either section or a keyword, this is just a regular section
            if all(str(k).upper() in valid_keys for k in content.keys()):
                return [TreeNode(path + [name], content, node, indent + self._shift)]

            # 3. if not any of the keys is a valid key, we can safely assume that the user specified simplified expanded section
            if not any(str(k).upper() in valid_keys for k in content.keys()):
                # return a list of sections with this param merged into the section as normal section parameter
                return [
                    TreeNode(path + [name], dict(_=param, **section), node, indent + self._shift)
                    for param, section in content.items()
                ]

            # otherwise the user has either specified a mix (not supported) or has a typo
            raise SimplifiedSectionAmbiguityError(
                f"the section '{name}' contains elements which could either be"
                f" keywords/subsections or section parameters within the simplified"
                f" format scheme. Please use the canonical format for this section."
            )

        return [TreeNode(path + [name], content, node, indent + self._shift)]

    def line_iter(self, tree):
        """
        A generator for CP2K input file lines

        :param tree: a canonical or simplified input option tree,
                     as generated by `CP2KInputParser` or `CP2KInputParserSimplified`
        """

        treerefs = [TreeNode([""], tree, self._parse_tree.getroot(), -self._shift)]

        while treerefs:
            path, content, node, indent = treerefs.pop()
            sname = path[-1]

            if sname:
                if "_" in content:  # default parameter is a keyword "_"
                    sparam = self._get_section_parameter(node)
                    # the default parameter can only be mentioned once, so _render_keyword will only yield once
                    yield f"{str():>{indent}}&{sname} {next(self._render_keyword(content['_'], sparam))}"
                else:
                    yield f"{str():>{indent}}&{sname}"

            for key, value in content.items():
                section = None

                if key.startswith(("&", "+")):  # a section can be explicitly tagged by a starting + or &
                    section_name = key[1:].upper()
                else:
                    section_name = key.upper()

                try:
                    # if key being parsed points to a section (resp. multiple repeated sections, add them to the stack)
                    section = self._get_section(section_name, node)
                    treerefs += self._parse_section(section_name, section, path, value, indent)
                    continue
                except SectionNotFoundError:
                    # if no section name was found then go on parsing it as a key
                    pass
                except InvalidSectionDataError as exc:
                    # if the value for this key didn't match the one expected for a section
                    # check whether there is also a key with the same name and if there is, continue
                    try:
                        self._get_keyword(section_name, node)
                    except KeywordNotFoundError:
                        raise exc from None

                if key == "_":  # section parameters are handled above
                    continue

                if key == "*":
                    keyword = self._get_default_keyword(node)

                    for valuestr in self._render_keyword(value, keyword):
                        yield f"{str():>{indent + self._shift}}{valuestr}"

                    continue

                key_name = key.upper()

                keyword = self._get_keyword(key_name, node)
                for valuestr in self._render_keyword(value, keyword):
                    yield f"{str():>{indent + self._shift}}{key_name} {valuestr}"

            if treerefs:
                next_indent = treerefs[-1].indent
            else:
                next_indent = 0

            for eindent in range(indent, next_indent - 1, -self._shift):
                parent_sname = path.pop()
                yield f"{str():>{eindent}}&END {parent_sname}"
