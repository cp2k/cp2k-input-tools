
import collections
import xml.etree.ElementTree as ET


class GeneratorError(Exception):
    pass


TreeNode = collections.namedtuple("TreeNode", ["name", "dictref", "xmlnode", "indent"])


class CP2KInputGenerator:
    def __init__(self, xmlspec, indent_shift=3):
        self._parse_tree = ET.parse(xmlspec)
        self._shift = indent_shift

    def _get_section(self, name, content, node):
        for section in node.iterfind("./SECTION"):
            if name.upper() in [e.text for e in section.iterfind("./NAME")]:
                break
        else:  # if we come to the end without a break:
            raise GeneratorError("invalid section")

        repeats = True if section.get("repeats") == "yes" else False

        if repeats and not isinstance(content, (dict, list)):
            # repeating sections can be list of dicts or directly dicts
            raise GeneratorError("invalid repeating section")
        elif not isinstance(content, dict):
            # non-repeating sections can only be dicts
            raise GeneratorError("invalid non-repeating section")

        return section

    def _get_keyword(self, name, section_node):

        for kw in section_node.iterfind("./KEYWORD"):
            if name.upper() in [e.text for e in kw.iterfind("./NAME")]:
                break
        else:  # if we come to the end without a break:
            raise GeneratorError(f"invalid keyword")

        return kw

    def _get_section_parameter(self, section_node):
        # there is only one
        sparam = section_node.find("./SECTION_PARAMETERS")

        if sparam:
            return sparam

        raise GeneratorError(f"invalid section parameter")

    def _get_default_keyword(self, section_node):
        # there is only one
        dkw = section_node.find("./DEFAULT_KEYWORD")

        if dkw:
            return dkw

        raise GeneratorError(f"invalid default keyword")

    def _render_keyword(self, value, kw_node):
        repeats = True if kw_node.get("repeats") == "yes" else False
        dt = kw_node.find("./DATA_TYPE")
        kind = dt.get("kind")
        n_var = int(dt.find("./N_VAR").text)

        TYPE_RENDERERS = {
            "integer": lambda v: str(v),
            "keyword": lambda v: v,
            "logical": lambda v: ".TRUE." if bool(v) else ".FALSE.",  # TODO: better check for passed values
            "real": lambda v: str(v),
            "string": lambda v: v,
            "word": lambda v: v,
        }

        #print(f"repeats: {repeats}, n_var: {n_var}, is_list: {isinstance(value, list)}")

        if isinstance(value, list):
            if not repeats and n_var == 1:
                raise GeneratorError("got multiple values for non-repeating single-valued keyword")

            nested_list = any(isinstance(v, list) for v in value)
            #print(f"nested_list: {nested_list}")

            if repeats and n_var == 1 and nested_list:
                raise GeneratorError("got multiple values for repeating single-valued keyword")

            # if any of the values is a list itself we know the outer list to be for the repetion and the inner for the values
            # and we only have to ensure that those entries where the inner list is not a list but a single word are also a list
            if nested_list:
                value = [v if isinstance(v, list) else [v] for v in value]

            elif n_var ==1:
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


    def line_iter(self, tree):
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

                # a section can be explicitly tagged by a starting + or &, or implicitly by having a dict value
                if key[0] in "&+":
                    section_name = key[1:].upper()
                    section = self._get_section(section_name, value, node)
                elif isinstance(value, dict):
                    section_name = key.upper()
                    section = self._get_section(section_name, value, node)

                if section:
                    # if key being parsed points to a section (resp. multiple repeated sections, add them to the stack)
                    if isinstance(value, list):
                        treerefs += [TreeNode(path + [section_name], v, section, indent + self._shift) for v in value]
                    else:
                        treerefs += [TreeNode(path + [section_name], value, section, indent + self._shift)]

                    continue

                if key == "_":  # section parameters are handled above
                    continue

                if key == "*":
                    keyword = self._get_default_keyword(node)

                    for valuestr in self._render_keyword(value, keyword):
                        yield f"{str():>{indent + self._shift}}{valuestr}"

                    continue

                keyword = self._get_keyword(key.upper(), node)
                for valuestr in self._render_keyword(value, keyword):
                    yield f"{str():>{indent + self._shift}}{key} {valuestr}"

            if treerefs:
                next_indent = treerefs[-1].indent
            else:
                next_indent = 0

            for eindent in range(indent, next_indent - 1, -self._shift):
                parent_sname = path.pop()
                yield f"{str():>{eindent}}&END {parent_sname}"
