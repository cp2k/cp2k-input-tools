import re
from typing import NamedTuple, Sequence
from collections.abc import Iterator
from collections import defaultdict
from pathlib import Path


from .lineiterator import MultiFileLineIterator
from .tokenizer import Context, TokenizerError, COMMENT_CHARS, tokenize
from .parser_errors import PreprocessorError


class _Variable(NamedTuple):
    value: str
    ctx: defaultdict


class _ConditionalBlock(NamedTuple):
    condition: str
    ctx: defaultdict


_VALID_VAR_NAME_MATCH = re.compile(r"^[a-z_]\w*$", flags=re.IGNORECASE | re.ASCII)
_CONDITIONAL_MATCH = re.compile(r"\s*@(?P<stmt>IF|ENDIF)\s*(?P<cond>.*)", flags=re.IGNORECASE)
_SET_MATCH = re.compile(r"\s*@SET\s+(?P<var>\S+)\s+(?P<value>.+)", flags=re.IGNORECASE)
_INCLUDE_MATCH = re.compile(r"\s*(?P<complete>@(?P<type>INCLUDE|XCTYPE)\b\s*(?P<file>.*))", flags=re.IGNORECASE)


class CP2KPreprocessor(Iterator):
    def __init__(self, fhandle, base_dir, initial_variable_values=None):
        self._varstack = {}
        self._lineiter = MultiFileLineIterator()
        self._conditional_block = None
        self._current_line_entry = None

        if isinstance(base_dir, (str, Path)):
            self._inc_dirs = [Path(base_dir)]
        elif isinstance(base_dir, Sequence):
            self._inc_dirs = [Path(b) for b in base_dir]
        else:
            raise TypeError("invalid type passed for base_dir")

        if initial_variable_values:
            self._varstack.update({k.upper(): _Variable(v, None) for k, v in initial_variable_values.items()})

        self._lineiter.add_file(fhandle, managed=False)

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
            inctype = include_match["type"]

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
                    f"@{inctype} requires exactly one argument",
                    Context(colnr=include_match.start("complete"), ref_colnr=include_match.end("complete")),
                )

            filename = filename.strip("'\"")

            if inctype.upper() == "XCTYPE":
                filename = f"xc_section/{filename}.sec"

            for inc_dir in self._inc_dirs:
                try:
                    # if the filename is an absolute path, joinpath uses that one and will ignore the dir
                    fhandle = inc_dir.joinpath(filename).open("r")

                    # the _lineiter takes over the handle and closes it at EOF
                    self._lineiter.add_file(fhandle)

                    break
                except OSError:
                    continue
            else:
                raise PreprocessorError(f"specified INCLUDE/XCTYPE {filename} could not be opened", ctx)

            return

        raise PreprocessorError(f"unknown preprocessor directive found", ctx)

    def __next__(self):
        for line in self._lineiter:
            try:
                # ignore empty lines and comments:
                if not line or line.startswith(COMMENT_CHARS):
                    continue

                if line.startswith("@"):
                    self._parse_preprocessor_instruction(line)
                    continue

                # ignore everything in a disable @IF/@ENDIF block
                if self._conditional_block and not self._conditional_block.condition:
                    continue

                return self._resolve_variables(line)

            except (PreprocessorError, TokenizerError) as exc:
                exc.args[1]["filename"] = self._lineiter.fname
                exc.args[1]["linenr"] = self._lineiter.line_range[1]
                exc.args[1]["colnrs"] = self._lineiter.colnrs
                exc.args[1]["line"] = line
                raise

        if self._conditional_block is not None:
            raise PreprocessorError(
                f"conditional block not closed at end of file", Context(ref_line=self._conditional_block.ctx["line"])
            )

        raise StopIteration

    @property
    def line_range(self):
        """Original line numbers (start and end) of the last (possibly combined) line"""
        return self._lineiter.line_range

    @property
    def colnrs(self):
        """Original column numbers where non-whitespace content started for most recent emitted line"""
        return self._lineiter.colnrs

    @property
    def starts(self):
        """Index in the most recent emitted line where content from a new line in file starts"""
        return self._lineiter.starts

    @property
    def fname(self):
        return self._lineiter.fname
