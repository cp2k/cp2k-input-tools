# Copyright (c), Tiziano Müller
# SPDX-License-Identifier: MIT

import itertools
import re
import warnings
from dataclasses import dataclass
from typing import (
    IO,
    Any,
    Callable,
    Iterator,
    List,
    Mapping,
    Optional,
    Sequence,
    Type,
    TypeVar,
    Union,
)

from typing_extensions import Protocol

NUM2SYM = [
    "",
    "H",
    "He",
    "Li",
    "Be",
    "B",
    "C",
    "N",
    "O",
    "F",
    "Ne",
    "Na",
    "Mg",
    "Al",
    "Si",
    "P",
    "S",
    "Cl",
    "Ar",
    "K",
    "Ca",
    "Sc",
    "Ti",
    "V",
    "Cr",
    "Mn",
    "Fe",
    "Co",
    "Ni",
    "Cu",
    "Zn",
    "Ga",
    "Ge",
    "As",
    "Se",
    "Br",
    "Kr",
    "Rb",
    "Sr",
    "Y",
    "Zr",
    "Nb",
    "Mo",
    "Tc",
    "Ru",
    "Rh",
    "Pd",
    "Ag",
    "Cd",
    "In",
    "Sn",
    "Sb",
    "Te",
    "I",
    "Xe",
    "Cs",
    "Ba",
    "La",
    "Ce",
    "Pr",
    "Nd",
    "Pm",
    "Sm",
    "Eu",
    "Gd",
    "Tb",
    "Dy",
    "Ho",
    "Er",
    "Tm",
    "Yb",
    "Lu",
    "Hf",
    "Ta",
    "W",
    "Re",
    "Os",
    "Ir",
    "Pt",
    "Au",
    "Hg",
    "Tl",
    "Pb",
    "Bi",
    "Po",
    "At",
    "Rn",
    "Fr",
    "Ra",
    "Ac",
    "Th",
    "Pa",
    "U",
    "Np",
    "Pu",
    "Am",
    "Cm",
    "Bk",
    "Cf",
    "Es",
    "Fm",
    "Md",
    "No",
    "Lr",
    "Rf",
    "Db",
    "Sg",
    "Bh",
    "Hs",
    "Mt",
    "Ds",
    "Rg",
    "Cn",
    "Nh",
    "Fl",
    "Mc",
    "Lv",
    "Ts",
    "Og",
]

SYM2NUM = {sym: Z for Z, sym in enumerate(NUM2SYM[1:], start=1)}

EOF_MARKER_LINE = "Eof marker"
EMPTY_LINE_MATCH = re.compile(r"^(\s*|\s*#.*)$")
BLOCK_MATCH = re.compile(r"^\s*(?P<element>[a-zA-Z]{1,3})\s+(?P<family>\S+).*")


def chained_exception(cls: type, msg: str, prev: Exception):
    """Create a chained exception"""
    exc = cls(msg)
    exc.__cause__ = prev
    return exc


class MulitpleValueErrorsException(ValueError):
    """An exception which contains multiple value error exceptions"""


_T = TypeVar("_T")


class SupportsFromLines(Protocol):
    @classmethod
    def from_lines(cls: Type[_T], lines: Sequence[str]) -> _T:
        pass

    @staticmethod
    def is_block_start(line: str) -> bool:
        pass


@dataclass
class DatafileIterMixin:
    @classmethod
    def datafile_iter(
        cls: SupportsFromLines, content: Union[IO, str, Sequence[str]], keep_going=True, emit_comments=False
    ) -> Iterator[Union[SupportsFromLines, str]]:
        """
        Generates a sequence of data instances, one for each data entry found in the given file

        :param fhandle: Open file handle (in text mode) to a data file
        :param keep_going: Whether to ignore invalid entries and keep going
        :param emit_comments: Whether to also emit line breaks and comments when found
        """

        # find the beginning of a new data entry, then
        # continue until the next one, the iterator chain and Eof marker guarantee
        # that we find a last (empty) one which will not be parsed.

        line_buffer: List[str] = []
        errors: List[Exception] = []
        post_comments: List[str] = []

        if isinstance(content, str):
            content = content.splitlines()

        for line in itertools.chain(content, [EOF_MARKER_LINE]):
            if EMPTY_LINE_MATCH.match(line):
                if line_buffer:
                    post_comments.append(line.strip())
                elif emit_comments:
                    yield (line.strip())
                continue  # ignore empty and comment lines

            if (cls.is_block_start(line) or line == EOF_MARKER_LINE) and line_buffer:
                try:
                    yield cls.from_lines(line_buffer)
                except ValueError as exc:
                    msg = f"failed to parse block for: {line_buffer[0].strip()}"
                    if not keep_going:
                        raise ValueError(msg) from exc
                    errors.append(chained_exception(ValueError, msg, exc))

                if post_comments:
                    if emit_comments:
                        yield from post_comments
                    post_comments = []

                line_buffer = []

            line_buffer.append(line.strip())

        assert len(line_buffer) == 1 and line_buffer[0] == EOF_MARKER_LINE

        if post_comments and emit_comments:
            yield from post_comments

        if len(errors) == 1:
            raise errors[0]

        if errors:
            raise MulitpleValueErrorsException(errors)

    @staticmethod
    def is_block_start(line: str) -> bool:
        return BLOCK_MATCH.match(line) is not None


class FromDictMixin:
    @classmethod
    def from_dict(cls: Type[_T], data: Mapping[str, Any], type_hooks: Optional[Mapping[Type, Callable[[Any], Any]]] = None) -> _T:
        """Create a data instance from a nested dictionary"""
        warnings.warn("This helper function will be removed, use '.parse_obj' instead.", PendingDeprecationWarning, stacklevel=1)
        if type_hooks:
            warnings.warn("The 'type_hooks' attribute has been removed and is being ignored.", DeprecationWarning, stacklevel=1)

        return cls.parse_obj(data)  # type: ignore


def dformat(val, ndigits, slen):
    """
    Right-pads a decimal with spaces such that there are max_exp number of characters after the dot
    and the complete string is max_len characters in width.
    """

    digits = ndigits + val.as_tuple().exponent if val.as_tuple().exponent < 0 else ndigits + 1
    return f"{format(val, 'f') + ' '*(digits):>{slen}}"
