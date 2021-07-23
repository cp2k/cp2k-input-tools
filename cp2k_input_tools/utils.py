# Copyright (c), Tiziano Müller
# SPDX-License-Identifier: MIT

import itertools
import re
from dataclasses import dataclass
from decimal import Decimal
from typing import (
    IO,
    Any,
    Callable,
    Dict,
    Iterator,
    List,
    Optional,
    Sequence,
    Type,
    TypeVar,
    Union,
)

from typing_extensions import Protocol

SYM2NUM = {
    "H": 1,
    "He": 2,
    "Li": 3,
    "Be": 4,
    "B": 5,
    "C": 6,
    "N": 7,
    "O": 8,
    "F": 9,
    "Ne": 10,
    "Na": 11,
    "Mg": 12,
    "Al": 13,
    "Si": 14,
    "P": 15,
    "S": 16,
    "Cl": 17,
    "Ar": 18,
    "K": 19,
    "Ca": 20,
    "Sc": 21,
    "Ti": 22,
    "V": 23,
    "Cr": 24,
    "Mn": 25,
    "Fe": 26,
    "Co": 27,
    "Ni": 28,
    "Cu": 29,
    "Zn": 30,
    "Ga": 31,
    "Ge": 32,
    "As": 33,
    "Se": 34,
    "Br": 35,
    "Kr": 36,
    "Rb": 37,
    "Sr": 38,
    "Y": 39,
    "Zr": 40,
    "Nb": 41,
    "Mo": 42,
    "Tc": 43,
    "Ru": 44,
    "Rh": 45,
    "Pd": 46,
    "Ag": 47,
    "Cd": 48,
    "In": 49,
    "Sn": 50,
    "Sb": 51,
    "Te": 52,
    "I": 53,
    "Xe": 54,
    "Cs": 55,
    "Ba": 56,
    "La": 57,
    "Ce": 58,
    "Pr": 59,
    "Nd": 60,
    "Pm": 61,
    "Sm": 62,
    "Eu": 63,
    "Gd": 64,
    "Tb": 65,
    "Dy": 66,
    "Ho": 67,
    "Er": 68,
    "Tm": 69,
    "Yb": 70,
    "Lu": 71,
    "Hf": 72,
    "Ta": 73,
    "W": 74,
    "Re": 75,
    "Os": 76,
    "Ir": 77,
    "Pt": 78,
    "Au": 79,
    "Hg": 80,
    "Tl": 81,
    "Pb": 82,
    "Bi": 83,
    "Po": 84,
    "At": 85,
    "Rn": 86,
    "Fr": 87,
    "Ra": 88,
    "Ac": 89,
    "Th": 90,
    "Pa": 91,
    "U": 92,
    "Np": 93,
    "Pu": 94,
    "Am": 95,
    "Cm": 96,
    "Bk": 97,
    "Cf": 98,
    "Es": 99,
    "Fm": 100,
    "Md": 101,
    "No": 102,
    "Lr": 103,
    "Rf": 104,
    "Db": 105,
    "Sg": 106,
    "Bh": 107,
    "Hs": 108,
    "Mt": 109,
    "Ds": 110,
    "Rg": 111,
    "Cn": 112,
    "Fl": 114,
    "Lv": 116,
}

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


@dataclass
class FromDictMixin:
    @classmethod
    def from_dict(cls: Type[_T], data: Dict[str, Any], type_hooks: Optional[Dict[Type, Callable[[Any], Any]]] = None) -> _T:
        """Create a data instance from a nested dictionary"""
        import dacite

        config = dacite.Config(cast=[tuple, Decimal])
        if type_hooks:
            config.type_hooks = type_hooks

        return dacite.from_dict(data_class=cls, data=data, config=config)  # type: ignore


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

            match = BLOCK_MATCH.match(line)

            if match and line_buffer:
                try:
                    yield cls.from_lines(line_buffer)
                except ValueError as exc:
                    newexc = chained_exception(ValueError, f"failed to parse block for: {line_buffer[0].strip()}", exc)
                    if not keep_going:
                        raise newexc
                    errors.append(newexc)

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


def dformat(val, ndigits, slen):
    """
    Right-pads a decimal with spaces such that there are max_exp number of characters after the dot
    and the complete string is max_len characters in width.
    """

    digits = ndigits + val.as_tuple().exponent if val.as_tuple().exponent < 0 else ndigits + 1
    return f"{format(val, 'f') + ' '*(digits):>{slen}}"
