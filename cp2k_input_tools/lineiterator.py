from typing import NamedTuple, Tuple, List, TextIO
from collections.abc import Iterator


class LineContinuationError(Exception):
    pass


class ContinuationLineIterator(Iterator):
    def __init__(self, fhandle):
        self._fhandle = fhandle
        self._linenr = 0
        self._colnrs = []  # number of stripped whitespace (from current line, continued line, 2nd continued line, ...)
        self._starts = [0]  # list of newline starts (in case of line continuations)

    @property
    def line_range(self) -> Tuple[int, int]:
        """Original line numbers (start and end) of the last (possibly combined) line"""
        return (self._linenr - len(self._starts) - 1, self._linenr)

    @property
    def colnrs(self) -> List[int]:
        """Original column numbers where non-whitespace content started for most recent emitted line"""
        return self._colnrs

    @property
    def starts(self) -> List[int]:
        """Index in the most recent emitted line where content from a new line in file starts"""
        return self._starts

    def __next__(self) -> str:
        """yields LineEntries"""

        self._starts = [0]
        self._colnrs = []
        line = ""

        for raw_line in self._fhandle:
            self._linenr += 1  # line numbers are more intuitively when starting at 1
            lstripped = raw_line.lstrip()  # CP2K consequently strips all left whitespace
            self._colnrs += [len(raw_line) - len(lstripped)]  # remember where the original colnr started

            # Python has universal line endings (always only \n)
            line += lstripped.rstrip("\n")

            if line.endswith("\\"):
                line = line[:-1]
                self._starts += [len(line)]
                continue

            return line

        if self._colnrs:
            raise LineContinuationError("stray line continuation at end of file")

        raise StopIteration


class _FileIterPair(NamedTuple):
    fhandle: TextIO
    iter: ContinuationLineIterator


class MultiFileLineIterator(Iterator):
    def __init__(self):
        self._fileiterpairs = []

    def __del__(self):
        while self._fileiterpairs:
            self._fileiterpairs.pop().fhandle.close()

    def add_file(self, fhandle):
        self._fileiterpairs += [_FileIterPair(fhandle, ContinuationLineIterator(fhandle))]

    def __next__(self) -> str:
        while self._fileiterpairs:
            try:
                # yield the next (possibly continued) line from the file on top of the stack
                return next(self._fileiterpairs[-1].iter)
            except StopIteration:
                # end of current file, remove the file from the stack and close it
                self._fileiterpairs.pop().fhandle.close()

        raise StopIteration

    @property
    def line_range(self) -> Tuple[int, int]:
        """Original line numbers (start and end) of the last (possibly combined) line"""
        return self._fileiterpairs[-1].iter.line_range

    @property
    def colnrs(self) -> List[int]:
        """Original column numbers where non-whitespace content started for most recent emitted line"""
        return self._fileiterpairs[-1].iter.colnrs

    @property
    def starts(self) -> List[int]:
        """Index in the most recent emitted line where content from a new line in file starts"""
        return self._fileiterpairs[-1].iter.starts

    @property
    def fname(self) -> str:
        return getattr(self._fileiterpairs[-1].fhandle, "name", "<BUFFER>")
