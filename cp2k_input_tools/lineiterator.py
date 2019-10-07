import collections


def LineContinuationError(Exception):
    pass


LineEntry = collections.namedtuple("LineEntry", ["line", "linenr", "starts", "colnrs", "fname"])


def continuation_lines(fhandle):
    """yields LineEntries"""

    starts = [0]  # list of newline starts (in case of line continuations)
    colnrs = []  # number of stripped whitespace
    line = ""

    fname = getattr(fhandle, "name", "<NONAME>")

    for linenr, raw_line in enumerate(fhandle, 1):  # line numbers are more intuitively when starting at 1
        lstripped = raw_line.lstrip()  # CP2K consequently strips all left whitespace
        colnrs += [len(raw_line) - len(lstripped)]  # remember where the original colnr started

        # Python has universal line endings (always only \n)
        line += lstripped.rstrip("\n")

        if line.endswith("\\"):
            line = line[:-1]
            starts += [len(line)]
            continue

        yield LineEntry(line, linenr, starts, colnrs, fname)  # the linenr here is the nr of the last line without continuation
        starts = [0]
        colnrs = []
        line = ""

    if line:
        raise LineContinuationError("stray line continuation at end of file", fname)


_FileIterPair = collections.namedtuple("FileIterPair", ["fhandle", "iter"])


class MultiFileLineIterator:
    def __init__(self):
        self._fileiterpairs = []

    def __del__(self):
        while self._fileiterpairs:
            self._fileiterpairs.pop().fhandle.close()

    def add_file(self, fhandle):
        self._fileiterpairs += [_FileIterPair(fhandle, continuation_lines(fhandle))]

    def lines(self):
        while self._fileiterpairs:
            try:
                # yield the next (possibly continued) line from the file on top of the stack
                yield next(self._fileiterpairs[-1].iter)
            except StopIteration:
                # end of current file, remove the file from the stack and close it
                self._fileiterpairs.pop().fhandle.close()
