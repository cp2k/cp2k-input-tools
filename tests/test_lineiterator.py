import io

import pytest

from cp2k_input_tools.lineiterator import continuation_lines, LineContinuationError


def test_broken_line_continuation():
    fhandle = io.StringIO("\\")

    with pytest.raises(LineContinuationError):
        list(continuation_lines(fhandle))
