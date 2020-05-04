import io

import pytest

from cp2k_input_tools.lineiterator import ContinuationLineIterator, LineContinuationError


def test_broken_line_continuation():
    fhandle = io.StringIO("\\")

    with pytest.raises(LineContinuationError):
        list(ContinuationLineIterator(fhandle))
