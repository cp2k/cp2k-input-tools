"""
Parsers and serializers for the Basis Set format used by CP2K
"""

import re
from decimal import Decimal
from typing import Iterator, List, Optional, Sequence, Tuple

from pydantic import BaseModel, Extra

from ..utils import SYM2NUM, DatafileIterMixin, FromDictMixin, dformat

N_VAL_EL_MATCH = re.compile(r"q(?P<nvalel>\d+)$")


class BasisSetCoefficients(BaseModel, extra=Extra.forbid):
    """A 'shell' in one single basis set"""

    n: int
    l: List[Tuple[int, int]]
    coefficients: List[List[Decimal]]


class BasisSetData(BaseModel, DatafileIterMixin, FromDictMixin, extra=Extra.forbid):
    """Basis set data for a single element"""

    element: str
    identifiers: List[str]
    n_el: Optional[int]
    blocks: List[BasisSetCoefficients]

    @classmethod
    def from_lines(cls, lines: Sequence[str]) -> "BasisSetData":
        # the first line contains the element and one or more identifiers/names
        identifiers = lines[0].split()
        element = identifiers.pop(0)

        n_el: Optional[int] = None

        try:
            n_el = next(int(m["nvalel"]) for m in (N_VAL_EL_MATCH.search(n) for n in identifiers) if m)
        except StopIteration:
            pass

        # the ALL* tags indicate an all-electron basis set, but they might be ambigious,
        # ignore them if we found an explicit #(val.el.) spec already
        if (
            not n_el
            and any(kw in identifiers for kw in ("ALL", "ALLELECTRON"))
            or any(identifier.endswith("-ae") for identifier in identifiers)
        ):
            n_el = SYM2NUM[element]

        # The second line contains the number of sets, conversion to int ignores any whitespace
        n_blocks = int(lines[1])

        nline = 2

        blocks = []

        # go through all blocks containing different sets of orbitals
        for nblock in range(n_blocks):
            # get the quantum numbers for this set, formatted as follows:
            # n lmin lmax nexp nshell(lmin) nshell(lmin+1) ... nshell(lmax-1) nshell(lmax)
            # ignore everything after nshell(lmax) on the same line (as CP2K does)
            tokens = lines[nline].split()
            qn_n, qn_lmin, qn_lmax, nexp = [int(qn) for qn in tokens[:4]]
            ncoeffs = [int(qn) for qn in tokens[4 : 5 + qn_lmax - qn_lmin]]  # noqa: E203

            nline += 1

            try:
                coefficients = [[Decimal(c) for c in lines[nline + n].split()] for n in range(nexp)]
            except IndexError:
                raise ValueError(f"Not enough exponents found. Expected {nexp} lines for block {nblock+1}") from None

            blocks.append(
                BasisSetCoefficients(
                    n=qn_n,
                    l=[(lqn, nl) for lqn, nl in zip(range(qn_lmin, qn_lmax + 1), ncoeffs)],
                    coefficients=coefficients,
                )
            )

            # advance by the number of exponents
            nline += nexp

        return cls(element=element, identifiers=identifiers, n_el=n_el, blocks=blocks)

    def cp2k_format_line_iter(self) -> Iterator[str]:
        """Generate lines of strings from this Basis Set in the format expected by CP2K."""

        yield f"{self.element:2} {' '.join(n for n in self.identifiers)}"
        yield f" {len(self.blocks):2}"  # the number of sets this basis set contains

        max_exp = -min(c.as_tuple().exponent for b in self.blocks for r in b.coefficients for c in r)
        max_len = max(len(f"{c:.{max_exp}f}") for b in self.blocks for r in b.coefficients for c in r[1:])
        max_len_exp = max(9 + max_exp, *(len(str(r[0])) for b in self.blocks for r in b.coefficients))

        for block in self.blocks:
            l_str = " ".join(f"{lqn[1]:2}" for lqn in block.l)
            yield f" {block.n:2} {block.l[0][0]:2} {block.l[-1][0]:2} {len(block.coefficients):2} {l_str}"

            for row in block.coefficients:
                yield (
                    f" {dformat(row[0], max_exp, max_len_exp)} " + " ".join(f"{dformat(c, max_exp, max_len)}" for c in row[1:])
                ).rstrip()
