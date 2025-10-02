from decimal import Decimal
from typing import Iterator, List, Tuple

from pydantic import BaseModel

from ..utils import NUM2SYM, dformat


class ECP(BaseModel, extra="forbid"):
    """ECP for a single element"""

    Z: int
    Znuc: int  # effective core charge = Z - n_elec in pseudo
    M: Tuple[int, int, int, int, int, int]  # M, M0, M1, ..., M4
    coefficients: List[Tuple[Decimal, Decimal, int]]  # alfkl, cgkl, nkl

    def crystal_format_line_iter(self) -> Iterator[str]:
        yield f"{self.Znuc} " + " ".join(str(m) for m in self.M)

        exp_max_exp = -min(int(r[0].as_tuple().exponent) for r in self.coefficients)
        exp_max_len = max(exp_max_exp, *(len(f"{r[0]:.{exp_max_exp}f}") for r in self.coefficients))
        coeff_max_exp = -min(int(r[1].as_tuple().exponent) for r in self.coefficients)
        coeff_max_len = max(exp_max_exp, *(len(f"{r[1]:.{coeff_max_exp}f}") for r in self.coefficients))

        for row in self.coefficients:
            yield f"  {dformat(row[0], exp_max_exp, exp_max_len)} {dformat(row[1], coeff_max_exp, coeff_max_len)} {row[2]}"

    def nwchem_format_line_iter(self) -> Iterator[str]:
        """Generate lines of strings for isolated ECP pseudopotentials"""

        sym = NUM2SYM[self.Z]

        yield f"{sym} nelec {self.Z-self.Znuc}"  # NWchem has the number of electrons in the pseudo
        yield f"{sym} ul"

        row = 0

        if self.M[0]:
            for _ in range(self.M[0]):
                yield f"{self.coefficients[row][2]} {self.coefficients[row][0]} {self.coefficients[row][1]}"
                row += 1
        else:
            yield "2     1.000000    0.000000"

        for shelln, shell in zip(self.M[1:], "SPDFG"):
            if not shelln:  # skip empty shells completely
                continue
            yield f"{sym} {shell}"
            for _ in range(shelln):
                yield f"{self.coefficients[row][2]:<2} {self.coefficients[row][0]:>11} {self.coefficients[row][1]:>11}"
                row += 1
