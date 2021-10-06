# Copyright (c), Tiziano Müller
# SPDX-License-Identifier: MIT

import itertools
from decimal import Decimal, InvalidOperation
from typing import Iterator, List, Sequence

from pydantic import BaseModel, Extra, Field, root_validator

from .utils import DatafileIterMixin, dformat


class PseudopotentialDataLocal(BaseModel):
    r: Decimal
    coefficients: List[Decimal] = Field(..., alias="coeffs")

    class Config:
        extra = "forbid"
        allow_population_by_field_name = True


class PseudopotentialDataNonLocal(BaseModel):
    r: Decimal
    nproj: int
    coefficients: List[Decimal] = Field(..., alias="coeffs")

    @root_validator
    def check_coefficients(cls, values):
        assert (
            len(values["coefficients"]) == values["nproj"] * (values["nproj"] + 1) // 2
        ), "invalid number of coefficients for non-local projection"
        return values

    class Config:
        extra = "forbid"
        allow_population_by_field_name = True


class PseudopotentialDataNLCC(BaseModel, extra=Extra.forbid):
    """Nonlinear Core Correction data"""

    r: Decimal
    n: int
    c: Decimal


class PseudopotentialData(BaseModel, DatafileIterMixin, extra=Extra.forbid):
    element: str
    identifiers: List[str]
    n_el: List[int]
    local: PseudopotentialDataLocal
    non_local: List[PseudopotentialDataNonLocal]
    nlcc: List[PseudopotentialDataNLCC] = Field(default_factory=list)

    @classmethod
    def from_lines(cls, lines: Sequence[str]) -> "PseudopotentialData":
        """
        Parse a single CP2K pseudopotential entry

        :param lines: List of strings where each string is a line from the original file
        :return:      A dictionary containing the element, name, tags, aliases, n_el, local, non_local attributes
        """

        # pylint: disable=too-many-locals

        # the first line contains the element and one or more identifiers/names
        identifiers = lines[0].split()
        element = identifiers.pop(0)

        # The second line contains the number of electrons for each angular momentum
        n_el = [int(n) for n in lines[1].split()]

        # The third line contains the local part in the format
        #   <radius> <nfuncs> [<func-coeff-1> [<func-coeff-2> ...]]
        r_loc_s, nexp_ppl_s, *cexp_ppl_s = lines[2].split()

        local = PseudopotentialDataLocal(r=Decimal(r_loc_s), coefficients=[Decimal(f) for f in cexp_ppl_s])

        if int(nexp_ppl_s) != len(local.coefficients):
            raise ValueError("less coefficients found than expected while parsing the block")

        prj_ppnl: List[PseudopotentialDataNonLocal] = []
        nlcc: List[PseudopotentialDataNLCC] = []
        nline = 3

        if len(lines) > nline:
            if lines[nline].startswith("NLCC"):
                try:
                    _, n_nlcc_str = lines[nline].split()
                    n_nlcc = int(n_nlcc_str)
                    nline += 1
                    while len(nlcc) < n_nlcc:
                        nlcc_r, nlcc_n, nlcc_c = lines[nline].split()
                        nlcc.append(PseudopotentialDataNLCC(r=Decimal(nlcc_r), n=int(nlcc_n), c=Decimal(nlcc_c)))
                        nline += 1
                except IndexError as exc:
                    raise ValueError("premature end-of-lines while reading the NLCC parameters") from exc
                except (ValueError, InvalidOperation) as exc:
                    raise ValueError(f"parsing values failed for line: '{lines[nline]}'") from exc

            nprj = int(lines[nline])
            nline += 1  # start processing the non-local function blocks (if any) at this line
            while len(prj_ppnl) < nprj:
                try:
                    r_nprj_s, nprj_ppnl_s, *hprj_ppnl_strings = lines[nline].split()
                    hprj_ppnl = [Decimal(c) for c in hprj_ppnl_strings]
                except IndexError as exc:
                    raise ValueError("premature end-of-lines while reading a block of non-local projectors") from exc

                nline += 1

                nprj_ppnl = int(nprj_ppnl_s)
                ncoeffs = nprj_ppnl * (nprj_ppnl + 1) // 2  # number of elements in the upper triangular matrix

                # the matrix may be distributed over multiple lines, add those values as well
                while len(hprj_ppnl) < ncoeffs:
                    try:
                        hprj_ppnl += [Decimal(c) for c in lines[nline].split()]
                    except IndexError as exc:
                        raise ValueError("premature end-of-lines while reading coefficients of non-local projects") from exc
                    except InvalidOperation as exc:
                        raise ValueError(f"parsing coefficients failed for line: '{lines[nline]}'") from exc
                    nline += 1

                if len(hprj_ppnl) > ncoeffs:
                    raise ValueError("unknown format of the non-local projector coefficients")

                prj_ppnl.append(
                    PseudopotentialDataNonLocal(
                        r=Decimal(r_nprj_s),
                        nproj=nprj_ppnl,  # store for convenience
                        coefficients=hprj_ppnl,  # upper triangular matrix
                    )
                )

        return cls(element=element, identifiers=identifiers, n_el=n_el, local=local, non_local=prj_ppnl, nlcc=nlcc)

    def cp2k_format_line_iter(self) -> Iterator[str]:
        """Generate lines of strings from this PP in the format expected by CP2K."""

        def all_radii():
            return itertools.chain((self.local.r,), (nl.r for nl in self.non_local))

        def all_coeffs():
            return itertools.chain(self.local.coefficients, (c for nl in self.non_local for c in nl.coefficients))

        i_fmt = 4

        r_max_exp = -min(r.as_tuple().exponent for r in all_radii())
        r_max_len = max(6 + r_max_exp, *(len(f"{r:.{r_max_exp}f}") for r in all_radii()))

        try:
            c_max_exp = -min(c.as_tuple().exponent for c in all_coeffs())
            c_max_len = max(6 + c_max_exp, *(len(f"{c:.{c_max_exp}f}") for c in all_coeffs()))
        except (ValueError, TypeError):  # for all-electron pseudos all_coeffs is an empty sequence and min/max start to fail
            pass

        yield f"{self.element:2} {' '.join(n for n in self.identifiers)}"
        yield " " + " ".join(f"{i:{i_fmt}}" for i in self.n_el)
        yield (
            f" {dformat(self.local.r, r_max_exp, r_max_len)} {len(self.local.coefficients):{i_fmt}} "
            + " ".join(f"{dformat(c, c_max_exp, c_max_len)}" for c in self.local.coefficients)
        ).rstrip()

        if self.nlcc:
            yield f" NLCC {len(self.nlcc):{i_fmt}}"

            r_nlcc_max_exp = -min(nlcc.r.as_tuple().exponent for nlcc in self.nlcc)
            r_nlcc_max_len = max(6 + r_nlcc_max_exp, *(len(f"{nlcc.r:.{r_nlcc_max_exp}f}") for nlcc in self.nlcc))

            c_nlcc_max_exp = -min(nlcc.c.as_tuple().exponent for nlcc in self.nlcc)
            c_nlcc_max_len = max(6 + c_nlcc_max_exp, *(len(f"{nlcc.c:.{c_nlcc_max_exp}f}") for nlcc in self.nlcc))

            for nlcc in self.nlcc:
                yield (
                    f" {dformat(nlcc.r, r_nlcc_max_exp, r_nlcc_max_len)} {nlcc.n:{i_fmt}}"
                    f" {dformat(nlcc.c, c_nlcc_max_exp, c_nlcc_max_len)}"
                ).rstrip()

        yield f" {len(self.non_local):{i_fmt}}"
        nl_matrix_indent = 1 + r_max_len + 1 + i_fmt + 1

        for nonl in self.non_local:
            # print the first N (=nproj) coefficients (first row of the matrix)
            yield (
                f" {dformat(nonl.r, r_max_exp, r_max_len)} {nonl.nproj:{i_fmt}} "
                + " ".join(f"{dformat(c, c_max_exp, c_max_len)}" for c in nonl.coefficients[: nonl.nproj])
            ).rstrip()

            # for a non-scalar non-empty matrix, print the rest of the coefficients
            for nrow in range(1, nonl.nproj):
                scol = nrow * nonl.nproj - nrow * (nrow - 1) // 2
                ecol = scol + nonl.nproj - nrow
                yield (
                    " " * (nl_matrix_indent + nrow * (c_max_len + 1))
                    + " ".join(f"{dformat(c, c_max_exp, c_max_len)}" for c in nonl.coefficients[scol:ecol])
                ).rstrip()
