"""
Parsers and serializers for the Basis Set format used by Crystal
"""

import re
from decimal import Decimal
from typing import Iterator, List, Optional, Sequence, Tuple

from pydantic import BaseModel, Extra

from ..pseudopotentials.ecp import ECP
from ..utils import NUM2SYM, DatafileIterMixin, FromDictMixin

BLOCK_MATCH = re.compile(r"^\s*\d+\s+\d+\s*$")


class BasisSetCoefficients(BaseModel, extra=Extra.forbid):
    """A 'shell' in one single basis set"""

    shell: int  # 0: s, 1: s and p, 2: p, 3: d, 4:f
    charge: Decimal
    scaling: Decimal
    coefficients: List[Tuple[Decimal, Decimal]]


class BasisSetData(BaseModel, DatafileIterMixin, FromDictMixin, extra=Extra.forbid):
    """Basis set data for a single element"""

    Z: int
    shells: List[BasisSetCoefficients]
    ecp: Optional[ECP] = None

    @classmethod
    def from_lines(cls, lines: Sequence[str]) -> "BasisSetData":
        # the first line contains Z and the number of shells
        nat, nshells = (int(w) for w in lines[0].split())
        nline = 1
        shells = []
        ecp: Optional[ECP] = None

        Z = nat % 100  # according to CRYSTAL manual, I guess Oganesson will always require an ECP ;-)

        if nat > 200:
            pseudo_type = lines[nline].strip()
            assert pseudo_type == "INPUT", f"Unsupported pseudo type: {pseudo_type}, only INPUT is currently supported"
            nline += 1

            if pseudo_type == "INPUT":
                tokens = lines[nline].split()
                znuc = Decimal(tokens[0])
                M = tuple(int(t) for t in tokens[1:])
                assert len(M) == 6, f"Invalid number of term numbers M found in ECP spec, expected: 6, found: {len(M)}"
                nline += 1

                ecp_coefficients = []

                for _ in range(sum(M)):
                    tokens = lines[nline].split()
                    ecp_coefficients.append((Decimal(tokens[0]), Decimal(tokens[1]), int(tokens[2])))
                    nline += 1

                ecp = ECP(Z=Z, Znuc=znuc, M=M, coefficients=ecp_coefficients)

        # go through all blocks containing different sets of orbitals (in CRYSTAL shells)
        for shelln in range(nshells):
            tokens = lines[nline].split()
            btype, shell, ngaussians = (int(qn) for qn in tokens[:3])
            charge, scaling = (Decimal(v) for v in tokens[3:])

            assert btype == 0, "Unsupported basis set type, currently only 'free' is supported"

            nline += 1

            try:
                coefficients = [tuple(Decimal(c) for c in lines[nline + n].split(maxsplit=1)) for n in range(ngaussians)]
            except IndexError:
                raise ValueError(f"Not enough exponents found. Expected {ngaussians} lines for block {shelln+1}") from None

            shells.append(
                BasisSetCoefficients(
                    shell=shell,
                    charge=charge,
                    scaling=scaling,
                    coefficients=coefficients,
                )
            )

            # advance by the number of exponents
            nline += ngaussians

        return cls(Z=Z, shells=shells, ecp=ecp)

    def cp2k_format_line_iter(self, identifier) -> Iterator[str]:
        identifiers = [identifier]

        if self.ecp:
            identifiers.append(f"{identifiers[0]}-q{self.ecp.Znuc}")

        yield from self._to_cp2k(identifiers).cp2k_format_line_iter()

    def crystal_format_line_iter(self) -> Iterator[str]:
        """Generate lines of strings from this Basis Set in the format expected by CRYSTAL."""

        if self.ecp:
            yield f"{self.Z + 200} {len(self.shells)}"
            yield "INPUT"
            yield from self.ecp.crystal_format_line_iter()
        else:
            yield f"{self.Z} {len(self.shells)}"

        for shell in self.shells:
            yield f"0 {shell.shell} {len(shell.coefficients)} {shell.charge} {shell.scaling}"

            for row in shell.coefficients:
                yield f" {str(row[0]):>13} {str(row[1]):>20}"

    def nwchem_ecp_format_line_iter(self) -> Iterator[str]:
        if not self.ecp:
            return

        yield from self.ecp.nwchem_format_line_iter()

    @staticmethod
    def is_block_start(line: str) -> bool:
        return BLOCK_MATCH.match(line) is not None

    def _to_cp2k(self, identifiers: List[str]):
        from .cp2k import BasisSetCoefficients as BasisSetCoefficientsCP2K
        from .cp2k import BasisSetData as BasisSetDataCP2K

        element = NUM2SYM[self.Z]
        total_charges = Decimal(0)
        blocks = []

        shell_cnt = {0: -1, 1: -1, 2: -1, 3: -1, 4: -1}

        for shell in self.shells:
            # CRYSTAL has like Gaussian 0: s, 1: sp, 2: p, 3: d, 4: f
            if shell.shell > 1:
                qn_lmin = qn_lmax = shell.shell - 1
            elif shell.shell == 1:
                qn_lmin = 0
                qn_lmax = 1
            else:
                qn_lmin = qn_lmax = 0

            if shell.charge > 0:
                shell_cnt[shell.shell] += 1

            n = qn_lmax + 1 + max(0, shell_cnt[shell.shell])

            blocks.append(
                BasisSetCoefficientsCP2K(
                    n=n,
                    l=[(qn_l, 1) for qn_l in range(qn_lmin, qn_lmax + 1)],
                    coefficients=shell.coefficients,
                )
            )

            total_charges += shell.charge

        return BasisSetDataCP2K(element=element, identifiers=identifiers, n_el=int(total_charges // 1), blocks=blocks)
