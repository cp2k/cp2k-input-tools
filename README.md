# cp2k-input-tools

[![Build status](https://github.com/cp2k/cp2k-input-tools/actions/workflows/test.yml/badge.svg)](https://github.com/cp2k/cp2k-input-tools/actions) [![codecov](https://codecov.io/gh/cp2k/cp2k-input-tools/branch/develop/graph/badge.svg)](https://codecov.io/gh/cp2k/cp2k-input-tools) [![PyPI](https://img.shields.io/pypi/pyversions/cp2k-input-tools)](https://pypi.org/project/cp2k-input-tools/)

Fully validating pure-python CP2K input file parsers including preprocessing capabilities

Available commands (also available through an API, see below):

* `cp2klint` .. a CP2K input file linter
* `fromcp2k` .. create a JSON/YAML configuration file or an [AiiDA](https://github.com/aiidateam/aiida-cp2k) run script from a CP2K input file (includes validation)
* `tocp2k` .. convert a JSON or YAML configuration back to CP2K's input file format (includes validation)
* `cp2kgen` .. generate new input files based on a given input file and expressions to change parameters programmatically
* `cp2kget` .. get values from a CP2K input file (most likely a restart file) given a path of sections and attribute
* `cp2k-language-server` .. a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) implementation for the CP2K input file format
* `cp2k-datafile-lint` .. linters for other CP2K-related data file formats (like pseudos and basissets)

For a description of the JSON/YAML formats used, see below.

## Requirements

* Python 3.9+
* https://pypi.org/project/transitions/
* https://pypi.org/project/pint/
* optional: https://pypi.org/project/ruamel.yaml/ for YAML support
* optional: https://github.com/openlawlibrary/pygls for the cp2k-language-server

For development: https://poetry.eustace.io/ https://pytest.org/

## Idea

* have a pure-python CP2K input file linter with proper syntax error reporting (context, etc.)
* a final & complete restart file parser
* basis for an AiiDA CP2K project importer
* testbed for alternative import formats (YAML, JSON) for CP2K
* possible testbed for a re-implementation of the CP2K input parser itself

## TODOs

* parser: improve error reporting with context
* preprocessor: don't lose original context when interpolating variables
* parser: parsing the XML is slow (easily 70% of the time), pickle or generate Python code directly instead and keep XML parsing as fallback
* parser: preserve comments when going to/from YAML

# Usage

## Installation

You will get most tools using simply:

```console
$ pip install cp2k-input-tools
```

For YAML support you should use

```console
$ pip install cp2k-input-tools[yaml]
```

and for the Language Server:

```console
$ pip install cp2k-input-tools[lsp]
```

## Command Line Interface

Generate JSON, YAML or aiida-cp2k run script from a CP2K input file:

```console
$ fromcp2k --help
usage: fromcp2k [-h] [-y] [-c] [-b BASE_DIR] [-t TRAFO] <file>

Convert CP2K input to JSON (default) or YAML

positional arguments:
  <file>                CP2K input file

optional arguments:
  -h, --help            show this help message and exit
  -y, --yaml            output yaml instead of json
  -c, --canonical       use the canonical output format
  -b BASE_DIR, --base-dir BASE_DIR
                        search path used for relative @include's
  -t TRAFO, --trafo TRAFO
                        transformation applied to key and section names (auto,
                        upper, lower)
```

Generate an [aiida-cp2k](https://github.com/aiidateam/aiida-cp2k) template run script:

```console
$ fromcp2k --format aiida-cp2k-calc tests/inputs/test01.inp
Any key transformation function other than 'auto' is ignored when generating an aiida-cp2k run script template

from aiida.engine import run
from aiida.orm import (load_code, Dict, StructureData)

cp2k_code = load_code('...')

# Structure
structure = StructureData(...)

# Parameters
parameters = Dict(
    dict={
           "FORCE_EVAL": {
               "DFT": {
                   "KPOINTS": {
                       "FULL_GRID": False,
                       "PARALLEL_GROUP_SIZE": -1,
                       "SCHEME": "MONKHORST-PACK 3 3 3",
                       "SYMMETRY": False,
                   },
                   "MGRID": {
                       "CUTOFF": 1000.0,
                       "REL_CUTOFF": 100.0,
                   },
                   "POISSON": {
                       "PERIODIC": "XYZ",
                   },
                   "PRINT": {
                       "OVERLAP_CONDITION": {
                           "_": "ON",
                           "1-NORM": True,
                           "DIAGONALIZATION": True,
                       },
                   },
                   "QS": {
                       "EPS_DEFAULT": 1e-16,
                       "EXTRAPOLATION": "USE_GUESS",
                       "METHOD": "GAPW",
                   },
                   "SCF": {
                       "SMEAR": {
                           "_": True,
                           "ELECTRONIC_TEMPERATURE": 300.0,
                           "METHOD": "FERMI_DIRAC",
                       },
                       "ADDED_MOS": 40,
                       "EPS_SCF": 1e-08,
                       "MAX_SCF": 50,
                   },
                   "XC": {
                       "XC_FUNCTIONAL": {
                           "_": "PBE",
                       },
                   },
                   "BASIS_SET_FILE_NAME": "./BASIS_SETS",
                   "POTENTIAL_FILE_NAME": "./POTENTIALS",
               },
               "SUBSYS": {
                   "CELL": {
                       "CELL_REF": {
                           "A": "4.32947291598 0.0 0.0",
                           "B": "2.16473645799 3.7494335304 0.0",
                           "C": "2.16473645799 1.24981118034 3.53499983838",
                           "PERIODIC": "XYZ",
                       },
                       "A": "4.07419 0.0 0.0",
                       "B": "2.037095 3.52835204 0.0",
                       "C": "2.037095 1.17611735 3.32656221",
                       "PERIODIC": "XYZ",
                   },
                   "KIND": [
                       {
                       "_": "Ge",
                       "ELEMENT": "Ge",
                       "POTENTIAL": "ALL-q32",
                       "BASIS_SET": "ORB pob-TZVP",
                       },
                   ],
                   "TOPOLOGY": {
                       "COORD_FILE_NAME": "./struct.xyz",
                       "COORD_FILE_FORMAT": "XYZ",
                   },
               },
               "METHOD": "QUICKSTEP",
           },
           "GLOBAL": {
               "PRINT_LEVEL": "MEDIUM",
               "PROJECT_NAME": "fatman.calc",
               "RUN_TYPE": "ENERGY",
           },
    })

# Construct process builder.
builder = cp2k_code.get_builder()
builder.structure = structure
builder.parameters = parameters
builder.code = cp2k_code
builder.metadata.options.resources = {
    "num_machines": 1,
    "num_mpiprocs_per_machine": 1,
}
builder.metadata.options.max_wallclock_seconds = 1 * 3 * 60

run(builder)
```


Generate a CP2K input file from a JSON or YAML:

```console
$ tocp2k --help
usage: tocp2k [-h] [-y] <file>

Convert JSON or YAML input to CP2K

positional arguments:
  <file>      JSON or YAML input file

optional arguments:
  -h, --help  show this help message and exit
  -y, --yaml
```

Lint a CP2K input file:

```console
$ cp2klint tests/inputs/unterminated_var.inp
Syntax error: unterminated variable, in tests/inputs/unterminated_var.inp:
line   36: @IF ${HP
               ~~~~^
```

Generate input files for a `CUTOFF` convergence study (multiple expressions will be combined as a cartesian product):

```console
$ cp2kgen tests/inputs/NaCl.inp "force_eval/dft/mgrid/cutoff=[800,900,1000]"
Writing 'NaCl-cutoff_800.inp'...
Writing 'NaCl-cutoff_900.inp'...
Writing 'NaCl-cutoff_1000.inp'...
$ diff -Naurb NaCl-cutoff_800.inp NaCl-cutoff_900.inp
--- NaCl-cutoff_800.inp	2019-10-21 18:52:09.994323474 +0200
+++ NaCl-cutoff_900.inp	2019-10-21 18:52:10.680996641 +0200
@@ -69,7 +69,7 @@
       POTENTIAL_FILE_NAME ALL_POTENTIALS
       &MGRID
          REL_CUTOFF 80.0
-         CUTOFF 800
+         CUTOFF 900
          NGRIDS 6
       &END MGRID
       &XC
```

Get a value from a CP2K input file, for example a `RESTART` file generated in a cell optimization:

```console
$ cp2kget tests/inputs/NaCl.inp "force_eval/subsys/cell/a/0"
force_eval/subsys/cell/a/0: 5.64123539364476
```

## API

Convert a CP2K input file to a nested Python dictionary:

```python
from cp2k_input_tools.parser import CP2KInputParser, CP2KInputParserSimplified

canonical = False

if canonical:
    parser = CP2KInputParser()
else:
    parser = CP2KInputParserSimplified()

with open("project.inp") as fhandle:
    tree = parser.parse(fhandle)
```

Convert a nested Python dictionary back to a CP2K input file:

```python
from cp2k_input_tools.generator import CP2KInputGenerator

generator = CP2KInputGenerator()

tree = {"global": {}}  # ... the input tree

with open("project.inp", "w") as fhandle:
    for line in generator.line_iter(tree):
        fhandle.write(f"{line}\n")
```

## Language Server Protocol

The executable providing the language server is: `cp2k-language-server`

For `vim` you need a plugin to be able to use language servers. One such plugin is [ALE](https://github.com/dense-analysis/ale) for which you can create in its directory the file `ale_linters/cp2k/language_server.vim` with the content

```vim
call ale#Set('cp2k_lsp_executable', 'cp2k-language-server')

function! ale_linters#cp2k#language_server#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('cp2k', {
\   'name': 'language_server',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'cp2k_lsp_executable')},
\   'project_root': function('ale_linters#cp2k#language_server#GetProjectRoot'),
\   'command': '%e',
\})
```
Afterwards you must set the filetype when editing a CP2K input file to `cp2k` to get it running. This can be done explicitly using `:set filetype=cp2k`.


# The CP2K JSON and YAML formats

A reference to the CP2K input format can be found here: https://manual.cp2k.org/

## Canonical format

For everything except the pre-processor capabilities (`@IF/@ENDIF/$var/@SET`) there is a canonical one-to-one mapping of the CP2K input format to either JSON or YAML:

* repeatable sections are mapped to dictionaries
* keywords or subsections are key/value entries in sections
* all repeatable elements (sections and keywords) are mapped to lists of their respective mapped datatype
* section parameters are mapped to a special key named `_`
* default section keywords are mapped to a special key name `*`
* sections in JSON or YAML must be prefixed to avoid double definition of a key in case of same name for a section and a keyword (like the `POTENTIAL` in `KIND`), to avoid quotation marks, instead of CP2K's `&` we are using the `+`
* keyword values are mapped based on their datatypes: a list of values is always mapped to a list of their respective datatypes

The following example input:

```
&GLOBAL
   PRINT_LEVEL MEDIUM
   PROJECT test
   RUN_TYPE ENERGY
&END GLOBAL
&FORCE_EVAL
   METHOD Quickstep
   &DFT
      BASIS_SET_FILE_NAME "./BASIS_SETS"
      POTENTIAL_FILE_NAME ./POTENTIALS
      &XC
         &XC_FUNCTIONAL PBE
         &END XC_FUNCTIONAL
      &END XC
   &END DFT
   &SUBSYS
      &CELL
         A [angstrom] 4.07419 0.0 0.0
         B [angstrom] 2.037095 3.52835204 0.0
         C [angstrom] 2.037095 1.17611735 3.32656221
         PERIODIC XYZ
      &END CELL
      &KIND Ge
         ELEMENT Ge
         POTENTIAL ALL-q32
         BASIS_SET ORB pob-TZVP
      &END KIND
      &TOPOLOGY
         COORD_FILE ./struct.xyz
         COORD_FILE_FORMAT XYZ
      &END TOPOLOGY
   &END SUBSYS
&END FORCE_EVAL
```

would generate the (canonical) JSON:

```json
{
  "+global": {
    "print_level": "medium",
    "project_name": "test",
    "run_type": "energy"
  },
  "+force_eval": [
    {
      "method": "quickstep",
      "+DFT": {
        "basis_set_file_name": [
          "./BASIS_SETS"
        ],
        "potential_file_name": "./POTENTIALS"
      },
      "+XC": {
        "+xc_functional": {
          "_": "PBE"
        }
      },
      "+subsys": {
        "cell": {
          "A": [ 4.07419, 0, 0 ],
          "B": [ 2.037095, 3.52835204, 0 ],
          "C": [ 2.037095, 1.17611735, 3.32656221 ],
          "periodic": "XYZ"
        },
        "+kind": [
          {
            "_": "Ge",
            "element": "Ge",
            "potential": "ALL-q32",
            "basis_set": [
              [ "ORB", "pob-TZVP" ]
            ]
          }
          ],
        "+topology": {
          "coord_file_name": "./struct.xyz",
          "coord_file_format": "XYZ"
        }
      }
    }
  ]
}
```

*Caveats*:

* the full input format needs be known and is being loaded from a bundled `cp2k_input.xml`
* the YAML/JSON is quiet verbose and one has to know exactly which keywords can be repeated

While there is no solution to remedy the first caveat, the second can be solved with the simplified output format

## Simplified format

Still based on the canonical format the simplified format relaxes some of the rules

1. a section must only be prefixed with a `+` if a keyword with the same name is present at the same time in the same section (since we can figure out whether the user wanted to specify the section or the keyword by inspecting the value for the key: `dict` for a section)
2. if a repeated keyword or section contains only one entry, the list can be omitted (in case of ambiguity priority is given to multiple values per keyword rather than keyword repetition)
3. sections with default parameters can be formulated as dictionaries, as long as the default parameter values are unique and do not match section keyword or subsection names

the example from before in the simplified format:

```json
{
  "global": {
    "print_level": "medium",
    "project_name": "test",
    "run_type": "energy"
  },
  "force_eval": {
    "method": "quickstep",
    "DFT": {
      "basis_set_file_name": "./BASIS_SETS",
      "potential_file_name": "./POTENTIALS"
    },
    "xc": {
      "xc_functional": {
        "_": "PBE"
      }
    },
    "subsys": {
      "cell": {
        "A": [ 4.07419, 0, 0 ],
        "B": [ 2.037095, 3.52835204, 0 ],
        "C": [ 2.037095, 1.17611735, 3.32656221 ],
        "periodic": "XYZ"
      },
      "kind": {
        "_": "Ge",
        "element": "Ge",
        "potential": "ALL-q32",
        "basis_set": [ "ORB", "pob-TZVP" ]
      },
      "topology": {
        "coord_file_name": "./struct.xyz",
        "coord_file_format": "XYZ"
      }
    }
  }
}
```

or in YAML (with simplification rule #3 applied):

```yaml
global:
  print_level: medium
  project_name: test
  run_type: energy
force_eval:
  DFT:
    basis_set_file_name: ./BASIS_SETS
    potential_file_name: ./POTENTIALS
  XC:
    xc_functional:
      _: PBE  # this can NOT be simplified since PBE could also be a subsection of xc_functional
  method: quickstep
  subsys:
    cell:
      A: [ 4.07419, 0.0, 0.0]
      B: [ 2.037095, 3.52835204, 0.0]
      C: [ 2.037095, 1.17611735, 3.32656221]
      periodic: XYZ
    kind:
      Ge:
        basis_set: [ORB, pob-TZVP]
        element: Ge
        potential: ALL-q32
    topology:
      coord_file_format: XYZ
      coord_file_name: ./struct.xyz
```
