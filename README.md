# cp2k-input-tools

[![Build Status](https://travis-ci.com/dev-zero/cp2k-input-tools.svg?branch=develop)](https://travis-ci.com/dev-zero/cp2k-input-tools) [![codecov](https://codecov.io/gh/dev-zero/cp2k-input-tools/branch/develop/graph/badge.svg)](https://codecov.io/gh/dev-zero/cp2k-input-tools) [![PyPI](https://img.shields.io/pypi/pyversions/cp2k-input-tools)](https://pypi.org/project/cp2k-input-tools/)

Fully validating pure-python CP2K input file parsers including preprocessing capabilities

Available commands:

* `cp2klint` .. a CP2K input file linter
* `fromcp2k` .. create a (almost simplified) JSON or YAML configuration file from a CP2K input file (includes validation)
* `tocp2k` .. convert a (simplified) JSON or YAML configuration back to CP2K's input file format (includes validation)

For a description of the JSON/YAML formats used, see below.

## Requirements

* Python 3.6+
* https://pypi.org/project/transitions/
* optional: https://pypi.org/project/PyYAML/

For development: https://poetry.eustace.io/

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
* parser: maybe generate AST using an emitting (`yield`) parser for more flexibility

# Usage

## Command Line Interface

Generate JSON or YAML from a CP2K input file:

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
