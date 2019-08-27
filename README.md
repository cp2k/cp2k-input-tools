# cp2k-input-parser

PoC of a fully validating CP2K input file parser including preprocessing capabilities

## Requirements

* Python 3.6+
* https://pypi.org/project/transitions/

## Idea

* have a pure-python CP2K input file linter with proper syntax error reporting (context, etc.)
* a final & complete restart file parser
* basis for an AiiDA CP2K project importer
* testbed for alternative import formats (YAML, JSON) for CP2K
* possible testbed for a re-implementation of the CP2K input parser itself

## TODOs

* [ ] parser: improve error reporting with context (tokenizer/preprocessor is already done)
* [ ] preprocessor: losing original context when replacing variables
* [ ] parser: unit conversion of values
* [ ] parser: parsing the XML is sloooow (easily 70% of the time), pickle or generate Python code directly instead and keep XML parsing as backup?

## Observations

* nested if/endif blocks wouldn't be too difficult (needs sharing of state between nested tokenizers, similar stack building like in the parser for nodes)
* CP2K doesn't do nested variable interpolation, wouldn't be so difficult either
