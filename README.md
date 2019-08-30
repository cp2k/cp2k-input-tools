# cp2k-input-tools

Fully validating pure-python CP2K input file parsers including preprocessing capabilities

Available commands:

* `cp2klint` .. a CP2K input file linter
* `fromcp2k` .. create a JSON or YAML configuration file from a CP2K input file
* `tocp2k` .. convert a JSON or YAML configuration back to CP2K's input file format

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
