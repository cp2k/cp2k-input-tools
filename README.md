# cp2k-input-parser

PoC of a fully validating CP2K input file parser including preprocessing capabilities

## Idea

* have a pure-python CP2K input file linter with proper syntax error reporting (context, etc.)
* a final & complete restart file parser
* basis for an AiiDA CP2K project importer
* testbed for alternative import formats (YAML, JSON) for CP2K
* possible testbed for a re-implementation of the CP2K input parser itself

## TODOs

* [ ] general: give full context on errors (add it to exceptions)
* [ ] preprocessor: uses regexes where we should employ another state machine to get nested variables right (possibly use the HSM feature of the transitions pkg)
* [ ] preprocessor: some of the regexes do not cover all corner cases
* [ ] preprocessor: we ignore some garbage where we shouldn't
* [ ] preprocessor: losing original context when replacing variables
* [ ] preprocessor: doing var replacement before other pre-processor directives (possible waste of time plus we allow some tricks which CP2K doesn't)
* [ ] preprocessor: variable substitution might add additional line endings which we are currently lumping together with tokens from the other lines
* [ ] parser: unit conversion of values

## Observations

* nested if/endif blocks wouldn't be too difficult (needs sharing of state between nested tokenizers, similar stack building like in the parser for nodes)
