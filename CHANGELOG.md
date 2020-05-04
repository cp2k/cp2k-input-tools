# Change Log

## [0.5.0] - 2020-05-04

* implement support for XCTYPE
* implement support for multiple include directories
* `.parse()` is now fully idempotent (but not re-entry safe)
* the parser object has now a well-defined state after parsing
  (requirement for using it as a completion engine)
* internal representation change to a tree with dataclasses,
  and nested dicts (canonical and simplified) are now "views" of that
* adding method to obtain parsed (and unit converted) `COORD` section
* preprocessor is now a proper iterator (returning pre-processed lines)
  with access to its internal state (requirement for improved error messages)
* more tests

## [0.4.0] - 2020-04-07

* misc bug fixes
* more testing for error cases
* initial release with the language server

## [0.3.3] - 2020-04-03

* updated URLs after the move to cp2k
* updated version for `transition` dependency

## [0.3.2] - 2020-02-17

* support setting preprocessor variables and default values for them (introduced in CP2K 8.0)
* update dependencies and add more tests of unit conversions
* don't simplify sections under bool- or int-valued keys

## [0.3.1] - 2019-10-23

* initial release of the cp2kget tool to fetch values from a CP2K input (restart) file

## [0.3.0] - 2019-10-22

* initial release with the cp2kgen tool
* added more tests and some bugfixes
* improved documentation

## [0.2.5] - 2019-10-07

* fix parsing comments starting with # instead of ! (thanks to stanos4)
* switch to ruamel.yaml for proper scientific notation support in YAML
* implement parsing of fractional numbers

## [0.2.4] - 2019-10-03

* fix logic for simplified possibly ambiguous input
* fix issue with inline comments not properly filtered

## [0.2.3] - 2019-09-24

* fix issue with preprocessor variable substitution

## [0.2.2] - 2019-09-16

* fix issue with parsing empty lines

## [0.2.1] - 2019-09-12

* make API easier to use by moving the default XML path
* add API documentation to README.md

## [0.2.0] - 2019-09-11

* improve test coverage (a lot)
* implement seperate simplified and canonical formats
* implement different key transformation functions
* implement unit conversion via pint
* resolve ambiguities better in simplified schemes
* fix corner cases

## [0.1.0] - 2019-08-30

Initial release
