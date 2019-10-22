# Change Log

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
