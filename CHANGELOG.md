# Change Log

## [0.8.0] - 2021-10-08

`PseudopotentialData`/`BasisSetData`:

* Ue `pydantic.BaseModel` instead of dataclasses wrapper.
* This fixes an issue with default values provided for example for the `nlcc` for Pseudos attribute.
* `from_dict` helper is still provided but deprecated, the `type_hooks` parameter gets ignored.
* Some fields can now be loaded with both their name and their alias (`coeffs` vs `coefficients`),
  which was the motivation for the `type_hooks` in the first place.
* Drop `dacite` requirement.

## [0.7.3] - 2021-07-23

* `PseudopotentialData`/`BasisSetData`: expose dacites `type_hooks` for field aliasing
* fix a typing issue

## [0.7.2] - 2021-07-22

* relax click dependency to permit 7.1.x

## [0.7.1] - 2021-07-22

* updated XML definition to include SIRIUS options
* the `emit_comments` argument of the `datafile_iter` is now properly respected
* files generated with `cp2kgen` now have a comment header to indicate
  how they were generated

## [0.7.0] - 2021-06-22

* add new `cp2k-datatafile-lint` script to lint/pretty-print CP2K datafiles (pseudos/basissets)
* switch to click for CLI generation
* tighten internal parser types by using `dataclasses` and `mypy`
* include CLI in docs

## [0.6.3] - 2021-05-13

* fix the language-server-protocol implementation
  various errors, introduced by pygls update, not detected by running tests due to bogus skip structure
* update CP2K XML schema to get revised libxc sections (#42)
* update jinja2 and sphinx dependencies

## [0.6.2] - 2021-05-05

* fix bug in the simplified parser where repeated keywords were not emitted properly,
  again #32 but slightly different. This reintroduced the usage of tuples for
  multi-word keywords as a way to do repeated keyword and multi-word merging.
  Mostly visible internally since JSON and YAML do not emit tuples.
* introduce switch to select the string to which a repeated default keyword is mapped to,
  as it seems, aiida-cp2k chose `" "` (one space), as requested in #36

## [0.6.1] - 2021-05-05

* fix bug in the simplified parser where repeated keywords were not emitted properly (#32)

## [0.6.0] - 2021-05-03

* `fromcp2k` now has a `-f/--format` to select the format
* `fromcp2k` can now emit an aiida-cp2k calculation run script template
* the API now has a `CP2KParserAiiDA` which sets the required options
  to generate an aiida-cp2k compatible parameter dictionary, as a convenience
  function instead of the user having to tune `CP2KParserSimplified`
* unit parsing is now case insensitive
* the exception thrown by the parser now includes more context by
  referencing the current Section
* more documentation

## [0.5.1] - 2020-05-12

* simplified parser: add tuning knobs to adjust tree output for aiida-cp2k
* give proper message for invalid preprocessor variables
* add tests for the language-server
* more tests

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
