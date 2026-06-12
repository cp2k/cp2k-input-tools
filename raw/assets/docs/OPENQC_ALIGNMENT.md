# OpenQC Alignment

`cp2k-lsp-enhanced` provides CP2K parsing, linting, conversion, and language-server tooling. `newtontech/OpenQC-VSCode` should expose the same language behavior in VS Code.

## Keep aligned

- CP2K `.inp` file detection and language IDs.
- Diagnostics from parser and linter behavior.
- Command expectations for `cp2k-language-server`.
- Fixture behavior for valid input, invalid input, and preprocessor-heavy input.

## Release check

Before a public OpenQC release, smoke test one valid and one invalid CP2K input against this repository and the extension.
