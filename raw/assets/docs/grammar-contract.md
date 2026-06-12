# CP2K Grammar Compatibility Contract

## Overview

This document defines which subset of CP2K input syntax the enhanced parser
(`cp2k_lsp.parser`) supports, and how it relates to the canonical parser
(`cp2k_input_tools.parser`).

## Supported Syntax

### 1. Sections

| Syntax | Example | Supported |
|--------|---------|-----------|
| Section start | `&GLOBAL` | ✅ |
| Section end | `&END GLOBAL` | ✅ |
| Section end (implicit) | `&END` | ✅ |
| Section name mismatch warning | `&GLOBAL` ... `&END MOTION` | ⚠️ Warning |
| Nested sections | `&MOTION` → `&GEO_OPT` | ✅ |

### 2. Keywords

| Syntax | Example | Supported |
|--------|---------|-----------|
| Assignment form | `RUN_TYPE = ENERGY` | ✅ |
| Whitespace-separated form | `RUN_TYPE ENERGY` | ✅ |
| Keyword only (no value) | `PRINT` | ✅ |
| Boolean values | `UKS .TRUE.` | ✅ |
| Numeric values | `MAX_ITER 100` | ✅ |
| String values | `PROJECT_NAME "my project"` | ✅ |
| Values with units | `CUTOFF 400 Ry` | ✅ |
| Comments | `# this is a comment` | ✅ |
| Inline comments | `RUN_TYPE ENERGY # comment` | ✅ |

### 3. Values

| Type | Example | Supported |
|------|---------|-----------|
| String | `"hello"` | ✅ |
| Integer | `42` | ✅ |
| Float | `1.0E-6` | ✅ |
| Boolean | `.TRUE.`, `.FALSE.` | ✅ |
| Keyword as value | `ENERGY` (in `RUN_TYPE ENERGY`) | ✅ |
| Unit | `Ry`, `Bohr`, `Angstrom` | ✅ |

### 4. Not Yet Supported

| Feature | Status | Notes |
|---------|--------|-------|
| Preprocessor directives (`@INCLUDE`, etc.) | ❌ | Use canonical parser |
| HERITAGE / section parameters | ❌ | Future work |
| Multi-line values with `\` | ❌ | Future work |
| Default keyword inference | ❌ | Use canonical parser |

## Relationship to Canonical Parser

- The **canonical parser** (`cp2k_input_tools.parser`) is the authoritative
  parser for CP2K input files. It handles preprocessor directives, section
  parameters, and the full CP2K grammar.
- The **enhanced parser** (`cp2k_lsp.parser`) is a simplified, AST-producing
  parser designed for LSP use cases (completion, diagnostics, hover).
- When the enhanced parser encounters syntax it cannot handle, it records
  errors in `parser.errors` but continues parsing.
- LSP diagnostics should fall back to the canonical parser for inputs that
  trigger enhanced parser errors.

## Testing

Grammar compatibility is verified by:
1. `tests/test_enhanced_parser_grammar.py` — fixtures for each supported syntax
2. Cross-validation against canonical parser for representative CP2K inputs
