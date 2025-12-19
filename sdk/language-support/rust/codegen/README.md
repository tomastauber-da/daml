# Rust Codegen

This is a code generator for Rust bindings to a Daml package. To run it, you must execute:

```console
$ bazel run //sdk/language-support/rust/codegen:codegen -- --help
Usage: codegen-rust [options] <DAR-file>...

Code generator for the Daml ledger Rust bindings.

  <DAR-file>...            DAR files to generate Rust bindings for
  -o, --output-directory <value>
                           Output directory for the generated sources
  -V, --verbosity <value>  Verbosity between 0 (only show errors) and 4 (show all messages) -- defaults to 0
  --help                   This help text
```

## Overview

The Rust codegen generates Rust type definitions and serialization/deserialization code for Daml types, templates, and choices. This allows Rust applications to interact with the Daml ledger through the JSON API or gRPC Ledger API.

## Generated Code

The codegen produces:
- Rust struct definitions corresponding to Daml records
- Enum definitions for Daml variants
- Template and choice definitions
- Serialization and deserialization implementations using serde

## Development

This is a minimal implementation scaffolding for Rust codegen support. The actual code generation logic needs to be implemented to produce working Rust bindings.
