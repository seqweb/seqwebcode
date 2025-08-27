# Common Lisp Dependencies for SeqWeb System

This document outlines the dependencies required to build and run the Common Lisp components of the SeqWeb polyglot system.

## Core Dependencies

- **cl-toml**: For TOML parsing
- **alexandria**: For utility functions (file reading)
- **UIOP**: For process management (usually included with ASDF)

## Quicklisp Dependencies

```lisp
(ql:quickload :cl-toml)
(ql:quickload :alexandria)
```

## ASDF System Definition

```lisp
(asdf:defsystem :seqweb
  :depends-on (:cl-toml :alexandria)
  :components ((:file "seqvar")
               (:file "seqvar-toml")))
```

## Package Structure

```
seqwebcode/src/cl/
├── app/                    # CL applications (future)
├── lib/
│   ├── seqvar.lisp             # Core get/set functionality
│   └── seqvar-toml.lisp        # TOML parsing and integration
└── test/                    # CL tests (future)
```

## Current System Components

The Common Lisp implementation currently supports:

- **seqvar facility**: Core key-value configuration system with TOML integration

*Note: Additional Common Lisp components, libraries, and applications are expected to be added as the SeqWeb system expands.*

## Build and Test

```lisp
;; Load the system
(asdf:load-system :seqweb)

;; Run tests (when implemented)
;; TODO: Add fiveam test suite
```

## Notes

- **Quicklisp ecosystem**: Depends on community package manager
- **Not implemented**: Implementation not yet started
- **Research needed**: SQLite integration approach to be determined
- **Future ready**: Structure in place for applications and testing
- **Extensible**: Architecture supports adding new system components beyond seqvar
