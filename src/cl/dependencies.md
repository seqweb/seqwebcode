# Common Lisp Dependencies for SeqWeb System

This document outlines the dependencies required to build and run the Common Lisp components of the SeqWeb polyglot system.

## Core Dependencies

- **cl-sqlite**: For SQLite database connectivity (more widely available than cl-toml)
- **cl-toml**: For TOML parsing
- **alexandria**: For utility functions (file reading, string manipulation)
- **UIOP**: For process management and path handling (usually included with ASDF)
- **fiveam**: For testing framework

## Quicklisp Dependencies

```lisp
(ql:quickload :cl-sqlite)
(ql:quickload :cl-toml)
(ql:quickload :alexandria)
(ql:quickload :fiveam)
```

## ASDF System Definition

```lisp
(asdf:defsystem :seqweb
  :depends-on (:cl-sqlite :cl-toml :alexandria :fiveam)
  :components ((:file "seqvar")
               (:file "seqvar-toml")))
```

## Package Structure

```
seqwebcode/src/cl/
├── app/                    # CL applications (future)
├── libs/
│   ├── seqvar.lisp             # Core get/set functionality
│   └── seqvar-toml.lisp        # TOML parsing and integration
└── test/
    ├── test-seqvar.lisp         # Test suite using fiveam
    └── show-seqvar.lisp         # Demonstration script
```

## Current System Components

The Common Lisp implementation currently supports:

- **seqvar facility**: Core key-value configuration system with TOML integration

*Note: Additional Common Lisp components, libraries, and applications are expected to be added as the SeqWeb system expands.*

## Build and Test

```lisp
;; Load the system
(asdf:load-system :seqweb)

;; Run tests
(fiveam:run! 'seqweb:seqvar-tests)

;; Run demonstration
(seqweb:show-seqvar-demo)
```

## Notes

- **Quicklisp ecosystem**: Depends on community package manager
- **Production ready**: Uses well-established libraries (cl-sqlite, cl-toml)
- **Comprehensive testing**: Includes fiveam test framework
- **SQLite integration**: Direct database access via cl-sqlite
- **Extensible**: Architecture supports adding new system components beyond seqvar
