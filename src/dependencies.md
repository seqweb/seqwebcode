# Common Dependencies for SeqWeb Polyglot System

This document outlines the general, cross-cutting, and polyglot dependencies required to build and run the SeqWeb system across all supported languages.

## Overview

The SeqWeb polyglot system provides a unified development platform with support for multiple programming languages. Each language has its own `dependencies.md` file with specific requirements.

## Current System Components

The SeqWeb system currently consists of:

- **seqvar facility**: Unified key-value configuration system with TOML file support

*Note: This list is expected to expand as the SeqWeb system grows to include additional components, services, and facilities.*

## Language-Specific Dependencies

For detailed dependencies by language, see:
- **Python**: `src/python/dependencies.md`
- **Java**: `src/java/dependencies.md`
- **Common Lisp**: `src/cl/dependencies.md`
- **Bash**: `src/bash/dependencies.md`

## Cross-Cutting Dependencies

### Database Requirements

#### SQLite
- **SQLite 3.24+**: Required for `ON CONFLICT` clause support
- **WAL mode**: Enabled for better concurrency
- **File permissions**: Write access to `$SEQWEBDEV_HOME/.state/` directory

#### Schema
```sql
CREATE TABLE IF NOT EXISTS seqvars(
  ns   TEXT NOT NULL,
  key  TEXT NOT NULL,
  val  TEXT NOT NULL DEFAULT '',
  src  TEXT,
  ts   INTEGER NOT NULL,
  PRIMARY KEY(ns,key)
);
```

*Note: Additional database schemas may be added as new system components are developed.*

### Environment Variables

#### Required
- **SEQWEBDEV_HOME**: Base directory for SeqWeb development environment
- **PATH**: Must include sqlite3 and python3 executables

#### Optional
- **SEQWEBDEV_DEBUG**: Enable debug logging
- **SEQWEBDEV_LOG_LEVEL**: Set logging verbosity

## Polyglot Architecture Dependencies

### Directory Structure
```
seqwebcode/
└─ src/
   ├─ python/              # Python implementation
   │  ├─ app/              # Applications (CLI, etc.)
   │  ├─ lib/              # Libraries
   │  └─ test/             # Tests
   ├─ java/                # Java implementation
   │  ├─ app/              # Applications
   │  ├─ lib/              # Libraries
   │  └─ test/             # Tests
   ├─ cl/                  # Common Lisp implementation
   │  ├─ app/              # Applications
   │  ├─ lib/              # Libraries
   │  └─ test/             # Tests
   └─ bash/                # Bash implementation
      ├─ app/              # Applications
      ├─ lib/              # Libraries
      └─ test/             # Tests
```

### Common Contract (All Languages)
- **DB path**: `$SEQWEBDEV_HOME/.state/env.sqlite`
- **Table**: `seqvars(ns TEXT NOT NULL, key TEXT NOT NULL, val TEXT NOT NULL DEFAULT '', src TEXT, ts INTEGER NOT NULL, PRIMARY KEY(ns,key))`
- **Defaults**: `ns="SeqVar"`, `src="seqweb"`
- **get(key, ns)**: always returns a string (empty `""` if missing)
- **set(key, val, ns, src)**: upsert; no schema creation here
- **On missing DB/table**: throw/raise a clear error:
  > "seqvar store not initialized (missing env.sqlite or seqvars table). Run SeqWeb bootstrap."

*Note: Additional common contracts will be defined as new system components are added.*

### TOML Integration (All Languages)
- **Flattening**: TOML tables become dotted keys (`[service] host="..."` → `service.host = "..."`)
- **Strings only**: sidecars stringify non-string TOML values (keeps seqvar DB uniform)
- **APIs to expose** (same names everywhere):
  - `load_toml(paths: list|array)` → `map/dict<string,string>`
  - `write_toml_to_seqvar(bindings, ns="SeqVar", src="seqweb")` → `None`

*Note: TOML integration patterns established here will serve as templates for future configuration systems.*

## Build Order

1. **Bootstrap**: Initialize SQLite database and schema
2. **Python**: Install Python package in development mode (see `src/python/dependencies.md`)
3. **Java**: Resolve Maven/Gradle dependencies (see `src/java/dependencies.md`)
4. **Common Lisp**: Install Quicklisp dependencies (see `src/cl/dependencies.md`)
5. **Bash**: Ensure system dependencies are available (see `src/bash/dependencies.md`)

## Integration Testing Dependencies

### Cross-Language Testing
```
sqlite3 (for database inspection)
python3 (for TOML validation)
```

### Performance Benchmarking
```
# Tools for measuring cross-language performance
time (built-in shell command)
python3 -m timeit
java -XX:+PrintCompilation
```

## Troubleshooting

### Common Issues
1. **Missing SQLite**: Ensure sqlite3 is in PATH and accessible
2. **Permission errors**: Check write access to `$SEQWEBDEV_HOME/.state/`
3. **Import errors**: Verify all language-specific dependencies are installed
4. **Version conflicts**: Ensure minimum version requirements are met

### Validation Commands
```bash
# Test SQLite
sqlite3 --version

# Test Python (required for Bash delegation)
python3 -c "import tomllib; print('TOML support OK')"

# Test Java
java -version

# Test Common Lisp
sbcl --eval "(ql:quickload :cl-toml)" --quit
```

## Notes

- **Shared database**: All implementations share the same SQLite database schema
- **Consistent error messages**: Same error text across all languages
- **Unified architecture**: Common lib/app/test structure across all languages
- **Polyglot development**: Each language can be developed independently
- **Integration focus**: Cross-language compatibility and testing
- **Extensible design**: Architecture supports adding new system components beyond seqvar
