# Source Code

The `src/` directory contains the implementation code for all SeqWeb components.

## Structure

Each language has its own directory with a consistent internal structure:
- **app/**: Language-specific applications (CLI, web services, etc.)
- **lib/**: Shared libraries and core functionality
- **test/**: Language-specific tests

## Language Directories

- **java/**: Java implementations
- **python/**: Python implementations
- **cl/**: Common Lisp implementations  
- **bash/**: Shell script implementations

## Components

- OEIS data parsers
- RDF conversion pipelines
- Ontology management tools
- Data validation utilities
- Web services and APIs

## SeqVar Facility

# Created by Waldo 2025-08-20

```
seqwebcode/
└─ src/
   ├─ python/
   │  ├─ app/
   │  │  └─ cli/
   │  │     └─ seqwebdev
   │  ├─ lib/
   │  │  ├─ __init__.py
   │  │  ├─ seqvar/
   │  │  │  ├─ __init__.py
   │  │  │  ├─ seqvar.py         # get/set (assumes DB exists)
   │  │  │  └─ seqvar_toml.py    # load TOML → dict[str,str]; write dict → DB
   │  │  └─ ...
   │  └─ test/
   │     ├─ test_seqvar.py
   │     └─ show_seqwebdev.py
   ├─ java/
   │  ├─ app/
   │  ├─ lib/
   │  │  └─ seqvar/
   │  │     ├─ SeqVar.java       # get/set (assumes DB exists)
   │  │     └─ SeqvarToml.java   # load TOML; write to DB
   │  └─ test/
   ├─ cl/
   │  ├─ app/
   │  ├─ lib/
   │  │  ├─ seqvar.lisp          # get/set (assumes DB exists)
   │  │  └─ seqvar-toml.lisp     # read TOML; write to DB
   │  └─ test/
   └─ bash/
      ├─ app/
      ├─ lib/
      │  ├─ seqvar.sh            # get/set (assumes DB exists) — reference
      │  └─ seqvar_toml.sh       # calls out to python to load TOML — reference
      └─ test/
```

## SeqVar Facility Common Contract (All Languages)

- **DB path**: `$SEQWEBDEV_HOME/.state/seqvar/seqvar.db`
- **Table**: `seqvars(ns TEXT NOT NULL, key TEXT NOT NULL, val TEXT NOT NULL DEFAULT '', src TEXT, ts INTEGER NOT NULL, PRIMARY KEY(ns,key))`
- **Defaults**: `ns="SeqVar"`, `src="seqweb"`
- **get(key, ns)**: always returns a string (empty `""` if missing)
- **set(key, val, ns, src)**: upsert; no schema creation here
- **On missing DB/table**: throw/raise a clear error:
  > "seqvar store not initialized (missing seqvar.db or seqvars table)"

## TOML "Sidecar" for SeqVar Facility

### Common Contract (All Languages):
- **Flattening**: TOML tables become dotted keys (`[service] host="..."` → `service.host = "..."`)
- **Strings only**: sidecars stringify non-string TOML values (keeps seqvar DB uniform)
- **APIs to expose** (same names everywhere):
  - `load_toml(paths: list|array)` → `map/dict<string,string>`
  - `write_toml_to_seqvar(bindings, ns="SeqVar", src="seqweb")` → `None`

### Language-specific implementation/dependency notes:
- **Python**: use stdlib tomllib (read-only, 3.11+)
- **Java**: tiny dep org.tomlj:tomlj (read-only)
- **Lisp**: cl-toml 
- **Bash**: avoid parsing TOML; call Python helper from scripts if needed

## Testing

Tests are co-located with their respective language implementations:
- **Python tests**: `src/python/test/`
- **Java tests**: `src/java/test/`
- **CL tests**: `src/cl/test/`
- **Bash tests**: `src/bash/test/`


## Build Dependencies Documentation

The SeqWeb polyglot system maintains dependency documentation via separate `dependencies.md` files for each language, along with a common/cross-cutting overview doc at the `src/` level.

### Language-Specific Dependencies

Each language implementation has its own detailed dependency documentation:

- **`src/python/dependencies.md`** - Python-specific dependencies, package structure, and testing requirements
- **`src/java/dependencies.md`** - Java dependencies, Maven/Gradle configuration, and build instructions  
- **`src/cl/dependencies.md`** - Common Lisp Quicklisp dependencies and ASDF system definitions
- **`src/bash/dependencies.md`** - Bash system requirements and Python delegation strategy

### Common Dependencies Overview

**`src/dependencies.md`** contains:
- Cross-cutting dependencies (SQLite, environment variables)
- Polyglot architecture requirements
- Common contracts and schemas
- Integration testing dependencies
- Build order and troubleshooting


## Current Status

*Status as of 2025-08-27*

### Implementation Status Summary

- **Python**: ✅ **COMPLETE** - Fully functional with built-in libraries
- **Java**: 🟡 **IMPLEMENTATION COMPLETE** - Requires external libraries
- **Common Lisp**: ✅ **IMPLEMENTATION COMPLETE** - Fully functional with well-established libraries
- **Bash**: ✅ **IMPLEMENTATION COMPLETE** - Fully functional with Python delegation

### Detailed Status by Language

#### Python Implementation ✅ COMPLETE
**Location**: `src/python/lib/seqvar/`
**Status**: Production ready
**Dependencies**: Built-in libraries only (sqlite3, tomllib)
**Testing**: ✅ All functionality verified working
**Features Working**:
- Core seqvar operations (get, set, dump, getDict)
- TOML file loading and parsing
- SQLite database management
- Source tracking and timestamps
- CLI integration

#### Java Implementation 🟡 IMPLEMENTATION COMPLETE
**Location**: `src/java/lib/seqvar/`
**Status**: Code complete, requires external libraries
**Dependencies**: SQLite JDBC driver, TOML library
**Testing**: ❌ Cannot run due to missing dependencies
**What's Working**:
- Complete Java class structure
- All seqvar functionality implemented
- Proper exception handling
- Database schema management
**What's Blocking Progress**:
- Missing SQLite JDBC driver
- Missing TOML parsing library

#### Common Lisp Implementation ✅ IMPLEMENTATION COMPLETE
**Location**: `src/cl/lib/seqvar/`
**Status**: Fully implemented and production-ready
**Dependencies**: cl-sqlite, cl-toml, alexandria, fiveam
**Features Working**:
- Core seqvar operations (get, set, dump, get_dict)
- TOML file handling with flattening
- SQLite database integration via cl-sqlite
- Source tracking and timestamps
- Pattern matching with SQLite LIKE
- Comprehensive test suite with fiveam
- ASDF system definition

#### Bash Implementation ✅ IMPLEMENTATION COMPLETE
**Location**: `src/bash/lib/seqvar/`
**Status**: Fully implemented and functional
**Dependencies**: sqlite3, python3 (for TOML delegation)
**Features Working**:
- Core seqvar operations (get, set, dump, get_dict)
- TOML file handling (delegates to Python)
- SQLite database integration
- Source tracking and timestamps
- Pattern matching with SQLite LIKE
- Comprehensive test suite

### External Library Requirements

#### Python
- ✅ sqlite3 (built-in)
- ✅ tomllib (built-in, Python 3.11+)

#### Java
- ❌ SQLite JDBC driver (org.xerial:sqlite-jdbc:3.42.0.0)
- ❌ TOML parser (org.tomlj:tomlj:1.1.0)

#### Common Lisp
- ✅ cl-sqlite (SQLite database connectivity)
- ✅ cl-toml (TOML parsing)
- ✅ alexandria (utility functions)
- ✅ fiveam (testing framework)

#### Bash
- ✅ sqlite3 (system command)
- ✅ python3 (for TOML parsing delegation)

### Development Phases

#### Phase 1: Python Foundation ✅ COMPLETE
- Establish lib/app directory structure
- Implement working seqvar system
- Verify CLI integration
- Document patterns and architecture

#### Phase 2: Language-Specific Implementation 🟡 IN PROGRESS
- Java: Implementation complete, needs dependencies
- CL: ✅ Implementation complete and production-ready
- Bash: ✅ Implementation complete and functional

#### Phase 3: Integration and Testing ❌ NOT STARTED
- Cross-language compatibility testing
- Performance benchmarking
- Error handling consistency
- Documentation standardization

### Next Steps

#### Immediate (Next Session)
1. **Research CL and Bash options** for seqvar implementation
2. **Document library requirements** for each language
3. **Plan dependency management** approach

#### Short-term (Next 1-2 Sessions)
1. **Implement CL seqvar** if feasible
2. **Implement Bash seqvar** if feasible
3. **Add comprehensive testing** for all implementations

### Key Learnings Applied

1. **File Management**: `cat` is reliable, `edit_file` causes rework
2. **Status Documentation**: Leave comprehensive TODO notes for incomplete work
3. **External Libraries**: Use standards, don't roll your own
4. **Testing**: Verify functionality before proceeding
5. **Architecture**: Plan for scalability and polyglot development

### Commands That Work

```bash
# From seqweb directory
seqwebdev help                    # Show available commands
seqwebdev vars                    # Display seqvar contents
seqwebdev get <key>              # Get seqvar value
seqwebdev set <key> <value>      # Set seqvar value

# From seqwebcode directory
python3 src/python/test/test_seqvar.py  # Run Python tests
javac -cp src/java src/java/lib/seqvar/*.java  # Compile Java
```

### Questions for Future Sessions

1. **CL Implementation**: What CL SQLite and TOML libraries are available?
2. **Bash Implementation**: How to integrate SQLite and TOML with Bash?
3. **Dependency Management**: When to transition to Gradle vs. language-native tools?
4. **Testing Framework**: How to implement cross-language testing?
5. **Performance**: What performance characteristics are acceptable for each language?

**Recommendation**: Continue with CL and Bash implementation research, focusing on feasibility and library availability before committing to full implementation.
