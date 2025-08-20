# Build Dependencies for SeqVar Facility

This document outlines the dependencies required to build and run the seqvar facility implementations across all supported languages.

## Overview

The seqvar facility provides a unified key-value configuration system with TOML file support. Each language implementation has specific dependencies that need to be resolved for successful compilation and execution.

## Python Dependencies

### Core Dependencies
- **Python 3.11+**: Required for `tomllib` standard library module
- **No external packages required**: All dependencies are in the Python standard library

### Development Dependencies (Optional)
```
# For development and testing
pytest
black
flake8
mypy
```

### Package Structure
```
seqwebcode/src/python/
├── pyproject.toml          # Package configuration
├── seqvar/
│   ├── __init__.py        # Package exports
│   ├── seqvar.py          # Core get/set functionality
│   └── seqvar_toml.py     # TOML parsing and integration
```

## Java Dependencies

### Core Dependencies
- **Java 8+**: Required for Path, Files, and other modern Java features
- **SQLite JDBC Driver**: For database connectivity
- **org.tomlj:tomlj**: For TOML parsing

### Maven Dependencies
```xml
<dependencies>
    <!-- TOML parsing -->
    <dependency>
        <groupId>org.tomlj</groupId>
        <artifactId>tomlj</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- SQLite JDBC driver -->
    <dependency>
        <groupId>org.xerial</groupId>
        <artifactId>sqlite-jdbc</artifactId>
        <version>3.42.0.0</version>
    </dependency>
</dependencies>
```

### Gradle Dependencies
```gradle
dependencies {
    implementation 'org.tomlj:tomlj:1.0.0'
    implementation 'org.xerial:sqlite-jdbc:3.42.0.0'
}
```

### Package Structure
```
seqwebcode/src/java/
└── seqvar/
    ├── SeqVar.java         # Core get/set functionality
    └── SeqvarToml.java     # TOML parsing and integration
```

## Common Lisp Dependencies

### Core Dependencies
- **cl-toml**: For TOML parsing
- **alexandria**: For utility functions (file reading)
- **UIOP**: For process management (usually included with ASDF)

### Quicklisp Dependencies
```lisp
(ql:quickload :cl-toml)
(ql:quickload :alexandria)
```

### ASDF System Definition
```lisp
(asdf:defsystem :seqvar
  :depends-on (:cl-toml :alexandria)
  :components ((:file "seqvar")
               (:file "seqvar-toml")))
```

### Package Structure
```
seqwebcode/src/cl/
├── seqvar.lisp             # Core get/set functionality
└── seqvar-toml.lisp        # TOML parsing and integration
```

## Bash Dependencies

### Core Dependencies
- **Bash 4.0+**: For advanced shell features
- **sqlite3**: Command-line SQLite tool
- **python3**: For TOML parsing (delegates to Python implementation)

### System Requirements
```bash
# Ubuntu/Debian
sudo apt-get install sqlite3 python3

# macOS
brew install sqlite3 python3

# CentOS/RHEL
sudo yum install sqlite python3
```

### Package Structure
```
seqwebcode/src/bash/
├── seqvar.sh               # Core get/set functionality
└── seqvar_toml.sh          # TOML integration (Python delegation)
```

## Database Dependencies

### SQLite Requirements
- **SQLite 3.24+**: Required for `ON CONFLICT` clause support
- **WAL mode**: Enabled for better concurrency
- **File permissions**: Write access to `$SEQWEBDEV_HOME/.state/` directory

### Schema
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

## Environment Variables

### Required
- **SEQWEBDEV_HOME**: Base directory for SeqWeb development environment
- **PATH**: Must include sqlite3 and python3 executables

### Optional
- **SEQWEBDEV_DEBUG**: Enable debug logging
- **SEQWEBDEV_LOG_LEVEL**: Set logging verbosity

## Build Order

1. **Bootstrap**: Initialize SQLite database and schema
2. **Python**: Install Python package in development mode
3. **Java**: Resolve Maven/Gradle dependencies
4. **Common Lisp**: Install Quicklisp dependencies
5. **Bash**: Ensure system dependencies are available

## Testing Dependencies

### Python
```
pytest
pytest-cov
```

### Java
```
JUnit 5
Mockito
```

### Common Lisp
```
fiveam
```

### Integration
```
sqlite3 (for database inspection)
```

## Notes

- **Python**: Most self-contained, minimal external dependencies
- **Java**: Requires external TOML and SQLite JDBC libraries
- **Common Lisp**: Depends on Quicklisp ecosystem packages
- **Bash**: Minimal dependencies, delegates complex operations to Python
- **All implementations**: Share the same SQLite database schema and error messages

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

# Test Python
python3 -c "import tomllib; print('TOML support OK')"

# Test Java
java -version

# Test Common Lisp
sbcl --eval "(ql:quickload :cl-toml)" --quit
```
