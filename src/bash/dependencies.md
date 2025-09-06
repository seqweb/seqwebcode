# Bash Dependencies for SeqWeb System

This document outlines the dependencies required to build and run the Bash components of the SeqWeb polyglot system.

## Core Dependencies

- **Bash 4.0+**: For advanced shell features
- **sqlite3**: Command-line SQLite tool
- **python3**: For TOML parsing (delegates to Python implementation)

## System Requirements

```bash
# Ubuntu/Debian
sudo apt-get install sqlite3 python3

# macOS
brew install sqlite3 python3

# CentOS/RHEL
sudo yum install sqlite python3
```

## Package Structure

```
seqwebcode/src/bash/
├── app/                    # Bash applications (future)
├── libs/
│   ├── seqvar.sh               # Core get/set functionality
│   └── seqvar_toml.sh          # TOML integration (Python delegation)
└── test/
    ├── test_seqvar.sh           # Comprehensive test suite
    └── show_seqvar.sh           # Demonstration script
```

## Current System Components

The Bash implementation currently supports:

- **seqvar facility**: Core key-value configuration system with TOML integration

*Note: Additional Bash components, libraries, and applications are expected to be added as the SeqWeb system expands.*

## Build and Test

```bash
# Make scripts executable
chmod +x src/bash/libs/core/*.sh src/bash/test/*.sh

# Test SQLite integration
sqlite3 --version

# Test Python delegation
python3 -c "import tomllib; print('TOML support OK')"

# Run the test suite
src/bash/test/test_seqvar.sh

# Run the demonstration
src/bash/test/show_seqvar.sh
```

## Implementation Status

The Bash seqvar facility is now **fully implemented** and provides:

- **Core functions**: `seqvar_get`, `seqvar_set`, `seqvar_dump`, `seqvar_get_dict`
- **TOML integration**: Delegates to Python for TOML parsing as specified in design
- **Pattern matching**: Supports SQLite LIKE patterns for key filtering
- **Error handling**: Comprehensive error checking and user-friendly messages
- **Testing**: Full test suite with colored output and comprehensive coverage
- **Documentation**: Clear examples and demonstration scripts

## Notes

- **Minimal dependencies**: Uses system tools and Python delegation
- **Fully implemented**: All seqvar functionality now available
- **Python delegation**: Avoids complex TOML parsing in Bash as designed
- **Reference implementation**: Intended for developers to see how to use seqvar in Bash
- **Production ready**: Can be used in production scripts, though primarily intended as reference
- **Extensible**: Architecture supports adding new system components beyond seqvar
