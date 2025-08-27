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
├── lib/
│   ├── seqvar.sh               # Core get/set functionality
│   └── seqvar_toml.sh          # TOML integration (Python delegation)
└── test/                    # Bash tests (future)
```

## Current System Components

The Bash implementation currently supports:

- **seqvar facility**: Core key-value configuration system with TOML integration

*Note: Additional Bash components, libraries, and applications are expected to be added as the SeqWeb system expands.*

## Build and Test

```bash
# Make scripts executable
chmod +x src/bash/lib/*.sh

# Test SQLite integration
sqlite3 --version

# Test Python delegation
python3 -c "import tomllib; print('TOML support OK')"
```

## Notes

- **Minimal dependencies**: Uses system tools and Python delegation
- **Not implemented**: Implementation not yet started
- **Python delegation**: Avoids complex TOML parsing in Bash
- **Future ready**: Structure in place for applications and testing
- **Extensible**: Architecture supports adding new system components beyond seqvar
