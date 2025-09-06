# Python Dependencies for SeqWeb System

This document outlines the dependencies required to build and run the Python components of the SeqWeb polyglot system.

## Core Dependencies

- **Python 3.11+**: Required for standard library modules (such as `tomllib` and others)
- **No external packages required**: All dependencies are in the Python standard library

## Development Dependencies (Optional)

```
# For development and testing
pytest
pytest-cov
black
flake8
mypy
```

## Package Structure

```
seqwebcode/src/python/
├── app/
│   └── cli/
│       └── seqwebdev      # CLI application
├── libs/
│   ├── __init__.py        # Package exports
│   └── seqvar/
│       ├── __init__.py    # Package exports
│       ├── seqvar.py      # Core get/set functionality
│       └── toml.py        # TOML parsing and integration
└── test/
    ├── test_seqvar.py     # Unit tests
    └── show_seqwebdev.py  # Test utilities
```

## Current System Components

The Python implementation currently supports:

- **seqvar facility**: Core key-value configuration system with TOML integration

*Note: Additional Python components, libraries, and applications are expected to be added as the SeqWeb system expands.*

## Installation

```bash
# Install in development mode
cd seqwebcode/src/python
pip install -e .

# Run tests
python3 -m pytest test/
```

*Note: If a virtual environment is needed for isolated development, it can be recreated with:*
```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt  # if requirements file exists
```

## Notes

- **Most self-contained**: Minimal external dependencies
- **Built-in libraries**: Uses `sqlite3` and `tomllib` from standard library
- **CLI integration**: Full command-line interface available
- **Testing**: Comprehensive test suite with pytest
- **Extensible**: Architecture supports adding new system components beyond seqvar
