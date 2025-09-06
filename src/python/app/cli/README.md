# SeqWeb CLI System

This directory contains the chaining CLI system for SeqWeb development. This document explains the architecture and maintenance guidelines for future developers.

## Entry Point Flow

The CLI system uses a three-tier entry point architecture:

1. **CLI Universal Entry Point** (aka `$SEQWEBDEV_HOME/seqwebdev`)
   - Bash script initially written by bootstrap that discovers the seqwebcode repository
   - Exports environment variables (`SEQWEBDEV_HOME`, `SEQWEBCODE_PATH`)
   - Delegates to the Implementation Entry Point
   - Later automatically overwritten by foothold with hard-coded paths to cut discovery overhead.

2. **CLI Implementation Entry Point** (`cli/_seqwebdev`)
   - Script that sets up the Python path to include all of `seqwebcode:src/python/`
   - Initially also runs additional environment "foothold" setup
   - Delegates to the "anchor" CLI command

3. **CLI Anchor Command** (`seqwebdev/seqwebdev.py`)
   - The root command of the chaining CLI system
   - Handles command discovery and delegation
   - Implements the main CLI interface

## Command Directory Structure

The CLI follows a hierarchical command structure that supports any number of levels of subcommands, subsubcommands, and so on.  Therefore, the command tree structure follows a recursive pattern:

```
cli/
├── _seqwebdev                     # Implementation entry point (executable)
├── base_command.py                # Base class for all commands
└── seqwebdev/                     # Command implementations directory
    ├── seqwebdev.py                    # Root "anchor" command implementation
    ├── hello.py                        # Simple subcommand (no subsubcommands)
    ├── run.py                          # Simple subcommand (no subsubcommands)
    ├── setup/                          # Group subcommand (has subsubcommands)
    │   ├── setup.py                        # Group subcommand implementation
    │   └── cursor.py                       # Subsubcommand
    └── var/                            # Group subcommand (has subsubcommands)
        ├── var.py                          # Group subcommand implementation
        ├── dict.py                         # Subsubcommand
        ├── dump.py                         # Subsubcommand
        ├── get.py                          # Subsubcommand
        └── set.py                          # Subsubcommand

(_as of 2025-09-06_)
```
Commands must have simple names that don't begin with an underscore `_`.  (Files like `__init__.py` are thus filtered out)

All command implementations inherit from `BaseCommand` and supply required properties to support the help facility an/or do specific operations.


- The "anchor" command `seqwebdev` is the root directory of the tree, `.../seqwebdev/`.
- A subcommand _without_ subsubcommands (aka a "leaf") is implemented by a _file_ with that name
- A subcommand _with_ subsubcommands (aka a "group") is represented by a _directory_ with that name, that contains
  - a `.py` _file_ with that group name (typically with a basic implementation)
  - other _files_ ***or*** _directories_ implementing its subsubcommands


## External References

The CLI system is referenced from several external locations:

### Bootstrap Process
- **File**: `seqwebcode/bootstrap`
- **Purpose**: Discovers seqwebcode repository and sets up environment
- **Reference**: Points to `_seqwebdev` entry point

### Foothold System
- **File**: `libs/core/foothold/foothold.py`
- **Purpose**: Generates the Universal Entry Point script
- **Reference**: Creates script that points to `_seqwebdev` entry point

### Universal Entry Point
- **File**: `/Users/mlb/Devo/seqweb/seqwebdev` (generated)
- **Purpose**: User-facing CLI entry point
- **Reference**: Points to `_seqwebdev` entry point

## Development Guidelines

### Adding New Commands

1. **Simple Command**: Create `seqwebdev/newcommand.py`
   ```python
   from app.cli.base_command import BaseCommand
   
   class NewCommandCommand(BaseCommand):
       has_subcommands: bool = False
       # ... implement required properties and methods
   ```

2. **Command Group**: Create `seqwebdev/newgroup/` directory with `newgroup.py`
   ```python
   from app.cli.base_command import BaseCommand
   
   class NewGroupCommand(BaseCommand):
       has_subcommands: bool = True
       # ... implement required properties and methods
   ```

3. **Subcommand**: Create `seqwebdev/newgroup/subcommand.py`
   ```python
   from app.cli.base_command import BaseCommand
   
   class SubcommandCommand(BaseCommand):
       has_subcommands: bool = False
       # ... implement required properties and methods
   ```

### Required Command Properties

All commands must implement:
- `name`: Command name (string)
- `description`: Short description (string)
- `help_text`: Detailed help text (string)
- `has_subcommands`: Whether command supports subcommands (boolean)

### Module Discovery

The `BaseCommand` class automatically discovers commands by:
1. Scanning the `seqwebdev/` directory for `.py` files and subdirectories
2. Converting file paths to Python module names
3. Dynamically importing and instantiating command classes

### Import Paths

- **Base Command**: `from app.cli.base_command import BaseCommand`
- **SeqVar Functions**: `from libs.core.seqvar.seqvar import get as seqvar_get`

## Maintenance Notes

### File Naming
- Entry point files (`_seqwebdev`) are executable and have no extension
- Command files use descriptive names matching their functionality
- All Python files must have proper shebang and docstrings

### Testing
- Test commands individually: `./seqwebdev <command> --help`
- Test command groups: `./seqwebdev <group> --help`
- Test subcommands: `./seqwebdev <group> <subcommand> --help`

### Common Issues
- **Import errors**: Check that all imports use correct paths after restructuring
- **Module discovery failures**: Verify file structure matches expected patterns
- **Command not found**: Ensure command file is in correct directory and inherits from BaseCommand

### Architecture Principles
- **"Leap without looking"**: Commands fail hard on missing preconditions
- **Velocity over rigor**: Manual testing, rapid iteration
- **Modular design**: Clear separation between support code and command implementations
- **Polyglot consistency**: Similar patterns across all supported languages

## History

This CLI system was restructured from a registry-based system to a chaining CLI system in September 2025. The old `commands/` directory was removed and replaced with the current hierarchical structure in `seqwebdev/`.
