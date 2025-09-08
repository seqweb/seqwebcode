#!/usr/bin/env python3
"""
Base Command Class for SeqWeb Chaining CLI

This class implements the core chaining logic that all commands inherit.
It provides a uniform two-method override structure for command implementations.
"""

import sys
import re
import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from typing import List


class BaseCommand(ABC):
    """
    Base class for all SeqWeb CLI commands.

    Implements the chaining CLI pattern:
    1. do_initializations() - Command-specific setup (abstract)
    2. find_and_call_subcommand() - Try to find and call subcommand
    3. do_command() - Command-specific functionality (abstract)

    Override has_subcommands to indicate if the command supports subcommands.
    """

    # Methods to be supplied/overridden by command implementation

    # Override to return True when the command supports subcommands
    has_subcommands: bool = False

    def do_initializations(self):   # this runs before any subcommand code
        """Override with command-specific initialization."""
        pass

    def do_command(self):   # this gets run if no subcommand preempts control
        """Override with command-specific functionality."""
        self.do_help()

    # Methods implementing the generic chaining CLI framework

    def __init__(self):
        self.args = sys.argv[1:]  # Skip command name

        # Check for help mode
        self.help_mode = "--help" in self.args

        # Capture the directory where THIS command is defined
        # Use the class's own module file location, not the calling frame
        module = sys.modules[self.__class__.__module__]
        self.command_dir = Path(module.__file__).parent

    def main(self):
        """Main entry point following chaining CLI pattern."""
        # 1. Do command-specific initializations (unless in help mode)
        if not self.help_mode:
            self.do_initializations()

        # 2. Try to find and call subcommand
        if self.find_and_call_subcommand():
            return  # Subcommand handled everything

        # 3. No subcommand found - branch based on help mode
        if self.help_mode:
            self.do_help()
        else:
            self.do_command()

    def find_and_call_subcommand(self) -> bool:
        """Command-invariant: find and call subcommand if it exists."""
        # Gating process: check if command supports subcommands
        if not self.has_subcommands:
            return False

        # Check if there's another CLI argument
        if not self.args:
            return False

        # Check if there's a canonical form of the argument
        canonical_name = self.canonical_subcommand_name(self.args[0])
        if canonical_name is None:
            return False

        # Try to find and run that canonical name
        subcommand_filename = f"{canonical_name}.py"

        # Check for file flavor subcommand (S.py) in the command's own directory
        file_path = self.command_dir / subcommand_filename
        if self._execute_subcommand(file_path, canonical_name):
            return True

        # Check for folder flavor subcommand (S/S.py) in the command's own directory
        folder_path = self.command_dir / canonical_name / subcommand_filename
        return self._execute_subcommand(folder_path, canonical_name)

    def canonical_subcommand_name(self, name: str) -> str | None:
        """
        Validate and canonicalize a subcommand name.

        Rules:
        - Must start with alpha (a-z, A-Z)
        - Followed by alphanumeric + underscores (a-z, A-Z, 0-9, _)
        - No leading underscores (eliminates Python magic files)
        - Returns lowercase canonical form if valid, None if invalid
        """
        if not re.match(r'^[a-zA-Z][a-zA-Z0-9_]*$', name):
            return None
        return name.lower()

    def _path_to_module_name(self, file_path: Path) -> str:
        """
        Convert a file path to a Python module name.
        
        Args:
            file_path: Path to the Python file
            
        Returns:
            Module name suitable for import (e.g., "seqwebdev.hello")
        """
        cli_root = Path(__file__).parent  # /cli/
        relative_path = file_path.relative_to(cli_root)  # seqwebdev/hello.py
        module_name = str(relative_path).replace("\\", ".").replace("/", ".")
        
        if module_name.endswith(".py"):
            module_name = module_name[:-3]
        else:
            # This shouldn't happen in the current CLI design, but log it for debugging
            print(f"⚠️  Warning: Unexpected subcommand file without .py extension: {relative_path}")
        
        return module_name

    def _import_and_instantiate_command(self, file_path: Path, command_name: str):
        """
        Import a command module and instantiate its command class.
        
        Args:
            file_path: Path to the Python file containing the command
            command_name: Name of the command (e.g., "hello", "var")
            
        Returns:
            Tuple of (module, command_instance) or (None, None) if import fails
        """
        try:
            # Convert file path to Python module name
            module_name = self._path_to_module_name(file_path)
            
            # Import the module
            module = __import__(module_name, fromlist=[command_name])
            
            # Convert command name to PascalCase for class name
            # e.g., "var" -> "VarCommand", "hello" -> "HelloCommand"
            command_class_name = f"{command_name.capitalize()}Command"
            command_class = getattr(module, command_class_name)
            
            # Create instance
            command_instance = command_class()
            
            return module, command_instance
            
        except (ImportError, AttributeError, Exception) as e:
            # Log the error for debugging but don't crash
            print(f"❌ Error importing command {command_name}: {e}")
            return None, None

    def _execute_subcommand(self, subcommand_file: Path, subcommand_name: str) -> bool:
        """Helper method to execute a subcommand if the file exists."""
        if not subcommand_file.exists():
            return False

        try:
            # Import and instantiate the command
            module, subcommand_instance = self._import_and_instantiate_command(subcommand_file, subcommand_name)
            
            if subcommand_instance is None:
                return False

            # Set up and execute the command
            subcommand_instance.args = self.args[1:]  # Pass remaining arguments
            subcommand_instance.main()
            return True

        except (ImportError, AttributeError) as e:
            print(f"Error loading subcommand '{subcommand_name}': {e}")
            return False

    # Help system properties
    @property
    @abstractmethod
    def name(self) -> str:
        """Command name"""
        pass

    @property
    @abstractmethod
    def description(self) -> str:
        """Command description - terse, good for lists"""
        pass

    @property
    def help_text(self) -> str:
        """Default help text"""
        base_help = f"{self.name} - {self.description}"
        return base_help

    # Argument parsing helpers
    def create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser for this command"""
        parser = argparse.ArgumentParser(description=self.description)
        self.add_arguments(parser)
        return parser

    def add_arguments(self, parser: argparse.ArgumentParser):
        """Add command-specific arguments to parser"""
        # Override in subclasses
        pass

    def _extract_subcommand_info(self, file_path: Path, name: str) -> dict | None:
        """Extract basic info from subcommand by importing and calling its description property"""
        # First canonicalize the name - if invalid, return None
        canonical_name = self.canonical_subcommand_name(name)
        if canonical_name is None:
            return None

        try:
            # Import and instantiate the command
            module, command_instance = self._import_and_instantiate_command(file_path, canonical_name)
            
            if command_instance is None:
                return None

            # Get description from the instance
            description = command_instance.description

            return {"name": canonical_name, "description": description}

        except (ImportError, AttributeError, Exception):
            # Fallback to a default description if import fails
            return {"name": canonical_name, "description": "No description available"}

    # Subcommand discovery (for help_text)
    def _discover_subcommands(self) -> List[dict]:
        """Discover subcommands - called only for help_text generation"""
        subcommands = []

        # Single loop through directory contents
        for item in self.command_dir.iterdir():
            name = None
            file_path = None

            if item.is_dir():
                # Folder case: S/S.py
                path = (item / f"{item.name}.py")
                if path.exists():
                    name = item.name
                    file_path = path
            else:
                # File case: S.py
                if item.suffix == '.py':
                    name = item.stem
                    # Skip if this file is the current command itself
                    if name == self.name:
                        continue
                    file_path = item

            # Only process if we found a valid subcommand
            if name is not None and file_path is not None:
                info = self._extract_subcommand_info(file_path, name)
                if info is not None:  # Skip invalid subcommand names
                    subcommands.append(info)

        # Sort subcommands alphabetically by name
        subcommands.sort(key=lambda cmd: cmd['name'])
        return subcommands

    def _has_subsubcommands(self, subcommand_name: str) -> bool:
        """Check if a subcommand has sub-subcommands."""
        # Check if the subcommand is a folder case (has sub-subcommands)
        subcommand_dir = self.command_dir / subcommand_name
        subcommand_file = subcommand_dir / f"{subcommand_name}.py"
        return subcommand_dir.is_dir() and subcommand_file.exists()

    def do_help(self):
        """Display help for this command and its subcommands."""
        # Print the command's description followed by detailed help_text
        print(self.description)
        print(self.help_text)

        # Then append a listing of any subcommands. Each subcommand in the list is presented on one line
        # with the following contents, in left-aligned columns:
        # - the canonical name of the subcommand
        #   - possibly suffixed with an ellipsis … when the command has sub-subcommands
        # - the subcommand's short description text
        if self.has_subcommands:
            subcommands = self._discover_subcommands()
            if subcommands:
                print("Subcommands:")
                for cmd in subcommands:
                    # Add ellipsis for commands with sub-subcommands
                    suffix = "…" if self._has_subsubcommands(cmd['name']) else ""
                    name_with_suffix = f"{cmd['name']}{suffix}"
                    print(f"  {name_with_suffix:<20} {cmd['description']}")
                print(f"Use '{self.name} <subcommand> --help' for more information.")

