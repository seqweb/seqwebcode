#!/usr/bin/env python3
"""
Base Command Class for SeqWeb Chaining CLI

This class implements the core chaining logic that all commands inherit.
It provides the uniform two-method structure for command implementations.
"""

import sys
import re
import inspect
from pathlib import Path
from abc import ABC, abstractmethod


def auto_run_command():
    """Automatically discover and run the command class in the current module."""
    # Get the current module
    current_module = sys.modules[__name__]
    
    # Find the class that inherits from BaseCommand
    for name, obj in inspect.getmembers(current_module):
        if (inspect.isclass(obj) and 
            issubclass(obj, BaseCommand) and 
            obj != BaseCommand):
            # Found it! Run the command
            obj.main()
            return
    
    raise RuntimeError("No command class found in module")


class BaseCommand(ABC):
    """
    Base class for all SeqWeb CLI commands.
    
    Follows the chaining CLI pattern:
    1. do_initializations() - Command-specific setup (abstract)
    2. find_and_call_subcommand() - Try to find and call subcommand
    3. do_command() - Command-specific functionality (abstract)
    
    Commands can override has_subcommands to indicate if they support subcommands.
    """
    
    # Override this to indicate if the command supports subcommands
    has_subcommands: bool = False
    
    def __init__(self):
        self.args = sys.argv[1:]  # Skip command name
        # Capture the directory where THIS command is defined
        self.command_dir = Path(__file__).parent
    
    def main(self):
        """Main entry point following chaining CLI pattern."""
        # 1. Do command-specific initializations
        self.do_initializations()
        
        # 2. Try to find and call subcommand
        if self.find_and_call_subcommand():
            return  # Subcommand handled everything
        
        # 3. No subcommand found - do this command's own work
        self.do_command()
    
    @abstractmethod
    def do_initializations(self):
        """Override with command-specific initialization."""
        pass
    
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
        
        # Check for file flavor subcommand (S.py)
        if self._execute_subcommand(self.command_dir / subcommand_filename, canonical_name):
            return True
        
        # Check for folder flavor subcommand (S/S.py)
        return self._execute_subcommand(self.command_dir / canonical_name / subcommand_filename, canonical_name)
    
    def canonical_subcommand_name(self, name: str) -> str | None:
        """
        Validate and canonicalize a subcommand name.
        
        Rules:
        - Must start with alpha (a-z, A-Z)
        - Followed by alphanumeric + underscores (a-z, A-Z, 0-9, _)
        - No leading underscores (eliminates Python magic files)
        - Returns lowercase canonical form if valid, None if invalid
        
        Examples:
        - 'Setup' -> 'setup'
        - 'OEIS' -> 'oeis'
        - 'my_command' -> 'my_command'
        - '1command' -> None (starts with digit)
        - '_private' -> None (starts with underscore)
        - 'my-command' -> None (contains hyphen)
        """
        if not re.match(r'^[a-zA-Z][a-zA-Z0-9_]*$', name):
            return None
        return name.lower()
    
    def _execute_subcommand(self, subcommand_file: Path, subcommand_name: str) -> bool:
        """Helper method to execute a subcommand if the file exists."""
        if not subcommand_file.exists():
            return False
        
        try:
            # Dynamic import of subcommand module
            module_name = f"chain.{subcommand_name}.{subcommand_name}"
            module = __import__(module_name, fromlist=[subcommand_name])
            command_class = getattr(module, f"{subcommand_name.capitalize()}Command")
            
            # Create instance and call with remaining args
            subcommand_instance = command_class()
            subcommand_instance.args = self.args[1:]  # Pass remaining arguments
            subcommand_instance.main()
            return True
            
        except (ImportError, AttributeError) as e:
            print(f"Error loading subcommand '{subcommand_name}': {e}")
            return False
    
    @abstractmethod
    def do_command(self):
        """Override with command-specific functionality."""
        pass
