#!/usr/bin/env python3
"""
Base Command Class for SeqWeb Chaining CLI

This class implements the core chaining logic that all commands inherit.
It provides the uniform two-method structure for command implementations.
"""

import sys
import re
import inspect
import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from typing import List


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
    
    Implements the chaining CLI pattern:
    1. do_initializations() - Command-specific setup (abstract)
    2. find_and_call_subcommand() - Try to find and call subcommand
    3. do_command() - Command-specific functionality (abstract)
    
    Override has_subcommands to indicate if the command supports subcommands.
    """
    
    ## Methods to be supplied/overridden by command implementation

    # Override to return True when the command supports subcommands
    has_subcommands: bool = False
    
    @abstractmethod
    def do_initializations(self):   # this runs before any subcommand code
        """Override with command-specific initialization."""
        pass
    
    @abstractmethod
    def do_command(self):   # this gets run if no subcommand preempts control 
        """Override with command-specific functionality."""
        pass

    ## Methods implementing the generic chaining CLI framework

    def __init__(self):
        self.args = sys.argv[1:]  # Skip command name
        
        # Check for help mode
        self.help_mode = "--help" in self.args
        if self.help_mode:
            # Remove --help from args for normal processing
            self.args = [arg for arg in self.args if arg != "--help"]
        
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
    
    def _execute_subcommand(self, subcommand_file: Path, subcommand_name: str) -> bool:
        """Helper method to execute a subcommand if the file exists."""
        if not subcommand_file.exists():
            return False
        
        try:
            # Simple approach: convert file path to Python module name
            # Convert slashes to dots and remove .py extension
            chain_root = Path(__file__).parent  # /chain/
            relative_path = subcommand_file.relative_to(chain_root)  # seqwebdev/hello.py
            module_name = "chain." + str(relative_path).replace("/", ".").replace(".py", "")
            
            # Use Python's built-in import mechanism
            module = __import__(module_name, fromlist=[subcommand_name])
            
            # Convert subcommand name to PascalCase for class name
            # e.g., "var" -> "VarCommand", "hello" -> "HelloCommand"
            command_class_name = f"{subcommand_name.capitalize()}Command"
            command_class = getattr(module, command_class_name)
            
            # Create instance and call with remaining args
            subcommand_instance = command_class()
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
        """Detailed help text - automatically includes subcommands for group commands"""
        base_help = f"{self.name} - {self.description}"
        
        if self.has_subcommands:
            subcommands = self._discover_subcommands()
            if subcommands:
                base_help += "\n\nAvailable subcommands:"
                for cmd in subcommands:
                    base_help += f"\n  {cmd['name']:<15} {cmd['description']}"
                base_help += f"\n\nFor detailed usage: {self.name} <subcommand> --help"
        
        base_help += f"\n\nFor full documentation: {self._get_docs_url()}"
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
    
    # Subcommand discovery (for help_text)
    def _discover_subcommands(self) -> List[dict]:
        """Discover subcommands - called only for help_text generation"""
        subcommands = []
        
        # Quick file system scan without imports
        for py_file in self.command_dir.glob("*.py"):
            if py_file.name != "__init__.py":
                name = py_file.stem
                info = self._extract_subcommand_info(py_file, name)
                subcommands.append(info)
        
        # Folder-based subcommands
        for folder in self.command_dir.iterdir():
            if folder.is_dir() and (folder / f"{folder.name}.py").exists():
                name = folder.name
                info = self._extract_subcommand_info(folder / f"{folder.name}.py", name)
                subcommands.append(info)
        
        return subcommands
    
    def _extract_subcommand_info(self, file_path: Path, name: str) -> dict:
        """Extract basic info from subcommand file without full import"""
        try:
            # Quick docstring extraction
            with open(file_path, 'r') as f:
                content = f.read()
                # Simple extraction of class docstring
                if f'class {name.capitalize()}Command' in content:
                    # Extract first line of docstring if available
                    lines = content.split('\n')
                    for i, line in enumerate(lines):
                        if f'class {name.capitalize()}Command' in line:
                            # Look for docstring in next few lines
                            for j in range(i+1, min(i+5, len(lines))):
                                if '"""' in lines[j]:
                                    desc = lines[j].strip().strip('"').strip("'")
                                    return {"name": name, "description": desc}
                    return {"name": name, "description": "No description available"}
        except:
            pass
        return {"name": name, "description": "No description available"}
    

    def do_help(self):
        """Display help for this command and its subcommands."""
        # Print the command's detailed help_text
        print(self.help_text)
        print()
        
        # Next print a listing of its subcommands, if any. Each subcommand in the list is presented on one line with the following contents, in left-aligned columns:
        # - the canonical name of the subcommand
        #   - possibly suffixed with an ellipsis … when the command has sub-subcommands
        # - the subcommand's short description text
        if self.has_subcommands:
            subcommands = self._discover_subcommands()
            if subcommands:
                print("Available subcommands:")
                for cmd in subcommands:
                    # Add ellipsis for commands with sub-subcommands
                    suffix = "…" if self._has_subsubcommands(cmd['name']) else ""
                    name_with_suffix = f"{cmd['name']}{suffix}"
                    print(f"  {name_with_suffix:<20} {cmd['description']}")
                print()
                print(f"Use '{self.name} <subcommand> --help' for more information.")
    
    def _has_subsubcommands(self, subcommand_name: str) -> bool:
        """Check if a subcommand has sub-subcommands."""
        # Check if the subcommand is a folder case (has sub-subcommands)
        subcommand_dir = self.command_dir / subcommand_name
        subcommand_file = subcommand_dir / f"{subcommand_name}.py"
        return subcommand_dir.is_dir() and subcommand_file.exists()
