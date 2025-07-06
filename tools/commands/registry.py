#!/usr/bin/env python3
"""
Command registry for discovering and loading SeqWeb commands
"""

import importlib
import inspect
from pathlib import Path
from typing import Dict, Type

from .base import BaseCommand


def get_available_commands() -> Dict[str, Type[BaseCommand]]:
    """Get all available commands"""
    commands = {}
    commands_dir = Path(__file__).parent
    
    for py_file in commands_dir.glob("*.py"):
        if py_file.name in ["__init__.py", "base.py", "registry.py"]:
            continue
            
        module_name = py_file.stem
        try:
            module = importlib.import_module(f"commands.{module_name}")
            
            # Find command classes in the module
            for name, obj in inspect.getmembers(module):
                if (inspect.isclass(obj) and 
                    issubclass(obj, BaseCommand) and 
                    obj != BaseCommand):
                    commands[module_name] = obj
                    break
                    
        except ImportError as e:
            print(f"Warning: Could not load command {module_name}: {e}")
    
    return commands


def load_command(command_name: str) -> Type[BaseCommand]:
    """Load a specific command by name"""
    commands = get_available_commands()
    
    if command_name not in commands:
        raise ImportError(f"Command '{command_name}' not found")
    
    return commands[command_name] 