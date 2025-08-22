#!/usr/bin/env python3
"""
SeqWeb CLI Dispatcher
"""

import sys
# from pathlib import Path
# from typing import Dict, Type

# from lib.base import BaseCommand
from .registry import load_command
from .setup import setup_for_cli


def main():
    """Main CLI entry point"""
    # Set up CLI environment first
    setup_for_cli()
    
    # Get available commands
    # available_commands = get_available_commands()

    if len(sys.argv) < 2:
        # print("❌ Missing command")
        print("  Run 'seqwebdev help' for a list of available commands")
        sys.exit(1)

    command_name = sys.argv[1]
    command_args = sys.argv[1:]  # Include command name as first arg

    # Load and execute command
    try:
        command_class = load_command(command_name)
        command = command_class()
        command.run(command_args)
    except ImportError:
        print(f"❌ Unknown command: {command_name}")
        print("  Run 'seqwebdev help' for a list of available commands")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Error executing command '{command_name}': {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
