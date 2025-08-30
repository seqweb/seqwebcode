#!/usr/bin/env python3
"""
SeqWeb Development CLI - Root Command

This is the root command for the SeqWeb development system.
It inherits from BaseCommand and follows the chaining CLI pattern.
"""

from ..base_command import BaseCommand


class SeqWebDevCommand(BaseCommand):
    """
    Root command for seqwebdev CLI.
    
    Inherits from BaseCommand and only overrides the two required methods:
    1. do_initializations() - Command-specific setup
    2. do_command() - Command-specific functionality
    """
    
    # Root command supports subcommands
    has_subcommands: bool = True
    
    @property
    def name(self) -> str:
        return "seqwebdev"
    
    @property
    def description(self) -> str:
        return "SeqWeb Development CLI"
    
    @property
    def help_text(self) -> str:
        return "SeqWeb Development CLI - manages development environments and tools"
    
    def do_initializations(self):
        """Override with command-specific initialization."""
        # Root command sets up fundamental environment
        pass
    
    def do_command(self):
        """Override with command-specific functionality."""
        # ToDo: Root command shows help when no subcommand specified
        print("\nSeqWeb Development CLI")
        print("=====================")
        print("\nAvailable commands:")
        print("  help      - Show help information")
        print("  <to be continued>")
        print("\nUse 'seqwebdev <command> --help' for more information.")


def main():
    """Entry point function for the chaining CLI."""
    command = SeqWebDevCommand()
    command.main()


# Entry point - generic, no command-specific names
if __name__ == "__main__":
    from .base_command import auto_run_command
    auto_run_command()
