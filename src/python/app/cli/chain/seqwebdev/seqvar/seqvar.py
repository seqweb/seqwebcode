#!/usr/bin/env python3
"""
Seqvar command for SeqWeb CLI - manages seqvar values

This is a group command that delegates to get and set subcommands.
"""

from ...base_command import BaseCommand


class SeqvarCommand(BaseCommand):
    """Manage seqvar values (get, set)"""

    # This command has subcommands
    has_subcommands: bool = True
    
    @property
    def name(self) -> str:
        return "seqvar"
    
    @property
    def description(self) -> str:
        return "Manage seqvar values"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev seqvar <command> [args...]
  Manage seqvar values in the SeqWeb environment.
  
  Commands:
    get <key>     Get a seqvar value by key
    set <key> <value>  Set a seqvar value by key
  
  Examples:
    seqwebdev seqvar get seqwebdev.home
    seqwebdev seqvar set my.key "my value\""""
    
    def do_initializations(self):
        """Command-specific initialization."""
        pass
    
    def do_command(self):
        """Show help when no subcommand specified."""
        print("Seqvar Command")
        print("==============")
        print()
        print("Available subcommands:")
        print("  get <key>     Get a seqvar value by key")
        print("  set <key> <value>  Set a seqvar value by key")
        print()
        print("Use 'seqwebdev seqvar <command> --help' for more information.")


# Entry point - generic, no command-specific names
if __name__ == "__main__":
    from ...base_command import auto_run_command
    auto_run_command()
