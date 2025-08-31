#!/usr/bin/env python3
"""
Get subcommand for SeqVar - retrieves seqvar values
"""

from ...base_command import BaseCommand


class GetCommand(BaseCommand):
    """Get a seqvar value by key"""

    # This command has no subcommands
    has_subcommands: bool = False
    
    @property
    def name(self) -> str:
        return "get"
    
    @property
    def description(self) -> str:
        return "Get a seqvar value by key"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev seqvar get <key>
  Get a seqvar value by key. The value is printed quoted on a single line.
  Example: seqwebdev seqvar get seqwebdev.home"""
    
    def do_initializations(self):
        """Command-specific initialization."""
        pass
    
    def do_command(self):
        """Get a seqvar value by key."""
        if not self.args:
            print("❌ Error: No key specified")
            print("Usage: seqwebdev seqvar get <key>")
            return
        
        key = self.args[0]
        try:
            # ToDo: Implement actual seqvar get functionality
            # For now, just show what we would do
            print(f"Would get seqvar '{key}' (implementation pending)")
            print("Note: This will integrate with lib.seqvar.seqvar.get when available")
        except Exception as e:
            print(f"❌ Error getting seqvar '{key}': {e}")


# Entry point - generic, no command-specific names
if __name__ == "__main__":
    from ...base_command import auto_run_command
    auto_run_command()
