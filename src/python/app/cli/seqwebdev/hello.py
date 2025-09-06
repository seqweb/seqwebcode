#!/usr/bin/env python3
"""
Hello command for SeqWeb CLI - simple greeting command
"""

from app.cli.base_command import BaseCommand


class HelloCommand(BaseCommand):
    """Simple greeting command"""
    
    has_subcommands: bool = False
    
    @property
    def name(self) -> str:
        return "hello"
    
    @property
    def description(self) -> str:
        return "Print a greeting message"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev hello [what]
  Print a greeting message.
  Example: seqwebdev hello
  Example: seqwebdev hello "Universe" """
    
    def add_arguments(self, parser):
        parser.add_argument(
            "what",
            nargs="?",
            default="World",
            help="What to greet (default: World)"
        )
    
    def do_command(self):
        """Override with command-specific functionality."""
        # Use argparse for argument parsing
        parser = self.create_parser()
        args = parser.parse_args(self.args)
        
        print(f"Hello {args.what}!")

