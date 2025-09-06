#!/usr/bin/env python3
"""
Setup command for SeqWeb CLI - manages setup and configuration
"""

from app.cli.chain.base_command import BaseCommand


class SetupCommand(BaseCommand):
    """Setup and configuration commands"""

    # This command has subcommands
    has_subcommands: bool = True
    
    @property
    def name(self) -> str:
        return "setup"
    
    @property
    def description(self) -> str:
        return "Setup and configuration commands"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev setup <command> [args...]
  Examples:
    seqwebdev setup cursor
    seqwebdev setup cursor --help"""
