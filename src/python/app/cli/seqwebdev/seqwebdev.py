#!/usr/bin/env python3
"""
SeqWeb Development CLI - Root Command

This is the root command for the SeqWeb development system.
It inherits from BaseCommand and follows the chaining CLI pattern.
"""

from app.cli.base_command import BaseCommand


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
        return "SeqWeb Development chaining CLI"

    @property
    def help_text(self) -> str:
        return "Say 'seqwebdev <commands> --help' for more information on a specific command."


def main():
    """Entry point function for the chaining CLI."""
    command = SeqWebDevCommand()
    command.main()
