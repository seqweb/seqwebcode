#!/usr/bin/env python3
"""
Base command class for SeqWeb CLI commands
"""

import argparse
from abc import ABC, abstractmethod
from typing import List


class BaseCommand(ABC):
    """Base class for all SeqWeb commands"""

    # All commands are enabled by default. To disable a specific command,
    # override this in the subclass by adding: is_seqwebdevcommand = False
    is_seqwebdevcommand = True

    @property
    @abstractmethod
    def name(self) -> str:
        """Command name"""
        pass

    @property
    @abstractmethod
    def description(self) -> str:
        """Command description"""
        pass

    @property
    @abstractmethod
    def help_text(self) -> str:
        """Detailed help text"""
        pass

    def create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser for this command"""
        parser = argparse.ArgumentParser(
            description=self.description
        )
        self.add_arguments(parser)
        return parser

    def add_arguments(self, parser: argparse.ArgumentParser):
        """Add command-specific arguments to parser"""
        # Override in subclasses
        pass

    def run(self, args: List[str]):
        """Run the command with given arguments"""
        # First argument is the command name, skip it for argparse
        parser = self.create_parser()
        parsed_args = parser.parse_args(args[1:])
        self.execute(parsed_args)

    @abstractmethod
    def execute(self, args):
        """Execute the command with parsed arguments"""
        pass
