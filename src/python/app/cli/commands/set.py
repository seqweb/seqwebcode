#!/usr/bin/env python3
"""
Set command for SeqWeb CLI - sets seqvar values
"""

from ..base import BaseCommand
from lib.seqvar.seqvar import set as seqvar_set


class SetCommand(BaseCommand):
    """Set a seqvar value by key"""

    @property
    def name(self) -> str:
        return "set"

    @property
    def description(self) -> str:
        return "Set a seqvar value by key"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev set <key> <value>
  Set a seqvar value by key. The value will be stored in the default namespace.
  Example: seqwebdev set my.key "my value"
  Example: seqwebdev set config.database.url "sqlite:///data.db" """

    def add_arguments(self, parser):
        parser.add_argument(
            "key",
            help="The seqvar key to set"
        )
        parser.add_argument(
            "value",
            help="The value to set for the key"
        )

    def execute(self, args):
        try:
            # Set the value in the seqvar database
            seqvar_set(args.key, args.value)
            # print(f"Set '{args.key}' = '{args.value}'")

        except Exception as e:
            print(f"❌ Error setting seqvar '{args.key}': {e}")
