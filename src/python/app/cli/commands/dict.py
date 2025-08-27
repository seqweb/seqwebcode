#!/usr/bin/env python3
"""
Dict command for SeqWeb CLI - displays seqvar key-value pairs as a dictionary
"""

from ..base import BaseCommand
from lib.seqvar.seqvar import get_dict as seqvar_dict
import pprint


class DictCommand(BaseCommand):
    """Display seqvar key-value pairs as a dictionary"""

    @property
    def name(self) -> str:
        return "dict"

    @property
    def description(self) -> str:
        return "Display seqvar key-value pairs as a dictionary"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev dict [keys]
  Display seqvar key-value pairs as a dictionary.
  If no keys argument is provided, shows all key-value pairs.
  If keys is provided, filters to show only matching keys.

  Examples:
    seqwebdev dict                    # Show all key-value pairs
    seqwebdev dict "repos.*"         # Show keys starting with "repos."
    seqwebdev dict "config.*.url"    # Show keys matching "config.%.url" pattern
    seqwebdev dict "*.database"      # Show keys ending with ".database"

  Note: The keys argument uses SQLite LIKE patterns where:
    - % matches any sequence of characters
    - _ matches any single character
    - .* is automatically converted to .% for convenience"""

    def add_arguments(self, parser):
        parser.add_argument(
            "keys",
            nargs="?",
            default=None,
            help="Optional filter pattern for keys (e.g., 'repos.*', 'config.*.url')"
        )

    def execute(self, args):
        try:
            # Get the dictionary from seqvar store
            result_dict = seqvar_dict(args.keys)

            # Print the dictionary using pprint for clean formatting
            pprint.pprint(result_dict)

        except Exception as e:
            print(f"❌ Error: {e}")
