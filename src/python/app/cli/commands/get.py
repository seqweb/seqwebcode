#!/usr/bin/env python3
"""
Get command for SeqWeb CLI - retrieves seqvar values
"""

from ..base import BaseCommand
from libs.core.seqvar.seqvar import get as seqvar_get


class GetCommand(BaseCommand):
    """Get a seqvar value by key"""

    @property
    def name(self) -> str:
        return "get"

    @property
    def description(self) -> str:
        return "Get a seqvar value by key"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev get <key>
  Get a seqvar value by key. The value is printed quoted on a single line.
  Example: seqwebdev get seqwebdev.home"""

    def add_arguments(self, parser):
        parser.add_argument(
            "key",
            help="The seqvar key to retrieve"
        )

    def execute(self, args):
        try:
            # print(f'"{args.key}"')
            value = seqvar_get(args.key)
            print(f'"{value}"')
        except Exception as e:
            print(f"❌ Error getting seqvar '{args.key}': {e}")
