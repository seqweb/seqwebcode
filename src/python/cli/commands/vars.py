#!/usr/bin/env python3
"""
Vars command for SeqWeb CLI - displays all seqvar database contents
"""

from ..base import BaseCommand
from seqvar.seqvar import dump


class VarsCommand(BaseCommand):
    """Display all contents of the seqvar database"""

    @property
    def name(self) -> str:
        return "vars"

    @property
    def description(self) -> str:
        return "Display all contents of the seqvar database"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev vars
  Display all rows and columns from the seqvar database in a human-readable format.
  Each row shows: namespace, key, value, source, and timestamp."""

    def add_arguments(self, parser):
        # No additional arguments needed for vars command
        pass

    def execute(self, args):
        try:
            rows = dump()

            if not rows:
                print("No data found in seqvar database")
                return

            print(f"Found {len(rows)} entries in seqvar database:")

            # Calculate the maximum width needed for the namespace:key column
            max_key_width = 0
            for ns, key, val, src, ts in rows:
                key_col = f"{ns}:{key}"
                max_key_width = max(max_key_width, len(key_col))

            # Display column headings using the same template format
            print(f"{'source@timestamp':<20}  {'namespace:key':<{max_key_width}}  value")
            print("-" * (20 + 2 + max_key_width + 2 + 20))  # Adjust separator length

            for ns, key, val, src, ts in rows:
                # Convert timestamp to readable format
                from datetime import datetime
                try:
                    timestamp = datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H:%M:%S')
                except (ValueError, TypeError):
                    timestamp = str(ts)

                # Use the template format: {src}@{ts}<pad1>{ns}:{key}<pad2>{val}
                # where <pad1> is two spaces and <pad2> aligns all values in the same column
                key_col = f"{ns}:{key}"
                print(f"{src}@{timestamp}  {key_col:<{max_key_width}}  {val}")

            print("-" * (20 + 2 + max_key_width + 2 + 20))  # Adjust separator length

        except Exception as e:
            print(f"❌ Error accessing seqvar database: {e}")
            print("   Make sure the seqvar database is initialized (run bootstrap)")
