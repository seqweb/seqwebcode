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

            # Import datetime once at the top
            from datetime import datetime

            # Calculate the maximum width needed for each column
            max_timestamp_width = 0
            max_key_width = 0
            max_value_width = 0
            
            for ns, key, val, src, ts in rows:
                # Calculate timestamp width
                try:
                    timestamp = datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H:%M:%S')
                except (ValueError, TypeError):
                    timestamp = str(ts)
                timestamp_col = f"{src}@{timestamp}"
                max_timestamp_width = max(max_timestamp_width, len(timestamp_col))
                
                # Calculate key width
                key_col = f"{ns}:{key}"
                max_key_width = max(max_key_width, len(key_col))
                
                # Calculate value width
                max_value_width = max(max_value_width, len(str(val)))

            # Ensure minimum widths for readability
            max_timestamp_width = max(max_timestamp_width, 20)  # Minimum width for header
            max_key_width = max(max_key_width, 15)             # Minimum width for header
            max_value_width = max(max_value_width, 5)           # Minimum width for header

            # Display column headings with perfect alignment
            print(f"{'source@timestamp':<{max_timestamp_width}}  {'namespace:key':<{max_key_width}}  value")
            print("-" * (max_timestamp_width + 2 + max_key_width + 2 + max_value_width))

            for ns, key, val, src, ts in rows:
                # Convert timestamp to readable format
                try:
                    timestamp = datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H:%M:%S')
                except (ValueError, TypeError):
                    timestamp = str(ts)

                # Format each column with consistent alignment
                timestamp_col = f"{src}@{timestamp}"
                key_col = f"{ns}:{key}"
                
                print(f"{timestamp_col:<{max_timestamp_width}}  {key_col:<{max_key_width}}  {val}")

            print("-" * (max_timestamp_width + 2 + max_key_width + 2 + max_value_width))

        except Exception as e:
            print(f"❌ Error accessing seqvar database: {e}")
            print("   Make sure the seqvar database is initialized (run bootstrap)")
