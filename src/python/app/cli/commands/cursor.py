#!/usr/bin/env python3
"""
Cursor command for SeqWeb CLI - manages VS Code workspace file
"""

import json
import shutil
from pathlib import Path
from ..base import BaseCommand
from libs.core.seqvar.seqvar import get as seqvar_get


class CursorCommand(BaseCommand):
    """Manage VS Code workspace file for SeqWeb development"""

    @property
    def name(self) -> str:
        return "cursor"

    @property
    def description(self) -> str:
        return "Manage Cursor/VS Code workspace file for SeqWeb development"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev cursor [workspace_file] [--noisy]
  Creates or updates workspace file for SeqWeb development.
  Default workspace file: seqwebdev.code-workspace
  Respectfully merges with existing workspace settings, adding repos from seqweb.conf if needed.
  Optional --noisy flag enables printing progress messages."""

    def add_arguments(self, parser):
        parser.add_argument(
            "workspace_file",
            nargs="?",
            default="seqwebdev.code-workspace",
            help="Workspace filename (default: seqwebdev.code-workspace)"
        )
        parser.add_argument(
            "--noisy",
            action="store_true",
            default=False,
            help="Enable verbose output and pretty-printing (default: False)"
        )

    def execute(self, args):
        try:
            # Get the SeqWeb home directory
            home_path = Path(seqvar_get("seqwebdev.home"))
            workspace_file = home_path / args.workspace_file
            workspace_file_exists = workspace_file.exists()

            if args.noisy and workspace_file_exists:
                print(f"Found existing workspace file: {workspace_file}")

            # Make backup copy
            backup_file = Path(str(workspace_file) + ".bkp")
            if workspace_file_exists:
                shutil.copy2(workspace_file, backup_file)
                if args.noisy:
                    print(f"Backup created: {backup_file}")

            # Load existing workspace if it exists
            base_config = {}
            if workspace_file_exists:
                try:
                    with open(workspace_file, 'r') as f:
                        base_config = json.load(f)
                except json.JSONDecodeError as e:
                    print(f"⚠️ Warning: Existing workspace file has invalid JSON: {e}")
                    print("Will create new workspace file")
                    base_config = {}

            # Transform the base config using augmented_config
            from ..workspace.transforms import augmented_config
            result_config = augmented_config(base_config)

            # Pretty print the resulting config only if noisy
            if args.noisy:
                import pprint
                print("Updated workspace configuration:")
                pprint.pprint(result_config)

            # Write the updated config back to the source file
            with open(workspace_file, 'w') as f:
                json.dump(result_config, f, indent=2)
            if args.noisy:
                print(f"Workspace file updated: {workspace_file}")

        except Exception as e:
            print(f"❌ Error managing workspace file: {e}")
