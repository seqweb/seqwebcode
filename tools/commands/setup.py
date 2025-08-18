#!/usr/bin/env python3
"""
Setup command for SeqWeb CLI
"""

import os
from pathlib import Path

from lib.base import BaseCommand


class SetupCommand(BaseCommand):
    """Initialize SeqWeb development environment"""

    @property
    def name(self) -> str:
        return "setup"

    @property
    def description(self) -> str:
        return "Initialize SeqWeb development environment"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev setup
  Initialize the SeqWeb development environment.
  This includes generating workspace configuration.
  NOTE: This command is idempotent - safe to run multiple times."""

    def execute(self, args):
        # This function is idempotent - safe to run multiple times
        # It will only make changes if they haven't been made already

        print(" Setting up SeqWeb development environment...")

        self._generate_workspace_config()

        print("✅ Setup complete!")
        print("💡 Edit config/seqweb.conf to configure repository paths")







    def _generate_workspace_config(self):
        """Generate workspace configuration"""
        print(" Generating workspace configuration...")

        seqweb_home = Path(os.environ["SEQWEB_HOME"])

        try:
            import subprocess
            result = subprocess.run([
                "python3",
                str(seqweb_home / "tools" / "generate_workspace_config.py")
            ], capture_output=True, text=True, cwd=seqweb_home)

            if result.returncode == 0:
                print("✅ Workspace configuration generated")
            else:
                print("⚠️  Warning: Could not generate workspace configuration")
        except Exception as e:
            print(f"⚠️  Warning: Could not generate workspace configuration: {e}")