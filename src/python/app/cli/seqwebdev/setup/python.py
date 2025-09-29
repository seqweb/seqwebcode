#!/usr/bin/env python3
"""
Python subcommand for Setup - manages Python package installation
"""

import subprocess
import sys
from pathlib import Path
from app.cli.base_command import BaseCommand


class PythonCommand(BaseCommand):
    """Manage Python package installation for SeqWeb development"""

    # This command has no subcommands
    has_subcommands: bool = False
    
    @property
    def name(self) -> str:
        return "python"
    
    @property
    def description(self) -> str:
        return "Install Python packages from requirements.txt"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev setup python [--noisy]
  Installs all Python packages from requirements.txt
  This is a kitchen-sink install of everything needed for SeqWeb development.
  Optional --noisy flag enables verbose output."""
    
    def add_arguments(self, parser):
        parser.add_argument(
            "--noisy",
            action="store_true",
            default=False,
            help="Enable verbose output (default: False)"
        )
    
    def do_command(self):
        """Install Python packages from requirements.txt"""
        # Use argparse for argument parsing
        parser = self.create_parser()
        args = parser.parse_args(self.args)
        
        try:
            # Find the requirements.txt file
            # Look for it in the python/ directory relative to seqwebcode
            from libs.core.seqvar.seqvar import get as seqvar_get
            seqwebcode_path = seqvar_get("repos.seqwebcode")
            requirements_file = Path(seqwebcode_path) / "src" / "python" / "requirements.txt"

            if args.noisy:
                print(f"Looking for requirements.txt at: {requirements_file}")
                print(f"seqwebcode_path: {seqwebcode_path}")

            if not requirements_file.exists():
                print(f"❌ Error: requirements.txt not found at {requirements_file}")
                sys.exit(1)

            if args.noisy:
                print(f"📦 Installing Python packages from {requirements_file}")
                print(f"🔧 Running: pip install -r {requirements_file}")

            # Run pip install with both --break-system-packages and --user flags
            # --break-system-packages: Override externally-managed-environment error
            # --user: Install to user directory to avoid breaking Homebrew installation
            cmd = [sys.executable, "-m", "pip", "install", "--break-system-packages", "--user", "-r", str(requirements_file)]
            
            if args.noisy:
                print(f"Command: {' '.join(cmd)}")
            
            # Execute the pip install command
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=True
            )
            
            if args.noisy:
                print("✅ Installation completed successfully!")
                if result.stdout:
                    print("Output:")
                    print(result.stdout)
            else:
                print("✅ Python packages installed successfully")
            
        except subprocess.CalledProcessError as e:
            print(f"❌ Error installing Python packages: {e}")
            if e.stdout:
                print("Output:", e.stdout)
            if e.stderr:
                print("Error:", e.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"❌ Error: {e}")
            sys.exit(1)
