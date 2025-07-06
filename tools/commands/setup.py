#!/usr/bin/env python3
"""
Setup command for SeqWeb CLI
"""

import os
import stat
from pathlib import Path

from .base import BaseCommand


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
        return """Usage: pythonseqweb setup
  Initialize the SeqWeb development environment.
  This includes making the seqweb CLI executable and optionally
  adding the current directory to PATH for global access.
  NOTE: This command is idempotent - safe to run multiple times."""
    
    def execute(self, args):
        # This function is idempotent - safe to run multiple times
        # It will only make changes if they haven't been made already
        print("🧬 Setting up SeqWeb development environment...")
        print()
        
        self._make_executable()
        self._create_data_directories()
        self._add_to_path()
        self._show_completion_message()
    
    def _make_executable(self):
        """Make seqweb executable"""
        seqweb_path = Path(os.environ["SEQWEB_HOME"]) / "seqweb"
        
        if not seqweb_path.exists():
            print("❌ seqweb file not found")
            return
        
        # Check if already executable
        current_mode = seqweb_path.stat().st_mode
        if current_mode & stat.S_IXUSR:
            print("✅ seqweb CLI is already executable")
        else:
            print("📝 Making seqweb CLI executable...")
            seqweb_path.chmod(current_mode | stat.S_IXUSR)
            print("✅ seqweb CLI is now executable")
    
    def _create_data_directories(self):
        """Create data directories"""
        print("📁 Creating data directories...")
        
        seqweb_home = Path(os.environ["SEQWEB_HOME"])
        data_dirs = [
            seqweb_home / "data" / "intermediate",
            seqweb_home / "data" / "output", 
            seqweb_home / "data" / "logs"
        ]
        
        for data_dir in data_dirs:
            data_dir.mkdir(parents=True, exist_ok=True)
        
        print("✅ Data directories ready")
    
    def _add_to_path(self):
        """Add current directory to PATH for this session"""
        seqweb_home = os.environ["SEQWEB_HOME"]
        current_path = os.environ.get("PATH", "")
        
        if seqweb_home not in current_path.split(os.pathsep):
            print("🌐 Adding seqweb directory to PATH for this session...")
            os.environ["PATH"] = f"{seqweb_home}{os.pathsep}{current_path}"
            print("✅ You can now use 'seqweb <command>' (no './' needed)")
        else:
            print("✅ seqweb directory already in PATH")
    
    def _show_completion_message(self):
        """Show completion message with instructions"""
        seqweb_home = os.environ["SEQWEB_HOME"]
        
        print()
        print("✅ Setup complete! You can now use 'seqweb <command>' from anywhere in this directory.")
        print()
        print("💡 To use 'seqweb' command from other directories or new sessions:")
        print("   Add this line to your shell profile (~/.bash_profile, ~/.zshrc, etc.):")
        print(f"   export PATH=\"{seqweb_home}:$PATH\"")
        print()
        print(f"💡 To configure additional directories, edit: {seqweb_home}/config/env.sh") 