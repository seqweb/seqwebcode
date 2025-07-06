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
        return """Usage: ./seqweb setup
  Initialize the SeqWeb development environment.
  This includes making the seqweb CLI executable and creating data directories.
  NOTE: This command is idempotent - safe to run multiple times."""
    
    def execute(self, args):
        # This function is idempotent - safe to run multiple times
        # It will only make changes if they haven't been made already
        
        print("🧬 Setting up SeqWeb development environment...")
        print()
        
        self._make_executable()
        self._create_data_directories()
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
    

    
    def _show_completion_message(self):
        """Show completion message with instructions"""
        print()
        print("✅ Setup complete! You can now use './seqweb <command>'")
        print()
        print("💡 To configure additional directories, edit: config/env.sh") 