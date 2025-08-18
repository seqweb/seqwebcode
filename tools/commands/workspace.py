#!/usr/bin/env python3
"""
Workspace command for SeqWeb CLI
"""

import os
import subprocess
from pathlib import Path

from lib.base import BaseCommand


class WorkspaceCommand(BaseCommand):
    """Manage SeqWeb workspace configuration"""

    @property
    def name(self) -> str:
        return "workspace"

    @property
    def description(self) -> str:
        return "Manage SeqWeb workspace configuration"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev workspace [update]
  Manage SeqWeb workspace configuration for multi-repo development.

  Commands:
    update    Generate/update workspace configuration based on config/env.local.sh
    (no args) Show current workspace configuration status"""

    def add_arguments(self, parser):
        parser.add_argument(
            "action",
            nargs="?",
            choices=["update"],
            help="Action to perform (update workspace configuration)"
        )

    def execute(self, args):
        if args.action == "update":
            self._update_workspace()
        else:
            self._show_status()

    def _update_workspace(self):
        """Update workspace configuration"""
        print("🔧 Updating workspace configuration...")

        seqweb_home = Path(os.environ["SEQWEB_HOME"])
        workspace_script = seqweb_home / "tools" / "generate_workspace_config.py"

        if not workspace_script.exists():
            print("❌ Workspace configuration script not found")
            return

        try:
            result = subprocess.run([
                "python3", str(workspace_script)
            ], capture_output=True, text=True, cwd=seqweb_home)

            if result.returncode == 0:
                print("✅ Workspace configuration updated")
                print(result.stdout.strip())
            else:
                print("❌ Error updating workspace configuration:")
                print(result.stderr)
        except Exception as e:
            print(f"❌ Error: {e}")

    def _show_status(self):
        """Show current workspace configuration status"""
        print("🔍 Workspace Configuration Status")
        print()

        seqweb_home = Path(os.environ["SEQWEB_HOME"])
        env_local = seqweb_home / "config" / "env.local.sh"
        workspace_config = seqweb_home / ".cursor" / "workspace.code-workspace"

        # Check environment configuration
        if env_local.exists():
            print("✅ Local environment configuration: config/env.local.sh")
        else:
            print("⚠️  Local environment configuration not found")
            print("   Run 'seqwebdev setup' to create it")

        # Check workspace configuration
        if workspace_config.exists():
            print("✅ Workspace configuration: .cursor/workspace.code-workspace")
        else:
            print("⚠️  Workspace configuration not found")
            print("   Run 'seqwebdev workspace update' to generate it")

        # Show repository paths if available
        if env_local.exists():
            print()
            print("📁 Repository paths (from config/env.local.sh):")
            try:
                # Source the environment file and extract paths
                result = subprocess.run([
                    "bash", "-c",
                    f"source {env_local} && echo 'SEQWEBDATA_PATH: $SEQWEBDATA_PATH' && echo 'OEISDATA_PATH: $OEISDATA_PATH'"
                ], capture_output=True, text=True)

                if result.returncode == 0:
                    for line in result.stdout.strip().split('\n'):
                        if ':' in line:
                            path_name, path_value = line.split(':', 1)
                            path_value = path_value.strip()
                            if path_value:
                                # Check if path exists
                                expanded_path = os.path.expanduser(path_value)
                                if os.path.exists(expanded_path):
                                    print(f"   {path_name}: {path_value} ✅")
                                else:
                                    print(f"   {path_name}: {path_value} ⚠️ (not found)")
                            else:
                                print(f"   {path_name}: (not set)")
                else:
                    print("   Could not read repository paths")
            except Exception as e:
                print(f"   Error reading paths: {e}")

        print()
        print("💡 To update workspace configuration, run: seqwebdev workspace update")
        print("💡 To edit repository paths, edit: config/env.local.sh")