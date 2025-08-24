#!/usr/bin/env python3
"""
Cursor command for SeqWeb CLI - manages VS Code workspace file
"""

import json
from pathlib import Path
from ..base import BaseCommand
from seqvar.seqvar import get as seqvar_get
from seqvar.seqvar_toml import load_toml


class CursorCommand(BaseCommand):
    """Manage VS Code workspace file for SeqWeb development"""

    @property
    def name(self) -> str:
        return "cursor"

    @property
    def description(self) -> str:
        return "Manage VS Code workspace file for SeqWeb development"

    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev cursor
  Creates or updates seqwebdev.code-workspace file in the SeqWeb home directory.
  Respectfully merges with existing workspace settings and adds repos from seqweb.conf."""

    def add_arguments(self, parser):
        # No additional arguments needed for cursor command
        pass

    def execute(self, args):
        try:
            # Get the SeqWeb home directory
            home_path = Path(seqvar_get("seqwebdev.home", ns=""))
            workspace_file = home_path / "seqwebdev.code-workspace"

            # Default workspace template
            default_workspace = {
                "folders": [
                    {
                        "name": "seqwebdev",
                        "path": "."
                    },
                    {
                        "name": "seqweb",
                        "path": "seqweb"
                    },
                    {
                        "name": "seqwebcode",
                        "path": "seqwebcode"
                    },
                    {
                        "name": "seqwebdata",
                        "path": "seqwebdata"
                    },
                    {
                        "name": "oeisdata",
                        "path": "oeisdata"
                    },
                    {
                        "name": ".github",
                        "path": ".github"
                    }
                ],
                "settings": {
                    "files.exclude": {
                        "**/data/output": True,
                        "**/data/intermediate": True,
                        "**/data/logs": True
                    },
                    "search.exclude": {
                        "**/seqwebdata": True,
                        "**/oeisdata": True,
                        "**/data/output": True,
                        "**/data/intermediate": True,
                        "**/data/logs": True
                    },
                    "files.watcherExclude": {
                        "**/seqwebdata": True,
                        "**/oeisdata": True,
                        "**/data/output": True,
                        "**/data/intermediate": True,
                        "**/data/logs": True
                    }
                }
            }

            # Load existing workspace if it exists
            existing_workspace = {}
            if workspace_file.exists():
                try:
                    with open(workspace_file, 'r') as f:
                        existing_workspace = json.load(f)
                    print(f"Found existing workspace file: {workspace_file}")
                except json.JSONDecodeError as e:
                    print(f"Warning: Existing workspace file has invalid JSON: {e}")
                    print("Will create new workspace file")
                    existing_workspace = {}

            # Start with existing workspace or default
            merged_workspace = existing_workspace.copy() if existing_workspace else default_workspace.copy()

            # Ensure folders and settings exist
            if "folders" not in merged_workspace:
                merged_workspace["folders"] = []
            if "settings" not in merged_workspace:
                merged_workspace["settings"] = {}

            # Load repos from seqweb.conf
            config_file = home_path / "seqweb.conf"
            if config_file.exists():
                try:
                    bindings = load_toml(str(config_file))
                    repos = {}
                    for key, value in bindings.items():
                        if key.startswith("repos."):
                            repo_name = key.split(".", 1)[1]
                            repos[repo_name] = value

                    # Add missing repos to folders
                    existing_folder_names = {folder["name"] for folder in merged_workspace["folders"]}
                    existing_folder_paths = {folder["path"] for folder in merged_workspace["folders"]}

                    for repo_name, repo_path in repos.items():
                        # Check if repo already exists by name or path
                        repo_exists = (repo_name in existing_folder_names or
                                       repo_path in existing_folder_paths or
                                       repo_name in existing_folder_paths or
                                       repo_path in existing_folder_names)
                        
                        if not repo_exists:
                            # Add new repo folder
                            merged_workspace["folders"].append({
                                "name": repo_name,
                                "path": repo_path
                            })
                            print(f"Added repo folder: {repo_name} -> {repo_path}")
                        else:
                            print(f"Repo folder already exists: {repo_name}")

                except Exception as e:
                    print(f"Warning: Could not load repos from seqweb.conf: {e}")

            # Ensure default settings are present
            for setting_key, setting_value in default_workspace["settings"].items():
                if setting_key not in merged_workspace["settings"]:
                    merged_workspace["settings"][setting_key] = setting_value
                else:
                    # Merge nested settings
                    if isinstance(setting_value, dict) and isinstance(merged_workspace["settings"][setting_key], dict):
                        for nested_key, nested_value in setting_value.items():
                            if nested_key not in merged_workspace["settings"][setting_key]:
                                merged_workspace["settings"][setting_key][nested_key] = nested_value

            # Write the merged workspace file
            with open(workspace_file, 'w') as f:
                json.dump(merged_workspace, f, indent=4)

            print(f"Workspace file updated: {workspace_file}")
            print(f"Total folders: {len(merged_workspace['folders'])}")

        except Exception as e:
            print(f"❌ Error managing workspace file: {e}")
