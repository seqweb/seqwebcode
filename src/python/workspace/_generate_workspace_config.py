#!/usr/bin/env python3
"""
Generate Cursor workspace configuration based on environment variables.
This allows each developer to have their own repository paths.
"""

import os
import json
import subprocess
from pathlib import Path


def get_repo_paths():
    """Get repository paths from environment or prompt user."""

    # Try to source the environment configuration
    config_dir = Path(__file__).parent.parent / "config"
    env_local = config_dir / "env.local.sh"
    env_default = config_dir / "env.sh"

    # Source the environment file to get variables
    env_vars = {}
    if env_local.exists():
        result = subprocess.run(
            f"source {env_local} && env | grep -E '^(SEQWEBDATA_PATH|OEISDATA_PATH)='",
            shell=True, capture_output=True, text=True
        )
        for line in result.stdout.strip().split('\n'):
            if '=' in line:
                key, value = line.split('=', 1)
                env_vars[key] = value
    elif env_default.exists():
        result = subprocess.run(
            f"source {env_default} && env | grep -E '^(SEQWEBDATA_PATH|OEISDATA_PATH)='",
            shell=True, capture_output=True, text=True
        )
        for line in result.stdout.strip().split('\n'):
            if '=' in line:
                key, value = line.split('=', 1)
                env_vars[key] = value

    # Get paths from environment or use defaults
    seqwebdata_path = env_vars.get('SEQWEBDATA_PATH', '~/Devo/Data/SeqWeb/seqwebdata')
    oeisdata_path = env_vars.get('OEISDATA_PATH', '~/Devo/Data/OEIS/oeisdata')

    # Expand ~ to home directory
    seqwebdata_path = os.path.expanduser(seqwebdata_path)
    oeisdata_path = os.path.expanduser(oeisdata_path)

    return seqwebdata_path, oeisdata_path


def generate_workspace_config():
    """Generate the workspace configuration file."""

    seqwebdata_path, oeisdata_path = get_repo_paths()

    # Create workspace configuration
    workspace_config = {
        "folders": [
            {
                "name": "seqwebcode (main)",
                "path": "."
            },
            {
                "name": "seqwebdata (generated RDF)",
                "path": seqwebdata_path
            },
            {
                "name": "oeisdata (OEIS corpus)",
                "path": oeisdata_path
            }
        ],
        "settings": {
            "search.exclude": {
                f"{seqwebdata_path}/**": True,
                f"{oeisdata_path}/**": True,
                "**/data/output/**": True,
                "**/data/intermediate/**": True,
                "**/tmp/**": True,
                "**/node_modules/**": True,
                "**/.git/**": True,
                "**/*.log": True,
                "**/logs/**": True
            },
            "files.exclude": {
                seqwebdata_path: True,
                oeisdata_path: True,
                "**/data/output": True,
                "**/data/intermediate": True,
                "**/tmp": True,
                "**/node_modules": True,
                "**/.git": True,
                "**/*.log": True,
                "**/logs": True
            },
            "files.watcherExclude": {
                f"{seqwebdata_path}/**": True,
                f"{oeisdata_path}/**": True,
                "**/data/output/**": True,
                "**/data/intermediate/**": True,
                "**/tmp/**": True
            }
        },
        "extensions": {
            "recommendations": [
                "ms-python.python",
                "ms-python.black-formatter",
                "ms-python.flake8",
                "redhat.java",
                "vscjava.vscode-java-debug",
                "vscjava.vscode-java-test",
                "vscjava.vscode-maven",
                "ms-vscode.vscode-json"
            ]
        }
    }

    # Write the configuration
    cursor_dir = Path(__file__).parent.parent / ".cursor"
    cursor_dir.mkdir(exist_ok=True)

    workspace_file = cursor_dir / "workspace.code-workspace"
    with open(workspace_file, 'w') as f:
        json.dump(workspace_config, f, indent=2)

    print(f"✓ Generated workspace configuration: {workspace_file}")
    print(f"  seqwebdata: {seqwebdata_path}")
    print(f"  oeisdata: {oeisdata_path}")

    # Validate paths exist
    if not os.path.exists(seqwebdata_path):
        print(f"⚠️  Warning: seqwebdata path not found: {seqwebdata_path}")
    if not os.path.exists(oeisdata_path):
        print(f"⚠️  Warning: oeisdata path not found: {oeisdata_path}")


if __name__ == "__main__":
    generate_workspace_config()
