# Created by Waldo 2025-08-21

from __future__ import annotations
import os
from pathlib import Path
from typing import Optional
from lib.seqvar.seqvar import get


def get_seqwebdev_home() -> str:
    """
    Get the SeqWeb development home directory from environment variable.
    
    Returns:
        str: Path to the SeqWeb development home directory
        
    Raises:
        RuntimeError: If SEQWEBDEV_HOME environment variable is not set
    """
    seqwebdev_home = os.environ.get('SEQWEBDEV_HOME')
    if not seqwebdev_home:
        raise RuntimeError("SEQWEBDEV_HOME environment variable is not set!\n"
                          "Check .bashrc, maybe rerun bootstrap...")
    return seqwebdev_home


def seqwebdev_home_path(suffix: Optional[str] = None) -> Path:
    """
    Resolve SEQWEBDEV_HOME from the environment and optionally append a suffix.

    Args:
        suffix: Optional path suffix to append to the home directory

    Returns:
        Path object representing the home directory (if no suffix) or home/suffix

    Raises:
        RuntimeError: If SEQWEBDEV_HOME is not set
    """
    home = get_seqwebdev_home()
    home_path = Path(home).expanduser().resolve()

    if suffix is None:
        return home_path
    else:
        return home_path / suffix


def seqweb_conf_path() -> Path:
    """
    Get path to the seqweb.conf configuration file.
    
    Returns:
        Path object representing the seqweb.conf file
        
    Raises:
        RuntimeError: If SEQWEBDEV_HOME is not set or seqweb.conf doesn't exist
    """
    config_file = seqwebdev_home_path("seqweb.conf")
    if not config_file.exists():
        raise RuntimeError(f"Required seqweb.conf file not found at {config_file}!\n"
                           "Refer to https://www.seqweb.org/docs/quickstart for context.")
    return config_file


def seqwebdev_state_path(suffix: Optional[str] = None) -> Path:
    """
    Get path to the .state subfolder within the SeqWeb dev home directory.

    Args:
        suffix: Optional path suffix to append to the .state directory

    Returns:
        Path object representing the .state directory (if no suffix) or .state/suffix

    Raises:
        RuntimeError: If SEQWEBDEV_HOME is not set
    """
    state_path = seqwebdev_home_path(".state")

    if suffix is None:
        return state_path
    else:
        return state_path / suffix


def get_repo_path(repo: str, suffix: Optional[str] = None) -> Path:
    """
    Get path to a configured repository.
    
    Args:
        repo: Repository name (e.g., "seqwebcode", "seqwebdata")
        suffix: Optional path suffix to append to the repo directory
        
    Returns:
        Path object representing the repo directory (if no suffix) or repo/suffix
        
    Raises:
        RuntimeError: If the repo is not configured in seqvar
    """
    repo_key = f"repos.{repo}"
    repo_path_str = get(repo_key)
    if not repo_path_str:
        raise RuntimeError(f"Repository '{repo}' is not configured in seqvar (key: {repo_key})")
    
    repo_path = Path(repo_path_str).expanduser().resolve()
    
    if suffix is None:
        return repo_path
    else:
        return repo_path / suffix

