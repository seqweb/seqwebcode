# Created by Waldo 2025-08-21

from __future__ import annotations
import os
from pathlib import Path
from typing import Optional


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
    home = os.environ.get("SEQWEBDEV_HOME")
    if not home:
        raise RuntimeError("SEQWEBDEV_HOME is not set; cannot resolve home directory path.")

    home_path = Path(home).expanduser().resolve()

    if suffix is None:
        return home_path
    else:
        return home_path / suffix


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
