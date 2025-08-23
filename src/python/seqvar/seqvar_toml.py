# Created by Waldo 2025-08-20

"""
TOML sidecar for SeqVar Facility - loads TOML files and writes to seqvar database.
Implements the common contract: flattens tables to dotted keys, stringifies values.
"""

from __future__ import annotations
import tomllib
from pathlib import Path
from typing import Dict, List, Union, Any
from .seqvar import set as seqvar_set


def _flatten_toml(data: Any, prefix: str = "") -> Dict[str, str]:
    """
    Recursively flatten TOML data structure into dotted key-value pairs.
    All values are converted to strings to maintain seqvar DB uniformity.
    """
    result = {}

    if isinstance(data, dict):
        for key, value in data.items():
            new_prefix = f"{prefix}.{key}" if prefix else key
            result.update(_flatten_toml(value, new_prefix))
    elif isinstance(data, list):
        # Convert lists to JSON-like string representation
        result[prefix] = str(data)
    else:
        # Convert any other type to string
        result[prefix] = str(data)

    return result


def load_toml(paths: Union[str, Path, List[Union[str, Path]]]) -> Dict[str, str]:
    """
    Load TOML file(s) and return flattened key-value dictionary.

    Args:
        paths: Single path or list of paths to TOML files

    Returns:
        Dictionary with flattened dotted keys and string values

    Raises:
        FileNotFoundError: If any TOML file doesn't exist
        tomllib.TOMLDecodeError: If TOML parsing fails
    """
    if isinstance(paths, (str, Path)):
        paths = [paths]

    all_bindings = {}

    for path in paths:
        path_obj = Path(path)
        if not path_obj.exists():
            raise FileNotFoundError(f"TOML file not found: {path}")

        with open(path_obj, 'rb') as f:
            data = tomllib.load(f)

        # Flatten the TOML structure
        flattened = _flatten_toml(data)
        all_bindings.update(flattened)

    return all_bindings


def write_toml_to_seqvar(bindings: Dict[str, str], ns: str = "", src: str = None) -> None:
    """
    Write flattened key-value bindings to the seqvar database.

    Args:
        bindings: Dictionary of key-value pairs to write
        ns: Namespace for the bindings (default: "")
        src: Source identifier (default: None)
    """
    for key, value in bindings.items():
        seqvar_set(key, value, ns, src)
