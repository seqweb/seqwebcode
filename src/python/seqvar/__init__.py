# Created by Waldo 2025-08-20

"""
SeqVar Facility - Simple key-value configuration system for SeqWeb.

This package provides persistent key-value storage with TOML file loading
capabilities and a unified interface across languages.
"""

from .seqvar import get, set, get_dict, SeqVarError, seqvar_store_path
from .seqvar_toml import load_toml, write_toml_to_seqvar

__version__ = "0.0.1"
__all__ = [
    "get",
    "set",
    "get_dict",
    "SeqVarError",
    "seqvar_store_path",
    "load_toml",
    "write_toml_to_seqvar"
]
