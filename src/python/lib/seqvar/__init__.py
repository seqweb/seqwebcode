# Created by Waldo 2025-08-20

"""
SeqVar Facility - Simple key-value configuration system for SeqWeb.

This package provides persistent key-value storage with TOML file loading
capabilities and a unified interface across languages.

Core API:
- get(key): Get a value
- set(key, value): Set a value  
- get_dict(pattern): Get multiple values
- set_dict(bindings): Set multiple values
- load_toml_as_dict(path): Load TOML file as dictionary
"""

from .seqvar import get, set, get_dict, seqvar_store_path, set_dict, init_seqvar_store
from .toml import load_toml_as_dict

__version__ = "0.0.1"
__all__ = [
    "get",
    "set", 
    "get_dict",
    "set_dict",
    "init_seqvar_store",
    "seqvar_store_path",
    "load_toml_as_dict"
]
