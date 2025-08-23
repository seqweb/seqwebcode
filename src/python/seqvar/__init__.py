# Created by Waldo 2025-08-20

"""
SeqVar Facility - Simple key-value configuration system for SeqWeb.

This package provides persistent key-value storage with namespace support,
TOML file loading capabilities, and a unified interface across languages.
"""

from .seqvar import get, set, SeqVarError, seqvar_store_path
from .seqvar_toml import load_toml, write_toml_to_seqvar

__version__ = "0.0.1"
__all__ = [
    "get",
    "set",
    "SeqVarError",
    "seqvar_store_path",
    "load_toml",
    "write_toml_to_seqvar"
]


# Convenience aliases for common operations
def put(key: str, val: str, ns: str = "SeqVar", src: str = "seqweb") -> None:
    """Alias for set() - follows the naming convention used in other languages."""
    return set(key, val, ns, src)


# Add the alias to __all__
__all__.append("put")
