#!/usr/bin/env python3
"""
get_oeis_data - Module that reads OEIS data files.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It reads the OEIS data file
for the given ID and stores the contents in the box under the key 'oeis_data'.

Dependencies:
- seqvar system for accessing oeisdata path
"""

import json
import sys
from pathlib import Path
from typing import Dict, Any


def get_oeis_data(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that reads OEIS data file for the given ID.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Reads the
    OEIS data file and stores its contents as a string.
    
    Args:
        box: Full input box dictionary
        id: The sequence ID (e.g., "A000001")
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with 'oeis_data' key containing the file contents
        
    Raises:
        ImportError: If seqvar system is not available
        RuntimeError: If seqvar 'repos.oeisdata' is not set
        FileNotFoundError: If the OEIS data file doesn't exist
        IOError: If the file cannot be read
    """
    # Import seqvar function inside the function (like CLI commands do)
    try:
        from libs.core.seqvar.seqvar import get
    except ImportError:
        raise ImportError("❌ seqvar system not available")
    
    # Get the oeisdata repository path from seqvar
    oeisdata_path = get("repos.oeisdata")
    if not oeisdata_path:
        raise RuntimeError("❌ seqvar 'repos.oeisdata' is not set")
    
    # Construct the file path: oeisdata:seq/{folder}/{id}.seq
    folder = id[:4]  # First four characters of ID
    file_path = Path(oeisdata_path) / "seq" / folder / f"{id}.seq"
    
    # Read the file contents
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            oeis_data = f.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"❌ OEIS data file not found: {file_path}")
    except IOError as e:
        raise IOError(f"❌ Cannot read OEIS data file {file_path}: {e}")
    
    # Print status in noisy mode
    if noisy:
        print(f"get_oeis_data: Read {len(oeis_data)} characters from {file_path}")
    
    # Return the outbox with the OEIS data added, preserving all other keys
    return {**box, 'oeis_data': oeis_data}


def main():
    """Shell wrapper for get_oeis_data module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="get_oeis_data - Read OEIS data file for sequence ID")
    parser.add_argument("id", help="The sequence ID to read data for")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    try:
        # Create box from command line arguments
        box = {
            'id': args.id,
            'noisy': args.noisy
        }
        
        # Process the box using destructuring pattern
        outbox = get_oeis_data(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        json_output = {
            'id': outbox.get('id'),
            'oeis_data_length': len(outbox.get('oeis_data', '')),
            'noisy': outbox.get('noisy', False)
        }
        
        json.dump(json_output, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
