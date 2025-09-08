#!/usr/bin/env python3
"""
standardize_id - Module that converts the id to uppercase.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It prints the conversion only when
--noisy is specified.
"""

import json
import sys
from typing import Dict, Any


def standardize_id(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that processes the box and returns an outbox.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Prints the
    conversion only when noisy=True.
    
    Args:
        box: Full input box dictionary
        id: The ID to convert to uppercase
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with 'id' converted to uppercase
    """
    # Convert id to uppercase
    uppercase_id = id.upper()
    
    # Print the conversion only in noisy mode
    if noisy:
        print(f"standardize_id: {id} -> {uppercase_id}")
    
    # Return the outbox with updated id, preserving all other keys
    return {**box, 'id': uppercase_id}


def main():
    """Shell wrapper for standardize_id module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="standardize_id - Convert id to uppercase from box")
    parser.add_argument("id", help="The ID to process")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    # Create box from command line arguments
    box = {
        'id': args.id,
        'noisy': args.noisy
    }
    
    # Process the box using destructuring pattern
    outbox = standardize_id(box, **box)
    
    # Output the result as JSON (following polyglot pattern)
    json.dump(outbox, sys.stdout)
    print()  # Add newline for readability


if __name__ == "__main__":
    main()
