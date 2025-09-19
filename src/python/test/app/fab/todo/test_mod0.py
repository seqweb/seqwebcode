#!/usr/bin/env python3
"""
mod0 - Simple test module that processes the incoming ID.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It prints the ID only when --noisy
is specified.
"""

import json
import sys
from typing import Dict, Any


def mod0(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that processes the box and returns an outbox.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Prints the ID
    only when noisy=True.
    
    Args:
        box: Full input box dictionary
        id: The ID to process
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box plus any modifications made by this module
    """
    # Print the ID only in noisy mode
    if noisy:
        print(f"mod0: ID={id}")
    
    # Return the outbox (box plus any modifications)
    # For this simple module, we just return the box unchanged
    # The destructuring pattern ensures all original keys are preserved
    return box


def main():
    """Shell wrapper for mod0 module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="mod0 - Print ID from box")
    parser.add_argument("id", help="The ID to process")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    # Create box from command line arguments
    box = {
        'id': args.id,
        'noisy': args.noisy
    }
    
    # Process the box using destructuring pattern
    outbox = mod0(box, **box)
    
    # Output the result as JSON (following polyglot pattern)
    json.dump(outbox, sys.stdout)
    print()  # Add newline for readability


if __name__ == "__main__":
    main()
