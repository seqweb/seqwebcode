#!/usr/bin/env python3
"""
fab0 - First SeqWeb fabricator for testing the pipeline framework.

This fabricator runs a simple pipeline consisting of a single module (mod0)
that prints the incoming A-number. It demonstrates the basic fabricator
pattern and pipeline execution.
"""

import json
import sys
import os
from typing import Dict, Any

# Add the current directory to the path so we can import our modules
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from pipeline import run_pipeline
from mod0 import mod0


def fab0(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Fabricator function that processes an ID through the pipeline.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics.
    
    Args:
        box: Full input box dictionary
        id: The ID to process (e.g., "A000001")
        noisy: Whether to enable verbose output
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        Final outbox from the pipeline execution
    """
    # Create initial box from destructured parameters
    initial_box = {
        'id': id,
        'noisy': noisy,
        **_rest  # Preserve any extra keys from the input box
    }
    
    # Define the pipeline modules
    modules = [mod0]
    
    # Run the pipeline using the box-then-kwargs pattern
    result = run_pipeline(modules, initial_box)
    
    return result


def main():
    """Shell wrapper for fab0 fabricator."""
    import argparse
    
    parser = argparse.ArgumentParser(description="fab0 - First SeqWeb fabricator")
    parser.add_argument("id", help="The ID to process")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    try:
        # Create box from command line arguments
        box = {
            'id': args.id,
            'noisy': args.noisy
        }
        
        # Run the fabricator using destructuring pattern
        result = fab0(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        json.dump(result, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        # Let Python fail naturally as specified
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
