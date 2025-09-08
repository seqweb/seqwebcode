#!/usr/bin/env python3
"""
fab1 - Second SeqWeb fabricator for testing the pipeline framework.

This fabricator runs a pipeline consisting of mod0 -> standardize_id -> mod0
that demonstrates chaining multiple modules together. The pipeline
performs: pass, standardize ID, pass.
"""

import json
import sys
from typing import Dict, Any

from pipeline import run_pipeline
from mod0 import mod0
from standardize_id import standardize_id
from mod2 import mod2


def fab1(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
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
    
    # Define the pipeline modules: mod0 -> standardize_id -> mod0
    modules = [mod0, standardize_id, mod0]
    
    # Run the pipeline using the box-then-kwargs pattern
    result = run_pipeline(modules, initial_box)
    
    return result


def main():
    """CLI wrapper for fab1 fabricator."""
    import argparse
    
    parser = argparse.ArgumentParser(description="fab1 - Second SeqWeb fabricator")
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
        result = fab1(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        json.dump(result, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        # Let Python fail naturally as specified
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
