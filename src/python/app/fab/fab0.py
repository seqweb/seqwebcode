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


def fab0(a_number: str, noisy: bool = False) -> Dict[str, Any]:
    """
    Main fabricator function that processes an A-number through the pipeline.
    
    Args:
        a_number: The A-number to process (e.g., "A000001")
        noisy: Whether to enable verbose output
        
    Returns:
        Final outbox from the pipeline execution
    """
    # Create initial inbox
    initial_inbox = {
        'A-number': a_number,
        'noisy': noisy
    }
    
    # Define the pipeline modules
    modules = [mod0]
    
    # Run the pipeline
    result = run_pipeline(modules, initial_inbox)
    
    return result


def main():
    """CLI wrapper for fab0 fabricator."""
    import argparse
    
    parser = argparse.ArgumentParser(description="fab0 - First SeqWeb fabricator")
    parser.add_argument("a_number", help="The A-number to process")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    try:
        # Run the fabricator
        result = fab0(args.a_number, args.noisy)
        
        # Output the result as JSON (following polyglot pattern)
        json.dump(result, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        # Let Python fail naturally as specified
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
