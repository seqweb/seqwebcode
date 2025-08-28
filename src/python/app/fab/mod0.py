#!/usr/bin/env python3
"""
mod0 - Simple test module that prints the incoming A-number.

This module follows the polyglot pipeline pattern with both an inner function
and a CLI wrapper for standalone execution.
"""

import json
import sys
from typing import Dict, Any


def mod0(inbox: Dict[str, Any]) -> Dict[str, Any]:
    """
    Inner function that processes the inbox and returns an outbox.
    
    Args:
        inbox: Dictionary containing at minimum 'A-number' and 'noisy' keys
        
    Returns:
        outbox: The inbox plus any modifications made by this module
    """
    # Extract A-number from inbox
    a_number = inbox.get('A-number', 'UNKNOWN')
    noisy = inbox.get('noisy', False)
    
    # Print the A-number (this is the "Hello World" functionality)
    if noisy:
        print(f"mod0: Processing A-number: {a_number}")
    else:
        print(f"mod0: {a_number}")
    
    # Return the outbox (inbox plus any modifications)
    # For this simple module, we just return the inbox unchanged
    return inbox


def main():
    """CLI wrapper for mod0 module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="mod0 - Print A-number from inbox")
    parser.add_argument("a_number", help="The A-number to process")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    # Create inbox from command line arguments
    inbox = {
        'A-number': args.a_number,
        'noisy': args.noisy
    }
    
    # Process the inbox
    outbox = mod0(inbox)
    
    # Output the result as JSON (following polyglot pattern)
    json.dump(outbox, sys.stdout)
    print()  # Add newline for readability


if __name__ == "__main__":
    main()
