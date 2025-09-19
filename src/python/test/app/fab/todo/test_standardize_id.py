#!/usr/bin/env python3
"""
standardize_id - Module that standardizes sequence IDs to A###### format.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It converts various ID formats
to a standardized 7-character format (A followed by 6 digits) and prints
the conversion only when --noisy is specified.

Accepts:
- Decimal digits (0-999999): 695 -> A000695
- 'a' or 'A' prefix: a695, A695 -> A000695
- Padded with leading zeros: 00695 -> A000695
"""

import json
import sys
from typing import Dict, Any


def get_standard_id(id: str) -> str:
    """
    Helper function that standardizes a sequence ID to A###### format.
    
    Converts various input formats to a standardized 7-character format where
    the first character is 'A' and the remaining 6 characters are digits.
    
    Args:
        id: The ID to standardize (various formats accepted)
        
    Returns:
        standardized_id: The ID in A###### format
        
    Raises:
        ValueError: If the input cannot be converted to the required format
    """
    # Remove any whitespace and convert to string
    input_id = str(id).strip()
    
    # Handle empty input
    if not input_id:
        raise ValueError("❌ Empty ID provided")
    
    # Check if input starts with 'a' or 'A' (case insensitive)
    if input_id.lower().startswith('a'):
        # Extract the numeric part after 'a' or 'A'
        numeric_part = input_id[1:]
    else:
        # Use the entire input as numeric part
        numeric_part = input_id
    
    # Validate that numeric part contains only digits
    if not numeric_part.isdigit():
        raise ValueError(f"❌ Invalid ID format '{id}': numeric part '{numeric_part}' contains non-digits")
    
    # Convert to integer and validate range
    try:
        numeric_value = int(numeric_part)
    except ValueError:
        raise ValueError(f"❌ Invalid ID format '{id}': cannot convert '{numeric_part}' to integer")
    
    # Check range (0 to 999999)
    if numeric_value < 0 or numeric_value > 999999:
        raise ValueError(f"❌ Invalid ID format '{id}': number {numeric_value} is out of range (0-999999)")
    
    # Format as A###### (A followed by 6 digits with leading zeros)
    standardized_id = f"A{numeric_value:06d}"
    
    return standardized_id


def standardize_id(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that standardizes sequence IDs to A###### format.
    
    Converts various input formats to a standardized 7-character format where
    the first character is 'A' and the remaining 6 characters are digits.
    
    Args:
        box: Full input box dictionary
        id: The ID to standardize (various formats accepted)
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with 'id' standardized to A###### format
        
    Raises:
        ValueError: If the input cannot be converted to the required format
    """
    # Use the helper function to get the standardized ID
    standardized_id = get_standard_id(id)
    
    # Print the conversion only in noisy mode
    if noisy:
        print(f"standardize_id: {id} -> {standardized_id}")
    
    # Return the outbox with updated id, preserving all other keys
    return {**box, 'id': standardized_id}


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
