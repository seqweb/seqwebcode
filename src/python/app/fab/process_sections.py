#!/usr/bin/env python3
"""
Module: process_sections

Processes the section_map by concatenating S, T, and U sections into a single V section.
Removes the original S, T, U sections and replaces them with the concatenated V section.
"""

from typing import Dict, List, Any


def process_sections(box: Dict[str, Any], *,
                     section_map: Dict[str, List[str]], noisy: bool = False,
                     **_rest) -> Dict[str, Any]:
    """
    Process section_map by concatenating S, T, U sections into V section.

    Args:
        box: The processing box containing input data
        section_map: Dictionary mapping section types to lists of strings
        noisy: Whether to print debug information
        **_rest: Additional keyword arguments (ignored)

    Returns:
        Updated box with processed section_map containing V section instead of S, T, U
    """
    if noisy:
        print("process_sections: Processing section_map with "
              f"{len(section_map)} section types: {list(section_map.keys())}")

    # Create new section_map using destructuring
    # Concatenate S, T, U sections and replace them with V section
    new_section_map = (lambda S, T, U, **_rest:
                            {**_rest, 'V': [''.join(S) + ''.join(T) + ''.join(U)]}
                       )(**section_map)

    if noisy:
        print("process_sections: New section_map has "
              f"{len(new_section_map)} section types: {list(new_section_map.keys())}")

    return {**box, 'section_map': new_section_map}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for process_sections module."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys

    # Define argument specifications for this module
    argument_definitions = [
        ('section_map', dict, 'Dictionary mapping section types to lists of strings', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)

    # Call core function with identical semantics
    outbox = process_sections(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == '__main__':
    main()
