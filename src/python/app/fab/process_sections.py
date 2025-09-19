#!/usr/bin/env python3
"""
Module: process_sections

Processes the section_map by concatenating S, T, and U sections into a single V section.
Removes the original S, T, U sections and replaces them with the concatenated V section.
"""

from typing import Dict, List, Any


def process_sections(box: Dict[str, Any], *, section_map: Dict[str, List[str]], noisy: bool = False, **_rest) -> Dict[str, Any]:
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
        print(f"process_sections: Processing section_map with {len(section_map)} section types")
    
    # Extract S, T, U sections and concatenate them
    s_sections = section_map.get('S', [])
    t_sections = section_map.get('T', [])
    u_sections = section_map.get('U', [])
    
    # Concatenate all strings from S, T, U sections (no line breaks)
    v_content = ''.join(s_sections) + ''.join(t_sections) + ''.join(u_sections)
    
    if noisy:
        print(f"process_sections: Concatenated {len(s_sections)} S + {len(t_sections)} T + {len(u_sections)} U sections")
        print(f"process_sections: V section has {len(v_content)} characters")
    
    # Create new section_map using destructuring
    # Remove S, T, U keys and add V key
    new_section_map = {
        **{k: v for k, v in section_map.items() if k not in ['S', 'T', 'U']},
        'V': [v_content]
    }
    
    if noisy:
        print(f"process_sections: New section_map has {len(new_section_map)} section types: {list(new_section_map.keys())}")
    
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
