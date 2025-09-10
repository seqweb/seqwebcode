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


def main():
    """CLI entry point for testing the process_sections module."""
    import sys
    import json
    from argparse import ArgumentParser
    
    parser = ArgumentParser(description='Process section_map by concatenating S, T, U into V')
    parser.add_argument('--section-map', required=True, help='JSON string of section_map to process')
    parser.add_argument('--noisy', action='store_true', help='Enable verbose output')
    
    args = parser.parse_args()
    
    try:
        section_map = json.loads(args.section_map)
        result = process_sections(
            box={},
            section_map=section_map,
            noisy=args.noisy
        )
        
        print(json.dumps(result, indent=2))
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
