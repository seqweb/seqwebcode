#!/usr/bin/env python3
"""
Module: segment_sections

Segments OEIS data into sections by parsing the % <type> A<number> <content> format.
Returns a section_map dict with single-letter keys and lists of strings as values.
"""

import re
from typing import Dict, List, Any


def segment_sections(box: Dict[str, Any], *, oeis_data: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Segment OEIS data into sections based on % <type> A<number> <content> format.
    
    Args:
        box: The processing box containing input data
        oeis_data: The raw OEIS data string to segment
        noisy: Whether to print debug information
        **_rest: Additional keyword arguments (ignored)
    
    Returns:
        Updated box with section_map containing segmented data
        
    Raises:
        ValueError: If a line doesn't match the expected format
    """
    if noisy:
        print(f"segment_sections: Processing {len(oeis_data)} characters of OEIS data")
    
    # Pattern to match: %<type> A<6-digit-number> <rest of line>
    # The type is a single character, A-number is exactly 6 digits
    pattern = re.compile(r'^%([A-Za-z]) A(\d{6}) (.+)$')
    
    section_map: Dict[str, List[str]] = {}
    current_section_type = None
    current_section_lines = []
    
    lines = oeis_data.strip().split('\n')
    
    for line_num, line in enumerate(lines, 1):
        if noisy:
            print(f"Processing line {line_num}: {repr(line)}")
        
        # Skip empty lines
        if not line.strip():
            continue
            
        match = pattern.match(line)
        if not match:
            raise ValueError(f"Line {line_num} doesn't match expected format '% <type> A<number> <content>': {repr(line)}")
        
        section_type, a_number, content = match.groups()
        
        if noisy:
            print(f"Matched: type='{section_type}', A-number='{a_number}', content='{content}'")
        
        # If we're starting a new section type, save the previous one
        if current_section_type is not None and current_section_type != section_type:
            if current_section_type not in section_map:
                section_map[current_section_type] = []
            section_map[current_section_type].append('\n'.join(current_section_lines))
            current_section_lines = []
        
        # Start or continue current section
        current_section_type = section_type
        current_section_lines.append(content)
    
    # Don't forget the last section
    if current_section_type is not None:
        if current_section_type not in section_map:
            section_map[current_section_type] = []
        section_map[current_section_type].append('\n'.join(current_section_lines))
    
    if noisy:
        print(f"Final section_map: {list(section_map.keys())}")
        for section_type, sections in section_map.items():
            print(f"  {section_type}: {len(sections)} section(s)")
            for i, section in enumerate(sections, 1):
                line_count = section.count('\n') + 1 if section else 0
                print(f"    Section {i}: {len(section)} characters, {line_count} lines")
    
    return {**box, 'section_map': section_map}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for segment_sections module."""
    from libs.core.wrapper import get_inbox, dump_outbox
    
    # Define argument specifications for this module
    argument_definitions = [
        ('oeis_data', str, 'OEIS data string to segment', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = get_inbox(argument_definitions)
    
    # Call core function with identical semantics
    outbox = segment_sections(inbox, **inbox)
    
    # Emit JSON output for pipeline consumption
    dump_outbox(outbox)


if __name__ == '__main__':
    main()
