#!/usr/bin/env python3
"""
Module: add_sections

Adds RDF triples for all text blocks in section_map, creating Text resources
and attaching them to the sequence via hasTextList properties.
"""

import re
from datetime import datetime
from typing import Dict, List, Any


def add_sections(box: Dict[str, Any], *, id: str, section_map: Dict[str, List[str]], graph, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Add RDF triples for all text blocks in section_map.
    
    Args:
        box: The processing box containing input data
        id: The sequence ID (e.g., "A000001")
        section_map: Dictionary mapping section types to lists of strings
        graph: The RDFLib Graph to add triples to
        noisy: Whether to print debug information
        **_rest: Additional keyword arguments (ignored)
    
    Returns:
        Updated box with the modified graph
    """
    if noisy:
        print(f"add_sections: Processing {len(section_map)} section types for {id}")
    
    # Determine stamp from I section or use current time
    stamp = _extract_stamp_from_i_section(section_map.get('I', []))
    if not stamp:
        stamp = datetime.now().strftime('%Y%m%dT%H%M%S')
        if noisy:
            print(f"add_sections: Using current time for stamp: {stamp}")
    else:
        if noisy:
            print(f"add_sections: Using extracted stamp: {stamp}")
    
    # Import RDFLib components inside function for robustness
    from rdflib import Namespace, Literal
    from rdflib.collection import Collection
    
    # Define namespaces
    seqweb_ns = Namespace("http://www.seqweb.org/")
    oeis_ns = Namespace("http://www.oeis.org/")
    cnt_ns = Namespace("http://www.w3.org/2011/content#")
    
    # Collect all text IRIs for the main hasTextList
    all_text_iris = []
    
    # Process each section type
    for section_type, strings in section_map.items():
        if noisy:
            print(f"add_sections: Processing {section_type} section with {len(strings)} text blocks")
        
        # Process each string in the section
        for index, text_content in enumerate(strings, 1):
            # Create text IRI using template: seq:{id}_{type}_{index}_{stamp}
            text_iri = f"seq:{id}_{section_type}_{index}_{stamp}"
            
            if noisy:
                print(f"add_sections: Creating text IRI: {text_iri}")
            
            # Add triples for this text block
            text_uri = seqweb_ns[text_iri.replace('seq:', '')]
            
            # {iri} a seq:Text .
            graph.add((text_uri, seqweb_ns["Sequence"], seqweb_ns["Text"]))
            
            # {iri} cnt:chars {str} .
            graph.add((text_uri, cnt_ns["chars"], Literal(text_content, lang="en")))
            
            all_text_iris.append(text_uri)
    
    # Add single hasTextList triple for all text blocks
    if all_text_iris:
        # Create RDF list from all text IRIs using BNode
        from rdflib import BNode
        list_head = BNode()
        text_list = Collection(graph, list_head, all_text_iris)
        
        # oeis:{id} seq:hasTextList (all_text_iris) .
        sequence_uri = oeis_ns[id]
        graph.add((sequence_uri, seqweb_ns["hasTextList"], list_head))
        
        if noisy:
            print(f"add_sections: Added hasTextList with {len(all_text_iris)} total text blocks")
    
    if noisy:
        print(f"add_sections: Added {len(graph)} total triples to graph")
    
    return {**box, 'graph': graph}


def _extract_stamp_from_i_section(i_sections: List[str]) -> str:
    """
    Extract timestamp from I section content.
    
    Args:
        i_sections: List of strings from the I section
        
    Returns:
        Timestamp string in yyyymmddThhmmss format, or empty string if not found
    """
    if not i_sections:
        return ""
    
    # Look for date/time pattern in I section content
    # Common patterns: "Feb 21 2025 13:07:04", "2025-02-21 13:07:04", etc.
    i_content = ' '.join(i_sections)
    
    # Try various date/time patterns
    patterns = [
        r'(\w{3})\s+(\d{1,2})\s+(\d{4})\s+(\d{1,2}):(\d{2}):(\d{2})',  # "Feb 21 2025 13:07:04"
        r'(\d{4})-(\d{2})-(\d{2})\s+(\d{1,2}):(\d{2}):(\d{2})',        # "2025-02-21 13:07:04"
        r'(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})',                # "20250221T130704"
    ]
    
    month_map = {
        'Jan': '01', 'Feb': '02', 'Mar': '03', 'Apr': '04',
        'May': '05', 'Jun': '06', 'Jul': '07', 'Aug': '08',
        'Sep': '09', 'Oct': '10', 'Nov': '11', 'Dec': '12'
    }
    
    for pattern in patterns:
        match = re.search(pattern, i_content)
        if match:
            groups = match.groups()
            
            if len(groups) == 6:
                if pattern == patterns[0]:  # Month name format
                    month_name, day, year, hour, minute, second = groups
                    month = month_map.get(month_name, '01')
                    return f"{year}{month}{day.zfill(2)}T{hour.zfill(2)}{minute}{second}"
                elif pattern == patterns[1]:  # ISO format
                    year, month, day, hour, minute, second = groups
                    return f"{year}{month}{day}T{hour.zfill(2)}{minute}{second}"
                elif pattern == patterns[2]:  # Already in target format
                    return f"{groups[0]}{groups[1]}{groups[2]}T{groups[3]}{groups[4]}{groups[5]}"
    
    return ""


def main():
    """CLI entry point for testing the add_sections module."""
    import sys
    import json
    from argparse import ArgumentParser
    
    parser = ArgumentParser(description='Add RDF triples for text blocks in section_map')
    parser.add_argument('--id', required=True, help='Sequence ID')
    parser.add_argument('--section-map', required=True, help='JSON string of section_map')
    parser.add_argument('--noisy', action='store_true', help='Enable verbose output')
    
    args = parser.parse_args()
    
    try:
        from rdflib import Graph
        section_map = json.loads(args.section_map)
        graph = Graph()
        
        result = add_sections(
            box={},
            id=args.id,
            section_map=section_map,
            graph=graph,
            noisy=args.noisy
        )
        
        print(f"Graph has {len(result['graph'])} triples")
        print("Turtle output:")
        print(result['graph'].serialize(format='turtle'))
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
