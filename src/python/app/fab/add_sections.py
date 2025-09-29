#!/usr/bin/env python3
"""
Module: add_sections

Adds RDF triples for all text blocks in section_map, creating Text resources
and attaching them to the sequence via hasTextList properties.
"""

import re
from datetime import datetime
from typing import Dict, List, Any

from rdflib import BNode, RDF, Literal, Namespace  # type: ignore[import-untyped]


def add_sections(box: Dict[str, Any], *,
                 id: str, section_map: Dict[str, List[str]], graph, ontology, noisy: bool = False,
                 **_rest) -> Dict[str, Any]:
    """
    Add RDF triples for all text blocks in section_map.

    Args:
        box: The processing box containing input data
        id: The sequence ID (e.g., "A000001")
        section_map: Dictionary mapping section types to lists of strings
        graph: The RDFLib Graph to add triples to
        ontology: The SeqWeb ontology Graph containing class definitions
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

    # Get namespace mappings as a dict for easy lookup
    namespaces = dict(graph.namespaces())

    # Convert URIRef objects to Namespace objects for use
    seq_ns = Namespace(str(namespaces["seq"]))
    oeis_ns = Namespace(str(namespaces["oeis"]))
    cnt_ns = Namespace(str(namespaces["cnt"]))


    # Create URI for the sequence
    sequence_uri = oeis_ns[id]
    # Collect all text IRIs for the main hasTextList
    all_text_iris = []

    # Process each section type
    for section_type, strings in section_map.items():
        if noisy:
            print(f"add_sections: Processing {section_type} section with {len(strings)} text blocks")

        # collect all the IRIs for the sections
        text_iris = []

        # Process each string in the section
        for index, text_content in enumerate(strings, 1):
            # Create text IRI using template: seq:{id}_{type}_{index}_{stamp}
            text_iri = f"seq:{id}_{section_type}_{index}_{stamp}"

            if noisy:
                print(f"add_sections: Creating text IRI: {text_iri}")

            # Add triples for this text block
            text_uri = seq_ns[text_iri.replace('seq:', '')]  # make a URI from the IRI

            # Query ontology for class with this specific letter code
            letter_literal = Literal(section_type)
            class_triples = list(ontology.triples((None, seq_ns["letterCode"], letter_literal)))
            if not class_triples:
                raise ValueError(f"❌ No class found for section type '{section_type}' in ontology")
            text_class = class_triples[0][0]  # First (and should be only) class URI
            graph.add((text_uri, RDF.type, text_class))                              # {iri} a <section-specific-class> .
            graph.add((text_uri, cnt_ns["chars"], Literal(text_content, lang="en")))  # {iri} cnt:chars {str} .

            text_iris.append(text_uri)

        # make a triple-style "list" of all the text IRIs for the section, and add it to the sequence
        head = RDF.nil
        for iri in reversed(text_iris): # we could do this the "classic" recursive way, but... <shrug>
            tail = head
            head = BNode()  # head is a cons "cell"
            graph.add((head, RDF.first, iri))
            graph.add((head, RDF.rest, tail))
        graph.add((sequence_uri, seq_ns["hasTextList"], head))

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


# Reclaimed from test hijacking
def main():
    """Shell wrapper for add_sections module."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys

    # Define argument specifications for this module
    argument_definitions = [
        ('id', str, 'Sequence ID', True),
        ('section_map', dict, 'Dictionary mapping section types to lists of strings', True),
        ('graph', object, 'The RDFLib Graph to add triples to', True),
        ('ontology', object, 'The SeqWeb ontology Graph containing class definitions', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)

    # Call core function with identical semantics
    outbox = add_sections(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == '__main__':
    main()
