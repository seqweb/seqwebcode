#!/usr/bin/env python3
"""
mod2: RDF Generation Module

This module generates simple RDF/Turtle output using RDFLib.
It takes an 'id' from the inbox and creates RDF statements about it.
"""

import argparse
import sys
from pathlib import Path

try:
    from rdflib import Graph, Namespace, Literal, URIRef
    from rdflib.namespace import RDF, RDFS
except ImportError as e:
    print(f"Error importing RDFLib: {e}")
    print("Please run: seqwebdev setup python")
    sys.exit(1)


def mod2(inbox: dict) -> dict:
    """
    Generate RDF for the given ID.
    
    Args:
        inbox: Dictionary containing 'id' and other data
        
    Returns:
        outbox: Dictionary with the same data plus RDF output
    """
    # Extract the ID from the inbox
    id_value = inbox.get('id', 'unknown')
    
    print(f"mod2: Generating RDF for ID: {id_value}")
    
    # Create a new RDF graph
    g = Graph()
    
    # Define namespaces
    seqweb = Namespace("http://seqweb.org/ontology/")
    example = Namespace("http://example.org/")
    
    # Create a URI for our sequence
    sequence_uri = example[f"sequence/{id_value}"]
    
    # Add some basic RDF statements
    g.add((sequence_uri, RDF.type, seqweb.Sequence))
    g.add((sequence_uri, RDFS.label, Literal(f"Sequence {id_value}")))
    g.add((sequence_uri, seqweb.hasId, Literal(id_value)))
    g.add((sequence_uri, seqweb.status, Literal("active")))
    
    # Serialize to Turtle format
    turtle_output = g.serialize(format='turtle')
    
    print("mod2: Generated RDF/Turtle:")
    print("=" * 50)
    print(turtle_output)
    print("=" * 50)
    
    # Add RDF output to the outbox
    outbox = inbox.copy()
    outbox['rdf_turtle'] = turtle_output
    
    print(f"mod2: RDF generation complete for {id_value}")
    return outbox


# Reclaimed from test hijacking
def main():
    """Shell wrapper for mod2 module."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys
    
    # Define argument specifications for this module
    argument_definitions = [
        ('id', str, 'The ID to generate RDF for', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)
    
    # Call core function with identical semantics
    outbox = mod2(inbox)
    
    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == "__main__":
    main()
