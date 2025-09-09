#!/usr/bin/env python3
"""
add_raw_text - Module that adds raw text triples to an RDFLib Graph.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It adds three triples to the
graph: a text resource, a list containing that text, and the raw text content.

Dependencies:
- RDFLib (installed via 'seqwebdev setup python')
"""

import json
import sys
from typing import Dict, Any


def add_raw_text(box: Dict[str, Any], *, id: str, graph, oeis_data: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that adds raw text triples to the RDFLib Graph.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Adds three
    triples: text resource, RDF list, and raw text content.
    
    Args:
        box: Full input box dictionary
        id: The sequence ID (e.g., "A000001")
        graph: The RDFLib Graph to add triples to
        oeis_data: The raw OEIS data content
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with the updated graph containing the raw text triples
        
    Raises:
        ImportError: If RDFLib is not available
        ValueError: If graph is not a valid RDFLib Graph
    """
    # Import RDFLib inside the function (like CLI commands do)
    try:
        from rdflib import Graph, Namespace, Literal
        from rdflib.namespace import RDF
        from rdflib.collection import Collection
    except ImportError:
        raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
    
    # Validate that graph is an RDFLib Graph
    if not isinstance(graph, Graph):
        raise ValueError(f"❌ Invalid graph type: expected RDFLib Graph, got {type(graph)}")
    
    # Define namespaces
    seqweb_ns = Namespace("http://www.seqweb.org/")
    oeis_ns = Namespace("http://www.oeis.org/")
    cnt_ns = Namespace("http://www.w3.org/2011/content#")
    
    # Create URIs
    sequence_uri = oeis_ns[id]
    text_uri = seqweb_ns[f"{id}-raw-text"]
    
    # Add first triple: seq:{id}-raw-text a seq:Text
    graph.add((text_uri, RDF.type, seqweb_ns["Text"]))
    
    # Add second triple: oeis:{id} seq:hasTextList (seq:{id}-raw-text)
    # Create an RDF list containing the text resource
    text_list = Collection(graph, text_uri)
    text_list.append(text_uri)
    
    # Add the hasTextList property
    graph.add((sequence_uri, seqweb_ns["hasTextList"], text_list.uri))
    
    # Add third triple: seq:{id}-raw-text cnt:chars "{oeis_data}"@en
    graph.add((text_uri, cnt_ns["chars"], Literal(oeis_data, lang="en")))
    
    # Print status in noisy mode
    if noisy:
        print(f"add_raw_text: Added raw text triples for {id} with {len(oeis_data)} characters")
    
    # Return the outbox with the updated graph, preserving all other keys
    return {**box, 'graph': graph}


def main():
    """Shell wrapper for add_raw_text module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="add_raw_text - Add raw text triples to RDFLib Graph")
    parser.add_argument("id", help="The sequence ID to add raw text for")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    try:
        # Import RDFLib inside the function
        try:
            from rdflib import Graph, Namespace, Literal
            from rdflib.namespace import RDF
            from rdflib.collection import Collection
        except ImportError:
            raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
        
        # Create test graph and data for standalone testing
        test_graph = Graph()
        test_oeis_data = "Test OEIS data content"
        
        # Define namespaces and bind them
        seqweb_ns = Namespace("http://www.seqweb.org/")
        oeis_ns = Namespace("http://www.oeis.org/")
        cnt_ns = Namespace("http://www.w3.org/2011/content#")
        test_graph.bind("", seqweb_ns)
        test_graph.bind("oeis", oeis_ns)
        test_graph.bind("cnt", cnt_ns)
        
        box = {
            'id': args.id,
            'graph': test_graph,
            'oeis_data': test_oeis_data,
            'noisy': args.noisy
        }
        
        # Process the box using destructuring pattern
        outbox = add_raw_text(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        json_output = {
            'id': outbox.get('id'),
            'graph_type': 'rdflib.Graph',
            'graph_size': len(outbox.get('graph', [])),
            'noisy': outbox.get('noisy', False)
        }
        
        json.dump(json_output, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
