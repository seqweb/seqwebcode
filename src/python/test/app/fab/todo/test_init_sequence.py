#!/usr/bin/env python3
"""
init_sequence - Module that adds a sequence declaration to an RDFLib Graph.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It takes an 'id' from the box
and adds a triple to the 'graph' declaring it as a Sequence.

Dependencies:
- RDFLib (installed via 'seqwebdev setup python')
"""

import json
import sys
from typing import Dict, Any

try:
    from rdflib import Graph, Namespace, Literal
    from rdflib.namespace import RDF
except ImportError:
    Graph = None  # Will be handled in the function


def init_sequence(box: Dict[str, Any], *, id: str, graph: Graph, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that adds a sequence declaration to the RDFLib Graph.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Adds a triple
    declaring the sequence with the given ID.
    
    Args:
        box: Full input box dictionary
        id: The sequence ID (e.g., "A000001")
        graph: The RDFLib Graph to add the triple to
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with the updated graph containing the sequence declaration
        
    Raises:
        ImportError: If RDFLib is not available
        ValueError: If graph is not a valid RDFLib Graph
    """
    # Check if RDFLib is available
    if Graph is None:
        raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
    
    # Validate that graph is an RDFLib Graph
    if not isinstance(graph, Graph):
        raise ValueError(f"❌ Invalid graph type: expected RDFLib Graph, got {type(graph)}")
    
    # Define namespaces
    seqweb_ns = Namespace("http://www.seqweb.org/")
    oeis_ns = Namespace("http://www.oeis.org/")
    
    # Create URI for the sequence
    sequence_uri = oeis_ns[id]
    
    # Add the triple: oeis:{id} a :Sequence
    graph.add((sequence_uri, RDF.type, seqweb_ns["Sequence"]))
    
    # Print status in noisy mode
    if noisy:
        print(f"init_sequence: Added sequence declaration for {id}")
    
    # Return the outbox with the updated graph, preserving all other keys
    return {**box, 'graph': graph}


def main():
    """Shell wrapper for init_sequence module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="init_sequence - Add sequence declaration to RDFLib Graph")
    parser.add_argument("id", help="The sequence ID to declare")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    try:
        if Graph is None:
            raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
        
        # Create an empty graph for testing
        test_graph = Graph()
        
        # Define namespaces and bind them
        seqweb_ns = Namespace("http://www.seqweb.org/")
        oeis_ns = Namespace("http://www.oeis.org/")
        test_graph.bind("", seqweb_ns)
        test_graph.bind("oeis", oeis_ns)
        
        box = {
            'id': args.id,
            'graph': test_graph,
            'noisy': args.noisy
        }
        
        # Process the box using destructuring pattern
        outbox = init_sequence(box, **box)
        
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
