#!/usr/bin/env python3
"""
init_graph - Module that creates an empty RDFLib Graph object.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It creates an empty RDFLib Graph
and adds it to the box with the key 'graph'.

Dependencies:
- RDFLib (installed via 'seqwebdev setup python')
"""

import json
import sys
from typing import Dict, Any

try:
    from rdflib import Graph, Namespace, Literal
    from rdflib.namespace import RDFS
except ImportError:
    Graph = None  # Will be handled in the function


def init_graph(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that creates an RDFLib Graph with SeqWeb prefix declarations.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Creates a new
    RDFLib Graph object with SeqWeb namespace prefixes and adds initial triples.
    
    Args:
        box: Full input box dictionary
        id: The sequence ID to add initial triples for
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with 'graph' key containing an initialized RDFLib Graph
        
    Raises:
        ImportError: If RDFLib is not available
    """
    # Check if RDFLib is available
    if Graph is None:
        raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
    
    # Create a new RDFLib Graph
    graph = Graph()
    
    # Define SeqWeb namespaces and bind them to prefixes
    seqweb_ns = Namespace("http://www.seqweb.org/")
    oeis_ns = Namespace("http://www.oeis.org/")
    cnt_ns = Namespace("http://www.w3.org/2011/content#")
    
    # Bind prefixes to the graph
    graph.bind("", seqweb_ns)  # Default namespace
    graph.bind("seq", seqweb_ns)
    graph.bind("oeis", oeis_ns)
    graph.bind("rdfs", RDFS)
    graph.bind("cnt", cnt_ns)
    
    # Add initial triples for the sequence
    sequence_uri = oeis_ns[id]
    
    # Add rdfs:label triple: oeis:{id} rdfs:label "id"@en
    graph.add((sequence_uri, RDFS.label, Literal(id, lang="en")))
    
    # Print status in noisy mode
    if noisy:
        print(f"init_graph: Created RDFLib Graph with {len(graph)} statements and SeqWeb prefixes")
    
    # Return the outbox with the graph added, preserving all other keys
    return {**box, 'graph': graph}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for init_graph module."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys
    
    # Define argument specifications for this module
    argument_definitions = [
        ('id', str, 'The sequence ID to add initial triples for', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)
    
    # Call core function with identical semantics
    outbox = init_graph(inbox, **inbox)
    
    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == "__main__":
    main()
