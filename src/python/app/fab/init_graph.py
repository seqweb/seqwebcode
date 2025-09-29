#!/usr/bin/env python3
"""
init_graph - Module that creates an empty RDFLib Graph object.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It creates an empty RDFLib Graph
and adds it to the box with the key 'graph'.

Dependencies:
- RDFLib (installed via 'seqwebdev setup python')
"""

from typing import Dict, Any

from rdflib import Graph, Literal  # type: ignore[import-untyped]
from rdflib.namespace import RDFS  # type: ignore[import-untyped]


def init_graph(box: Dict[str, Any], *,
               id: str, ontology: Graph, noisy: bool = False,
               **_rest) -> Dict[str, Any]:
    """
    Core function that creates an RDFLib Graph with SeqWeb prefix declarations.

    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Creates a new
    RDFLib Graph object with SeqWeb namespace prefixes and adds initial triples.

    Args:
        box: Full input box dictionary
        id: The sequence ID to add initial triples for
        ontology: The SeqWeb ontology Graph containing namespace definitions
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)

    Returns:
        outbox: The box with 'graph' key containing an initialized RDFLib Graph

    Raises:
        ImportError: If RDFLib is not available (fails fast)
    """
    # Create a new RDFLib Graph
    graph = Graph()

    # Copy namespace prefixes from the ontology
    for prefix, namespace in ontology.namespaces():
        graph.bind(prefix, namespace)

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
        ('ontology', object, 'The SeqWeb ontology Graph containing namespace definitions', True),
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
