#!/usr/bin/env python3
"""
init_sequence - Module that adds a sequence declaration to an RDFLib Graph.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It takes an 'id' from the box
and adds a triple to the 'graph' declaring it as a Sequence.

Dependencies:
- RDFLib (installed via 'seqwebdev setup python')
"""

from typing import Dict, Any

try:
    from rdflib import Graph, Namespace, Literal  # type: ignore[import-untyped]
    from rdflib.namespace import RDF, RDFS  # type: ignore[import-untyped]
except ImportError:
    Graph = None  # Will be handled in the function


def init_sequence(box: Dict[str, Any], *,
                  id: str, graph: Graph, noisy: bool = False,
                  **_rest) -> Dict[str, Any]:
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

    # Get namespace mappings as a dict for easy lookup
    namespaces = dict(graph.namespaces())

    # Convert URIRef objects to Namespace objects for use
    seq_ns = Namespace(str(namespaces["seq"]))
    oeis_ns = Namespace(str(namespaces["oeis"]))

    # Create URI for the sequence
    sequence_uri = oeis_ns[id]

    # Add the triple: oeis:{id} a seq:Sequence
    graph.add((sequence_uri, RDF.type, seq_ns["Sequence"]))

    # Add the triple: oeis:{id} rdfs:label "id"@en
    graph.add((sequence_uri, RDFS.label, Literal(id, lang="en")))


    # Print status in noisy mode
    if noisy:
        print(f"init_sequence: Added sequence declaration for {id}")

    # Return the outbox with the updated graph, preserving all other keys
    return {**box, 'graph': graph}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for init_sequence module."""
    from libs.core.wrapper import get_inbox, dump_outbox

    # Define argument specifications for this module
    argument_definitions = [
        ('id', str, 'The sequence ID to declare', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = get_inbox(argument_definitions)

    # Call core function with identical semantics
    outbox = init_sequence(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    dump_outbox(outbox)


if __name__ == "__main__":
    main()
