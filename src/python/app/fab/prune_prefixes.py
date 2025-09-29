#!/usr/bin/env python3
"""
prune_prefixes - Module that removes unused namespace prefixes from an RDFlib Graph.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It analyzes the graph's triples
to determine which prefixes are actually used and removes unused ones.
"""

from typing import Dict, Any, Set

from rdflib import Graph, URIRef  # type: ignore[import-untyped]


def prune_prefixes(box: Dict[str, Any], *, graph: Graph, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Remove unused namespace prefixes from the graph.

    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Analyzes the
    graph's triples to find which prefixes are actually used and removes
    unused prefix bindings.

    Args:
        box: Full input box dictionary
        graph: The RDFlib Graph to prune prefixes from
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)

    Returns:
        outbox: The box with the pruned graph

    Raises:
        ValueError: If graph is not a valid RDFlib Graph
    """
    # Validate that graph is an RDFlib Graph
    if not isinstance(graph, Graph):
        raise ValueError(f"❌ Invalid graph type: expected RDFlib Graph, got {type(graph)}")

    if noisy:
        print(f"prune_prefixes: Starting with {len(list(graph.namespaces()))} bound prefixes")

    # Get all currently bound prefixes
    current_prefixes = dict(graph.namespaces())

    if not current_prefixes:
        if noisy:
            print("prune_prefixes: No prefixes bound, nothing to prune")
        return {**box, 'graph': graph}

    # Find all URIs used in the graph's triples
    used_namespaces: Set[str] = set()

    for s, p, o in graph:
        # Check subject
        if isinstance(s, URIRef):
            used_namespaces.add(str(s))
        # Check predicate
        if isinstance(p, URIRef):
            used_namespaces.add(str(p))
        # Check object (if it's a URI)
        if isinstance(o, URIRef):
            used_namespaces.add(str(o))

    if noisy:
        print(f"prune_prefixes: Found {len(used_namespaces)} unique URIs in graph")

    # Determine which prefixes are actually used
    used_prefixes = {}
    for prefix, namespace in current_prefixes.items():
        namespace_str = str(namespace)
        # Check if any URI in the graph starts with this namespace
        if any(uri.startswith(namespace_str) for uri in used_namespaces):
            used_prefixes[prefix] = namespace
            if noisy:
                print(f"prune_prefixes: Keeping prefix '{prefix}' -> {namespace_str}")
        else:
            if noisy:
                print(f"prune_prefixes: Removing unused prefix '{prefix}' -> {namespace_str}")

    # Create a new graph with only used prefixes
    pruned_graph = Graph()

    # Copy all triples from original graph
    for triple in graph:
        pruned_graph.add(triple)

    # Bind only the used prefixes
    for prefix, namespace in used_prefixes.items():
        pruned_graph.bind(prefix, namespace)

    if noisy:
        print(f"prune_prefixes: Pruned to {len(used_prefixes)} used prefixes")
        print(f"prune_prefixes: Removed {len(current_prefixes) - len(used_prefixes)} unused prefixes")

    return {**box, 'graph': pruned_graph}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for prune_prefixes module."""
    from libs.core.wrapper import get_inbox, dump_outbox

    # Define argument specifications for this module
    argument_definitions = [
        ('graph', object, 'The RDFlib Graph to prune prefixes from', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = get_inbox(argument_definitions)

    # Call core function with identical semantics
    outbox = prune_prefixes(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    dump_outbox(outbox)


if __name__ == '__main__':
    main()
