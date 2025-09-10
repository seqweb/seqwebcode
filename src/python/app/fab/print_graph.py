#!/usr/bin/env python3
"""
dump_graph - Module that serializes an RDFLib Graph to RDF/Turtle format.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It takes a 'graph' from the box,
serializes it to RDF/Turtle format, and prints it prettily.

Dependencies:
- RDFLib (installed via 'seqwebdev setup python')
"""

import json
import sys
from typing import Dict, Any

try:
    from rdflib import Graph
except ImportError:
    Graph = None  # Will be handled in the function


def dump_graph(box: Dict[str, Any], *, graph: Graph, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that serializes an RDFLib Graph to RDF/Turtle format.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Serializes
    the graph to Turtle format and prints it prettily.
    
    Args:
        box: Full input box dictionary
        graph: The RDFLib Graph to serialize
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with 'turtle_output' key containing the serialized graph
        
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
    
    # Serialize the graph to Turtle format
    turtle_output = graph.serialize(format='turtle')
    
    # Print the Turtle output prettily
    if noisy:
        print("dump_graph: RDF/Turtle output:")
        print("=" * 60)
        print(turtle_output)
        print("=" * 60)
    else:
        # Even in silent mode, we print the Turtle output since that's the main purpose
        print(turtle_output)
    
    # Return the outbox with the turtle output added, preserving all other keys
    return {**box, 'turtle_output': turtle_output}


def main():
    """Shell wrapper for dump_graph module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="dump_graph - Serialize RDFLib Graph to RDF/Turtle format")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    # Create box from command line arguments
    # Note: This module expects a graph to be passed in, but for standalone testing
    # we'll create an empty graph
    try:
        if Graph is None:
            raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
        
        # Create an empty graph for testing
        test_graph = Graph()
        
        box = {
            'graph': test_graph,
            'noisy': args.noisy
        }
        
        # Process the box using destructuring pattern
        outbox = dump_graph(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        json_output = {
            'turtle_output_length': len(outbox.get('turtle_output', '')),
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
