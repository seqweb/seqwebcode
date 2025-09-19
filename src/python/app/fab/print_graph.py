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
    
    # Check for metadata and prepend as comment line if present
    metadata = box.get('metadata')
    if metadata:
        import json
        metadata_line = f"# {json.dumps(metadata)}\n"
        final_output = metadata_line + turtle_output
    else:
        final_output = turtle_output
    
    # Print the Turtle output prettily
    if noisy:
        print("print_graph: RDF/Turtle output:")
        print("=" * 60)
        print(final_output)
        print("=" * 60)
    else:
        # Even in silent mode, we print the Turtle output since that's the main purpose
        print(final_output)
    
    # Return the outbox with the turtle output added, preserving all other keys
    return {**box, 'turtle_output': turtle_output}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for print_graph module."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys
    
    # Define argument specifications for this module
    argument_definitions = [
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)
    
    # Call core function with identical semantics
    outbox = dump_graph(inbox, **inbox)
    
    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == "__main__":
    main()
