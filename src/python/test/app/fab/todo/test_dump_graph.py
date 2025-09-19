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
import os
import sys
from typing import Dict, Any

try:
    from rdflib import Graph
except ImportError:
    Graph = None  # Will be handled in the function


def dump_graph(box: Dict[str, Any], *, graph: Graph, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Core function that serializes an RDFLib Graph to RDF/Turtle format and writes to file.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Serializes
    the graph to Turtle format and writes it to a TTL file in seqwebdata.
    
    Args:
        box: Full input box dictionary
        graph: The RDFLib Graph to serialize
        id: The sequence ID (e.g., "A000001")
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box with 'turtle_output' and 'file_path' keys
        
    Raises:
        ImportError: If RDFLib is not available
        ValueError: If graph is not a valid RDFLib Graph
        RuntimeError: If seqvar system fails or file I/O fails
    """
    # Check if RDFLib is available
    if Graph is None:
        raise ImportError("❌ RDFLib not available. Please run: seqwebdev setup python")
    
    # Validate that graph is an RDFLib Graph
    if not isinstance(graph, Graph):
        raise ValueError(f"❌ Invalid graph type: expected RDFLib Graph, got {type(graph)}")
    
    if noisy:
        print(f"dump_graph: Serializing graph with {len(graph)} statements")
    
    # Get seqwebdata path from seqvar system
    from libs.core.seqvar.seqvar import get as seqvar_get
    
    try:
        seqwebdata_path = seqvar_get("repos.seqwebdata")
    except Exception as e:
        raise RuntimeError(f"❌ Failed to get seqwebdata path from seqvar: {e}")
    
    if not seqwebdata_path:
        raise RuntimeError("❌ seqwebdata path not set in seqvar system")
    
    # Compute folder and file path (analogous to get_oeis_data)
    folder = id[:4]  # First 4 characters of ID
    file_path = os.path.join(seqwebdata_path, "seq", folder, f"{id}.ttl")
    
    if noisy:
        print(f"dump_graph: Writing to {file_path}")
    
    # Create target directory if it doesn't exist
    target_dir = os.path.dirname(file_path)
    if not os.path.exists(target_dir):
        os.makedirs(target_dir)
        if noisy:
            print(f"dump_graph: Created directory {target_dir}")
    
    # Check if file already exists and create backup
    if os.path.exists(file_path):
        backup_path = f"{file_path}.bkp"
        if os.path.exists(backup_path):
            if noisy:
                print(f"dump_graph: Overwriting existing backup {backup_path}")
        os.rename(file_path, backup_path)
        if noisy:
            print(f"dump_graph: Created backup {backup_path}")
    
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
    
    # Write to file
    try:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(final_output)
        if noisy:
            print(f"dump_graph: Successfully wrote {len(final_output)} characters to {file_path}")
    except Exception as e:
        raise RuntimeError(f"❌ Failed to write TTL file {file_path}: {e}")
    
    # Print the Turtle output prettily
    if noisy:
        print("dump_graph: RDF/Turtle output:")
        print("=" * 60)
        print(final_output)
        print("=" * 60)
    else:
        # Even in silent mode, we print the Turtle output since that's the main purpose
        print(final_output)
    
    # Return the outbox with the turtle output and file path added, preserving all other keys
    return {**box, 'turtle_output': turtle_output, 'file_path': file_path}


def main():
    """Shell wrapper for dump_graph module."""
    import argparse
    
    parser = argparse.ArgumentParser(description="dump_graph - Serialize RDFLib Graph to RDF/Turtle format and write to file")
    parser.add_argument("--id", required=True, help="Sequence ID (e.g., A000001)")
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
            'id': args.id,
            'noisy': args.noisy
        }
        
        # Process the box using destructuring pattern
        outbox = dump_graph(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        json_output = {
            'turtle_output_length': len(outbox.get('turtle_output', '')),
            'graph_size': len(outbox.get('graph', [])),
            'file_path': outbox.get('file_path', ''),
            'noisy': outbox.get('noisy', False)
        }
        
        json.dump(json_output, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
