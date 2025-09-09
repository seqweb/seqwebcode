#!/usr/bin/env python3
"""
make_one - Fabricator that standardizes ID and initializes an RDF graph.

This fabricator runs a pipeline consisting of standardize_id -> init_graph -> init_sequence -> get_oeis_data -> add_raw_text -> dump_graph
that demonstrates ID standardization followed by RDF graph initialization, sequence declaration, OEIS data loading, raw text triples, and output.
The pipeline performs: standardize ID, create RDF graph with prefixes, declare sequence, load OEIS data, add raw text triples, dump graph to Turtle.
"""

import json
import sys
from typing import Dict, Any

from pipeline import run_pipeline
from standardize_id import standardize_id
from init_graph import init_graph
from init_sequence import init_sequence
from get_oeis_data import get_oeis_data
from add_raw_text import add_raw_text
from dump_graph import dump_graph


def make_one(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Fabricator function that processes an ID through the pipeline.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics.
    
    Args:
        box: Full input box dictionary
        id: The ID to process (e.g., "a000001")
        noisy: Whether to enable verbose output
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        Final outbox from the pipeline execution
    """
    # Create initial box from destructured parameters
    initial_box = {
        'id': id,
        'noisy': noisy,
        **_rest  # Preserve any extra keys from the input box
    }
    
    # Define the pipeline modules: standardize_id -> init_graph -> init_sequence -> get_oeis_data -> add_raw_text -> dump_graph
    modules = [
        standardize_id,    # Standardize ID to A###### format
        init_graph,        # Create RDF graph with SeqWeb prefixes and rdfs:label
        init_sequence,     # Add sequence declaration (oeis:{id} a seq:Sequence)
        get_oeis_data,     # Load OEIS data file contents
        add_raw_text,      # Add raw text triples with RDF list
        dump_graph         # Serialize graph to Turtle format
    ]
    
    # Run the pipeline using the box-then-kwargs pattern
    result = run_pipeline(modules, initial_box)
    
    return result


def main():
    """CLI wrapper for make_one fabricator."""
    import argparse
    
    parser = argparse.ArgumentParser(description="make_one - Standardize ID and initialize RDF graph")
    parser.add_argument("id", help="The ID to process")
    parser.add_argument("--noisy", action="store_true", help="Enable verbose output")
    
    args = parser.parse_args()
    
    try:
        # Create box from command line arguments
        box = {
            'id': args.id,
            'noisy': args.noisy
        }
        
        # Run the fabricator using destructuring pattern
        result = make_one(box, **box)
        
        # Output the result as JSON (following polyglot pattern)
        # Include both JSON-LD serialization and manual summary
        graph = result.get('graph')
        json_ld = None
        if graph:
            try:
                json_ld = json.loads(graph.serialize(format='json-ld'))
            except Exception:
                json_ld = None
        
        json_output = {
            'id': result.get('id'),
            'graph_type': 'rdflib.Graph',
            'graph_size': len(graph) if graph else 0,
            'graph_json_ld': json_ld,
            'oeis_data_length': len(result.get('oeis_data', '')),
            'noisy': result.get('noisy', False)
        }
        
        json.dump(json_output, sys.stdout)
        print()  # Add newline for readability
        
    except Exception as e:
        # Let Python fail naturally as specified
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
