#!/usr/bin/env python3
"""
Module: get_metadata

Extracts metadata about the RDF graph and processing context.
Creates a metadata dictionary with version, timestamp, call stack, and graph statistics.
"""

import sys
from datetime import datetime
from typing import Dict, Any, Set
from libs.core.util import get_call_trace


def get_metadata(box: Dict[str, Any], *, graph, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Extract metadata about the RDF graph and processing context.
    
    Requirements:
    - "metadata": "v1.0" (version string)
    - "at": {current date/time in ISO format}
    - "by": {full call stack with function names separated by ">"}
    - "triples": {number of triples in graph}
    - "entities": {number of distinct entities (subjects with rdf:type predicates) in graph}
    - "chars": {total bytes needed for string literals in graph}
    
    Args:
        box: The processing box containing input data
        graph: The RDFLib Graph to analyze
        noisy: Whether to print debug information
        **_rest: Additional keyword arguments (ignored)
    
    Returns:
        Updated box with metadata dictionary
    """
    if noisy:
        print(f"get_metadata: Analyzing graph with {len(graph)} statements")
    
    # Requirements: Basic metadata
    metadata = {
        "metadata": "v1.0",
        "at": datetime.now().isoformat() + "Z"
    }
    
    # Requirements: Call stack (inspired by seqvar's approach)
    try:
        metadata["by"] = get_call_trace()
    except Exception as e:
        if noisy:
            print(f"get_metadata: Warning - could not determine call stack: {e}")
        metadata["by"] = "unknown"
    
    # Requirements: Graph statistics using RDFLib methods
    metadata["triples"] = len(graph)
    
    # Count distinct entities (subjects with rdf:type predicates) and string literal bytes in a single iteration
    entities: Set[Any] = set()
    total_chars = 0
    
    try:
        from rdflib import Literal, RDF
        
        for triple in graph:
            subject, predicate, object = triple
            
            # Collect entity (subject) only if predicate is rdf:type
            if predicate == RDF.type:
                entities.add(subject)
            
            # Count string literal bytes for all terms in this triple
            for term in triple:
                if isinstance(term, Literal):
                    # Get the string value and count UTF-8 bytes
                    str_value = str(term)
                    total_chars += len(str_value.encode('utf-8'))
                    
    except Exception as e:
        if noisy:
            print(f"get_metadata: Warning - could not analyze graph: {e}")
        total_chars = 0
    
    metadata["entities"] = len(entities)
    metadata["chars"] = total_chars
    
    if noisy:
        print(f"get_metadata: Generated metadata: {metadata}")
    
    return {**box, 'metadata': metadata}


def main():
    """CLI entry point for testing the get_metadata module."""
    import json
    from argparse import ArgumentParser
    
    parser = ArgumentParser(description='Extract metadata from RDF graph')
    parser.add_argument('--noisy', action='store_true', help='Enable verbose output')
    
    args = parser.parse_args()
    
    try:
        from rdflib import Graph
        
        # Create a test graph
        test_graph = Graph()
        test_graph.add((test_graph.namespace_manager.namespace('http://example.org/'), 
                       test_graph.namespace_manager.namespace('http://example.org/'), 
                       test_graph.namespace_manager.namespace('http://example.org/')))
        
        result = get_metadata(
            box={},
            graph=test_graph,
            noisy=args.noisy
        )
        
        print(json.dumps(result, indent=2))
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
