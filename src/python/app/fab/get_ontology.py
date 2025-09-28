#!/usr/bin/env python3
"""
get_ontology - Module that loads the SeqWeb ontology and passes it along in the box.

This module loads the ontology from the seqwebcode repository using the seqvar system
and makes it available to downstream modules as an RDFlib Graph object.
"""

import os
from pathlib import Path
from typing import Dict, Any, Optional
from rdflib import Graph


def get_ontology(box: Dict[str, Any], *, ontology: Optional[Graph] = None, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Load the SeqWeb ontology and return it as part of the outbox.
    
    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics.
    
    Args:
        box: Full input box dictionary
        ontology: Optional pre-loaded ontology Graph (if not provided, will load from file)
        noisy: Whether to enable verbose output (controls printing)
        **_rest: Any additional keys in the box (preserved for pass-through)
        
    Returns:
        outbox: The box plus the loaded ontology Graph
        
    Raises:
        ImportError: If seqvar system is not available
        RuntimeError: If seqvar 'repos.seqwebcode' is not set or ontology file not found
    """
    if ontology is not None:
        if noisy:
            print(f"get_ontology: Using provided ontology with {len(ontology)} triples")
        return {**box, 'ontology': ontology}
    
    # Import seqvar function inside the function (like CLI commands do)
    try:
        from libs.core.seqvar.seqvar import get as seqvar_get
    except ImportError:
        raise ImportError("❌ seqvar system not available")
    
    # Get the seqwebcode repository path from seqvar
    try:
        seqwebcode_path = Path(seqvar_get("repos.seqwebcode"))
    except Exception as e:
        raise RuntimeError(f"❌ Failed to get seqwebcode path from seqvar: {e}")
    
    if not seqwebcode_path.exists():
        raise RuntimeError(f"❌ seqwebcode path does not exist: {seqwebcode_path}")
    
    # Construct path to ontology file
    ontology_path = seqwebcode_path / "ontology" / "seqweb.ttl"
    
    if not ontology_path.exists():
        raise RuntimeError(f"❌ Ontology file not found at {ontology_path}")
    
    if noisy:
        print(f"get_ontology: Loading ontology from {ontology_path}")
    
    # Load the ontology
    ontology = Graph()
    ontology.parse(str(ontology_path), format="turtle")
    
    if noisy:
        print(f"get_ontology: Loaded ontology with {len(ontology)} triples")
    
    return {**box, 'ontology': ontology}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for get_ontology module."""
    from libs.core.wrapper import get_inbox, dump_outbox
    
    # Define argument specifications for this module
    argument_definitions = [
        ('ontology', object, 'Optional pre-loaded ontology Graph', False),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = get_inbox(argument_definitions)
    
    # Call core function with identical semantics
    outbox = get_ontology(inbox, **inbox)
    
    # Emit JSON output for pipeline consumption
    dump_outbox(outbox)


if __name__ == '__main__':
    main()
