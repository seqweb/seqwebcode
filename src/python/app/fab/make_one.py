#!/usr/bin/env python3
"""
make_one - Fabricator
"""

from typing import Dict, Any

from tools.pipeline import run_pipeline
from standardize_id import standardize_id
from get_ontology import get_ontology
from init_graph import init_graph
from init_sequence import init_sequence
from get_oeis_data import get_oeis_data
from segment_sections import segment_sections
from process_sections import process_sections
from add_sections import add_sections
from get_metadata import get_metadata
from dump_graph import dump_graph
from id_only import id_only


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

    # Define the pipeline modules
    modules = [
        standardize_id,    # Standardize ID to A###### format
        get_ontology,      # Load the SeqWebn ontology
        init_graph,        # Create RDF graph with SeqWeb prefixes and rdfs:label
        init_sequence,     # Add sequence declaration (oeis:{id} a seq:Sequence)
        get_oeis_data,     # Load OEIS data file contents
        segment_sections,  # Segment OEIS data into sections by type
        process_sections,  # Concatenate S, T, U sections into V section
        add_sections,      # Add RDF triples for all text blocks
        get_metadata,      # Extract metadata about graph and processing context
        dump_graph,        # Serialize graph to Turtle format
        id_only            # just output the ID
    ]

    # Run the pipeline using the box-then-kwargs pattern
    result = run_pipeline(modules, initial_box)

    return result


# Reclaimed from test hijacking
def main():
    """Shell wrapper for make_one fabricator."""
    from libs.core.wrapper import get_inbox, dump_outbox

    # Define argument specifications for this fabricator
    argument_definitions = [
        ('id', str, 'The ID to process', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = get_inbox(argument_definitions)

    # Call core function with identical semantics
    outbox = make_one(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    dump_outbox(outbox)


if __name__ == "__main__":
    main()
