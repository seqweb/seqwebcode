#!/usr/bin/env python3
"""
fab0 - First SeqWeb fabricator for testing the pipeline framework.

This fabricator runs a simple pipeline consisting of a single module (mod0)
that prints the incoming A-number. It demonstrates the basic fabricator
pattern and pipeline execution.
"""

from typing import Dict, Any

from tools.pipeline import run_pipeline
from mod0 import mod0


def fab0(box: Dict[str, Any], *, id: str, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Fabricator function that processes an ID through the pipeline.

    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics.

    Args:
        box: Full input box dictionary
        id: The ID to process (e.g., "A000001")
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
    modules = [mod0]

    # Run the pipeline using the box-then-kwargs pattern
    result = run_pipeline(modules, initial_box)

    return result


# Reclaimed from test hijacking
def main():
    """Shell wrapper for fab0 fabricator."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys

    # Define argument specifications for this fabricator
    argument_definitions = [
        ('id', str, 'The ID to process', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)

    # Call core function with identical semantics
    outbox = fab0(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == "__main__":
    main()
