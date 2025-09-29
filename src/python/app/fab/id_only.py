#!/usr/bin/env python3
"""
id_only - Module that extracts only the ID from the input box.

This module follows the polyglot pipeline pattern with both a core function
and a shell wrapper for standalone execution. It extracts the 'id' from the
input box and returns an outbox containing only that ID.

Dependencies:
- None (pure Python)
"""

from typing import Dict, Any


def id_only(box: Dict[str, Any], *, id: str, **_rest) -> Dict[str, Any]:
    """
    Extract only the ID from the input box and print it.

    Uses destructuring pattern to bind only needed parameters while preserving
    the full box and any extra keys for pass-through semantics. Returns an
    outbox containing only the ID.

    Args:
        box: Full input box dictionary
        id: The ID to extract
        **_rest: Any additional keys in the box (ignored)

    Returns:
        outbox: The box containing only the 'id' key

    Raises:
        None
    """

    print(id)

    return {}

# Reclaimed from test hijacking
def main():
    """Shell wrapper for d_only module."""
    from libs.core.wrapper import get_inbox, dump_outbox

    # Define argument specifications for this module
    argument_definitions = [
        ('id', str, 'The ID to extract', True)
    ]

    # Build inbox from stdin + CLI args using shared utility
    inbox = get_inbox(argument_definitions)

    # Call core function with identical semantics
    outbox = id_only(inbox, **inbox)

    # Emit JSON output for pipeline consumption
    dump_outbox(outbox)


if __name__ == '__main__':
    main()
