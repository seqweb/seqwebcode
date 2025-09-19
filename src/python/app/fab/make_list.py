#!/usr/bin/env python3
"""
Fabricator: make_list

Processes a list of OEIS sequence IDs by calling make_one for each ID.
Simple implementation for one-off tasks like processing core sequences.
"""

import argparse
import json
import sys
from typing import Dict, Any, List
from make_one import make_one


def make_list(box: Dict[str, Any], *, id_list: List[str], replace: bool = False, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Process a list of OEIS sequence IDs by calling make_one for each ID.
    
    Args:
        box: The processing box containing input data
        id_list: List of sequence IDs to process (e.g., ["A000001", "A000002"])
        replace: Whether to replace existing .ttl files (default: False)
        noisy: Whether to print debug information
        **_rest: Additional keyword arguments (ignored)
    
    Returns:
        Updated box with processing results
    """
    if noisy:
        print(f"make_list: Processing {len(id_list)} IDs, replace={replace}")
    
    processed_ids = []
    preexisting_ids = []
    missing_ids = []
    error_ids = []
    
    # Process each ID
    for seq_id in id_list:
        if noisy:
            print(f"make_list: Processing {seq_id}")
        
        # Call make_one for this ID
        try:
            result = make_one(
                box={
                    'id': seq_id,
                    'noisy': noisy,
                    'replace': replace,
                    **_rest
                },
                id=seq_id,
                noisy=noisy,
                replace=replace,
                **_rest
            )
            
            if noisy:
                print(f"make_list: Successfully processed {seq_id}")
            processed_ids.append(seq_id)
            
        except Exception as e:
            # Check if this is a pipeline-wrapped file error
            if isinstance(e, RuntimeError) and "Pipeline failed" in str(e):
                # Check the original cause
                original_error = e.__cause__
                if isinstance(original_error, (FileNotFoundError, IOError)):
                    # Missing or unreadable source file - skip silently
                    if noisy:
                        print(f"make_list: Skipping {seq_id} - source file missing/unreadable")
                    missing_ids.append(seq_id)
                else:
                    # Real pipeline error
                    if noisy:
                        print(f"make_list: Error processing {seq_id}: {e}")
                    error_ids.append({'id': seq_id, 'error': str(e)})
            elif isinstance(e, (FileNotFoundError, IOError)):
                # Direct file error (shouldn't happen with pipeline, but just in case)
                if noisy:
                    print(f"make_list: Skipping {seq_id} - source file missing/unreadable")
                missing_ids.append(seq_id)
            else:
                # This is a real error, not just missing/unreadable source
                if noisy:
                    print(f"make_list: Error processing {seq_id}: {e}")
                error_ids.append({'id': seq_id, 'error': str(e)})
    
    # Build results
    results = {
        'processed_ids': processed_ids,
        'preexisting_ids': preexisting_ids,
        'missing_ids': missing_ids,
        'error_ids': error_ids,
        'total_requested': len(id_list),
        'total_processed': len(processed_ids),
        'total_preexisting': len(preexisting_ids),
        'total_missing': len(missing_ids),
        'total_errors': len(error_ids)
    }
    
    if noisy:
        print(f"make_list: Summary - Processed: {len(processed_ids)}, Skipped: {len(preexisting_ids)}, Missing: {len(missing_ids)}, Errors: {len(error_ids)}")
    
    return {**box, 'list_results': results}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for make_list fabricator."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys
    
    # Define argument specifications for this fabricator
    argument_definitions = [
        ('id_list', list, 'JSON list of sequence IDs (e.g., ["A000001", "A000002"])', True),
        ('replace', bool, 'Replace existing .ttl files', False),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)
    
    # Call core function with identical semantics
    outbox = make_list(inbox, **inbox)
    
    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == "__main__":
    main()
