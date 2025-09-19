#!/usr/bin/env python3
"""
Fabricator: make_range

Processes a range of OEIS sequence IDs by calling make_one for each ID.
Supports range specification and optional replacement of existing files.
"""

import argparse
import json
import sys
from pathlib import Path
from typing import Dict, Any, List
from make_one import make_one
from standardize_id import get_standard_id


def make_range(box: Dict[str, Any], *, start_id: str, end_id: str, replace: bool = False, noisy: bool = False, **_rest) -> Dict[str, Any]:
    """
    Process a range of OEIS sequence IDs by calling make_one for each ID.
    
    Args:
        box: The processing box containing input data
        start_id: Starting sequence ID (e.g., "A000001")
        end_id: Ending sequence ID (e.g., "A000010") 
        replace: Whether to replace existing .ttl files (default: False)
        noisy: Whether to print debug information
        **_rest: Additional keyword arguments (ignored)
    
    Returns:
        Updated box with processing results
    """
    if noisy:
        print(f"make_range: Processing range {start_id} to {end_id}, replace={replace}")
    
    # Standardize input IDs to A###### format
    try:
        start_id_std = get_standard_id(start_id)
        end_id_std = get_standard_id(end_id)
    except ValueError as e:
        raise ValueError(f"Invalid ID format: {e}") from e
    
    # Extract numeric parts of standardized IDs
    try:
        start_num = int(start_id_std[1:])  # Remove 'A' prefix
        end_num = int(end_id_std[1:])
    except (ValueError, IndexError) as e:
        raise ValueError(f"Invalid ID format: {start_id_std} or {end_id_std}. Expected format: A######") from e
    
    if start_num > end_num:
        raise ValueError(f"Start ID {start_id_std} must be <= end ID {end_id_std}")
    
    # Generate list of IDs to process
    ids_to_process = [f"A{i:06d}" for i in range(start_num, end_num + 1)]
    
    if noisy:
        print(f"make_range: Generated {len(ids_to_process)} IDs to process")
    
    # Import seqvar to get data paths
    from libs.core.seqvar.seqvar import get as seqvar_get
    
    try:
        seqwebdata_path = Path(seqvar_get("repos.seqwebdata"))
        oeisdata_path = Path(seqvar_get("repos.oeisdata"))
    except Exception as e:
        raise RuntimeError(f"Could not get data paths: {e}") from e
    
    processed_ids = []
    preexisting_ids = []
    missing_ids = []
    error_ids = []
    
    # Process each ID
    for seq_id in ids_to_process:
        if noisy:
            print(f"make_range: Processing {seq_id}")
        
        # Check if file already exists (unless replace=True)
        if not replace:
            folder = seq_id[:4]  # First 4 characters (e.g., "A000")
            ttl_file = seqwebdata_path / "seq" / folder / f"{seq_id}.ttl"
            
            if ttl_file.exists():
                if noisy:
                    print(f"make_range: Skipping {seq_id} - file already exists: {ttl_file}")
                preexisting_ids.append(seq_id)
                continue
        
        # Check if source file exists before calling make_one
        folder = seq_id[:4]  # First 4 characters (e.g., "A000")
        source_file = Path(oeisdata_path) / "seq" / folder / f"{seq_id}.seq"
        
        if not source_file.exists():
            if noisy:
                print(f"make_range: Skipping {seq_id} - source file missing: {source_file}")
            missing_ids.append(seq_id)
            continue
        
        # Call make_one for this ID
        try:
            result = make_one(
                box={
                    'id': seq_id,
                    'noisy': noisy,
                    **_rest
                },
                id=seq_id,
                noisy=noisy,
                **_rest
            )
            
            if noisy:
                print(f"make_range: Successfully processed {seq_id}")
            processed_ids.append(seq_id)
            
        except Exception as e:
            # Any error during processing is a real error
            if noisy:
                print(f"make_range: Error processing {seq_id}: {e}")
            error_ids.append({'id': seq_id, 'error': str(e)})
    
    # Build results
    results = {
        'processed_ids': processed_ids,
        'preexisting_ids': preexisting_ids,
        'missing_ids': missing_ids,
        'error_ids': error_ids,
        'total_requested': len(ids_to_process),
        'total_processed': len(processed_ids),
        'total_preexisting': len(preexisting_ids),
        'total_missing': len(missing_ids),
        'total_errors': len(error_ids)
    }
    
    if noisy:
        print(f"make_range: Summary - Processed: {len(processed_ids)}, Skipped: {len(preexisting_ids)}, Missing: {len(missing_ids)}, Errors: {len(error_ids)}")
    
    return {**box, 'range_results': results}


# Reclaimed from test hijacking
def main():
    """Shell wrapper for make_range fabricator."""
    from libs.core.util import build_inbox_from_args
    import json
    import sys
    
    # Define argument specifications for this fabricator
    argument_definitions = [
        ('start_id', str, 'Starting sequence ID (e.g., A000001)', True),
        ('end_id', str, 'Ending sequence ID (e.g., A000010)', True),
        ('replace', bool, 'Replace existing .ttl files', False),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    # Build inbox from stdin + CLI args using shared utility
    inbox = build_inbox_from_args(argument_definitions)
    
    # Call core function with identical semantics
    outbox = make_range(inbox, **inbox)
    
    # Emit JSON output for pipeline consumption
    json.dump(outbox, sys.stdout)


if __name__ == "__main__":
    main()
