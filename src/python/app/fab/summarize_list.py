#!/usr/bin/env python3
"""
Utility: summarize_list

Takes a list of sequence IDs, fetches metadata from their .ttl files,
and returns an accumulated summary dictionary.
"""

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List
from libs.core.util import get_call_trace
from libs.core.seqvar.seqvar import get as seqvar_get
from standardize_id import get_standard_id


def summarize_list(sequence_ids: List[str]) -> Dict[str, Any]:
    """
    Summarize metadata from a list of sequence .ttl files.
    
    Args:
        sequence_ids: List of sequence IDs (e.g., ["A000001", "A000002"])
    
    Returns:
        A dictionary containing the summary metadata.
    """
    total_triples = 0
    total_entities = 0
    total_chars = 0
    files_processed = 0
    files_skipped = 0
    
    # Get seqwebdata path
    try:
        seqwebdata_path = Path(seqvar_get("repos.seqwebdata"))
    except Exception as e:
        raise RuntimeError(f"Could not get seqwebdata path: {e}") from e
    
    # Process each sequence ID
    for seq_id in sequence_ids:
        # Standardize the sequence ID to A###### format
        try:
            seq_id_std = get_standard_id(seq_id)
        except ValueError as e:
            # Skip invalid IDs silently
            files_skipped += 1
            continue
            
        # Construct file path
        folder = seq_id_std[:4]  # First 4 characters (e.g., "A000")
        file_path = seqwebdata_path / "seq" / folder / f"{seq_id_std}.ttl"
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                first_line = f.readline().strip()
            
            if first_line.startswith("# {") and first_line.endswith("}"):
                metadata_str = first_line[2:]  # Remove '# ' prefix
                metadata = json.loads(metadata_str)
                
                total_triples += metadata.get("triples", 0)
                total_entities += metadata.get("entities", 0)
                total_chars += metadata.get("chars", 0)
                files_processed += 1
            else:
                files_skipped += 1
                
        except (IOError, json.JSONDecodeError, UnicodeDecodeError) as e:
            # Silently skip files with errors or malformed metadata
            files_skipped += 1
    
    summary = {
        "summary": "v1.0",
        "at": datetime.now().isoformat() + "Z",
        "by": get_call_trace(),
        "sequences": sequence_ids,
        "files": files_processed,
        "triples": total_triples,
        "entities": total_entities,
        "chars": total_chars,
        "skipped": files_skipped
    }
    
    return summary


def main():
    """Shell wrapper for summarize_list utility."""
    parser = argparse.ArgumentParser(description="Summarize metadata from a list of sequence .ttl files")
    parser.add_argument("sequence_ids", help="JSON list of sequence IDs (e.g., '[\"A000001\", \"A000002\"]')")
    
    args = parser.parse_args()
    
    # Parse the JSON list
    try:
        sequence_ids = json.loads(args.sequence_ids)
        if not isinstance(sequence_ids, list):
            raise ValueError("Input must be a JSON list")
    except (json.JSONDecodeError, ValueError) as e:
        print(f"Error: Invalid JSON list format: {e}", file=sys.stderr)
        sys.exit(1)
    
    try:
        # Generate summary
        summary = summarize_list(sequence_ids)
        
        # Emit JSON output
        json.dump(summary, sys.stdout, indent=2)
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
