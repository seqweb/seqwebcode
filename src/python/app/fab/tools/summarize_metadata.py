#!/usr/bin/env python3
"""
Utility: summarize_metadata

Scans a directory for .ttl files, extracts metadata from their first lines,
and creates a summary file with aggregated statistics.
"""

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Dict, Any


def summarize_metadata(directory_path: str) -> Dict[str, Any]:
    """
    Scan directory for .ttl files and extract metadata from their first lines.
    
    Args:
        directory_path: Path to directory containing .ttl files
        
    Returns:
        Summary dictionary with aggregated metadata
    """
    # Convert to Path object for easier handling
    dir_path = Path(directory_path)
    
    if not dir_path.exists():
        raise FileNotFoundError(f"Directory does not exist: {directory_path}")
    
    if not dir_path.is_dir():
        raise NotADirectoryError(f"Path is not a directory: {directory_path}")
    
    # Initialize summary counters
    total_triples = 0
    total_entities = 0
    total_chars = 0
    files_processed = 0
    files_skipped = 0
    
    # Scan for .ttl files
    ttl_files = list(dir_path.glob("*.ttl"))
    
    for ttl_file in ttl_files:
        try:
            with open(ttl_file, 'r', encoding='utf-8') as f:
                first_line = f.readline().strip()
                
                # Check if first line starts with metadata comment
                if first_line.startswith("# "):
                    # Extract JSON from comment line
                    json_str = first_line[2:]  # Remove "# " prefix
                    metadata = json.loads(json_str)
                    
                    # Extract values from metadata
                    triples = metadata.get('triples', 0)
                    entities = metadata.get('entities', 0)
                    chars = metadata.get('chars', 0)
                    
                    # Add to totals
                    total_triples += triples
                    total_entities += entities
                    total_chars += chars
                    files_processed += 1
                else:
                    # No metadata line found
                    files_skipped += 1
                    
        except Exception:
            # Any error reading/parsing file - silently skip
            files_skipped += 1
    
    # Build summary dictionary
    summary = {
        "summary": "v1.0",
        "at": "2025-09-11T08:10:00.000000Z",  # Current timestamp
        "by": "app.fab.summarize_metadata.main",  # Call trace
        "path": str(dir_path.absolute()),  # Absolute path
        "files": files_processed,
        "triples": total_triples,
        "entities": total_entities,
        "chars": total_chars,
        "skipped": files_skipped
    }
    
    return summary


def main():
    """Main entry point for the utility."""
    parser = argparse.ArgumentParser(description="Summarize metadata from .ttl files in a directory")
    parser.add_argument("directory", help="Path to directory containing .ttl files")
    
    args = parser.parse_args()
    
    try:
        # Generate summary
        summary = summarize_metadata(args.directory)
        
        # Write summary file
        summary_file = Path(args.directory) / ".summary.txt"
        with open(summary_file, 'w', encoding='utf-8') as f:
            f.write(f"# {json.dumps(summary)}\n")
        
        print(f"Summary written to: {summary_file}")
        print(f"Processed {summary['files']} files, skipped {summary['skipped']} files")
        print(f"Total: {summary['triples']} triples, {summary['entities']} entities, {summary['chars']} chars")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
