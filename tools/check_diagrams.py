#!/usr/bin/env python3
"""
Check and optionally export stale Draw.io diagrams.

Usage:
    python3 check_diagrams.py [directory] [--list-only]
    python3 check_diagrams.py docs/drawings/ --list-only  # Just list stale files
    python3 check_diagrams.py docs/drawings/              # Export stale files
"""

import argparse
import os
import subprocess
import sys
from pathlib import Path
from typing import List, Tuple


def find_stale_diagrams(directory: Path) -> List[Tuple[Path, Path]]:
    """Find .drawio files that need SVG export (SVG missing or older than .drawio)."""
    stale_files = []
    
    for drawio_file in directory.glob("*.drawio"):
        svg_file = drawio_file.with_suffix(".drawio.svg")
        
        # Check if SVG is missing or older than .drawio
        needs_export = False
        if not svg_file.exists():
            needs_export = True
        else:
            drawio_mtime = drawio_file.stat().st_mtime
            svg_mtime = svg_file.stat().st_mtime
            if drawio_mtime > svg_mtime:
                needs_export = True
        
        if needs_export:
            stale_files.append((drawio_file, svg_file))
    
    return stale_files


def export_diagram(drawio_path: Path, svg_path: Path) -> bool:
    """Export a single .drawio file to SVG."""
    try:
        cmd = [
            "drawio-export", "-f", "svg", "-o", str(svg_path), str(drawio_path)
        ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        
        if result.returncode == 0:
            print(f"✓ Exported {drawio_path.name}")
            return True
        else:
            print(f"✗ Failed to export {drawio_path.name}: {result.stderr}")
            return False
            
    except FileNotFoundError:
        print(f"✗ drawio-export CLI not found. Install with: npm install -g @mattiash/drawio-export")
        return False


def main():
    parser = argparse.ArgumentParser(description="Check and export stale Draw.io diagrams")
    parser.add_argument("directory", help="Directory to check for .drawio files")
    parser.add_argument("--list-only", action="store_true", help="Only list stale files, don't export")
    
    args = parser.parse_args()
    
    directory = Path(args.directory)
    if not directory.exists():
        print(f"Error: Directory not found: {directory}")
        sys.exit(1)
    
    stale_files = find_stale_diagrams(directory)
    
    if not stale_files:
        print("✓ All diagrams are up to date")
        return
    
    print(f"Found {len(stale_files)} diagram(s) that need export:")
    for drawio_path, svg_path in stale_files:
        if svg_path.exists():
            print(f"  ⚠ {drawio_path.name} (SVG outdated)")
        else:
            print(f"  ❌ {drawio_path.name} (SVG missing)")
    
    if args.list_only:
        return
    
    print(f"\nExporting {len(stale_files)} diagram(s)...")
    success_count = 0
    for drawio_path, svg_path in stale_files:
        if export_diagram(drawio_path, svg_path):
            success_count += 1
    
    print(f"\n✓ Successfully exported {success_count}/{len(stale_files)} diagram(s)")


if __name__ == "__main__":
    main() 