#!/usr/bin/env python3
"""
Simple tool to export Draw.io files to SVG format.

Usage:
    python drawio_export.py [drawio_file] [output_dir]
    python drawio_export.py --watch [drawio_file] [output_dir]
"""

import argparse
import os
import subprocess
import sys
import time
from pathlib import Path


def export_drawio_to_svg(drawio_path: Path, output_dir: Path = None) -> bool:
    """Export a Draw.io file to SVG format."""
    if not drawio_path.exists():
        print(f"Error: Draw.io file not found: {drawio_path}")
        return False
    
    if output_dir is None:
        output_dir = drawio_path.parent
    
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Generate output filename
    svg_filename = drawio_path.stem + ".svg"
    svg_path = output_dir / svg_filename
    
    try:
        # Use drawio-export CLI to export
        # Note: drawio-export ignores -o parameter and always appends title
        # So we run it in the output directory and then rename the result
        if drawio_path.parent == output_dir:
            drawio_arg = drawio_path.name
        else:
            drawio_arg = os.path.relpath(drawio_path, output_dir)
        cmd = [
            "drawio-export", "-f", "svg", drawio_arg
        ]
        result = subprocess.run(cmd, capture_output=True, text=True, cwd=output_dir)

        if result.returncode == 0:
            # The CLI creates files with the diagram title, find the one we want
            for svg_file in output_dir.glob("*.svg"):
                if "Box Glyph" in svg_file.name:
                    # Backup existing SVG if it exists
                    if svg_path.exists():
                        backup_path = svg_path.parent / f".${svg_path.name}.bkp"
                        svg_path.rename(backup_path)
                        print(f"📦 Backed up existing SVG to {backup_path.name}")
                    
                    # Rename the CLI output to our desired filename
                    svg_file.rename(svg_path)
                    print(f"✓ Exported {drawio_path} → {svg_path}")
                    return True
            
            print(f"✗ Export succeeded but no matching file found")
            return False
        else:
            print(f"✗ Export failed: {result.stderr}")
            return False
            
    except FileNotFoundError:
        print("Error: drawio-export CLI not found. Please install the drawio-export package.")
        print("  - CLI: npm install -g @mattiash/drawio-export")
        return False


def watch_and_export(drawio_path: Path, output_dir: Path = None):
    """Watch a Draw.io file and auto-export when it changes."""
    print(f"Watching {drawio_path} for changes... (Ctrl+C to stop)")
    
    last_modified = 0
    
    try:
        while True:
            current_modified = drawio_path.stat().st_mtime
            
            if current_modified > last_modified:
                print(f"\n🔄 File changed, exporting...")
                if export_drawio_to_svg(drawio_path, output_dir):
                    last_modified = current_modified
                time.sleep(1)  # Avoid multiple exports for rapid changes
            
            time.sleep(0.5)  # Check every 500ms
            
    except KeyboardInterrupt:
        print("\n👋 Stopped watching")


def main():
    parser = argparse.ArgumentParser(description="Export Draw.io files to SVG")
    parser.add_argument("drawio_file", help="Path to .drawio file")
    parser.add_argument("output_dir", nargs="?", help="Output directory (default: same as input)")
    parser.add_argument("--watch", action="store_true", help="Watch for changes and auto-export")
    
    args = parser.parse_args()
    
    drawio_path = Path(args.drawio_file)
    output_dir = Path(args.output_dir) if args.output_dir else None
    
    if args.watch:
        watch_and_export(drawio_path, output_dir)
    else:
        success = export_drawio_to_svg(drawio_path, output_dir)
        sys.exit(0 if success else 1)


if __name__ == "__main__":
    main() 