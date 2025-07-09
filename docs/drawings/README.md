# Drawings Directory

This directory contains Draw.io diagrams (`.drawio` files) and their corresponding SVG exports (`.svg` files) used in SeqWeb documentation.

## Diagram Management

### Best Practice
- **Always export SVGs manually** when editing diagrams in Draw.io
- Keep both `.drawio` and `.drawio.svg` files together in this directory
- Use descriptive filenames (e.g., `box-glyph.drawio` and `box-glyph.drawio.svg`)
- Draw.io automatically appends `.svg` to the full filename, creating `.drawio.svg` files

### Checking for Stale Diagrams
If you want to verify that all diagrams are up to date, or export any that are missing/outdated:

```bash
# List diagrams that need export (without exporting)
python3 tools/check_diagrams.py docs/drawings/ --list-only

# Export all stale diagrams
python3 tools/check_diagrams.py docs/drawings/
```

### Requirements
- `drawio-export` CLI: `npm install -g @mattiash/drawio-export`
- Python 3.12+

## Current Diagrams
- `box-glyph.drawio` / `box-glyph.drawio.svg` - Visual representation of the "box" concept in the polyglot pipeline
- `pipeline.drawio` / `pipeline.drawio.svg` - Polyglot pipeline architecture diagram 