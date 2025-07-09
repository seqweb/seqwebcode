# SeqWeb Documentation

This directory contains all SeqWeb project documentation, including technical specifications, development processes, and project plans.

## Documentation Structure

- **`technical/`** - Technical specifications, APIs, architecture docs
- **`process/`** - Development processes, workflows, guidelines  
- **`plans/`** - Project plans, roadmaps, milestones
- **`drawings/`** - Diagrams and visual assets (see below)

## Diagram Management

### Best Practices
- **Create diagrams in Draw.io** and save as `.drawio` files in `drawings/`
- **Export to SVG manually** using Draw.io's "Export as SVG" feature
- **Keep both files together** - `.drawio` source and `.svg` output in same directory
- **Use descriptive filenames** that clearly indicate the diagram's purpose

### Embedding Diagrams
Embed SVGs in markdown documentation using standard HTML or markdown syntax:

```markdown
<p align="center">
  <img src="../drawings/box-glyph.svg" width="150" alt="Box glyph" />
  <br> <em>a box</em>
</p>
```

### Maintenance Tools
For checking and updating diagrams, see [`drawings/README.md`](drawings/README.md) for details on using the `check_diagrams.py` tool.

## Documentation Standards

- Use clear, concise language
- Include code examples where helpful
- Keep diagrams up to date with code changes
- Follow the established directory structure
- Update this README when adding new documentation types 