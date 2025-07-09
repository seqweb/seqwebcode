# SeqWeb Multi-Repo Workspace

## Repository Structure

This workspace contains three repositories:

1. **seqwebcode** (main development) - Current directory
   - Code, documentation, ontology, tools
   - Primary development focus

2. **seqwebdata** (generated RDF data) - `~/Devo/Data/SeqWeb/seqwebdata`
   - Output .ttl files from the pipeline
   - Large number of files (will grow over time)

3. **oeisdata** (OEIS source corpus) - `~/Devo/Data/OEIS/oeisdata`
   - ~400,000 .seq files
   - Read-only data resource

## Performance Notes

- **oeisdata** and **seqwebdata** are excluded from indexing due to large file counts
- Focus development work in **seqwebcode**
- The workspace configuration automatically handles exclusions

## Quick Commands

- `./seqweb help` - Show available commands
- `./tools/setup_workspace.sh` - Set up the complete workspace
- `git consolidate` - Quick commit with standard message

## Workspace Configuration

- `.cursor/settings.json` - Cursor workspace settings
- `.cursor/workspace.code-workspace` - VS Code workspace definition
- `.cursor/rules/` - Cursor AI rules for the project

## Development Focus

- Main development happens in `seqwebcode`
- Data repositories are for reference and output
- Use the polyglot pipeline architecture for processing 