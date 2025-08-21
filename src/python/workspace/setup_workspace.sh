#!/bin/bash

# SeqWeb Multi-Repo Workspace Setup Script
# This script sets up the complete SeqWeb development workspace

set -e

echo "Setting up SeqWeb Multi-Repo Workspace..."

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Project root: $PROJECT_ROOT"

# Check if we're in the right location (should be in seqwebcode)
if [[ ! -f "$PROJECT_ROOT/seqweb" ]]; then
    echo "Error: This script should be run from within the seqwebcode repository"
    echo "Expected to find 'seqweb' executable in: $PROJECT_ROOT"
    exit 1
fi

# Source environment configuration if available
if [[ -f "config/env.local.sh" ]]; then
    source config/env.local.sh
elif [[ -f "config/env.sh" ]]; then
    source config/env.sh
fi

# Define repository paths (with fallbacks)
SEQWEBDATA_PATH="${SEQWEBDATA_PATH:-$HOME/Devo/Data/SeqWeb/seqwebdata}"
OEISDATA_PATH="${OEISDATA_PATH:-$HOME/Devo/Data/OEIS/oeisdata}"

# Clone or update seqwebdata
if [[ ! -d "$SEQWEBDATA_PATH" ]]; then
    echo "Cloning seqwebdata repository..."
    mkdir -p "$(dirname "$SEQWEBDATA_PATH")"
    git clone https://github.com/seqweb/seqwebdata.git "$SEQWEBDATA_PATH"
    echo "✓ seqwebdata cloned to $SEQWEBDATA_PATH"
else
    echo "✓ seqwebdata already exists at $SEQWEBDATA_PATH"
fi

# Clone or update oeisdata
if [[ ! -d "$OEISDATA_PATH" ]]; then
    echo "Cloning oeisdata repository..."
    echo "Note: This repository is very large (~400,000 files)"
    mkdir -p "$(dirname "$OEISDATA_PATH")"
    git clone https://github.com/oeis/oeisdata.git "$OEISDATA_PATH"
    echo "✓ oeisdata cloned to $OEISDATA_PATH"
else
    echo "✓ oeisdata already exists at $OEISDATA_PATH"
fi

# Create workspace configuration
echo "Creating workspace configuration..."

# Ensure .cursor directory exists
mkdir -p "$PROJECT_ROOT/.cursor"

# Create workspace setup instructions
cat > "$PROJECT_ROOT/.cursor/workspace-setup.md" << 'EOF'
# SeqWeb Multi-Repo Workspace Setup

This workspace contains three repositories:

1. **seqwebcode** (main development) - This repository
2. **seqwebdata** (generated RDF data) - `../seqwebdata`
3. **oeisdata** (OEIS source corpus) - `../oeisdata`

## Opening in Cursor

To open this workspace in Cursor:

1. Open Cursor
2. File → Open Folder
3. Select the `seqwebcode` directory
4. Cursor will automatically detect the workspace configuration

## Important Notes

- The data repositories (oeisdata, seqwebdata) contain large numbers of files and are excluded from indexing
- Focus development work in seqwebcode
- The workspace configuration automatically excludes large directories from search and file watching

## Manual Setup (if needed)

If the automatic workspace detection doesn't work:

1. Open seqwebcode in Cursor
2. File → Add Folder to Workspace
3. Add `../seqwebdata` and `../oeisdata`
4. Save workspace as `seqwebcode/.cursor/workspace.code-workspace`

## Repository URLs

- seqwebcode: https://github.com/seqweb/seqwebcode.git
- seqwebdata: https://github.com/seqweb/seqwebdata.git  
- oeisdata: https://github.com/oeis/oeisdata.git
EOF

echo "✓ Workspace configuration created"

# Generate workspace configuration
echo "Generating workspace configuration..."
python3 tools/generate_workspace_config.py

# Update .gitignore to exclude large data directories
if ! grep -q "# Large data repositories" "$PROJECT_ROOT/.gitignore" 2>/dev/null; then
    echo "" >> "$PROJECT_ROOT/.gitignore"
    echo "# Large data repositories" >> "$PROJECT_ROOT/.gitignore"
    echo "$SEQWEBDATA_PATH/" >> "$PROJECT_ROOT/.gitignore"
    echo "$OEISDATA_PATH/" >> "$PROJECT_ROOT/.gitignore"
    echo "✓ Updated .gitignore"
else
    echo "✓ .gitignore already updated"
fi

echo ""
echo "🎉 SeqWeb Multi-Repo Workspace setup complete!"
echo ""
echo "To open in Cursor:"
echo "1. Open Cursor"
echo "2. File → Open Folder"
echo "3. Select: $PROJECT_ROOT"
echo ""
echo "The workspace will automatically include all three repositories"
echo "with proper exclusions for large data directories." 