#!/bin/bash

# SeqWeb Environment Configuration
# Copy this file to config/env.local.sh and customize for your environment

# Repository paths - customize these for your local setup
export SEQWEBDATA_PATH="${SEQWEBDATA_PATH:-~/Devo/Data/SeqWeb/seqwebdata}"
export OEISDATA_PATH="${OEISDATA_PATH:-~/Devo/Data/OEIS/oeisdata}"

# Optional: Override with environment variables if set
# Example: export SEQWEBDATA_PATH="/path/to/your/seqwebdata"
# Example: export OEISDATA_PATH="/path/to/your/oeisdata"

# Validate paths exist (warn if not)
if [[ ! -d "$SEQWEBDATA_PATH" ]]; then
    echo "Warning: SEQWEBDATA_PATH not found: $SEQWEBDATA_PATH"
    echo "  Set SEQWEBDATA_PATH environment variable or update config/env.local.sh"
fi

if [[ ! -d "$OEISDATA_PATH" ]]; then
    echo "Warning: OEISDATA_PATH not found: $OEISDATA_PATH"
    echo "  Set OEISDATA_PATH environment variable or update config/env.local.sh"
fi

# Export for use in scripts
export SEQWEB_REPO_PATHS_CONFIGURED=true 