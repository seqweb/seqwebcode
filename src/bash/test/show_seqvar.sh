#!/bin/bash

# Created by Waldo 2025-08-27

# show_seqvar.sh — Demonstration script for Bash seqvar facility
# This script shows how to use the seqvar library in Bash scripts

set -euo pipefail

# Source the seqvar library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$SCRIPT_DIR/../libs/core/seqvar.sh"

echo "=== Bash SeqVar Facility Demonstration ==="
echo

# Check if we can access the database
if ! _seqvar_check_db 2>/dev/null; then
    echo "ERROR: Cannot access seqvar database."
    echo "Make sure SEQWEBDEV_HOME is set and the seqvar database is accessible."
    exit 1
fi

echo "✓ Database connection successful"
echo

# Demonstrate basic operations
echo "1. Setting values..."
seqvar_set "demo.key1" "value1" "SeqVar" "show_seqvar"
seqvar_set "demo.key2" "value2" "SeqVar" "show_seqvar"
seqvar_set "demo.key3" "value3" "SeqVar" "show_seqvar"
echo "   Set 3 demo keys"
echo

echo "2. Getting values..."
echo "   demo.key1 = $(seqvar_get "demo.key1")"
echo "   demo.key2 = $(seqvar_get "demo.key2")"
echo "   demo.key3 = $(seqvar_get "demo.key3")"
echo

echo "3. Dumping all values..."
echo "   Dump output:"
seqvar_dump "SeqVar" | grep "demo.key" | while read -r line; do
    echo "     $line"
done
echo

echo "4. Getting dictionary with pattern matching..."
echo "   All demo keys:"
seqvar_get_dict "demo.*" "SeqVar" | while read -r line; do
    echo "     $line"
done
echo

echo "5. Getting all values as dictionary..."
echo "   All values:"
seqvar_get_dict "" "SeqVar" | grep "demo.key" | while read -r line; do
    echo "     $line"
done
echo

# Cleanup demo data
echo "6. Cleaning up demo data..."
sqlite3 "$(_seqvar_db_path)" "DELETE FROM seqvars WHERE src='show_seqvar';" 2>/dev/null || true
echo "   Demo data cleaned up"
echo

echo "=== Demonstration Complete ==="
echo
echo "This shows the basic usage of the Bash seqvar facility."
echo "The library provides:"
echo "  - seqvar_get KEY [NS]     - Get a value"
echo "  - seqvar_set KEY VALUE [NS] [SRC] - Set a value"
echo "  - seqvar_dump [NS]        - Dump all values"
echo "  - seqvar_get_dict [PATTERN] [NS] - Get values as KEY=VALUE pairs"
echo
echo "For TOML integration, see seqvar_toml.sh"
