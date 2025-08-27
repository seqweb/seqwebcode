#!/bin/bash

# hello_world.sh - Simple Bash hello world program
# Created by Waldo 2025-08-27
#
# This program verifies basic Bash functionality in the SeqWeb environment.

set -euo pipefail

main() {
    echo "Hello, World!"
    echo "Bash is working in the SeqWeb environment!"
    
    # Verify some basic Bash functionality
    echo "Bash version: $BASH_VERSION"
    echo "Shell: $SHELL"
    
    # Test basic string operations
    test_string="Hello SeqWeb"
    echo "✓ String operations working: $(echo "$test_string" | tr '[:lower:]' '[:upper:]')"
    
    # Test basic math
    result=$((2 + 2))
    echo "✓ Basic math working: 2 + 2 = $result"
    
    # Test array operations
    test_array=("one" "two" "three" "four" "five")
    echo "✓ Array operations working: ${test_array[*]}"
    
    # Test command substitution
    current_dir=$(pwd)
    echo "✓ Command substitution working: $current_dir"
    
    echo "Bash environment verification complete!"
}

# Run main function
main "$@"
