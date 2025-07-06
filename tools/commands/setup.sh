#!/bin/bash
# @description: Initialize SeqWeb development environment
# @help: Usage: seqweb setup
#   Initialize the SeqWeb development environment.
#   This includes making the seqweb CLI executable and optionally
#   adding the current directory to PATH for global access.
#   NOTE: This command is idempotent - safe to run multiple times.

seqweb-command-setup() {
    # This function is idempotent - safe to run multiple times
    # It will only make changes if they haven't been made already
    echo "🧬 Setting up SeqWeb development environment..."
    echo ""
    
    # Make seqweb executable
    if [[ ! -x "$SEQWEB_HOME/seqweb" ]]; then
        echo "📝 Making seqweb CLI executable..."
        chmod +x "$SEQWEB_HOME/seqweb"
        echo "✅ seqweb CLI is now executable"
    else
        echo "✅ seqweb CLI is already executable"
    fi
    
    # Create data directories
    echo "📁 Creating data directories..."
    mkdir -p "$SEQWEB_INTERMEDIATE_DIR" "$SEQWEB_OUTPUT_DIR" "$SEQWEB_LOGS_DIR"
    echo "✅ Data directories ready"
    
    # Add current directory to PATH for this session
    if [[ ":$PATH:" != *":$SEQWEB_HOME:"* ]]; then
        echo "🌐 Adding seqweb directory to PATH for this session..."
        export PATH="$SEQWEB_HOME:$PATH"
        echo "✅ You can now use 'seqweb <command>' (no './' needed)"
    else
        echo "✅ seqweb directory already in PATH"
    fi
    
    echo ""
    echo "✅ Setup complete! You can now use 'seqweb <command>' from anywhere in this directory."
    echo ""
    echo "💡 To use 'seqweb' command from other directories or new sessions:"
    echo "   Add this line to your shell profile (~/.bash_profile, ~/.zshrc, etc.):"
    echo "   export PATH=\"$SEQWEB_HOME:\$PATH\""
    echo ""
    echo "💡 To configure additional directories, edit: $SEQWEB_HOME/config/env.sh"
} 