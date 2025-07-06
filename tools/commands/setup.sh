#!/bin/bash
# @description: Initialize SeqWeb development environment
# @help: Usage: seqweb setup
#   Initialize the SeqWeb development environment.
#   This includes making the seqweb CLI executable and optionally
#   adding the current directory to PATH for global access.

seqweb-command-setup() {
    echo "🧬 Installing SeqWeb development environment..."
    echo ""
    
    # Make seqweb executable
    if [[ ! -x "$SEQWEB_HOME/seqweb" ]]; then
        echo "📝 Making seqweb CLI executable..."
        chmod +x "$SEQWEB_HOME/seqweb"
        echo "✅ seqweb CLI is now executable"
    else
        echo "✅ seqweb CLI is already executable"
    fi
    
    # Create ~/opt/seqweb if it doesn't exist
    OPT_DIR="$HOME/opt/seqweb"
    if [[ ! -d "$OPT_DIR" ]]; then
        echo "📁 Creating $OPT_DIR directory..."
        mkdir -p "$OPT_DIR"
    fi
    
    # Copy seqweb to ~/opt/seqweb
    echo "📦 Installing seqweb to $OPT_DIR..."
    cp "$SEQWEB_HOME/seqweb" "$OPT_DIR/"
    cp -r "$SEQWEB_HOME/tools" "$OPT_DIR/"
    cp -r "$SEQWEB_HOME/config" "$OPT_DIR/"
    
    # Create ~/bin if it doesn't exist
    USER_BIN="$HOME/bin"
    if [[ ! -d "$USER_BIN" ]]; then
        echo "📁 Creating $USER_BIN directory..."
        mkdir -p "$USER_BIN"
    fi
    
    # Create symlink in ~/bin
    SYMLINK="$USER_BIN/seqweb"
    if [[ -L "$SYMLINK" ]]; then
        echo "🔄 Updating existing symlink..."
        rm "$SYMLINK"
    fi
    
    echo "🔗 Creating symlink in $USER_BIN..."
    ln -s "$OPT_DIR/seqweb" "$SYMLINK"
    
    # Add ~/bin to PATH if not already there
    if [[ ":$PATH:" != *":$USER_BIN:"* ]]; then
        echo "🌐 Adding $USER_BIN to PATH..."
        echo "export PATH=\"$USER_BIN:\$PATH\"" >> "$HOME/.bashrc"
        export PATH="$USER_BIN:$PATH"
        echo "✅ Added to .bashrc - restart shell or run: source ~/.bashrc"
    else
        echo "✅ $USER_BIN already in PATH"
    fi
    
    echo ""
    echo "✅ Installation complete! You can now use 'seqweb <command>' from anywhere."
    echo "💡 SeqWeb is installed in $OPT_DIR"
} 