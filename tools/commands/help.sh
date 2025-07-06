#!/bin/bash
# @description: Show help information for SeqWeb commands
# @help: Usage: seqweb help [command]
#   Show help information for all commands or a specific command.
#   If no command is specified, lists all available commands.
#   If a command is specified, shows detailed help for that command.

seqweb-command-help() {
    local target_command="$1"
    
    if [[ -n "$target_command" ]]; then
        # Show help for specific command
        local script="$SEQWEB_COMMAND_DIR/${target_command}.sh"
        if [[ -f "$script" ]]; then
            echo "🧬 Help for command: $target_command"
            echo ""
            # Extract @help lines from the script
            grep "^# @help:" "$script" | sed 's/^# @help://' | sed 's/^/  /'
            echo ""
        else
            echo "❌ Unknown command: $target_command"
            return 1
        fi
    else
        # Show help for all commands
        echo "🧬 SeqWeb Development Environment"
        echo ""
        echo "📜 Available commands:"
        echo ""
        
        for script in "$SEQWEB_COMMAND_DIR"/*.sh; do
            if [[ -f "$script" ]]; then
                local cmd_name=$(basename "$script" .sh)
                local description=$(grep "^# @description:" "$script" | sed 's/^# @description: //')
                echo "  $cmd_name: $description"
            fi
        done
        
        echo ""
        echo "💡 Run 'seqweb help <command>' for detailed help on a specific command."
    fi
} 