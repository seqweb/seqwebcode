#!/bin/bash
# @description: CLI dispatcher for SeqWeb development environment
# @help: Usage: seqweb <command> [args...]
#   Execute SeqWeb development commands.
#   Run 'seqweb help' to see available commands.

echo ""
echo "🧬 SeqWeb: OEIS Knowledge Graph Development 🧬"

# Load environment (atomic + derived variables)
set -o allexport
source "$SEQWEB_HOME/config/env.sh"
set +o allexport

# Helper: load command implementation
load_command() {
  local cmd="$1"
  local script="$SEQWEB_COMMAND_DIR/${cmd}.sh"
  if [[ -f "$script" ]]; then
    source "$script"
    return 0
  else
    return 1
  fi
}

# Helper: list available commands
list_available_commands() {
  find "$SEQWEB_COMMAND_DIR" -type f -name "*.sh" \
    | sed -E 's|^.*/||' \
    | sed -E 's/\.sh$//' \
    | sort
}

# Begin dispatch
command="$1"
shift || true

# Attempt to load the requested command
if ! load_command "$command"; then
  echo "❓ Unknown or missing command: ${command:-<none>}"
  echo ""
  echo "📜 Available commands:"
  list_available_commands | sed 's/^/ - /'
  exit 1
fi

# Construct the function name
function_name="seqweb-command-$command"

# Verify the function exists
if ! declare -F "$function_name" >/dev/null; then
  echo "❌ Error: Function $function_name not defined in $SEQWEB_COMMAND_DIR/${command}.sh"
  exit 1
fi

# Invoke the command
"$function_name" "$@" 