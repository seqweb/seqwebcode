# Created by Waldo 2025-08-20

# seqvar_toml.sh — TOML sidecar library (source me). Delegates to Python for TOML parsing.
# API:
#   load_toml PATH [PATH...]     -> calls Python to load TOML and write to seqvar
#   write_toml_to_seqvar         -> calls Python to write bindings to seqvar

set -euo pipefail

# Source the core seqvar functions
. "$(dirname "${BASH_SOURCE[0]}")/seqvar.sh"

# Find the Python seqvar_toml module
_seqvar_python_module() {
  local script_dir
  script_dir="$(dirname "${BASH_SOURCE[0]}")"
  echo "${script_dir}/../../python/libs/core/seqvar/toml.py"
}

# Validate Python module exists
_seqvar_check_python() {
  local module_path
  module_path="$(_seqvar_python_module)"
  if [[ ! -f "$module_path" ]]; then
    echo "ERROR: Python TOML module not found at: $module_path" >&2
    echo "Make sure the seqvar Python package is properly installed." >&2
    exit 1
  fi
}

load_toml() {
  """
  Load TOML file(s) and write flattened key-value pairs to seqvar database.
  Delegates to Python for TOML parsing as specified in README.
  
  Args:
    PATH [PATH...] - One or more TOML file paths to load
  """
  if [[ $# -eq 0 ]]; then
    echo "ERROR: load_toml requires at least one TOML file path" >&2
    exit 1
  fi
  
  _seqvar_check_python
  
  local module_path
  module_path="$(_seqvar_python_module)"
  
  # Call Python to load TOML and write to seqvar
  python3 -c "
import sys
sys.path.insert(0, '$(dirname "$module_path")')
from seqvar_toml import load_toml, write_toml_to_seqvar

try:
    paths = [$([printf '"%s",' "$@" | sed 's/,$//')]
    paths = [p for p in paths if p]  # Remove empty strings
    bindings = load_toml(paths)
    write_toml_to_seqvar(bindings)
    print(f'[seqvar_toml] loaded {len(bindings)} bindings from {len(paths)} TOML file(s)')
except Exception as e:
    print(f'ERROR: {e}', file=sys.stderr)
    sys.exit(1)
"
}

write_toml_to_seqvar() {
  """
  Write key-value bindings to seqvar database.
  This function is primarily for compatibility with the common contract.
  For TOML loading, use load_toml() instead.
  
  Args:
    KEY VALUE [KEY VALUE...] - Alternating key-value pairs to write
  """
  if [[ $# -eq 0 ]]; then
    echo "ERROR: write_toml_to_seqvar requires key-value pairs" >&2
    exit 1
  fi
  
  if [[ $(( $# % 2 )) -ne 0 ]]; then
    echo "ERROR: write_toml_to_seqvar requires even number of arguments (key-value pairs)" >&2
    exit 1
  fi
  
  local i=0
  while [[ $i -lt $# ]]; do
    local key="${!((i+1))}"
    local value="${!((i+2))}"
    seqvar_set "$key" "$value"
    i=$((i+2))
  done
  
  echo "[seqvar_toml] wrote $(( $# / 2 )) key-value pairs to seqvar"
}
