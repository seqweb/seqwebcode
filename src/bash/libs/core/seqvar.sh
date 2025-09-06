# Created by Waldo 2025-08-20

# seqvar.sh — library (source me). Assumes the seqvar database exists.
# API:
#   seqvar_get KEY [NS]         -> echoes value ("" if missing)
#   seqvar_set KEY VALUE [NS] [SRC]
#   seqvar_dump                 -> echoes all key-value pairs
#   seqvar_get_dict [PATTERN]   -> echoes key-value pairs as KEY=VALUE (one per line)

set -euo pipefail

_seqvar_db_path() {
  : "${SEQWEBDEV_HOME:?SEQWEBDEV_HOME is not set}"
  printf "%s/.state/env.sqlite" "$SEQWEBDEV_HOME"
}

_seqvar_check_db() {
  local db
  db="$(_seqvar_db_path)"
  if [[ ! -f "$db" ]]; then
    echo "seqvar store not initialized: $db missing." >&2
    exit 1
  fi
}

seqvar_get() {
  local key="${1:?key required}"
  local ns="${2:-SeqVar}"
  local db
  db="$(_seqvar_db_path)"
  _seqvar_check_db
  
  # Use printf to handle special characters in values
  sqlite3 -readonly "$db" "SELECT IFNULL((SELECT val FROM seqvars WHERE ns='$ns' AND key='$key'), '');" | tr -d '\n'
}

seqvar_set() {
  local key="${1:?key required}"
  local val="${2:-}"
  local ns="${3:-SeqVar}"
  local src="${4:-seqweb}"
  local db
  db="$(_seqvar_db_path)"
  _seqvar_check_db
  
  local now
  now="$(date +%s)"
  
  sqlite3 "$db" "INSERT INTO seqvars(ns,key,val,src,ts) VALUES('$ns','$key','$val','$src',$now)
                  ON CONFLICT(ns,key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts;"
}

seqvar_dump() {
  """
  Return all rows and columns from the seqvar database.
  Outputs each row as: KEY=VALUE SRC=source TS=timestamp
  """
  local ns="${1:-SeqVar}"
  local db
  db="$(_seqvar_db_path)"
  _seqvar_check_db
  
  sqlite3 "$db" "SELECT key, val, src, ts FROM seqvars WHERE ns='$ns' ORDER BY key;" | while IFS='|' read -r key val src ts; do
    printf "KEY=%s VAL=%s SRC=%s TS=%s\n" "$key" "$val" "$src" "$ts"
  done
}

seqvar_get_dict() {
  """
  Return key-value pairs from the seqvar database as KEY=VALUE format.
  
  Args:
    PATTERN - Optional filter pattern for keys. Uses SQLite LIKE operator with wildcards:
              - % matches any sequence of characters
              - _ matches any single character
              - Examples: "repos.*" -> "repos.%", "config.*.url" -> "config.%.url"
  """
  local pattern="${1:-}"
  local ns="${2:-SeqVar}"
  local db
  db="$(_seqvar_db_path)"
  _seqvar_check_db
  
  if [[ -z "$pattern" ]]; then
    # No filter - get all key-value pairs
    sqlite3 "$db" "SELECT key, val FROM seqvars WHERE ns='$ns' ORDER BY key;" | while IFS='|' read -r key val; do
      printf "%s=%s\n" "$key" "$val"
    done
  else
    # Convert common wildcard patterns to SQLite LIKE patterns
    # Replace .* with .% for SQLite compatibility
    local like_pattern
    like_pattern="${pattern//\.\*/\.%}"
    
    sqlite3 "$db" "SELECT key, val FROM seqvars WHERE ns='$ns' AND key LIKE '$like_pattern' ORDER BY key;" | while IFS='|' read -r key val; do
      printf "%s=%s\n" "$key" "$val"
    done
  fi
}
