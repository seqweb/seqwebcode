# Created by Waldo 2025-08-20

# seqvar.sh — library (source me). Assumes bootstrap created the DB/table.
# API:
#   seqvar_get KEY [NS]         -> echoes value ("" if missing)
#   seqvar_set KEY VALUE [NS] [SRC]

set -euo pipefail

_seqvar_db_path() {
  : "${SEQWEBDEV_HOME:?SEQWEBDEV_HOME is not set}"
  printf "%s/.state/env.sqlite" "$SEQWEBDEV_HOME"
}

seqvar_get() {
  local key="${1:?key}"; local ns="${2:-SeqVar}"
  local db; db="$(_seqvar_db_path)"
  [[ -f "$db" ]] || { echo "seqvar store not initialized: $db missing. Run SeqWeb bootstrap." >&2; exit 1; }
  sqlite3 -readonly "$db" "SELECT IFNULL((SELECT val FROM seqvars WHERE ns='$ns' AND key='$key'), '');"
}

seqvar_set() {
  local key="${1:?key}"; local val="${2-}"; local ns="${3:-SeqVar}"; local src="${4:-seqweb}"
  local db; db="$(_seqvar_db_path)"
  [[ -f "$db" ]] || { echo "seqvar store not initialized: $db missing. Run SeqWeb bootstrap." >&2; exit 1; }
  local now; now="$(date +%s)"
  sqlite3 "$db" "INSERT INTO seqvars(ns,key,val,src,ts) VALUES('$ns','$key','$val','$src',$now)
                  ON CONFLICT(ns,key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts;"
}
