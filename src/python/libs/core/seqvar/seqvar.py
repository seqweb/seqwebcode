# seqvar store untilities

from __future__ import annotations
import sqlite3
import time
import inspect
from pathlib import Path
from libs.core.util import get_call_trace
# Removed import to break circular dependency


def seqvar_store_path() -> Path:
    """Get the path to the seqvar store database file."""
    import os
    from pathlib import Path
    
    # Get SEQWEBDEV_HOME and construct .state/seqvar/seqvar.db path
    seqwebdev_home = os.environ.get('SEQWEBDEV_HOME')
    if not seqwebdev_home:
        raise RuntimeError("SEQWEBDEV_HOME environment variable is not set!")
    
    home_path = Path(seqwebdev_home).expanduser().resolve()
    return home_path / ".state" / "seqvar" / "seqvar.db"


# Database schema for seqvar store
SCHEMA_SQL = """
PRAGMA journal_mode=WAL;
PRAGMA synchronous=NORMAL;
CREATE TABLE IF NOT EXISTS seqvars(
  key  TEXT NOT NULL,
  val  TEXT NOT NULL DEFAULT '',
  src  TEXT,
  ts   INTEGER NOT NULL,
  PRIMARY KEY(key)
);
"""


def init_seqvar_store() -> Path:
    """
    Idempotently create the seqvar store and schema at:
      $SEQWEBDEV_HOME/.state/seqvar.sqlite

    - Creates the .state directory if needed
    - Enables WAL journaling
    - Ensures the seqvars table exists (no data is modified)
    - Returns the absolute store path
    """
    db_path = seqvar_store_path()
    db_path.parent.mkdir(parents=True, exist_ok=True)

    # Connect and apply schema in a single session
    try:
        with sqlite3.connect(db_path) as db:
            # Apply schema pragmas and table
            for stmt in filter(None, (s.strip() for s in SCHEMA_SQL.split(";"))):
                if stmt:
                    db.execute(stmt)
            # No data writes; just ensure structure exists
    except sqlite3.Error as e:
        raise RuntimeError(f"Failed to initialize seqvar store at {db_path}: {e}") from e

    return db_path


def _conn() -> sqlite3.Connection:
    p = seqvar_store_path()
    if not p.exists():
        # Create the store on demand
        init_seqvar_store()
    try:
        return sqlite3.connect(p)
    except sqlite3.Error as e:
        raise RuntimeError(f"cannot open seqvar store at {p}: {e}") from e


def get(key: str) -> str:
    with _conn() as db:
        row = db.execute("SELECT val FROM seqvars WHERE key=?", (key,)).fetchone()
        return "" if row is None or row[0] is None else row[0]


def set(key: str, val: str, src: str = None, as_default: bool = False) -> None:
    # If as_default is True and key already has a non-empty value, respect existing value
    if as_default and get(key):
        return

    now = int(time.time())

    # If src is not specified, try to get the caller function name
    if src is None:
        try:
            src = get_call_trace()
        except Exception:
            src = "??"

    with _conn() as db:
        db.execute(
            "INSERT INTO seqvars(key,val,src,ts) VALUES(?,?,?,?) "
            "ON CONFLICT(key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts",
            (key, val, src, now)
        )


def dump() -> list[tuple]:
    """
    Return all rows and columns from the seqvar database.
    Returns a list of tuples, each containing (key, val, src, ts)
    """
    with _conn() as db:
        rows = db.execute("SELECT key, val, src, ts FROM seqvars ORDER BY key").fetchall()
        return rows


def get_dict(keys: str = None) -> dict[str, str]:
    """
    Return key-value pairs from the seqvar database as a dictionary.

    Args:
        keys: Optional filter pattern for keys. Uses SQLite LIKE operator with wildcards:
              - % matches any sequence of characters
              - _ matches any single character
              - Examples: "repos.*" -> "repos.%", "config.*.url" -> "config.%.url"

    Returns:
        Dictionary mapping keys to values

    Raises:
        RuntimeError: If there are database access issues
    """
    with _conn() as db:
        if keys is None:
            # No filter - get all key-value pairs
            rows = db.execute("SELECT key, val FROM seqvars ORDER BY key").fetchall()
        else:
            # Convert common wildcard patterns to SQLite LIKE patterns
            # Replace .* with .% for SQLite compatibility
            like_pattern = keys.replace(".*", ".%")
            rows = db.execute(
                "SELECT key, val FROM seqvars WHERE key LIKE ? ORDER BY key",
                (like_pattern,)
            ).fetchall()

        return {key: val for key, val in rows}


def set_dict(bindings: dict, src: str = None, as_default: bool = True) -> None:
    """
    Write multiple key-value pairs to the seqvar database.

    Args:
        bindings: Dictionary of key-value pairs to write
        src: Optional source identifier for the values
        as_default: If True, only set values for keys that are currently empty
          True is the default value because this is often used to fill in defaults.
    """
    for key, value in bindings.items():
        set(key, value, src, as_default)
