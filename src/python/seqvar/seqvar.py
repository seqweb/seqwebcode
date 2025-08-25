# seqvar store untilities

from __future__ import annotations
import sqlite3
import time
import inspect
from pathlib import Path
from home.paths import seqwebdev_state_path


class SeqVarError(RuntimeError):
    pass


def seqvar_store_path() -> Path:
    """Get the path to the seqvar store database file."""
    return seqwebdev_state_path("seqvar.sqlite")


def _conn() -> sqlite3.Connection:
    p = seqvar_store_path()
    if not p.exists():
        raise SeqVarError(f"seqvar store {p} missing.")
    try:
        return sqlite3.connect(p)
    except sqlite3.Error as e:
        raise SeqVarError(f"cannot open seqvar store at {p}: {e}") from e


def get(key: str) -> str:
    try:
        with _conn() as db:
            row = db.execute("SELECT val FROM seqvars WHERE key=?", (key,)).fetchone()
            return "" if row is None or row[0] is None else row[0]
    except sqlite3.OperationalError as e:
        # Likely missing table
        raise SeqVarError("seqvar store missing.") from e


def set(key: str, val: str, src: str = None) -> None:
    now = int(time.time())

    # If src is not specified, try to get the caller function name
    if src is None:
        try:
            src = str(inspect.stack()[1].function)
        except Exception:
            src = "??"

    try:
        with _conn() as db:
            db.execute(
                "INSERT INTO seqvars(key,val,src,ts) VALUES(?,?,?,?) "
                "ON CONFLICT(key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts",
                (key, val, src, now)
            )
    except sqlite3.OperationalError as e:
        raise SeqVarError("seqvar store missing.") from e


def dump() -> list[tuple]:
    """
    Return all rows and columns from the seqvar database.
    Returns a list of tuples, each containing (key, val, src, ts)
    """
    try:
        with _conn() as db:
            rows = db.execute("SELECT key, val, src, ts FROM seqvars ORDER BY key").fetchall()
            return rows
    except sqlite3.OperationalError as e:
        # Likely missing table
        raise SeqVarError("seqvar store missing.") from e


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
        SeqVarError: If the seqvar store is missing or inaccessible
    """
    try:
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
    except sqlite3.OperationalError as e:
        # Likely missing table
        raise SeqVarError("seqvar store missing.") from e
