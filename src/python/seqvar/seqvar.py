# Created by Waldo 2025-08-20

from __future__ import annotations
import os
import sqlite3
import time
import inspect
from pathlib import Path


class SeqVarError(RuntimeError):
    pass


def _db_path() -> Path:
    home = os.environ.get("SEQWEBDEV_HOME")
    if not home:
        raise SeqVarError("SEQWEBDEV_HOME is not set")
    return (Path(home).expanduser().resolve() / ".state" / "seqvar.sqlite")


def _conn() -> sqlite3.Connection:
    p = _db_path()
    if not p.exists():
        raise SeqVarError(f"seqvar store not initialized: {p} missing. Run SeqWeb bootstrap.")
    try:
        return sqlite3.connect(p)
    except sqlite3.Error as e:
        raise SeqVarError(f"cannot open seqvar store at {p}: {e}") from e


def get(key: str, ns: str = "") -> str:
    try:
        with _conn() as db:
            row = db.execute("SELECT val FROM seqvars WHERE ns=? AND key=?", (ns, key)).fetchone()
            return "" if row is None or row[0] is None else row[0]
    except sqlite3.OperationalError as e:
        # Likely missing table
        raise SeqVarError("seqvar store missing.") from e


def set(key: str, val: str, ns: str = "", src: str = None) -> None:
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
                "INSERT INTO seqvars(ns,key,val,src,ts) VALUES(?,?,?,?,?) "
                "ON CONFLICT(ns,key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts",
                (ns, key, val, src, now)
            )
    except sqlite3.OperationalError as e:
        raise SeqVarError("seqvar store missing.") from e


def dump() -> list[tuple]:
    """
    Return all rows and columns from the seqvar database.
    Returns a list of tuples, each containing (ns, key, val, src, ts)
    """
    try:
        with _conn() as db:
            rows = db.execute("SELECT ns, key, val, src, ts FROM seqvars ORDER BY ns, key").fetchall()
            return rows
    except sqlite3.OperationalError as e:
        # Likely missing table
        raise SeqVarError("seqvar store missing.") from e
