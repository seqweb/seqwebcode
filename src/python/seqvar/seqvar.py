# Created by Waldo 2025-08-20

from __future__ import annotations
import os
import sqlite3
import time
from pathlib import Path


class SeqVarError(RuntimeError):
    pass


def _db_path() -> Path:
    home = os.environ.get("SEQWEBDEV_HOME")
    if not home:
        raise SeqVarError("SEQWEBDEV_HOME is not set")
    return (Path(home).expanduser().resolve() / ".state" / "env.sqlite")


def _conn() -> sqlite3.Connection:
    p = _db_path()
    if not p.exists():
        raise SeqVarError(f"seqvar store not initialized: {p} missing. Run SeqWeb bootstrap.")
    try:
        return sqlite3.connect(p)
    except sqlite3.Error as e:
        raise SeqVarError(f"cannot open seqvar store at {p}: {e}") from e


def get(key: str, ns: str = "SeqVar") -> str:
    try:
        with _conn() as db:
            row = db.execute("SELECT val FROM seqvars WHERE ns=? AND key=?", (ns, key)).fetchone()
            return "" if row is None or row[0] is None else row[0]
    except sqlite3.OperationalError as e:
        # Likely missing table
        raise SeqVarError("seqvar table missing. Run SeqWeb bootstrap.") from e


def set(key: str, val: str, ns: str = "SeqVar", src: str = "seqweb") -> None:
    now = int(time.time())
    try:
        with _conn() as db:
            db.execute(
                "INSERT INTO seqvars(ns,key,val,src,ts) VALUES(?,?,?,?,?) "
                "ON CONFLICT(ns,key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts",
                (ns, key, val, src, now)
            )
    except sqlite3.OperationalError as e:
        raise SeqVarError("seqvar table missing. Run SeqWeb bootstrap.") from e


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
        raise SeqVarError("seqvar table missing. Run SeqWeb bootstrap.") from e
