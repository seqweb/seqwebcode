# Created by Waldo 2025-08-21

from __future__ import annotations
import sqlite3
from pathlib import Path
from seqvar import seqvar_store_path


SCHEMA_SQL = """
PRAGMA journal_mode=WAL;
PRAGMA synchronous=NORMAL;
CREATE TABLE IF NOT EXISTS seqvars(
  ns   TEXT NOT NULL,
  key  TEXT NOT NULL,
  val  TEXT NOT NULL DEFAULT '',
  src  TEXT,
  ts   INTEGER NOT NULL,
  PRIMARY KEY(ns,key)
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


if __name__ == "__main__":
    # Allow invoking this module directly during bootstrap
    try:
        init_seqvar_store()
    except RuntimeError as e:
        print(f"[seqvar] ERROR: {e}")
        raise SystemExit(1)
