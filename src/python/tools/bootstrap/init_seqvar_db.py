# Created by Waldo 2025-08-20

# seqwebcode/tools/bootstrap/init_seqvar_db.py
from __future__ import annotations
import os
import sqlite3
from pathlib import Path
from typing import Optional


class BootstrapError(RuntimeError):
    pass


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


def seqwebdev_home(env: Optional[dict] = None) -> Path:
    """
    Resolve SEQWEBDEV_HOME from the environment (or a passed-in mapping for tests).
    """
    env = env or os.environ
    home = env.get("SEQWEBDEV_HOME")
    if not home:
        raise BootstrapError("SEQWEBDEV_HOME is not set; cannot initialize seqvar store.")
    return Path(home).expanduser().resolve()


def env_db_path(home: Path) -> Path:
    """
    Return the absolute path to $SEQWEBDEV_HOME/.state/env.sqlite
    """
    return home / ".state" / "env.sqlite"


def init_seqvar_db(verbose: bool = True) -> Path:
    """
    Idempotently create the seqvar SQLite store and schema at:
      $SEQWEBDEV_HOME/.state/env.sqlite

    - Creates the .state directory if needed
    - Enables WAL journaling
    - Ensures the seqvars table exists (no data is modified)
    - Returns the absolute DB path
    """
    home = seqwebdev_home()
    db_path = env_db_path(home)
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
        raise BootstrapError(f"Failed to initialize seqvar store at {db_path}: {e}") from e

    if verbose:
        print(f"[seqvar] initialized at {db_path}")

    return db_path


if __name__ == "__main__":
    # Allow invoking this module directly during bootstrap
    try:
        init_seqvar_db(verbose=True)
    except BootstrapError as e:
        print(f"[seqvar] ERROR: {e}")
        raise SystemExit(1)
