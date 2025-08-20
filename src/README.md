# Source Code

This directory contains the implementation code for SeqWeb components.

## Structure

- **java/**: Java implementations
- **python/**: Python implementations
- **cl/**: Common Lisp implementations  
- **scripts/**: Shell scripts and utilities

## Components

- OEIS data parsers
- RDF conversion pipelines
- Ontology management tools
- Data validation utilities
- Web services and APIs

## SeqVar Facility

# Created by Waldo 2025-08-20

```
seqwebcode/
└─ src/
   ├─ python/
   │  └─ seqvar/
   │     ├─ __init__.py
   │     ├─ seqvar.py         # get/set (assumes DB exists)
   │     └─ seqvar_toml.py    # load TOML → dict[str,str]; write dict → DB
   ├─ java/
   │  └─ seqvar/
   │     ├─ SeqVar.java       # get/set (assumes DB exists)
   │     └─ SeqvarToml.java   # load TOML; write to DB
   ├─ cl/
   │  ├─ seqvar.lisp          # get/set (assumes DB exists)
   │  └─ seqvar-toml.lisp     # read TOML; write to DB
   └─ bash/
      ├─ seqvar.sh            # get/set (assumes DB exists) — reference
      └─ seqvar_toml.sh       # calls out to python to load TOML — reference
```

## SeqVar Facility Common Contract (All Languages)

- **DB path**: `$SEQWEBDEV_HOME/.state/env.sqlite`
- **Table**: `seqvars(ns TEXT NOT NULL, key TEXT NOT NULL, val TEXT NOT NULL DEFAULT '', src TEXT, ts INTEGER NOT NULL, PRIMARY KEY(ns,key))`
- **Defaults**: `ns="SeqVar"`, `src="seqweb"`
- **get(key, ns)**: always returns a string (empty `""` if missing)
- **set(key, val, ns, src)**: upsert; no schema creation here
- **On missing DB/table**: throw/raise a clear error:
  > "seqvar store not initialized (missing env.sqlite or seqvars table). Run SeqWeb bootstrap."

## TOML "Sidecar" for SeqVar Facility 

### Common Contract (All Languages):
- **Flattening**: TOML tables become dotted keys (`[service] host="..."` → `service.host = "..."`)
- **Strings only**: sidecars stringify non-string TOML values (keeps seqvar DB uniform)
- **APIs to expose** (same names everywhere):
  - `load_toml(paths: list|array)` → `map/dict<string,string>`
  - `write_to_seqvar(bindings, ns="SeqVar", src="seqweb")` → `None`

### Language-specific implementation/dependency notes:
- **Python**: use stdlib tomllib (read-only, 3.11+)
- **Java**: tiny dep org.tomlj:tomlj (read-only)
- **Lisp**: cl-toml 
- **Bash**: avoid parsing TOML; call Python helper from scripts if needed
