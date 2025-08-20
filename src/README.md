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
   │     ├─ core.py           # get/set (assumes DB exists)
   │     └─ toml_io.py        # load TOML → dict[str,str]; write dict → DB
   ├─ java/
   │  └─ seqvar/
   │     ├─ SeqVar.java       # get/set (assumes DB exists)
   │     └─ TomlIO.java       # load TOML; write to DB
   ├─ cl/
   │  ├─ seqvar.lisp          # get/set (assumes DB exists)
   │  └─ seqvar-toml.lisp     # read TOML; write to DB
   └─ bash/
      ├─ seqvar.sh            # get/set (assumes DB exists) — reference
      └─ seqvar_toml.sh       # calls out to python to load TOML — reference
``` 
