# SeqWeb

SeqWeb is to the OEIS as DBpedia is to Wikipedia.

## Project Structure

```
seqwebcode/
├── docs/                    # Documentation
│   ├── technical/          # Technical specifications, APIs, etc.
│   ├── process/            # Development processes, workflows
│   └── plans/              # Project plans, roadmaps, milestones
├── src/                    # Source code
│   ├── java/              # Java implementations
│   ├── python/            # Python implementations  
│   ├── cl/               # Common Lisp implementations
│   └── scripts/           # Shell scripts and utilities
├── ontology/              # RDF ontology definitions and vocabularies
├── tests/                 # Test suites
│   ├── java/
│   ├── python/
│   └── cl/
├── data/                  # Local data files, samples
└── examples/              # Example usage and demonstrations
```

## Related Repositories

- **oeisdata**: `git@github.com:oeis/oeisdata.git` - OEIS source corpus (.seq files)
- **seqwebdata**: `git@github.com:seqweb/seqwebdata.git` - Generated RDF data (.ttl files)
- **seqwebcode**: `git@github.com:seqweb/seqwebcode.git` - This repository (code, docs, ontology)

## Development

This repository contains the code, documentation, and ontology for converting OEIS sequence data into a semantic web knowledge graph.
