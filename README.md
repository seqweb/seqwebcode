# seqwebcode

_**SeqWeb** is to the OEIS as DBpedia is to Wikipedia._<br>
Project and systems level documentation may be found at the [SeqWeb website](https://www.seqweb.org/)

## `seqwebcode` overview
This **seqwebcode** repository contains the code and associated metadata for all major SeqWeb subsystems:
- _Webwright_ - converts and extends OEIS sequence data into SeqWeb's semantic web knowledge graph 
- _Webhouse_ - builds, stores and serves the SeqWeb knowledge graph data as a public Internet resource
- SeqWeb dev env - supports developers, devops engineers, ontologists, mathematicians, et al via CLI & tooling

### SeqWeb repository ecosystem

Here's a recap of the Github repos associated with the SeqWeb project

- **oeisdata**: `git@github.com:oeis/oeisdata.git` - OEIS source corpus (~400,000 .seq files, read-only)
- **seqwebdata**: `git@github.com:seqweb/seqwebdata.git` - Generated RDF data (.ttl files, output data)
- **seqwebcode**: `git@github.com:seqweb/seqwebcode.git` - This repo (code, config, ontology, tools...)
- **seqweb**: `git@github.com:seqweb/seqweb.git` - Project website & documentation
- **.github**: `git@github.com:seqweb/.github.git` - Github's seqweb.org profile and repo (admin only)

## Repository structure

```
seqwebcode/
├── docs/                  # Documentation
│   ├── technical/            # Technical specifications, APIs, etc.
│   ├── process/              # Development processes, workflows
│   └── plans/                # Project plans, roadmaps, milestones
├── src/                   # Source code
│   ├── java/                 # Java implementations
│   ├── python/               # Python implementations  
│   ├── cl/                   # Common Lisp implementations
│   └── scripts/              # Shell scripts and utilities
├── ontology/                 # RDF ontology definitions and vocabularies
├── tests/                 # Test suites
│   ├── java/
│   ├── python/
│   └── cl/
├── data/                  # Local data files, samples
└── examples/              # Example usage and demonstrations
```

### Quickstart

See [Quickstart Guide](docs/quickstart) for getting started with development. 