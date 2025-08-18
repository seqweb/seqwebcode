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
├── data/                   # Local data files, samples
├── examples/               # Example usage and demonstrations
├── ontology/               # RDF ontology definitions and vocabularies
├── src/                    # Source code implementations
│   ├── java/                   # Java code
│   ├── python/                 # Python code
│   ├── cl/                     # Common Lisp code
│   └── scripts/                # Shell scripts and utilities
├── tests/                  # Test suites (subfolders mirror `src/`)
├── tools/                  # development tools, including CLI command support
├── bootstrap               # quickstart bootstrap script (called via curl command to Github repo)
├── LICENSE                 # MIT license for this repo
├── README.md               # This README file
└── seqwebdev               # seqwebdev CLI Python implementation & entry point
```

### Quickstart

See [Quickstart Guide](docs/quickstart) for getting started with development. 