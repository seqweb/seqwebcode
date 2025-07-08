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

- **oeisdata**: `git@github.com:oeis/oeisdata.git` - OEIS source corpus (~400,000 .seq files, read-only data resource)
- **seqwebdata**: `git@github.com:seqweb/seqwebdata.git` - Generated RDF data (.ttl files, output data)
- **seqwebcode**: `git@github.com:seqweb/seqwebcode.git` - This repository (code, docs, ontology, tools)

> **Note for Cursor users**: This workspace includes all three repositories. The data repositories (oeisdata, seqwebdata) contain large numbers of files and should not be indexed. Focus development work in seqwebcode.

## QuickStart

1. **Clone the repository:**
   ```bash
   git clone https://github.com/seqweb/seqwebcode.git
   cd seqwebcode
   ```

2. **Set up the development environment:**
   ```bash
   chmod +x seqweb
   ./seqweb setup
   ```

3. **Start using SeqWeb:**
   ```bash
   ./seqweb help
   ```

That's it! The `./seqweb` command is now available for development work.

## Development

This repository contains the code, documentation, and ontology for converting OEIS sequence data into a semantic web knowledge graph.
