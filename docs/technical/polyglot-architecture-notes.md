# Polyglot Architecture Notes

## Context

One of the primary functions of the SeqWeb codebase will be to ingest `.seq` files from the oeisdata repo and create/update corresponding `.ttl` (RDF) files in the seqwebdata repo.

## Multi-Language Approach Rationale

While the entire conversion could theoretically be implemented in a single language, different languages are better suited for different aspects of the processing pipeline:

### Java Strengths
- High-performance file I/O and parsing
- Strong typing for OEIS data structures  
- Excellent for bulk processing and memory management
- Good for the core conversion logic

### Python Strengths
- Rich NLP libraries (spaCy, NLTK, transformers)
- Mathematical libraries (SymPy, NumPy)
- Flexible text processing and regex
- Great for ad-hoc analysis and prototyping

## Integration Approaches

### 1. Pipeline with File Handoffs
- Java: Parse `.seq` → intermediate JSON/XML
- Python: Process text → enhanced annotations
- Java: Generate final `.ttl`

### 2. Microservices
- Java service for core conversion
- Python service for NLP/math processing
- REST/gRPC communication

### 3. Process Orchestration
- Shell scripts coordinate the pipeline
- Each language handles its optimal domain
- Clear data contracts between stages

### 4. Shared Data Format
- JSON Schema for intermediate representation
- Each stage validates input/output
- Enables independent development/testing

## Recommendation

Start with approach #1 (file handoffs) for simplicity, evolve to #3 (orchestration) as complexity grows. The key is defining clean interfaces between language boundaries.

## Pipeline Architecture Design

### A-Number as Primary Key
All operations associated with processing a given `.seq` into the corresponding `.ttl` can be coordinated via the associated "A-number" (e.g., `A000000`). Each stage uses this as a key to access input, output, and intermediate files.

### Pipeline Driver Implementation

**Language: Shell scripts (Bash)**
- **Why**: Natural for orchestration, easy to invoke different language programs
- **Structure**: Main driver takes A-number, coordinates stages, handles file paths
- **Benefits**: Simple, portable, easy to debug and modify

### Individual Stage Implementation

**Command-line programs in their native languages:**
- **Java**: `java -jar seq-parser.jar A000001`
- **Python**: `python seq_processor.py A000001`
- **CL**: `sbcl --script seq-analyzer.lisp A000001`

### Architecture Pattern

```
pipeline.sh A000001
├── java-parser A000001 → intermediate.json
├── python-processor A000001 → enhanced.json  
├── java-generator A000001 → A000001.ttl
└── validation A000001
```

### File Organization

```
data/
├── intermediate/
│   ├── A000001.json
│   └── A000001.enhanced.json
├── output/
│   └── A000001.ttl
└── logs/
    └── A000001.log
```

### Higher-Level Orchestration

- **Batch processor**: `batch-process.sh` (processes multiple A-numbers)
- **Change detector**: `process-changes.sh` (finds modified .seq files)
- **Validation suite**: `validate-all.sh` (runs validation across outputs)

### Benefits of This Approach

1. **Language independence**: Each stage can be implemented in optimal language
2. **Testability**: Each stage can be tested independently
3. **Scalability**: Easy to parallelize or distribute
4. **Debugging**: Clear data flow and intermediate states
5. **Flexibility**: Easy to add/remove/modify stages

## Implementation Strategy

**Recommendation**: Start with shell orchestration, evolve to more sophisticated orchestration (e.g., Python with asyncio, or dedicated workflow engine) as complexity grows.

## Next Steps

- Implement basic shell script pipeline structure
- Define intermediate data formats (JSON schemas)
- Establish language-specific component boundaries
- Create A-number based file organization

---

*Generated from discussion on polyglot architecture approaches for SeqWeb conversion pipeline* 