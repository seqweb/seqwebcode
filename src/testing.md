# Hello World Programs

This directory contains simple "hello world" programs for each language supported by SeqWeb. These programs can be used to quickly verify that basic language support is functional.

## Purpose

Each hello world program serves as a simple verification that:
- The language runtime is available and working
- Basic language features are functional
- The SeqWeb development environment is properly configured

## Available Programs

### Python
- **File**: `python/test/hello_world.py`
- **Run**: `python3 src/python/test/hello_world.py`
- **Tests**: Python 3.11+, sqlite3, tomllib modules

### Java
- **File**: `java/test/HelloWorld.java`
- **Compile**: `javac src/java/test/HelloWorld.java`
- **Run**: `java -cp src/java/test HelloWorld`
- **Tests**: Java 8+, basic language features

### Common Lisp
- **File**: `cl/test/hello-world.lisp`
- **Load**: `sbcl --load src/cl/test/hello-world.lisp`
- **Tests**: CL implementation, basic language features

### Bash
- **File**: `bash/test/hello_world.sh`
- **Run**: `src/bash/test/hello_world.sh`
- **Tests**: Bash 4.0+, basic shell features

## Usage

### Quick Verification
To verify that a language is working:

```bash
# Python
python3 src/python/test/hello_world.py

# Java
javac src/java/test/HelloWorld.java && java -cp src/java/test HelloWorld

# Bash
src/bash/test/hello_world.sh

# Common Lisp (when available)
sbcl --load src/cl/test/hello-world.lisp
```

### Expected Output
Each program should output:
- "Hello, World!" message
- Language version information
- Verification of basic language features
- Success confirmation

### Troubleshooting
If a hello world program fails:
1. Check that the language runtime is installed
2. Verify the language version meets requirements
3. Check that the SeqWeb environment is properly configured
4. Review the specific error message for guidance

## Notes

- These programs are intentionally simple to avoid dependencies
- They test only basic language features, not SeqWeb-specific functionality
- Use them as a first step in troubleshooting language issues
- For comprehensive testing, use the language-specific test suites
