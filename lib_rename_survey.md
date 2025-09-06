# Lib Directory Rename Survey

## Overview
Renaming all `*/lib/` directories to `*/libs/core/` and creating `*/libs/common/` directories.

## Directory Structure Changes
```
BEFORE:
src/python/lib/
src/java/lib/
src/bash/lib/
src/cl/lib/

AFTER:
src/python/libs/core/
src/python/libs/common/
src/java/libs/core/
src/java/libs/common/
src/bash/libs/core/
src/bash/libs/common/
src/cl/libs/core/
src/cl/libs/common/
```

## Survey Status Legend
- ✅ **CLEAR** - No lib/ references found
- 🔍 **NEEDS_REVIEW** - Contains lib/ references, needs analysis
- ⚠️ **KNOWN_ISSUES** - Known to have lib/ references that need fixing
- ✅ **FIXED** - References have been updated
- ❌ **UNREVIEWED** - Not yet checked

## File Survey

### Python Files
| File | Status | Notes |
|------|--------|-------|
| `src/python/app/cli/seqwebdev` | ⚠️ | Import paths, sys.path modifications |
| `src/python/app/cli/setup.py` | ⚠️ | Import statements |
| `src/python/lib/foothold/foothold.py` | ⚠️ | Path calculations, comments |
| `src/python/lib/home/paths.py` | ⚠️ | Path calculations |
| `src/python/lib/seqvar/__init__.py` | ⚠️ | Import statements |
| `src/python/lib/seqvar/seqvar.py` | ⚠️ | Path calculations, comments |
| `src/python/lib/seqvar/toml.py` | ⚠️ | Import statements |
| `src/python/test/test_seqvar.py` | ⚠️ | Import statements |
| `src/python/dependencies.md` | ⚠️ | Documentation references |

### Java Files
| File | Status | Notes |
|------|--------|-------|
| `src/java/lib/seqvar/SeqVar.java` | ⚠️ | Package declarations, imports |
| `src/java/lib/seqvar/SeqvarToml.java` | ⚠️ | Package declarations, imports |
| `src/java/lib/seqvar/README.md` | ⚠️ | Documentation references |

### Bash Files
| File | Status | Notes |
|------|--------|-------|
| `src/bash/lib/seqvar.sh` | ⚠️ | Path references, comments |
| `src/bash/lib/seqvar_toml.sh` | ⚠️ | Path references, comments |

### Common Lisp Files
| File | Status | Notes |
|------|--------|-------|
| `src/cl/lib/seqvar.lisp` | ⚠️ | Path references, comments |
| `src/cl/lib/seqvar-toml.lisp` | ⚠️ | Path references, comments |

### Configuration Files
| File | Status | Notes |
|------|--------|-------|
| `bootstrap` | ⚠️ | Path references in generated code |
| `.gitignore` | ⚠️ | lib/ ignore patterns |

### Documentation Files
| File | Status | Notes |
|------|--------|-------|
| `src/README.md` | 🔍 | May contain lib/ references |
| `src/python/dependencies.md` | ⚠️ | Known to reference lib/ |
| `src/python/lib/foothold/foothold.py` | ⚠️ | Comments and docstrings |

## Reference Types to Fix

### Python
1. **Import statements**: `from lib.seqvar import ...`
2. **sys.path modifications**: `sys.path.insert(0, str(foothold_path))`
3. **Path calculations**: `Path(__file__).parent.parent / "lib" / "foothold"`
4. **String literals**: Generated code, comments
5. **Documentation**: README files, docstrings

### Java
1. **Package declarations**: `package lib.seqvar;`
2. **Import statements**: `import lib.seqvar.SeqVar;`
3. **Path references**: File paths in comments

### Bash
1. **Path references**: `source "$SCRIPT_DIR/lib/seqvar.sh"`
2. **Variable assignments**: Path variables
3. **Comments**: Documentation

### Common Lisp
1. **Path references**: `(load "lib/seqvar.lisp")`
2. **Variable assignments**: Path variables
3. **Comments**: Documentation

## Next Steps
1. Create new directory structure
2. Move files from lib/ to libs/core/
3. Create libs/common/ directories with .gitkeep
4. Fix all references systematically
5. Test changes
6. Restore .gitignore

## Progress Tracking
- [ ] Directory structure created
- [ ] Files moved
- [ ] Python references fixed
- [ ] Java references fixed
- [ ] Bash references fixed
- [ ] Common Lisp references fixed
- [ ] Documentation updated
- [ ] Configuration files updated
- [ ] Testing completed

