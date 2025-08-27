# Java Dependencies for SeqWeb System

This document outlines the dependencies required to build and run the Java components of the SeqWeb polyglot system.

## Core Dependencies

- **Java 8+**: Required for Path, Files, and other modern Java features
- **SQLite JDBC Driver**: For database connectivity
- **org.tomlj:tomlj**: For TOML parsing

## Maven Dependencies

```xml
<dependencies>
    <!-- TOML parsing -->
    <dependency>
        <groupId>org.tomlj</groupId>
        <artifactId>tomlj</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- SQLite JDBC driver -->
    <dependency>
        <groupId>org.xerial</groupId>
        <artifactId>sqlite-jdbc</artifactId>
        <version>3.42.0.0</version>
    </dependency>
</dependencies>
```

## Gradle Dependencies

```gradle
dependencies {
    implementation 'org.tomlj:tomlj:1.0.0'
    implementation 'org.xerial:sqlite-jdbc:3.42.0.0'
}
```

## Package Structure

```
seqwebcode/src/java/
├── app/                    # Java applications (future)
├── lib/
│   └── seqvar/
│       ├── SeqVar.java         # Core get/set functionality
│       └── SeqvarToml.java     # TOML parsing and integration
└── test/
    ├── TestSeqvar.java         # Unit tests
    └── TestSeqvar.class        # Compiled tests
```

## Current System Components

The Java implementation currently supports:

- **seqvar facility**: Core key-value configuration system with TOML integration

*Note: Additional Java components, libraries, and applications are expected to be added as the SeqWeb system expands.*

## Build and Test

```bash
# Compile
javac -cp src/java src/java/lib/seqvar/*.java

# Run tests (when dependencies available)
# TODO: Add proper test runner configuration
```

## Notes

- **External libraries required**: Needs SQLite JDBC and TOML parser
- **Implementation complete**: All code written, waiting for dependencies
- **Testing blocked**: Cannot run tests until dependencies resolved
- **Future ready**: Structure in place for applications and comprehensive testing
- **Extensible**: Architecture supports adding new system components beyond seqvar
