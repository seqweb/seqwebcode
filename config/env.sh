#!/bin/bash
# SeqWeb Environment Configuration

# Base paths (can be overridden by environment variables)
export SEQWEB_HOME="${SEQWEB_HOME:-$(pwd)}"
export SEQWEB_COMMAND_DIR="${SEQWEB_COMMAND_DIR:-$SEQWEB_HOME/tools/commands}"

# Repository paths (relative to SEQWEB_HOME by default)
export SEQWEB_CODE_DIR="${SEQWEB_CODE_DIR:-$SEQWEB_HOME}"
export SEQWEB_DATA_DIR="${SEQWEB_DATA_DIR:-$SEQWEB_HOME/../seqwebdata}"
export OEIS_DATA_DIR="${OEIS_DATA_DIR:-$SEQWEB_HOME/../oeisdata}"

# Data directories
export SEQWEB_INTERMEDIATE_DIR="${SEQWEB_INTERMEDIATE_DIR:-$SEQWEB_HOME/data/intermediate}"
export SEQWEB_OUTPUT_DIR="${SEQWEB_OUTPUT_DIR:-$SEQWEB_HOME/data/output}"
export SEQWEB_LOGS_DIR="${SEQWEB_LOGS_DIR:-$SEQWEB_HOME/data/logs}"

# Language runtimes (can be overridden)
export SEQWEB_JAVA="${SEQWEB_JAVA:-java}"
export SEQWEB_PYTHON="${SEQWEB_PYTHON:-python3}"
export SEQWEB_CL="${SEQWEB_CL:-sbcl}"

# Ensure data directories exist
mkdir -p "$SEQWEB_INTERMEDIATE_DIR" "$SEQWEB_OUTPUT_DIR" "$SEQWEB_LOGS_DIR" 