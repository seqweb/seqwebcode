#!/usr/bin/env python3
"""
SeqWeb Python CLI Setup functions
"""

import os
import sys
from pathlib import Path

# Add the src/python directory to Python path to access the seqvar package
# Assumption: This file is located at seqwebcode/src/python/cli/setup.py
PYTHON_SRC_DIR = Path(__file__).parent  # Go up to src/python/
sys.path.insert(0, str(PYTHON_SRC_DIR))

# Import seqvar functionality
try:
    from seqvar.seqvar import get, set as seqvar_set
    from seqvar.seqvar_toml import load_toml, write_toml_to_seqvar
    from seqvar.init_seqvar_db import init_seqvar_db
except ImportError as e:
    print(f"❌ seqvar package not found: {e}")
    print("Please ensure seqvar package is available in seqwebcode/src/python/seqvar/")
    sys.exit(1)


def setup_for_cli():
    """
    Ensure seqvar facility is initialized and do any other environment prep
    needed for CLI operation. Called by CLI right before command dispatch.
    """
    try:
        # Check if SEQWEBDEV_HOME is set
        if not os.environ.get("SEQWEBDEV_HOME"):
            print("❌ SEQWEBDEV_HOME environment variable is not set")
            print("   Please set SEQWEBDEV_HOME to your SeqWeb development home directory")
            sys.exit(1)

        # Initialize seqvar database if needed
        try:
            init_seqvar_db(verbose=False)
            # print(f"✅ seqvar database ready at: {db_path}")
        except Exception as e:
            print(f"⚠️  Warning: Could not initialize seqvar database: {e}")
            print("   SeqWeb CLI may not function properly without seqvar initialization")

        # Set basic environment values if they're not already set
        try:
            if not get("seqweb.home", ns="env"):
                seqvar_set("seqweb.home", os.environ["SEQWEBDEV_HOME"], ns="env", src="seqwebdev")
            # print("✅ Basic environment configuration set")
        except Exception as e:
            print(f"⚠️  Warning: Could not set seqweb.home: {e}")

        # Load seqweb.conf configuration file if it exists
        config_file = Path(os.environ["SEQWEBDEV_HOME"]) / "seqweb.conf"
        if config_file.exists():
            try:
                bindings = load_toml(str(config_file))
                # Write to seqvar with 'config' namespace
                write_toml_to_seqvar(bindings, ns="config", src="seqwebdev")
                # print(f"📄 Loaded configuration from: {config_file}")
            except Exception as e:
                print(f"⚠️  Warning: Could not load config from {config_file}: {e}")

        # print("✅ SeqWeb CLI environment setup complete")

    except Exception as e:
        print(f"❌ Critical error during SeqWeb CLI setup: {e}")
        # Don't exit here - let the CLI try to continue
