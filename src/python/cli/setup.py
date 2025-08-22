#!/usr/bin/env python3
"""
SeqWeb Python CLI Setup functions
"""

import os
import sys
from pathlib import Path

# Add the src/python directory to Python path to access the seqvar package
SCRIPT_DIR = Path(__file__).parent.parent.parent  # Go up to seqwebcode/
PYTHON_SRC_DIR = SCRIPT_DIR / "src" / "python"
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
            # Try to set a default based on current working directory
            cwd = Path.cwd()
            if "seqweb" in cwd.parts:
                # Find the seqweb-related directory
                for part in cwd.parts:
                    if "seqweb" in part:
                        seqweb_root = cwd.parents[cwd.parts.index(part)]
                        os.environ["SEQWEBDEV_HOME"] = str(seqweb_root)
                        break
                else:
                    # Fallback to current directory
                    os.environ["SEQWEBDEV_HOME"] = str(cwd)
            else:
                # Fallback to current directory
                os.environ["SEQWEBDEV_HOME"] = str(cwd)
            
            print(f"📁 Set SEQWEBDEV_HOME to: {os.environ['SEQWEBDEV_HOME']}")
        
        # Initialize seqvar database if needed
        try:
            db_path = init_seqvar_db(verbose=False)
            # print(f"✅ seqvar database ready at: {db_path}")
        except Exception as e:
            print(f"⚠️  Warning: Could not initialize seqvar database: {e}")
            print("   SeqWeb CLI may not function properly without seqvar initialization")
        
        # Set up basic environment values if they don't exist
        try:
            if not get("seqweb.home", ns="env"):
                seqvar_set("seqweb.home", str(SCRIPT_DIR), ns="env", src="seqwebdev")
            
            if not get("seqweb.python_src", ns="env"):
                seqvar_set("seqweb.python_src", str(PYTHON_SRC_DIR), ns="env", src="seqwebdev")
            
            # print("✅ Basic environment configuration set")
        except Exception as e:
            print(f"⚠️  Warning: Could not set environment configuration: {e}")
        
        # Load any existing TOML configuration files
        config_files = [
            SCRIPT_DIR / "seqweb.conf",
            SCRIPT_DIR / "config" / "seqweb.toml"
        ]
        
        for config_file in config_files:
            if config_file.exists():
                try:
                    bindings = load_toml(str(config_file))
                    # Write to seqvar with 'config' namespace
                    write_toml_to_seqvar(bindings, ns="config", src="seqwebdev")
                    # print(f"📄 Loaded configuration from: {config_file}")
                except Exception as e:
                    print(f"⚠️  Warning: Could not load config from {config_file}: {e}")
        
        # print("🚀 CLI environment setup complete")
        
    except Exception as e:
        print(f"❌ Critical error during SeqWeb CLI setup: {e}")
        print("   SeqWeb CLI may not function properly")
        # Don't exit here - let the CLI try to continue
