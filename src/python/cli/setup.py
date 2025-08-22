#!/usr/bin/env python3
"""
SeqWeb Python CLI Setup functions
"""

import sys
from pathlib import Path

# Add the src/python directory to Python path to access the seqvar package
SEQWEBCODE_DIR = Path(__file__).parent
PYTHON_SRC_DIR = SEQWEBCODE_DIR / "src" / "python"
sys.path.insert(0, str(PYTHON_SRC_DIR))

# Import seqvar functionality
try:
    from seqvar.seqvar import get, set as seqvar_set
    from seqvar.seqvar_toml import load_toml, write_toml_to_seqvar
except ImportError:
    print("seqvar package not found, attempting to install...")

    # Try to install the package automatically
    try:
        import subprocess

        # Get the path to the Python source directory
        script_dir = Path(__file__).parent
        python_src = script_dir / "src" / "python"

        # Run pip install -e for local development (use --user to avoid system conflicts)
        result = subprocess.run([
            sys.executable, "-m", "pip", "install", "--user", "-e", str(python_src)
        ], capture_output=True, text=True)

        if result.returncode == 0:
            print("Successfully installed seqvar package")
            # Now try importing again
            from seqvar.seqvar import get, set as seqvar_set
            from seqvar.seqvar_toml import load_toml, write_toml_to_seqvar
        else:
            print(f"Failed to install seqvar package: {result.stderr}")
            print("Please install manually with: pip install -e seqwebcode/src/python")
            sys.exit(1)

    except Exception as install_error:
        print(f"Could not automatically install seqvar package: {install_error}")
        print("Please install manually with: pip install -e seqwebcode/src/python")
        sys.exit(1)


# Initialize seqvar database if needed
def init_seqvar_db():
    """Initialize the seqvar database using the bootstrap script"""
    try:
        # Import and run the bootstrap initialization
        bootstrap_dir = SCRIPT_DIR / "tools" / "bootstrap"
        sys.path.insert(0, str(bootstrap_dir))

        from seqvar.init_seqvar_db import init_seqvar_db as bootstrap_init
        bootstrap_init(verbose=False)

        # Remove bootstrap dir from path after use
        sys.path.pop(0)

    except ImportError:
        print("Warning: Could not import bootstrap module, seqvar DB may not be initialized")
    except Exception as e:
        print(f"Warning: Could not initialize seqvar DB: {e}")


# Set up environment using seqvar instead of OS env vars
def setup_seqvar_environment():
    """Set up environment configuration using seqvar facility"""
    try:
        # Initialize seqvar database
        init_seqvar_db()

        # Set default environment values if they don't exist
        if not get("seqweb.home", ns="env"):
            seqvar_set("seqweb.home", str(SCRIPT_DIR), ns="env", src="seqwebdev")

        if not get("seqweb.python_src", ns="env"):
            seqvar_set("seqweb.python_src", str(PYTHON_SRC_DIR), ns="env", src="seqwebdev")

        if not get("seqweb.tools", ns="env"):
            seqvar_set("seqweb.tools", str(SCRIPT_DIR / "tools"), ns="env", src="seqwebdev")

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
                    print(f"Loaded configuration from {config_file}")
                except Exception as e:
                    print(f"Warning: Could not load config from {config_file}: {e}")

    except Exception as e:
        print(f"Warning: Could not set up seqvar environment: {e}")

