# SeqWeb Python package - enables cross-domain imports between subdomains
# This file ensures that all modules under python/ can import from each other

import sys
from pathlib import Path

# Add the python/ directory to Python path so subdomains can import from each other
# This enables imports like: from home.paths import seqwebdev_home_path
python_dir = Path(__file__).parent
if str(python_dir) not in sys.path:
    sys.path.insert(0, str(python_dir))
