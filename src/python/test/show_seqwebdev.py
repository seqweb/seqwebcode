#!/usr/bin/env python3
"""
Simple program to show seqwebdev.home seqvar value
This program demonstrates basic seqvar access from a test script
"""

import sys

def main():
    try:
        # Import seqvar functionality
        from seqvar.seqvar import get
        
        # Get the seqwebdev.home value
        home_value = get("seqwebdev.home")
        
        print(f"seqwebdev.home = {home_value}")
        
        return 0
        
    except ImportError as e:
        print(f"❌ Failed to import seqvar: {e}")
        print("   This suggests the PYTHONPATH is not set correctly")
        return 1
        
    except Exception as e:
        print(f"❌ Error accessing seqvar: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
