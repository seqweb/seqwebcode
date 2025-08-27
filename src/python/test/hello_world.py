#!/usr/bin/env python3

"""
hello_world.py - Simple Python hello world program
Created by Waldo 2025-08-27

This program verifies basic Python functionality in the SeqWeb environment.
"""

def main():
    """Main function that prints hello world."""
    print("Hello, World!")
    print("Python is working in the SeqWeb environment!")
    
    # Verify some basic Python functionality
    import sys
    print(f"Python version: {sys.version}")
    
    import sqlite3
    print("✓ SQLite3 module imported successfully")
    
    try:
        import tomllib
        print("✓ tomllib module imported successfully")
    except ImportError:
        print("⚠ tomllib module not available (requires Python 3.11+)")
    
    print("Python environment verification complete!")

if __name__ == "__main__":
    main()
