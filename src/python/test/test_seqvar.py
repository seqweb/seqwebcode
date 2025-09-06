#!/usr/bin/env python3
"""
Simple test program for Python seqvar functionality.
Analogous to tests/java/TestSeqvar.java

This test demonstrates basic seqvar operations:
- Setting values
- Getting values
- Retrieving as dictionary
- Dumping all contents

Usage:
    python tests/python/test_seqvar.py

Note: This test requires the seqvar database to be initialized.
The test will create test data and display results.
"""

import sys
import os
from pathlib import Path

# Add src/python to Python path for imports
src_python = Path(__file__).parent.parent.parent / "src" / "python"
sys.path.insert(0, str(src_python))

try:
    from libs.core.seqvar.seqvar import get, set, get_dict, dump
    from libs.core.seqvar import init_seqvar_store
except ImportError as e:
    print(f"❌ Import error: {e}")
    print(f"Make sure you're running from the seqwebcode directory")
    print(f"and that the Python seqvar implementation is available")
    sys.exit(1)

def test_seqvar_operations():
    """Test basic seqvar operations."""
    print("🧪 Testing Python seqvar functionality...")
    
    try:
        # Initialize the seqvar store
        print("📁 Initializing seqvar store...")
        init_seqvar_store()
        
        # Test setting values
        print("✏️  Setting test values...")
        set("test.python.version", "3.11+", "test_seqvar.py")
        set("test.python.feature", "builtin_libraries", "test_seqvar.py")
        set("test.python.status", "working", "test_seqvar.py")
        
        # Test getting values
        print("🔍 Retrieving test values...")
        version = get("test.python.version")
        feature = get("test.python.feature")
        status = get("test.python.status")
        
        print(f"   test.python.version: {version}")
        print(f"   test.python.feature: {feature}")
        print(f"   test.python.status: {status}")
        
        # Test getting as dictionary
        print("📚 Getting all test values as dictionary...")
        test_dict = get_dict("test.python.%")
        print(f"   Test values: {test_dict}")
        
        # Test dumping all contents
        print("📋 Dumping all seqvar contents...")
        all_contents = dump()
        print(f"   Total entries: {len(all_contents)}")
        
        # Display some entries (limit to avoid overwhelming output)
        print("   Sample entries:")
        for i, (key, value, src, timestamp) in enumerate(all_contents[:5]):
            print(f"     {key} = {value} (from {src}, {timestamp})")
        
        if len(all_contents) > 5:
            print(f"     ... and {len(all_contents) - 5} more entries")
        
        print("✅ Python seqvar test completed successfully!")
        return True
        
    except Exception as e:
        print(f"❌ Error during seqvar operations: {e}")
        return False

def main():
    """Main test function."""
    print("=" * 60)
    print("Python SeqVar Test Program")
    print("=" * 60)
    
    success = test_seqvar_operations()
    
    print("=" * 60)
    if success:
        print("🎉 All tests passed!")
    else:
        print("💥 Some tests failed!")
    print("=" * 60)
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())
