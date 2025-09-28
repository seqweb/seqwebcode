#!/usr/bin/env python3
"""
Script to run all tests in the test/app/fab directory.
"""

import sys
import os
import subprocess
import unittest

# Add the current directory to the path
sys.path.insert(0, os.path.dirname(__file__))

def run_test_file(test_file):
    """Run a single test file and return the result."""
    print(f"\n{'='*60}")
    print(f"Running {test_file}")
    print(f"{'='*60}")
    
    try:
        result = subprocess.run([
            sys.executable, 
            os.path.join('test', 'app', 'fab', test_file)
        ], capture_output=True, text=True, cwd=os.path.dirname(__file__))
        
        print(result.stdout)
        if result.stderr:
            print("STDERR:", result.stderr)
        
        return result.returncode == 0
    except Exception as e:
        print(f"Error running {test_file}: {e}")
        return False

def main():
    """Run all test files."""
    test_dir = os.path.join('test', 'app', 'fab')
    test_files = [f for f in os.listdir(test_dir) if f.startswith('test_') and f.endswith('.py')]
    test_files.sort()
    
    print(f"Found {len(test_files)} test files:")
    for test_file in test_files:
        print(f"  - {test_file}")
    
    results = {}
    for test_file in test_files:
        success = run_test_file(test_file)
        results[test_file] = success
    
    print(f"\n{'='*60}")
    print("SUMMARY")
    print(f"{'='*60}")
    
    passed = 0
    failed = 0
    
    for test_file, success in results.items():
        status = "PASS" if success else "FAIL"
        print(f"{status:4} {test_file}")
        if success:
            passed += 1
        else:
            failed += 1
    
    print(f"\nTotal: {passed} passed, {failed} failed")
    
    if failed > 0:
        sys.exit(1)
    else:
        print("All tests passed!")

if __name__ == '__main__':
    main()
