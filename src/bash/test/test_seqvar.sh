#!/bin/bash

# Created by Waldo 2025-08-27

# test_seqvar.sh — Test suite for Bash seqvar implementation
# This is a reference implementation showing how to use the seqvar facility

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Source the seqvar library
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$SCRIPT_DIR/../lib/seqvar.sh"

# Test helper functions
test_start() {
    local test_name="$1"
    echo -e "${YELLOW}Testing: ${test_name}${NC}"
}

test_pass() {
    local test_name="$1"
    echo -e "  ${GREEN}✓ PASS: ${test_name}${NC}"
    ((TESTS_PASSED++))
}

test_fail() {
    local test_name="$1"
    local message="$2"
    echo -e "  ${RED}✗ FAIL: ${test_name} - ${message}${NC}"
    ((TESTS_FAILED++))
}

test_cleanup() {
    # Clean up test data
    local db
    db="$(_seqvar_db_path)"
    if [[ -f "$db" ]]; then
        sqlite3 "$db" "DELETE FROM seqvars WHERE src='test_seqvar';" 2>/dev/null || true
    fi
}

# Test 1: Basic get/set functionality
test_basic_get_set() {
    test_start "Basic get/set functionality"
    
    # Set a value
    seqvar_set "test_key" "test_value" "SeqVar" "test_seqvar"
    
    # Get the value back
    local result
    result="$(seqvar_get "test_key" "SeqVar")"
    
    if [[ "$result" == "test_value" ]]; then
        test_pass "Basic get/set"
    else
        test_fail "Basic get/set" "Expected 'test_value', got '$result'"
    fi
}

# Test 2: Default namespace
test_default_namespace() {
    test_start "Default namespace handling"
    
    # Set without specifying namespace
    seqvar_set "default_key" "default_value" "" "test_seqvar"
    
    # Get without specifying namespace
    local result
    result="$(seqvar_get "default_key")"
    
    if [[ "$result" == "default_value" ]]; then
        test_pass "Default namespace"
    else
        test_fail "Default namespace" "Expected 'default_value', got '$result'"
    fi
}

# Test 3: Empty value handling
test_empty_value() {
    test_start "Empty value handling"
    
    # Set empty value
    seqvar_set "empty_key" "" "SeqVar" "test_seqvar"
    
    # Get empty value
    local result
    result="$(seqvar_get "empty_key" "SeqVar")"
    
    if [[ "$result" == "" ]]; then
        test_pass "Empty value handling"
    else
        test_fail "Empty value handling" "Expected empty string, got '$result'"
    fi
}

# Test 4: Non-existent key
test_nonexistent_key() {
    test_start "Non-existent key handling"
    
    # Try to get a key that doesn't exist
    local result
    result="$(seqvar_get "nonexistent_key" "SeqVar")"
    
    if [[ "$result" == "" ]]; then
        test_pass "Non-existent key"
    else
        test_fail "Non-existent key" "Expected empty string, got '$result'"
    fi
}

# Test 5: Dump functionality
test_dump() {
    test_start "Dump functionality"
    
    # Set multiple values
    seqvar_set "dump_key1" "dump_value1" "SeqVar" "test_seqvar"
    seqvar_set "dump_key2" "dump_value2" "SeqVar" "test_seqvar"
    
    # Test dump
    local dump_result
    dump_result="$(seqvar_dump "SeqVar" | grep -c "dump_key")"
    
    if [[ "$dump_result" -ge 2 ]]; then
        test_pass "Dump functionality"
    else
        test_fail "Dump functionality" "Expected at least 2 dump keys, got $dump_result"
    fi
}

# Test 6: Get dict functionality
test_get_dict() {
    test_start "Get dict functionality"
    
    # Set values for dict test
    seqvar_set "dict_key1" "dict_value1" "SeqVar" "test_seqvar"
    seqvar_set "dict_key2" "dict_value2" "SeqVar" "test_seqvar"
    
    # Test get_dict without pattern
    local dict_result
    dict_result="$(seqvar_get_dict "" "SeqVar" | grep -c "dict_key")"
    
    if [[ "$dict_result" -ge 2 ]]; then
        test_pass "Get dict functionality"
    else
        test_fail "Get dict functionality" "Expected at least 2 dict keys, got $dict_result"
    fi
}

# Test 7: Pattern matching
test_pattern_matching() {
    test_start "Pattern matching functionality"
    
    # Set values with pattern
    seqvar_set "pattern.test.key1" "value1" "SeqVar" "test_seqvar"
    seqvar_set "pattern.test.key2" "value2" "SeqVar" "test_seqvar"
    seqvar_set "other.key" "other_value" "SeqVar" "test_seqvar"
    
    # Test pattern matching
    local pattern_result
    pattern_result="$(seqvar_get_dict "pattern.test.*" "SeqVar" | grep -c "pattern.test")"
    
    if [[ "$pattern_result" -eq 2 ]]; then
        test_pass "Pattern matching"
    else
        test_fail "Pattern matching" "Expected 2 pattern matches, got $pattern_result"
    fi
}

# Test 8: Error handling
test_error_handling() {
    test_start "Error handling"
    
    # Test with missing key parameter
    if ! seqvar_get 2>/dev/null; then
        test_pass "Error handling - missing key"
    else
        test_fail "Error handling - missing key" "Expected error for missing key"
    fi
}

# Main test runner
main() {
    echo "Running Bash seqvar tests..."
    echo "================================"
    
    # Check if we can access the database
    if ! _seqvar_check_db 2>/dev/null; then
        echo -e "${RED}ERROR: Cannot access seqvar database. Make sure SEQWEBDEV_HOME is set and the seqvar database is accessible.${NC}"
        exit 1
    fi
    
    # Run tests
    test_basic_get_set
    test_default_namespace
    test_empty_value
    test_nonexistent_key
    test_dump
    test_get_dict
    test_pattern_matching
    test_error_handling
    
    # Cleanup
    test_cleanup
    
    # Results
    echo "================================"
    echo "Test Results:"
    echo -e "  ${GREEN}Passed: ${TESTS_PASSED}${NC}"
    echo -e "  ${RED}Failed: ${TESTS_FAILED}${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        exit 0
    else
        echo -e "${RED}Some tests failed!${NC}"
        exit 1
    fi
}

# Run tests if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
