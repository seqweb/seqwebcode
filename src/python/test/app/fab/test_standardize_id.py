#!/usr/bin/env python3
"""
Test suite for standardize_id module.

Tests the core functionality of standardizing sequence IDs to A###### format.
"""

import unittest
import sys
import os
from unittest.mock import patch

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.standardize_id import standardize_id, get_standard_id


class TestStandardizeId(unittest.TestCase):
    """Test cases for standardize_id core function."""

    def test_standardize_id_success(self):
        """Test successful ID standardization."""
        # Arrange
        box = {
            'id': 'a000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = standardize_id(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertEqual(result['noisy'], False)

    def test_standardize_id_with_noisy_output(self):
        """Test standardize_id with noisy output enabled."""
        # Arrange
        box = {
            'id': 'a000001',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = standardize_id(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertEqual(print_args, 'standardize_id: a000001 -> A000001')

    def test_standardize_id_different_formats(self):
        """Test standardize_id with various input formats."""
        test_cases = [
            ('695', 'A000695'),
            ('a695', 'A000695'),
            ('A695', 'A000695'),
            ('00695', 'A000695'),
            ('A00695', 'A000695'),
            ('0', 'A000000'),
            ('999999', 'A999999'),
            (' 123 ', 'A000123'),  # With whitespace
        ]
        
        for input_id, expected_output in test_cases:
            with self.subTest(input_id=input_id):
                # Arrange
                box = {
                    'id': input_id,
                    'noisy': False
                }
                
                # Act
                result = standardize_id(box, **box)
                
                # Assert
                self.assertEqual(result['id'], expected_output)

    def test_standardize_id_preserves_extra_keys(self):
        """Test that standardize_id preserves extra keys in the box."""
        # Arrange
        box = {
            'id': 'a000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = standardize_id(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')

    def test_standardize_id_invalid_formats(self):
        """Test standardize_id with invalid input formats."""
        invalid_cases = [
            '',  # Empty string
            '   ',  # Whitespace only
            'abc',  # Non-numeric
            'A123abc',  # Mixed alphanumeric
            '123abc',  # Mixed alphanumeric
            'A',  # Just A
            'A1234567',  # Too many digits
            '1234567',  # Too many digits
            '-123',  # Negative number
            'A-123',  # Negative with A prefix
        ]
        
        for invalid_id in invalid_cases:
            with self.subTest(invalid_id=invalid_id):
                # Arrange
                box = {
                    'id': invalid_id,
                    'noisy': False
                }
                
                # Act & Assert
                with self.assertRaises(ValueError):
                    standardize_id(box, **box)

    def test_standardize_id_out_of_range(self):
        """Test standardize_id with out-of-range numbers."""
        out_of_range_cases = [
            '1000000',  # Too large
            'A1000000',  # Too large with A prefix
            '9999999',  # Way too large
        ]
        
        for out_of_range_id in out_of_range_cases:
            with self.subTest(out_of_range_id=out_of_range_id):
                # Arrange
                box = {
                    'id': out_of_range_id,
                    'noisy': False
                }
                
                # Act & Assert
                with self.assertRaises(ValueError) as context:
                    standardize_id(box, **box)
                
                self.assertIn("out of range", str(context.exception))

    def test_standardize_id_edge_cases(self):
        """Test standardize_id with edge cases."""
        edge_cases = [
            ('0', 'A000000'),
            ('000000', 'A000000'),
            ('A000000', 'A000000'),
            ('999999', 'A999999'),
            ('A999999', 'A999999'),
        ]
        
        for input_id, expected_output in edge_cases:
            with self.subTest(input_id=input_id):
                # Arrange
                box = {
                    'id': input_id,
                    'noisy': False
                }
                
                # Act
                result = standardize_id(box, **box)
                
                # Assert
                self.assertEqual(result['id'], expected_output)

    def test_standardize_id_case_insensitive(self):
        """Test that standardize_id handles case insensitively."""
        case_cases = [
            ('a123', 'A000123'),
            ('A123', 'A000123'),
            ('a000123', 'A000123'),
            ('A000123', 'A000123'),
        ]
        
        for input_id, expected_output in case_cases:
            with self.subTest(input_id=input_id):
                # Arrange
                box = {
                    'id': input_id,
                    'noisy': False
                }
                
                # Act
                result = standardize_id(box, **box)
                
                # Assert
                self.assertEqual(result['id'], expected_output)


class TestGetStandardId(unittest.TestCase):
    """Test cases for get_standard_id helper function."""

    def test_get_standard_id_success(self):
        """Test successful ID standardization with helper function."""
        test_cases = [
            ('695', 'A000695'),
            ('a695', 'A000695'),
            ('A695', 'A000695'),
            ('00695', 'A000695'),
            ('A00695', 'A000695'),
            ('0', 'A000000'),
            ('999999', 'A999999'),
            (' 123 ', 'A000123'),
        ]
        
        for input_id, expected_output in test_cases:
            with self.subTest(input_id=input_id):
                # Act
                result = get_standard_id(input_id)
                
                # Assert
                self.assertEqual(result, expected_output)

    def test_get_standard_id_invalid_formats(self):
        """Test get_standard_id with invalid input formats."""
        invalid_cases = [
            '',  # Empty string
            '   ',  # Whitespace only
            'abc',  # Non-numeric
            'A123abc',  # Mixed alphanumeric
            '123abc',  # Mixed alphanumeric
            'A',  # Just A
            'A1234567',  # Too many digits
            '1234567',  # Too many digits
            '-123',  # Negative number
            'A-123',  # Negative with A prefix
        ]
        
        for invalid_id in invalid_cases:
            with self.subTest(invalid_id=invalid_id):
                # Act & Assert
                with self.assertRaises(ValueError):
                    get_standard_id(invalid_id)

    def test_get_standard_id_out_of_range(self):
        """Test get_standard_id with out-of-range numbers."""
        out_of_range_cases = [
            '1000000',  # Too large
            'A1000000',  # Too large with A prefix
            '9999999',  # Way too large
        ]
        
        for out_of_range_id in out_of_range_cases:
            with self.subTest(out_of_range_id=out_of_range_id):
                # Act & Assert
                with self.assertRaises(ValueError) as context:
                    get_standard_id(out_of_range_id)
                
                self.assertIn("out of range", str(context.exception))

    def test_get_standard_id_error_messages(self):
        """Test that get_standard_id provides informative error messages."""
        # Test empty input
        with self.assertRaises(ValueError) as context:
            get_standard_id('')
        self.assertIn("Empty ID provided", str(context.exception))
        
        # Test non-numeric
        with self.assertRaises(ValueError) as context:
            get_standard_id('abc')
        self.assertIn("contains non-digits", str(context.exception))
        
        # Test out of range
        with self.assertRaises(ValueError) as context:
            get_standard_id('1000000')
        self.assertIn("out of range (0-999999)", str(context.exception))

    def test_get_standard_id_whitespace_handling(self):
        """Test that get_standard_id handles whitespace correctly."""
        whitespace_cases = [
            (' 123 ', 'A000123'),
            ('\t123\t', 'A000123'),
            ('\n123\n', 'A000123'),
            ('   A123   ', 'A000123'),
        ]
        
        for input_id, expected_output in whitespace_cases:
            with self.subTest(input_id=repr(input_id)):
                # Act
                result = get_standard_id(input_id)
                
                # Assert
                self.assertEqual(result, expected_output)


if __name__ == '__main__':
    unittest.main()
