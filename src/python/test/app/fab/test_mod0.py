#!/usr/bin/env python3
"""
Test suite for mod0 module.

Tests the core functionality of the simple test module that processes IDs.
"""

import unittest
import sys
import os
from unittest.mock import patch

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.mod0 import mod0


class TestMod0(unittest.TestCase):
    """Test cases for mod0 core function."""

    def test_mod0_success(self):
        """Test successful mod0 execution."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        self.assertEqual(result['extra_key'], 'extra_value')
        # mod0 should return the box unchanged
        self.assertEqual(result, box)

    def test_mod0_with_noisy_output(self):
        """Test mod0 with noisy output enabled."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], True)
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertEqual(print_args, 'mod0: ID=A000001')

    def test_mod0_without_noisy_output(self):
        """Test mod0 without noisy output (should not print)."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        
        # Verify no output was printed
        mock_print.assert_not_called()

    def test_mod0_different_id(self):
        """Test mod0 with different ID."""
        # Arrange
        box = {
            'id': 'A000123',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        
        # Verify correct ID was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertEqual(print_args, 'mod0: ID=A000123')

    def test_mod0_preserves_extra_keys(self):
        """Test that mod0 preserves extra keys in the box."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42,
            'complex_key': {'nested': 'value'}
        }
        
        # Act
        result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['complex_key'], {'nested': 'value'})
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)

    def test_mod0_returns_same_box(self):
        """Test that mod0 returns the exact same box object."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = mod0(box, **box)
        
        # Assert
        # mod0 should return the exact same box object
        self.assertIs(result, box)

    def test_mod0_empty_box(self):
        """Test mod0 with minimal box."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        self.assertEqual(len(result), 2)

    def test_mod0_with_all_parameters(self):
        """Test mod0 with all possible parameters."""
        # Arrange
        box = {
            'id': 'A000001',
            'noisy': True,
            'param1': 'value1',
            'param2': 42,
            'param3': [1, 2, 3],
            'param4': {'key': 'value'}
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = mod0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], True)
        self.assertEqual(result['param1'], 'value1')
        self.assertEqual(result['param2'], 42)
        self.assertEqual(result['param3'], [1, 2, 3])
        self.assertEqual(result['param4'], {'key': 'value'})
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertEqual(print_args, 'mod0: ID=A000001')

    def test_mod0_noisy_with_different_ids(self):
        """Test mod0 noisy output with various ID formats."""
        test_cases = [
            'A000001',
            'a000001',
            'A000123',
            'A123456',
            'B000001'
        ]
        
        for test_id in test_cases:
            with self.subTest(id=test_id):
                # Arrange
                box = {
                    'id': test_id,
                    'noisy': True
                }
                
                # Act
                with patch('builtins.print') as mock_print:
                    result = mod0(box, **box)
                
                # Assert
                self.assertEqual(result['id'], test_id)
                
                # Verify correct ID was printed
                mock_print.assert_called_once()
                print_args = mock_print.call_args[0][0]
                self.assertEqual(print_args, f'mod0: ID={test_id}')


if __name__ == '__main__':
    unittest.main()
