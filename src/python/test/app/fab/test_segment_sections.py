#!/usr/bin/env python3
"""
Test suite for segment_sections module.

Tests the core functionality of segmenting OEIS data into sections.
"""

import unittest
import sys
import os
from unittest.mock import patch

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.segment_sections import segment_sections


class TestSegmentSections(unittest.TestCase):
    """Test cases for segment_sections core function."""

    def test_segment_sections_success(self):
        """Test successful section segmentation."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3, 4, 5
%S A000001 1, 2, 3, 4, 5
%T A000001 1, 2, 3, 4, 5
%U A000001 1, 2, 3, 4, 5
%N A000001 The natural numbers
%A A000001 N. J. A. Sloane"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('section_map', result)
        
        section_map = result['section_map']
        self.assertIn('I', section_map)
        self.assertIn('S', section_map)
        self.assertIn('T', section_map)
        self.assertIn('U', section_map)
        self.assertIn('N', section_map)
        self.assertIn('A', section_map)
        
        # Verify content
        self.assertEqual(section_map['I'][0], '1, 2, 3, 4, 5')
        self.assertEqual(section_map['S'][0], '1, 2, 3, 4, 5')
        self.assertEqual(section_map['T'][0], '1, 2, 3, 4, 5')
        self.assertEqual(section_map['U'][0], '1, 2, 3, 4, 5')
        self.assertEqual(section_map['N'][0], 'The natural numbers')
        self.assertEqual(section_map['A'][0], 'N. J. A. Sloane')

    def test_segment_sections_with_noisy_output(self):
        """Test segment_sections with noisy output enabled."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%N A000001 The natural numbers"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = segment_sections(box, **box)
        
        # Assert
        self.assertIn('section_map', result)
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('segment_sections: Processing' in call for call in print_calls))
        self.assertTrue(any('Processing line' in call for call in print_calls))
        self.assertTrue(any('Matched:' in call for call in print_calls))
        self.assertTrue(any('Final section_map:' in call for call in print_calls))

    def test_segment_sections_multiple_sections_same_type(self):
        """Test segment_sections with multiple sections of the same type."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%I A000001 4, 5, 6
%N A000001 The natural numbers
%N A000001 First six natural numbers"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        section_map = result['section_map']
        self.assertEqual(len(section_map['I']), 1)  # Concatenated into one section
        self.assertEqual(len(section_map['N']), 1)  # Concatenated into one section
        self.assertEqual(section_map['I'][0], '1, 2, 3\n4, 5, 6')  # Concatenated with newline
        self.assertEqual(section_map['N'][0], 'The natural numbers\nFirst six natural numbers')  # Concatenated with newline

    def test_segment_sections_empty_lines(self):
        """Test segment_sections with empty lines."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3

%N A000001 The natural numbers

%A A000001 N. J. A. Sloane"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        section_map = result['section_map']
        self.assertIn('I', section_map)
        self.assertIn('N', section_map)
        self.assertIn('A', section_map)
        self.assertEqual(section_map['I'][0], '1, 2, 3')
        self.assertEqual(section_map['N'][0], 'The natural numbers')
        self.assertEqual(section_map['A'][0], 'N. J. A. Sloane')

    def test_segment_sections_invalid_format(self):
        """Test segment_sections with invalid format."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%Invalid A000001 This should fail
%N A000001 The natural numbers"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            segment_sections(box, **box)
        
        self.assertIn("doesn't match expected format", str(context.exception))
        self.assertIn("Line 2", str(context.exception))

    def test_segment_sections_invalid_a_number_format(self):
        """Test segment_sections with invalid A-number format."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%N A123 This should fail (not 6 digits)
%A A000001 N. J. A. Sloane"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            segment_sections(box, **box)
        
        self.assertIn("doesn't match expected format", str(context.exception))
        self.assertIn("Line 2", str(context.exception))

    def test_segment_sections_preserves_extra_keys(self):
        """Test that segment_sections preserves extra keys in the box."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%N A000001 The natural numbers"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertIn('section_map', result)

    def test_segment_sections_empty_data(self):
        """Test segment_sections with empty data."""
        # Arrange
        oeis_data = ""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        section_map = result['section_map']
        self.assertEqual(len(section_map), 0)

    def test_segment_sections_whitespace_only(self):
        """Test segment_sections with whitespace-only data."""
        # Arrange
        oeis_data = "   \n  \n  "
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        section_map = result['section_map']
        self.assertEqual(len(section_map), 0)

    def test_segment_sections_case_sensitive_types(self):
        """Test segment_sections with different case section types."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%i A000001 lowercase i
%N A000001 The natural numbers
%n A000001 lowercase n"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        section_map = result['section_map']
        self.assertIn('I', section_map)
        self.assertIn('i', section_map)
        self.assertIn('N', section_map)
        self.assertIn('n', section_map)
        self.assertEqual(section_map['I'][0], '1, 2, 3')
        self.assertEqual(section_map['i'][0], 'lowercase i')
        self.assertEqual(section_map['N'][0], 'The natural numbers')
        self.assertEqual(section_map['n'][0], 'lowercase n')

    def test_segment_sections_multiline_content(self):
        """Test segment_sections with multiline content in sections."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%I A000001 4, 5, 6
%I A000001 7, 8, 9
%N A000001 The natural numbers
%N A000001 First nine natural numbers"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': False
        }
        
        # Act
        result = segment_sections(box, **box)
        
        # Assert
        section_map = result['section_map']
        self.assertEqual(len(section_map['I']), 1)  # All I sections concatenated
        self.assertEqual(len(section_map['N']), 1)  # All N sections concatenated
        
        # Verify concatenated content
        i_content = section_map['I'][0]
        self.assertIn('1, 2, 3', i_content)
        self.assertIn('4, 5, 6', i_content)
        self.assertIn('7, 8, 9', i_content)
        self.assertEqual(i_content.count('\n'), 2)  # Two newlines between sections

    def test_segment_sections_noisy_output_details(self):
        """Test that noisy output shows correct details."""
        # Arrange
        oeis_data = """%I A000001 1, 2, 3
%N A000001 The natural numbers"""
        
        box = {
            'oeis_data': oeis_data,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = segment_sections(box, **box)
        
        # Assert
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        
        # Verify specific noisy output messages
        self.assertTrue(any('Processing' in call and 'characters' in call for call in print_calls))
        self.assertTrue(any('Processing line 1:' in call for call in print_calls))
        self.assertTrue(any('Processing line 2:' in call for call in print_calls))
        self.assertTrue(any("Matched: type='I'" in call for call in print_calls))
        self.assertTrue(any("Matched: type='N'" in call for call in print_calls))
        self.assertTrue(any('Final section_map:' in call for call in print_calls))


if __name__ == '__main__':
    unittest.main()
