#!/usr/bin/env python3
"""
Test suite for process_sections module.

Tests the core functionality of processing section maps by concatenating S, T, U sections into V.
"""

import unittest
import sys
import os
from unittest.mock import patch

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.process_sections import process_sections


class TestProcessSections(unittest.TestCase):
    """Test cases for process_sections core function."""

    def test_process_sections_success(self):
        """Test successful section processing."""
        # Arrange
        section_map = {
            'S': ['section1', 'section2'],
            'T': ['section3'],
            'U': ['section4', 'section5'],
            'I': ['info1'],
            'A': ['author1']
        }
        
        box = {
            'section_map': section_map,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('section_map', result)
        
        new_section_map = result['section_map']
        self.assertNotIn('S', new_section_map)
        self.assertNotIn('T', new_section_map)
        self.assertNotIn('U', new_section_map)
        self.assertIn('V', new_section_map)
        self.assertIn('I', new_section_map)
        self.assertIn('A', new_section_map)
        
        # Verify V section content
        v_content = new_section_map['V'][0]
        self.assertEqual(v_content, 'section1section2section3section4section5')

    def test_process_sections_with_noisy_output(self):
        """Test process_sections with noisy output enabled."""
        # Arrange
        section_map = {
            'S': ['section1'],
            'T': ['section2'],
            'U': ['section3']
        }
        
        box = {
            'section_map': section_map,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = process_sections(box, **box)
        
        # Assert
        self.assertIn('section_map', result)
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('process_sections: Processing section_map' in call for call in print_calls))
        self.assertTrue(any('process_sections: Concatenated' in call for call in print_calls))
        self.assertTrue(any('process_sections: V section has' in call for call in print_calls))
        self.assertTrue(any('process_sections: New section_map has' in call for call in print_calls))

    def test_process_sections_empty_sections(self):
        """Test process_sections with empty S, T, U sections."""
        # Arrange
        section_map = {
            'S': [],
            'T': [],
            'U': [],
            'I': ['info1']
        }
        
        box = {
            'section_map': section_map,
            'noisy': False
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        new_section_map = result['section_map']
        self.assertNotIn('S', new_section_map)
        self.assertNotIn('T', new_section_map)
        self.assertNotIn('U', new_section_map)
        self.assertIn('V', new_section_map)
        self.assertIn('I', new_section_map)
        
        # Verify V section is empty
        v_content = new_section_map['V'][0]
        self.assertEqual(v_content, '')

    def test_process_sections_missing_sections(self):
        """Test process_sections when S, T, U sections are missing."""
        # Arrange
        section_map = {
            'I': ['info1'],
            'A': ['author1']
        }
        
        box = {
            'section_map': section_map,
            'noisy': False
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        new_section_map = result['section_map']
        self.assertNotIn('S', new_section_map)
        self.assertNotIn('T', new_section_map)
        self.assertNotIn('U', new_section_map)
        self.assertIn('V', new_section_map)
        self.assertIn('I', new_section_map)
        self.assertIn('A', new_section_map)
        
        # Verify V section is empty
        v_content = new_section_map['V'][0]
        self.assertEqual(v_content, '')

    def test_process_sections_preserves_other_sections(self):
        """Test that process_sections preserves other section types."""
        # Arrange
        section_map = {
            'S': ['section1'],
            'T': ['section2'],
            'U': ['section3'],
            'I': ['info1', 'info2'],
            'A': ['author1'],
            'N': ['note1'],
            'X': ['extra1']
        }
        
        box = {
            'section_map': section_map,
            'noisy': False
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        new_section_map = result['section_map']
        
        # Verify S, T, U are removed
        self.assertNotIn('S', new_section_map)
        self.assertNotIn('T', new_section_map)
        self.assertNotIn('U', new_section_map)
        
        # Verify V is added
        self.assertIn('V', new_section_map)
        self.assertEqual(new_section_map['V'], ['section1section2section3'])
        
        # Verify other sections are preserved
        self.assertEqual(new_section_map['I'], ['info1', 'info2'])
        self.assertEqual(new_section_map['A'], ['author1'])
        self.assertEqual(new_section_map['N'], ['note1'])
        self.assertEqual(new_section_map['X'], ['extra1'])

    def test_process_sections_preserves_extra_keys(self):
        """Test that process_sections preserves extra keys in the box."""
        # Arrange
        section_map = {
            'S': ['section1'],
            'T': ['section2'],
            'U': ['section3']
        }
        
        box = {
            'section_map': section_map,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertIn('section_map', result)

    def test_process_sections_concatenation_order(self):
        """Test that sections are concatenated in S, T, U order."""
        # Arrange
        section_map = {
            'S': ['S1', 'S2'],
            'T': ['T1'],
            'U': ['U1', 'U2', 'U3']
        }
        
        box = {
            'section_map': section_map,
            'noisy': False
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        new_section_map = result['section_map']
        v_content = new_section_map['V'][0]
        self.assertEqual(v_content, 'S1S2T1U1U2U3')

    def test_process_sections_empty_section_map(self):
        """Test process_sections with empty section map."""
        # Arrange
        section_map = {}
        
        box = {
            'section_map': section_map,
            'noisy': False
        }
        
        # Act
        result = process_sections(box, **box)
        
        # Assert
        new_section_map = result['section_map']
        self.assertNotIn('S', new_section_map)
        self.assertNotIn('T', new_section_map)
        self.assertNotIn('U', new_section_map)
        self.assertIn('V', new_section_map)
        self.assertEqual(new_section_map['V'], [''])

    def test_process_sections_single_section_types(self):
        """Test process_sections with only one of S, T, U present."""
        test_cases = [
            ({'S': ['section1']}, 'section1'),
            ({'T': ['section2']}, 'section2'),
            ({'U': ['section3']}, 'section3'),
            ({'S': ['section1'], 'T': ['section2']}, 'section1section2'),
            ({'T': ['section2'], 'U': ['section3']}, 'section2section3'),
            ({'S': ['section1'], 'U': ['section3']}, 'section1section3')
        ]
        
        for section_map, expected_v in test_cases:
            with self.subTest(section_map=section_map):
                # Arrange
                box = {
                    'section_map': section_map,
                    'noisy': False
                }
                
                # Act
                result = process_sections(box, **box)
                
                # Assert
                new_section_map = result['section_map']
                v_content = new_section_map['V'][0]
                self.assertEqual(v_content, expected_v)

    def test_process_sections_noisy_output_details(self):
        """Test that noisy output shows correct counts and details."""
        # Arrange
        section_map = {
            'S': ['section1', 'section2'],
            'T': ['section3'],
            'U': ['section4', 'section5', 'section6'],
            'I': ['info1']
        }
        
        box = {
            'section_map': section_map,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = process_sections(box, **box)
        
        # Assert
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        
        # Verify specific noisy output messages
        self.assertTrue(any('Processing section_map with 4 section types' in call for call in print_calls))
        self.assertTrue(any('Concatenated 2 S + 1 T + 3 U sections' in call for call in print_calls))
        self.assertTrue(any('V section has' in call for call in print_calls))
        self.assertTrue(any('New section_map has 2 section types' in call for call in print_calls))


if __name__ == '__main__':
    unittest.main()
