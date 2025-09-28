#!/usr/bin/env python3
"""
Test suite for make_list fabricator.

Tests the core functionality of processing lists of OEIS sequence IDs.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.make_list import make_list


class TestMakeList(unittest.TestCase):
    """Test cases for make_list fabricator core function."""

    @patch('app.fab.make_list.make_one')
    def test_make_list_success(self, mock_make_one):
        """Test successful processing of ID list."""
        # Arrange
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        box = {
            'id_list': ['A000001', 'A000002'],
            'replace': False,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('list_results', result)
        
        list_results = result['list_results']
        self.assertEqual(list_results['total_requested'], 2)
        self.assertEqual(list_results['total_processed'], 2)
        self.assertEqual(list_results['total_preexisting'], 0)
        self.assertEqual(list_results['total_missing'], 0)
        self.assertEqual(list_results['total_errors'], 0)
        self.assertEqual(list_results['processed_ids'], ['A000001', 'A000002'])
        
        # Verify make_one was called for each ID
        self.assertEqual(mock_make_one.call_count, 2)

    @patch('app.fab.make_list.make_one')
    def test_make_list_with_noisy_output(self, mock_make_one):
        """Test make_list with noisy output enabled."""
        # Arrange
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = make_list(box, **box)
        
        # Assert
        self.assertIn('list_results', result)
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('make_list: Processing 1 IDs' in call for call in print_calls))
        self.assertTrue(any('make_list: Processing A000001' in call for call in print_calls))
        self.assertTrue(any('make_list: Successfully processed A000001' in call for call in print_calls))

    @patch('app.fab.make_list.make_one')
    def test_make_list_with_replace_flag(self, mock_make_one):
        """Test make_list with replace flag enabled."""
        # Arrange
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        box = {
            'id_list': ['A000001'],
            'replace': True,
            'noisy': False
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        # Verify make_one was called with replace=True
        mock_make_one.assert_called_once()
        call_kwargs = mock_make_one.call_args[1]
        self.assertTrue(call_kwargs['replace'])

    @patch('app.fab.make_list.make_one')
    def test_make_list_pipeline_error_with_file_error(self, mock_make_one):
        """Test make_list when make_one fails with file-related error."""
        # Arrange
        file_error = FileNotFoundError("File not found")
        pipeline_error = RuntimeError("Pipeline failed")
        pipeline_error.__cause__ = file_error
        mock_make_one.side_effect = pipeline_error
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        list_results = result['list_results']
        self.assertEqual(list_results['total_missing'], 1)
        self.assertEqual(list_results['total_errors'], 0)
        self.assertEqual(list_results['missing_ids'], ['A000001'])

    @patch('app.fab.make_list.make_one')
    def test_make_list_pipeline_error_with_real_error(self, mock_make_one):
        """Test make_list when make_one fails with real pipeline error."""
        # Arrange
        mock_make_one.side_effect = RuntimeError("Pipeline failed")
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        list_results = result['list_results']
        self.assertEqual(list_results['total_errors'], 1)
        self.assertEqual(list_results['total_missing'], 0)
        self.assertEqual(len(list_results['error_ids']), 1)
        self.assertEqual(list_results['error_ids'][0]['id'], 'A000001')
        self.assertIn('Pipeline failed', list_results['error_ids'][0]['error'])

    @patch('app.fab.make_list.make_one')
    def test_make_list_direct_file_error(self, mock_make_one):
        """Test make_list when make_one fails with direct file error."""
        # Arrange
        mock_make_one.side_effect = FileNotFoundError("File not found")
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        list_results = result['list_results']
        self.assertEqual(list_results['total_missing'], 1)
        self.assertEqual(list_results['total_errors'], 0)
        self.assertEqual(list_results['missing_ids'], ['A000001'])

    @patch('app.fab.make_list.make_one')
    def test_make_list_mixed_results(self, mock_make_one):
        """Test make_list with mixed success and failure results."""
        # Arrange
        def mock_make_one_side_effect(box, **kwargs):
            id_val = kwargs['id']
            if id_val == 'A000001':
                return {'id': id_val, 'processed': True}
            elif id_val == 'A000002':
                raise FileNotFoundError("File not found")
            else:  # A000003
                raise RuntimeError("Real error")
        
        mock_make_one.side_effect = mock_make_one_side_effect
        
        box = {
            'id_list': ['A000001', 'A000002', 'A000003'],
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        list_results = result['list_results']
        self.assertEqual(list_results['total_requested'], 3)
        self.assertEqual(list_results['total_processed'], 1)
        self.assertEqual(list_results['total_missing'], 1)
        self.assertEqual(list_results['total_errors'], 1)
        self.assertEqual(list_results['processed_ids'], ['A000001'])
        self.assertEqual(list_results['missing_ids'], ['A000002'])
        self.assertEqual(len(list_results['error_ids']), 1)
        self.assertEqual(list_results['error_ids'][0]['id'], 'A000003')

    @patch('app.fab.make_list.make_one')
    def test_make_list_empty_list(self, mock_make_one):
        """Test make_list with empty ID list."""
        # Arrange
        box = {
            'id_list': [],
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        list_results = result['list_results']
        self.assertEqual(list_results['total_requested'], 0)
        self.assertEqual(list_results['total_processed'], 0)
        self.assertEqual(list_results['total_preexisting'], 0)
        self.assertEqual(list_results['total_missing'], 0)
        self.assertEqual(list_results['total_errors'], 0)
        
        # Verify make_one was not called
        mock_make_one.assert_not_called()

    @patch('app.fab.make_list.make_one')
    def test_make_list_preserves_extra_keys(self, mock_make_one):
        """Test that make_list preserves extra keys in the box."""
        # Arrange
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertIn('list_results', result)

    @patch('app.fab.make_list.make_one')
    def test_make_list_passes_extra_keys_to_make_one(self, mock_make_one):
        """Test that make_list passes extra keys to make_one calls."""
        # Arrange
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = make_list(box, **box)
        
        # Assert
        # Verify make_one was called with extra keys
        mock_make_one.assert_called_once()
        call_kwargs = mock_make_one.call_args[1]
        self.assertEqual(call_kwargs['custom_key'], 'custom_value')
        self.assertEqual(call_kwargs['another_key'], 42)

    @patch('app.fab.make_list.make_one')
    def test_make_list_noisy_output_for_errors(self, mock_make_one):
        """Test that make_list prints appropriate messages for different error types."""
        # Arrange
        mock_make_one.side_effect = RuntimeError("Pipeline failed")
        
        box = {
            'id_list': ['A000001'],
            'replace': False,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = make_list(box, **box)
        
        # Assert
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('make_list: Error processing A000001' in call for call in print_calls))


if __name__ == '__main__':
    unittest.main()
