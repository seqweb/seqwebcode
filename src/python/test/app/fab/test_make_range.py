#!/usr/bin/env python3
"""
Test suite for make_range fabricator.

Tests the core functionality of processing ranges of OEIS sequence IDs.
"""

import unittest
import sys
import os
import tempfile
import shutil
from unittest.mock import patch, MagicMock
from pathlib import Path

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.make_range import make_range


class TestMakeRange(unittest.TestCase):
    """Test cases for make_range fabricator core function."""

    def setUp(self):
        """Set up test fixtures."""
        # Create temporary directories for testing
        self.temp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.temp_dir)

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_success(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test successful processing of ID range."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Create mock source files
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        (source_dir / "app.fab.A000002.seq").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000002',
            'replace': False,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('range_results', result)
        
        range_results = result['range_results']
        self.assertEqual(range_results['total_requested'], 2)
        self.assertEqual(range_results['total_processed'], 2)
        self.assertEqual(range_results['total_preexisting'], 0)
        self.assertEqual(range_results['total_missing'], 0)
        self.assertEqual(range_results['total_errors'], 0)
        self.assertEqual(range_results['processed_ids'], ['A000001', 'A000002'])
        
        # Verify make_one was called for each ID
        self.assertEqual(mock_make_one.call_count, 2)

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_with_noisy_output(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test make_range with noisy output enabled."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Create mock source file
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000001',
            'replace': False,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = make_range(box, **box)
        
        # Assert
        self.assertIn('range_results', result)
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('make_range: Processing range' in call for call in print_calls))
        self.assertTrue(any('make_range: Generated' in call for call in print_calls))
        self.assertTrue(any('make_range: Processing A000001' in call for call in print_calls))

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_with_replace_flag(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test make_range with replace flag enabled."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Create mock source file
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000001',
            'replace': True,
            'noisy': False
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        # Verify make_one was called with replace=True
        mock_make_one.assert_called_once()
        call_kwargs = mock_make_one.call_args[1]
        self.assertTrue(call_kwargs['replace'])

    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_invalid_id_format(self, mock_get_standard_id):
        """Test make_range with invalid ID format."""
        # Arrange
        mock_get_standard_id.side_effect = ValueError("Invalid ID format")
        
        box = {
            'start_id': 'invalid',
            'end_id': 'A000001',
            'replace': False,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            make_range(box, **box)
        
        self.assertIn("Invalid ID format", str(context.exception))

    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_start_greater_than_end(self, mock_get_standard_id):
        """Test make_range when start ID is greater than end ID."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        
        box = {
            'start_id': 'A000010',
            'end_id': 'A000001',
            'replace': False,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            make_range(box, **box)
        
        self.assertIn("Start ID A000010 must be <= end ID A000001", str(context.exception))

    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_seqvar_failure(self, mock_get_standard_id, mock_seqvar_get):
        """Test make_range when seqvar system fails."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = Exception("seqvar error")
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000001',
            'replace': False,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(RuntimeError) as context:
            make_range(box, **box)
        
        self.assertIn("Could not get data paths", str(context.exception))

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_missing_source_files(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test make_range when source files are missing."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Don't create any source files
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000002',
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        range_results = result['range_results']
        self.assertEqual(range_results['total_missing'], 2)
        self.assertEqual(range_results['total_processed'], 0)
        self.assertEqual(range_results['missing_ids'], ['A000001', 'A000002'])
        
        # Verify make_one was not called
        mock_make_one.assert_not_called()

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_preexisting_files(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test make_range when output files already exist."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Create mock source files
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        (source_dir / "app.fab.A000002.seq").touch()
        
        # Create mock output files (preexisting)
        output_dir = Path(self.temp_dir) / "seq" / "A000"
        (output_dir / "app.fab.A000001.ttl").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000002',
            'replace': False,  # Don't replace existing files
            'noisy': False
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        range_results = result['range_results']
        self.assertEqual(range_results['total_preexisting'], 1)
        self.assertEqual(range_results['total_processed'], 1)
        self.assertEqual(range_results['preexisting_ids'], ['A000001'])
        self.assertEqual(range_results['processed_ids'], ['A000002'])

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_make_one_error(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test make_range when make_one fails."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.side_effect = Exception("make_one error")
        
        # Create mock source file
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000001',
            'replace': False,
            'noisy': False
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        range_results = result['range_results']
        self.assertEqual(range_results['total_errors'], 1)
        self.assertEqual(range_results['total_processed'], 0)
        self.assertEqual(len(range_results['error_ids']), 1)
        self.assertEqual(range_results['error_ids'][0]['id'], 'A000001')
        self.assertIn('make_one error', range_results['error_ids'][0]['error'])

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_preserves_extra_keys(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test that make_range preserves extra keys in the box."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Create mock source file
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000001',
            'replace': False,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertIn('range_results', result)

    @patch('app.fab.make_range.make_one')
    @patch('app.fab.make_range.seqvar_get')
    @patch('app.fab.make_range.get_standard_id')
    def test_make_range_passes_extra_keys_to_make_one(self, mock_get_standard_id, mock_seqvar_get, mock_make_one):
        """Test that make_range passes extra keys to make_one calls."""
        # Arrange
        mock_get_standard_id.side_effect = lambda x: f"A{x[1:].zfill(6)}" if x.startswith('A') else f"A{x.zfill(6)}"
        mock_seqvar_get.side_effect = lambda key: self.temp_dir
        mock_make_one.return_value = {'id': 'A000001', 'processed': True}
        
        # Create mock source file
        source_dir = Path(self.temp_dir) / "seq" / "A000"
        source_dir.mkdir(parents=True)
        (source_dir / "app.fab.A000001.seq").touch()
        
        box = {
            'start_id': 'A000001',
            'end_id': 'A000001',
            'replace': False,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = make_range(box, **box)
        
        # Assert
        # Verify make_one was called with extra keys
        mock_make_one.assert_called_once()
        call_kwargs = mock_make_one.call_args[1]
        self.assertEqual(call_kwargs['custom_key'], 'custom_value')
        self.assertEqual(call_kwargs['another_key'], 42)


if __name__ == '__main__':
    unittest.main()
