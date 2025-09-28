#!/usr/bin/env python3
"""
Test suite for get_oeis_data module.

Tests the core functionality of reading OEIS data files.
"""

import unittest
import sys
import os
import tempfile
import shutil
from unittest.mock import patch, MagicMock, mock_open
from pathlib import Path

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.get_oeis_data import get_oeis_data


class TestGetOeisData(unittest.TestCase):
    """Test cases for get_oeis_data core function."""

    def setUp(self):
        """Set up test fixtures."""
        # Create a temporary directory for testing
        self.temp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.temp_dir)

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="Test OEIS data content")
    def test_get_oeis_data_success(self, mock_file, mock_seqvar_get):
        """Test successful OEIS data reading."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = get_oeis_data(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('oeis_data', result)
        self.assertEqual(result['oeis_data'], "Test OEIS data content")
        
        # Verify file was opened with correct path
        expected_path = Path(self.temp_dir) / "seq" / "A000" / "app.fab.A000001.seq"
        mock_file.assert_called_once_with(expected_path, 'r', encoding='utf-8')

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="Test OEIS data content")
    def test_get_oeis_data_with_noisy_output(self, mock_file, mock_seqvar_get):
        """Test get_oeis_data with noisy output enabled."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = get_oeis_data(box, **box)
        
        # Assert
        self.assertIn('oeis_data', result)
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertIn('get_oeis_data: Read', print_args)
        self.assertIn('characters from', print_args)

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="Test OEIS data content")
    def test_get_oeis_data_different_id(self, mock_file, mock_seqvar_get):
        """Test get_oeis_data with different ID."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000123',
            'noisy': False
        }
        
        # Act
        result = get_oeis_data(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        self.assertIn('oeis_data', result)
        
        # Verify file was opened with correct path for different ID
        expected_path = Path(self.temp_dir) / "seq" / "A000" / "app.fab.A000123.seq"
        mock_file.assert_called_once_with(expected_path, 'r', encoding='utf-8')

    @patch('app.fab.get_oeis_data.get')
    def test_get_oeis_data_seqvar_not_available(self, mock_seqvar_get):
        """Test get_oeis_data when seqvar system is not available."""
        # Arrange
        mock_seqvar_get.side_effect = ImportError("seqvar not available")
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ImportError) as context:
            get_oeis_data(box, **box)
        
        self.assertIn("seqvar system not available", str(context.exception))

    @patch('app.fab.get_oeis_data.get')
    def test_get_oeis_data_seqvar_not_set(self, mock_seqvar_get):
        """Test get_oeis_data when seqvar path is not set."""
        # Arrange
        mock_seqvar_get.return_value = None  # Path not set
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(RuntimeError) as context:
            get_oeis_data(box, **box)
        
        self.assertIn("seqvar 'app.fab.repos.oeisdata' is not set", str(context.exception))

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', side_effect=FileNotFoundError("File not found"))
    def test_get_oeis_data_file_not_found(self, mock_file, mock_seqvar_get):
        """Test get_oeis_data when file doesn't exist."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(FileNotFoundError) as context:
            get_oeis_data(box, **box)
        
        self.assertIn("OEIS data file not found", str(context.exception))

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', side_effect=IOError("Permission denied"))
    def test_get_oeis_data_io_error(self, mock_file, mock_seqvar_get):
        """Test get_oeis_data when file cannot be read."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(IOError) as context:
            get_oeis_data(box, **box)
        
        self.assertIn("Cannot read OEIS data file", str(context.exception))

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="Test OEIS data content")
    def test_get_oeis_data_preserves_extra_keys(self, mock_file, mock_seqvar_get):
        """Test that get_oeis_data preserves extra keys in the box."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = get_oeis_data(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertIn('oeis_data', result)

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="Test OEIS data content")
    def test_get_oeis_data_file_path_construction(self, mock_file, mock_seqvar_get):
        """Test that file path is constructed correctly."""
        # Arrange
        mock_seqvar_get.return_value = "/test/oeisdata"
        
        box = {
            'id': 'A000456',  # Test with different ID
            'noisy': False
        }
        
        # Act
        result = get_oeis_data(box, **box)
        
        # Assert
        self.assertIn('oeis_data', result)
        
        # Verify file was opened with correct path
        expected_path = Path("/test/oeisdata") / "seq" / "A000" / "app.fab.A000456.seq"
        mock_file.assert_called_once_with(expected_path, 'r', encoding='utf-8')

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="")
    def test_get_oeis_data_empty_file(self, mock_file, mock_seqvar_get):
        """Test get_oeis_data with empty file."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = get_oeis_data(box, **box)
        
        # Assert
        self.assertEqual(result['oeis_data'], "")

    @patch('app.fab.get_oeis_data.get')
    @patch('builtins.open', new_callable=mock_open, read_data="Line 1\nLine 2\nLine 3")
    def test_get_oeis_data_multiline_content(self, mock_file, mock_seqvar_get):
        """Test get_oeis_data with multiline content."""
        # Arrange
        mock_seqvar_get.return_value = self.temp_dir
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = get_oeis_data(box, **box)
        
        # Assert
        self.assertEqual(result['oeis_data'], "Line 1\nLine 2\nLine 3")


if __name__ == '__main__':
    unittest.main()
