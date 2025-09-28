#!/usr/bin/env python3
"""
Test suite for fab0 fabricator.

Tests the core functionality of the first SeqWeb fabricator that runs a simple pipeline.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.fab0 import fab0


class TestFab0(unittest.TestCase):
    """Test cases for fab0 fabricator core function."""

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_success(self, mock_run_pipeline):
        """Test successful fab0 execution."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000001',
            'noisy': False,
            'processed': True
        }
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = fab0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        self.assertEqual(result['processed'], True)
        self.assertEqual(result['extra_key'], 'extra_value')
        
        # Verify run_pipeline was called with correct arguments
        mock_run_pipeline.assert_called_once()
        call_args = mock_run_pipeline.call_args
        self.assertEqual(call_args[0][0], [MagicMock])  # modules list
        self.assertEqual(call_args[0][1]['id'], 'A000001')
        self.assertEqual(call_args[0][1]['noisy'], False)
        self.assertEqual(call_args[0][1]['extra_key'], 'extra_value')

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_with_noisy_output(self, mock_run_pipeline):
        """Test fab0 execution with noisy output enabled."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000001',
            'noisy': True,
            'processed': True
        }
        
        box = {
            'id': 'A000001',
            'noisy': True
        }
        
        # Act
        result = fab0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], True)
        self.assertEqual(result['processed'], True)
        
        # Verify run_pipeline was called with noisy=True
        call_args = mock_run_pipeline.call_args
        self.assertEqual(call_args[0][1]['noisy'], True)

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_preserves_extra_keys(self, mock_run_pipeline):
        """Test that fab0 preserves extra keys in the box."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42,
            'third_key': 'third_value'
        }
        
        # Act
        result = fab0(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['third_key'], 'third_value')
        
        # Verify run_pipeline was called with all extra keys
        call_args = mock_run_pipeline.call_args
        initial_box = call_args[0][1]
        self.assertEqual(initial_box['custom_key'], 'custom_value')
        self.assertEqual(initial_box['another_key'], 42)
        self.assertEqual(initial_box['third_key'], 'third_value')

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_pipeline_failure(self, mock_run_pipeline):
        """Test fab0 when pipeline execution fails."""
        # Arrange
        mock_run_pipeline.side_effect = Exception("Pipeline failed")
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(Exception) as context:
            fab0(box, **box)
        
        self.assertEqual(str(context.exception), "Pipeline failed")

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_different_id(self, mock_run_pipeline):
        """Test fab0 with different ID."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000123',
            'noisy': False,
            'processed': True
        }
        
        box = {
            'id': 'A000123',
            'noisy': False
        }
        
        # Act
        result = fab0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        
        # Verify run_pipeline was called with correct ID
        call_args = mock_run_pipeline.call_args
        self.assertEqual(call_args[0][1]['id'], 'A000123')

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_empty_extra_keys(self, mock_run_pipeline):
        """Test fab0 with no extra keys."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000001',
            'noisy': False
        }
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = fab0(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        
        # Verify run_pipeline was called with minimal box
        call_args = mock_run_pipeline.call_args
        initial_box = call_args[0][1]
        self.assertEqual(len(initial_box), 2)  # Only id and noisy
        self.assertEqual(initial_box['id'], 'A000001')
        self.assertEqual(initial_box['noisy'], False)

    @patch('app.fab.fab0.run_pipeline')
    def test_fab0_returns_pipeline_result(self, mock_run_pipeline):
        """Test that fab0 returns the exact result from run_pipeline."""
        # Arrange
        expected_result = {
            'id': 'A000001',
            'noisy': False,
            'pipeline_data': 'some_data',
            'complex_key': {'nested': 'value'}
        }
        mock_run_pipeline.return_value = expected_result
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = fab0(box, **box)
        
        # Assert
        self.assertEqual(result, expected_result)
        self.assertEqual(result['pipeline_data'], 'some_data')
        self.assertEqual(result['complex_key'], {'nested': 'value'})


if __name__ == '__main__':
    unittest.main()
