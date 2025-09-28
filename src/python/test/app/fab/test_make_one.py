#!/usr/bin/env python3
"""
Test suite for make_one fabricator.

Tests the core functionality of the main fabricator that processes IDs through a complete pipeline.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.make_one import make_one


class TestMakeOne(unittest.TestCase):
    """Test cases for make_one fabricator core function."""

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_success(self, mock_run_pipeline):
        """Test successful make_one execution."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000001',
            'noisy': False,
            'graph': MagicMock(),
            'oeis_data': 'Test data',
            'section_map': {'S': ['test']},
            'metadata': {'version': '1.0'},
            'turtle_output': '@prefix ex: <http://example.org/> .',
            'file_path': '/path/to/file.ttl'
        }
        
        box = {
            'id': 'a000001',  # Test with lowercase ID
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = make_one(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('graph', result)
        self.assertIn('oeis_data', result)
        self.assertIn('section_map', result)
        self.assertIn('metadata', result)
        self.assertIn('turtle_output', result)
        self.assertIn('file_path', result)
        
        # Verify run_pipeline was called with correct modules
        mock_run_pipeline.assert_called_once()
        call_args = mock_run_pipeline.call_args
        modules = call_args[0][0]
        initial_box = call_args[0][1]
        
        # Verify modules list
        self.assertEqual(len(modules), 9)
        self.assertEqual(modules[0].__name__, 'standardize_id')
        self.assertEqual(modules[1].__name__, 'init_graph')
        self.assertEqual(modules[2].__name__, 'init_sequence')
        self.assertEqual(modules[3].__name__, 'get_oeis_data')
        self.assertEqual(modules[4].__name__, 'segment_sections')
        self.assertEqual(modules[5].__name__, 'process_sections')
        self.assertEqual(modules[6].__name__, 'add_sections')
        self.assertEqual(modules[7].__name__, 'get_metadata')
        self.assertEqual(modules[8].__name__, 'dump_graph')
        
        # Verify initial box
        self.assertEqual(initial_box['id'], 'a000001')
        self.assertEqual(initial_box['noisy'], False)
        self.assertEqual(initial_box['extra_key'], 'extra_value')

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_with_noisy_output(self, mock_run_pipeline):
        """Test make_one execution with noisy output enabled."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000001',
            'noisy': True,
            'graph': MagicMock(),
            'oeis_data': 'Test data',
            'section_map': {'S': ['test']},
            'metadata': {'version': '1.0'},
            'turtle_output': '@prefix ex: <http://example.org/> .',
            'file_path': '/path/to/file.ttl'
        }
        
        box = {
            'id': 'A000001',
            'noisy': True
        }
        
        # Act
        result = make_one(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], True)
        
        # Verify run_pipeline was called with noisy=True
        call_args = mock_run_pipeline.call_args
        initial_box = call_args[0][1]
        self.assertEqual(initial_box['noisy'], True)

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_pipeline_failure(self, mock_run_pipeline):
        """Test make_one when pipeline execution fails."""
        # Arrange
        mock_run_pipeline.side_effect = Exception("Pipeline failed")
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(Exception) as context:
            make_one(box, **box)
        
        self.assertEqual(str(context.exception), "Pipeline failed")

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_preserves_extra_keys(self, mock_run_pipeline):
        """Test that make_one preserves extra keys in the box."""
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
        result = make_one(box, **box)
        
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

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_different_id(self, mock_run_pipeline):
        """Test make_one with different ID."""
        # Arrange
        mock_run_pipeline.return_value = {
            'id': 'A000123',
            'noisy': False,
            'graph': MagicMock(),
            'oeis_data': 'Test data',
            'section_map': {'S': ['test']},
            'metadata': {'version': '1.0'},
            'turtle_output': '@prefix ex: <http://example.org/> .',
            'file_path': '/path/to/file.ttl'
        }
        
        box = {
            'id': 'a000123',  # Test with lowercase
            'noisy': False
        }
        
        # Act
        result = make_one(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        
        # Verify run_pipeline was called with correct ID
        call_args = mock_run_pipeline.call_args
        initial_box = call_args[0][1]
        self.assertEqual(initial_box['id'], 'a000123')

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_returns_pipeline_result(self, mock_run_pipeline):
        """Test that make_one returns the exact result from run_pipeline."""
        # Arrange
        expected_result = {
            'id': 'A000001',
            'noisy': False,
            'graph': MagicMock(),
            'oeis_data': 'Test OEIS data content',
            'section_map': {'S': ['section1'], 'T': ['section2']},
            'metadata': {'version': '1.0', 'triples': 5},
            'turtle_output': '@prefix ex: <http://example.org/> .\nex:test a ex:Test .',
            'file_path': '/path/to/A000001.ttl',
            'pipeline_data': 'some_data',
            'complex_key': {'nested': 'value'}
        }
        mock_run_pipeline.return_value = expected_result
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = make_one(box, **box)
        
        # Assert
        self.assertEqual(result, expected_result)
        self.assertEqual(result['oeis_data'], 'Test OEIS data content')
        self.assertEqual(result['section_map'], {'S': ['section1'], 'T': ['section2']})
        self.assertEqual(result['metadata'], {'version': '1.0', 'triples': 5})
        self.assertEqual(result['turtle_output'], '@prefix ex: <http://example.org/> .\nex:test a ex:Test .')
        self.assertEqual(result['file_path'], '/path/to/A000001.ttl')
        self.assertEqual(result['pipeline_data'], 'some_data')
        self.assertEqual(result['complex_key'], {'nested': 'value'})

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_pipeline_modules_order(self, mock_run_pipeline):
        """Test that make_one defines modules in the correct order."""
        # Arrange
        mock_run_pipeline.return_value = {'id': 'A000001', 'noisy': False}
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = make_one(box, **box)
        
        # Assert
        # Verify run_pipeline was called with modules in correct order
        mock_run_pipeline.assert_called_once()
        call_args = mock_run_pipeline.call_args
        modules = call_args[0][0]
        
        # Verify the exact order of modules
        expected_order = [
            'standardize_id',
            'init_graph', 
            'init_sequence',
            'get_oeis_data',
            'segment_sections',
            'process_sections',
            'add_sections',
            'get_metadata',
            'dump_graph'
        ]
        
        actual_order = [module.__name__ for module in modules]
        self.assertEqual(actual_order, expected_order)

    @patch('app.fab.make_one.run_pipeline')
    def test_make_one_empty_extra_keys(self, mock_run_pipeline):
        """Test make_one with no extra keys."""
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
        result = make_one(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['noisy'], False)
        
        # Verify run_pipeline was called with minimal box
        call_args = mock_run_pipeline.call_args
        initial_box = call_args[0][1]
        self.assertEqual(len(initial_box), 2)  # Only id and noisy
        self.assertEqual(initial_box['id'], 'A000001')
        self.assertEqual(initial_box['noisy'], False)


if __name__ == '__main__':
    unittest.main()
