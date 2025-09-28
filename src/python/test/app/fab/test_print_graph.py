#!/usr/bin/env python3
"""
Test suite for print_graph module.

Tests the core functionality of serializing RDFLib Graphs to RDF/Turtle format.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.print_graph import dump_graph


class TestPrintGraph(unittest.TestCase):
    """Test cases for print_graph core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_success(self, mock_graph_class):
        """Test successful graph serialization and printing."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        box = {
            'graph': self.mock_graph,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('turtle_output', result)
        self.assertEqual(result['turtle_output'], "@prefix ex: <http://example.org/> .\nex:test a ex:Test .")
        
        # Verify graph serialization
        self.mock_graph.serialize.assert_called_once_with(format='turtle')
        
        # Verify output was printed (even in silent mode)
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertEqual(print_args, "@prefix ex: <http://example.org/> .\nex:test a ex:Test .")

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_with_noisy_output(self, mock_graph_class):
        """Test print_graph with noisy output enabled."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        box = {
            'graph': self.mock_graph,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        self.assertIn('turtle_output', result)
        
        # Verify noisy output was printed
        self.assertEqual(mock_print.call_count, 3)  # Header, content, footer
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('print_graph: RDF/Turtle output:' in call for call in print_calls))
        self.assertTrue(any('=' * 60 in call for call in print_calls))
        self.assertTrue(any('@prefix ex: <http://example.org/> .' in call for call in print_calls))

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_with_metadata(self, mock_graph_class):
        """Test print_graph with metadata comment."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        metadata = {"version": "1.0", "timestamp": "2025-01-21"}
        box = {
            'graph': self.mock_graph,
            'metadata': metadata,
            'noisy': False
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        self.assertIn('turtle_output', result)
        
        # Verify output includes metadata comment
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertIn('{"version": "1.0", "timestamp": "2025-01-21"}', print_args)
        self.assertTrue(print_args.startswith('# '))

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_without_metadata(self, mock_graph_class):
        """Test print_graph without metadata."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        box = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        self.assertIn('turtle_output', result)
        
        # Verify output does not include metadata comment
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertFalse(print_args.startswith('# '))
        self.assertEqual(print_args, "@prefix ex: <http://example.org/> .\nex:test a ex:Test .")

    def test_print_graph_invalid_graph_type(self):
        """Test print_graph with invalid graph type."""
        # Arrange
        box = {
            'graph': "not a graph",  # Invalid graph type
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            dump_graph(box, **box)
        
        self.assertIn("Invalid graph type", str(context.exception))
        self.assertIn("expected RDFLib Graph", str(context.exception))

    @patch('app.fab.print_graph.Graph', None)
    def test_print_graph_rdflib_not_available(self):
        """Test print_graph when RDFLib is not available."""
        # Arrange
        box = {
            'graph': MagicMock(),
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ImportError) as context:
            dump_graph(box, **box)
        
        self.assertIn("RDFLib not available", str(context.exception))

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_preserves_extra_keys(self, mock_graph_class):
        """Test that print_graph preserves extra keys in the box."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        box = {
            'graph': self.mock_graph,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        with patch('builtins.print'):
            result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertIn('turtle_output', result)

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_metadata_json_serialization(self, mock_graph_class):
        """Test that metadata is properly JSON serialized."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        metadata = {
            "version": "1.0",
            "timestamp": "2025-01-21T12:00:00Z",
            "complex": {"nested": "value", "list": [1, 2, 3]}
        }
        box = {
            'graph': self.mock_graph,
            'metadata': metadata,
            'noisy': False
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        
        # Verify metadata is properly JSON serialized
        self.assertTrue(print_args.startswith('# '))
        metadata_line = print_args[2:]  # Remove '# ' prefix
        self.assertIn('"version": "1.0"', metadata_line)
        self.assertIn('"timestamp": "2025-01-21T12:00:00Z"', metadata_line)
        self.assertIn('"complex": {"nested": "value", "list": [1, 2, 3]}', metadata_line)

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_empty_metadata(self, mock_graph_class):
        """Test print_graph with empty metadata."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        box = {
            'graph': self.mock_graph,
            'metadata': {},
            'noisy': False
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertTrue(print_args.startswith('# {}'))

    @patch('app.fab.print_graph.Graph')
    def test_print_graph_noisy_vs_silent_output(self, mock_graph_class):
        """Test difference between noisy and silent output modes."""
        # Arrange
        mock_graph_class.return_value = self.mock_graph
        
        # Test noisy mode
        box_noisy = {
            'graph': self.mock_graph,
            'noisy': True
        }
        
        with patch('builtins.print') as mock_print_noisy:
            dump_graph(box_noisy, **box_noisy)
        
        noisy_calls = len(mock_print_noisy.call_args_list)
        
        # Test silent mode
        box_silent = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        with patch('builtins.print') as mock_print_silent:
            dump_graph(box_silent, **box_silent)
        
        silent_calls = len(mock_print_silent.call_args_list)
        
        # Assert
        self.assertGreater(noisy_calls, silent_calls)
        self.assertEqual(silent_calls, 1)  # Only the turtle output
        self.assertEqual(noisy_calls, 3)   # Header, content, footer


if __name__ == '__main__':
    unittest.main()
