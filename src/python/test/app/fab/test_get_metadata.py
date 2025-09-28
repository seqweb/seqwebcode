#!/usr/bin/env python3
"""
Test suite for get_metadata module.

Tests the core functionality of extracting metadata from RDF graphs.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock
from datetime import datetime

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.get_metadata import get_metadata


class TestGetMetadata(unittest.TestCase):
    """Test cases for get_metadata core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_graph.__len__.return_value = 3
        
        # Mock RDF components
        self.mock_literal = MagicMock()
        self.mock_rdf = MagicMock()
        self.mock_rdf.type = "rdf:type"

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_success(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test successful metadata extraction."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock graph iteration
        mock_triple1 = (MagicMock(), mock_rdf.type, MagicMock())
        mock_triple2 = (MagicMock(), MagicMock(), self.mock_literal)
        mock_triple3 = (MagicMock(), MagicMock(), MagicMock())
        self.mock_graph.__iter__.return_value = [mock_triple1, mock_triple2, mock_triple3]
        
        # Mock literal string conversion
        self.mock_literal.__str__ = MagicMock(return_value="test literal")
        
        box = {
            'graph': self.mock_graph,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('metadata', result)
        
        metadata = result['metadata']
        self.assertEqual(metadata['metadata'], 'app.fab.v1.0')
        self.assertEqual(metadata['at'], '2025-01-21T12:00:00Z')
        self.assertEqual(metadata['by'], 'app.fab.test.module>function')
        self.assertEqual(metadata['triples'], 3)
        self.assertIn('entities', metadata)
        self.assertIn('chars', metadata)

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_with_noisy_output(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test get_metadata with noisy output enabled."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock graph iteration
        mock_triple = (MagicMock(), MagicMock(), MagicMock())
        self.mock_graph.__iter__.return_value = [mock_triple]
        
        box = {
            'graph': self.mock_graph,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = get_metadata(box, **box)
        
        # Assert
        self.assertIn('metadata', result)
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('get_metadata: Analyzing graph' in call for call in print_calls))
        self.assertTrue(any('get_metadata: Generated metadata' in call for call in print_calls))

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_call_trace_failure(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test get_metadata when call trace extraction fails."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.side_effect = Exception("Call trace error")
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock graph iteration
        mock_triple = (MagicMock(), MagicMock(), MagicMock())
        self.mock_graph.__iter__.return_value = [mock_triple]
        
        box = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        metadata = result['metadata']
        self.assertEqual(metadata['by'], 'unknown')

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_graph_analysis_failure(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test get_metadata when graph analysis fails."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock graph iteration to raise exception
        self.mock_graph.__iter__.side_effect = Exception("Graph analysis error")
        
        box = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        metadata = result['metadata']
        self.assertEqual(metadata['chars'], 0)  # Should default to 0 on error

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_entity_counting(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test that entities are counted correctly (subjects with rdf:type predicates)."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Create mock subjects
        subject1 = MagicMock()
        subject2 = MagicMock()
        subject3 = MagicMock()
        
        # Mock graph iteration with rdf:type predicates
        mock_triple1 = (subject1, mock_rdf.type, MagicMock())  # Should count as entity
        mock_triple2 = (subject2, MagicMock(), MagicMock())    # Should not count
        mock_triple3 = (subject3, mock_rdf.type, MagicMock())  # Should count as entity
        self.mock_graph.__iter__.return_value = [mock_triple1, mock_triple2, mock_triple3]
        
        box = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        metadata = result['metadata']
        self.assertEqual(metadata['entities'], 2)  # Only subjects with rdf:type

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_character_counting(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test that string literal characters are counted correctly."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        
        # Create mock literals with different string values
        literal1 = MagicMock()
        literal1.__str__ = MagicMock(return_value="hello")
        literal2 = MagicMock()
        literal2.__str__ = MagicMock(return_value="world")
        
        mock_literal.side_effect = lambda *args: literal1 if args[0] == "hello" else literal2
        
        # Mock graph iteration with literals
        mock_triple1 = (MagicMock(), MagicMock(), literal1)
        mock_triple2 = (MagicMock(), MagicMock(), literal2)
        mock_triple3 = (MagicMock(), MagicMock(), MagicMock())  # Non-literal
        self.mock_graph.__iter__.return_value = [mock_triple1, mock_triple2, mock_triple3]
        
        box = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        metadata = result['metadata']
        # "hello" = 5 bytes, "world" = 5 bytes = 10 total
        self.assertEqual(metadata['chars'], 10)

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_preserves_extra_keys(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test that get_metadata preserves extra keys in the box."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock graph iteration
        mock_triple = (MagicMock(), MagicMock(), MagicMock())
        self.mock_graph.__iter__.return_value = [mock_triple]
        
        box = {
            'graph': self.mock_graph,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertIn('metadata', result)

    @patch('app.fab.get_metadata.get_call_trace')
    @patch('app.fab.get_metadata.datetime')
    @patch('app.fab.get_metadata.Literal')
    @patch('app.fab.get_metadata.RDF')
    def test_get_metadata_empty_graph(self, mock_rdf, mock_literal, mock_datetime, mock_call_trace):
        """Test get_metadata with empty graph."""
        # Arrange
        mock_datetime.now.return_value.isoformat.return_value = "2025-01-21T12:00:00"
        mock_call_trace.return_value = "app.fab.test.module>function"
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock empty graph
        self.mock_graph.__iter__.return_value = []
        self.mock_graph.__len__.return_value = 0
        
        box = {
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = get_metadata(box, **box)
        
        # Assert
        metadata = result['metadata']
        self.assertEqual(metadata['triples'], 0)
        self.assertEqual(metadata['entities'], 0)
        self.assertEqual(metadata['chars'], 0)


if __name__ == '__main__':
    unittest.main()
