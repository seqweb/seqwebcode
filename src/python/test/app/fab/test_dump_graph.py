#!/usr/bin/env python3
"""
Test suite for dump_graph module.

Tests the core functionality of serializing an RDFLib Graph to RDF/Turtle format.
"""

import unittest
import sys
import os
import tempfile
import shutil
from unittest.mock import patch, MagicMock, mock_open

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.dump_graph import dump_graph


class TestDumpGraph(unittest.TestCase):
    """Test cases for dump_graph core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        self.mock_graph.__len__.return_value = 1
        
        # Create a temporary directory for testing
        self.temp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.temp_dir)

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    def test_dump_graph_success(self, mock_makedirs, mock_exists, mock_file, 
                               mock_seqvar_get, mock_graph):
        """Test successful graph serialization and file writing."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = self.temp_dir
        mock_exists.return_value = False  # Directory doesn't exist, file doesn't exist
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('turtle_output', result)
        self.assertIn('file_path', result)
        self.assertTrue(result['file_path'].endswith('app.fab.A000001.ttl'))
        
        # Verify file operations
        mock_file.assert_called_once()
        mock_makedirs.assert_called_once()
        self.mock_graph.serialize.assert_called_once_with(format='turtle')

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    @patch('app.fab.os.rename')
    def test_dump_graph_with_backup(self, mock_rename, mock_makedirs, mock_exists, 
                                   mock_file, mock_seqvar_get, mock_graph):
        """Test graph serialization when file already exists (creates backup)."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = self.temp_dir
        mock_exists.side_effect = lambda path: path.endswith('.ttl')  # File exists
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertIn('turtle_output', result)
        self.assertIn('file_path', result)
        
        # Verify backup was created
        mock_rename.assert_called_once()

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    def test_dump_graph_with_metadata(self, mock_makedirs, mock_exists, mock_file,
                                    mock_seqvar_get, mock_graph):
        """Test graph serialization with metadata comment."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = self.temp_dir
        mock_exists.return_value = False
        
        metadata = {"version": "1.0", "timestamp": "2025-01-21"}
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'metadata': metadata,
            'noisy': False
        }
        
        # Act
        result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertIn('turtle_output', result)
        self.assertIn('file_path', result)
        
        # Verify file was written with metadata
        mock_file.assert_called_once()
        written_content = ''.join(call[0][0] for call in mock_file().write.call_args_list)
        self.assertIn('{"version": "1.0", "timestamp": "2025-01-21"}', written_content)

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    def test_dump_graph_with_noisy_output(self, mock_makedirs, mock_exists, mock_file,
                                        mock_seqvar_get, mock_graph):
        """Test dump_graph with noisy output enabled."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = self.temp_dir
        mock_exists.return_value = False
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('dump_graph: Serializing graph' in call for call in print_calls))
        self.assertTrue(any('dump_graph: Writing to' in call for call in print_calls))

    def test_dump_graph_invalid_graph_type(self):
        """Test dump_graph with invalid graph type."""
        # Arrange
        box = {
            'id': 'A000001',
            'graph': "not a graph",  # Invalid graph type
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            dump_graph(box, **box)
        
        self.assertIn("Invalid graph type", str(context.exception))
        self.assertIn("expected RDFLib Graph", str(context.exception))

    @patch('app.fab.dump_graph.Graph', None)
    def test_dump_graph_rdflib_not_available(self):
        """Test dump_graph when RDFLib is not available."""
        # Arrange
        box = {
            'id': 'A000001',
            'graph': MagicMock(),
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ImportError) as context:
            dump_graph(box, **box)
        
        self.assertIn("RDFLib not available", str(context.exception))

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    def test_dump_graph_seqvar_failure(self, mock_seqvar_get, mock_graph):
        """Test dump_graph when seqvar system fails."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.side_effect = Exception("seqvar error")
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(RuntimeError) as context:
            dump_graph(box, **box)
        
        self.assertIn("Failed to get seqwebdata path from seqvar", str(context.exception))

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    def test_dump_graph_seqvar_not_set(self, mock_seqvar_get, mock_graph):
        """Test dump_graph when seqvar path is not set."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = None  # Path not set
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(RuntimeError) as context:
            dump_graph(box, **box)
        
        self.assertIn("seqwebdata path not set in seqvar system", str(context.exception))

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    def test_dump_graph_file_write_error(self, mock_makedirs, mock_exists, mock_file,
                                       mock_seqvar_get, mock_graph):
        """Test dump_graph when file writing fails."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = self.temp_dir
        mock_exists.return_value = False
        mock_file.side_effect = IOError("Write error")
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(RuntimeError) as context:
            dump_graph(box, **box)
        
        self.assertIn("Failed to write TTL file", str(context.exception))

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    def test_dump_graph_preserves_extra_keys(self, mock_makedirs, mock_exists, mock_file,
                                           mock_seqvar_get, mock_graph):
        """Test that dump_graph preserves extra keys in the box."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = self.temp_dir
        mock_exists.return_value = False
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = dump_graph(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertIn('turtle_output', result)
        self.assertIn('file_path', result)

    @patch('app.fab.dump_graph.Graph')
    @patch('app.fab.dump_graph.seqvar_get')
    @patch('builtins.open', new_callable=mock_open)
    @patch('app.fab.os.path.exists')
    @patch('app.fab.os.makedirs')
    def test_dump_graph_file_path_construction(self, mock_makedirs, mock_exists, mock_file,
                                             mock_seqvar_get, mock_graph):
        """Test that file path is constructed correctly."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_seqvar_get.return_value = "/test/seqwebdata"
        mock_exists.return_value = False
        
        box = {
            'id': 'A000123',  # Test with different ID
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = dump_graph(box, **box)
        
        # Assert
        expected_path = "/test/seqwebdata/seq/A000/A000123.ttl"
        self.assertEqual(result['file_path'], expected_path)


if __name__ == '__main__':
    unittest.main()
