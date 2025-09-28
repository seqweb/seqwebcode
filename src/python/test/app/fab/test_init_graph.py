#!/usr/bin/env python3
"""
Test suite for init_graph module.

Tests the core functionality of creating an RDFLib Graph with SeqWeb prefixes.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.init_graph import init_graph


class TestInitGraph(unittest.TestCase):
    """Test cases for init_graph core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_namespace = MagicMock()
        self.mock_literal = MagicMock()
        self.mock_rdfs = MagicMock()

    @patch('app.fab.init_graph.Graph')
    @patch('app.fab.init_graph.Namespace')
    @patch('app.fab.init_graph.Literal')
    @patch('app.fab.init_graph.RDFS')
    def test_init_graph_success(self, mock_rdfs, mock_literal, mock_namespace, mock_graph):
        """Test successful graph initialization."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = init_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertEqual(result['graph'], self.mock_graph)
        
        # Verify graph operations
        self.assertEqual(self.mock_graph.bind.call_count, 5)  # 5 namespace bindings
        self.mock_graph.add.assert_called_once()
        
        # Verify namespace bindings
        bind_calls = self.mock_graph.bind.call_args_list
        self.assertTrue(any(call[0] == ("", seqweb_ns) for call in bind_calls))
        self.assertTrue(any(call[0] == ("seq", seqweb_ns) for call in bind_calls))
        self.assertTrue(any(call[0] == ("oeis", oeis_ns) for call in bind_calls))
        self.assertTrue(any(call[0] == ("rdfs", mock_rdfs) for call in bind_calls))
        self.assertTrue(any(call[0] == ("cnt", cnt_ns) for call in bind_calls))

    @patch('app.fab.init_graph.Graph')
    @patch('app.fab.init_graph.Namespace')
    @patch('app.fab.init_graph.Literal')
    @patch('app.fab.init_graph.RDFS')
    def test_init_graph_with_noisy_output(self, mock_rdfs, mock_literal, mock_namespace, mock_graph):
        """Test init_graph with noisy output enabled."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdfs.label = "rdfs:label"
        self.mock_graph.__len__.return_value = 1
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        
        box = {
            'id': 'A000001',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = init_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertIn('init_graph: Created RDFLib Graph', print_args)
        self.assertIn('statements and SeqWeb prefixes', print_args)

    @patch('app.fab.init_graph.Graph')
    @patch('app.fab.init_graph.Namespace')
    @patch('app.fab.init_graph.Literal')
    @patch('app.fab.init_graph.RDFS')
    def test_init_graph_different_id(self, mock_rdfs, mock_literal, mock_namespace, mock_graph):
        """Test init_graph with different ID."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        
        box = {
            'id': 'A000123',
            'noisy': False
        }
        
        # Act
        result = init_graph(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        
        # Verify URI was created with correct ID
        oeis_ns.__getitem__.assert_called_once_with('A000123')

    @patch('app.fab.init_graph.Graph')
    @patch('app.fab.init_graph.Namespace')
    @patch('app.fab.init_graph.Literal')
    @patch('app.fab.init_graph.RDFS')
    def test_init_graph_literal_creation(self, mock_rdfs, mock_literal, mock_namespace, mock_graph):
        """Test that literal is created with correct parameters."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation
        mock_sequence_uri = MagicMock()
        oeis_ns.__getitem__.return_value = mock_sequence_uri
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = init_graph(box, **box)
        
        # Assert
        # Verify literal was created with correct parameters
        mock_literal.assert_called_once_with('A000001', lang="en")
        
        # Verify graph.add was called with correct triple
        self.mock_graph.add.assert_called_once()
        add_call = self.mock_graph.add.call_args[0][0]
        self.assertEqual(add_call[0], mock_sequence_uri)
        self.assertEqual(add_call[1], mock_rdfs.label)
        self.assertEqual(add_call[2], self.mock_literal)

    def test_init_graph_rdflib_not_available(self):
        """Test init_graph when RDFLib is not available."""
        # Arrange
        with patch('app.fab.init_graph.Graph', None):
            box = {
                'id': 'A000001',
                'noisy': False
            }
            
            # Act & Assert
            with self.assertRaises(ImportError) as context:
                init_graph(box, **box)
            
            self.assertIn("RDFLib not available", str(context.exception))

    @patch('app.fab.init_graph.Graph')
    @patch('app.fab.init_graph.Namespace')
    @patch('app.fab.init_graph.Literal')
    @patch('app.fab.init_graph.RDFS')
    def test_init_graph_preserves_extra_keys(self, mock_rdfs, mock_literal, mock_namespace, mock_graph):
        """Test that init_graph preserves extra keys in the box."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = init_graph(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)

    @patch('app.fab.init_graph.Graph')
    @patch('app.fab.init_graph.Namespace')
    @patch('app.fab.init_graph.Literal')
    @patch('app.fab.init_graph.RDFS')
    def test_init_graph_namespace_bindings(self, mock_rdfs, mock_literal, mock_namespace, mock_graph):
        """Test that all required namespace bindings are created."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = init_graph(box, **box)
        
        # Assert
        # Verify all 5 namespace bindings were created
        self.assertEqual(self.mock_graph.bind.call_count, 5)
        
        # Verify specific namespace bindings
        bind_calls = [call[0] for call in self.mock_graph.bind.call_args_list]
        expected_bindings = [
            ("", seqweb_ns),
            ("seq", seqweb_ns),
            ("oeis", oeis_ns),
            ("rdfs", mock_rdfs),
            ("cnt", cnt_ns)
        ]
        
        for expected_binding in expected_bindings:
            self.assertIn(expected_binding, bind_calls)


if __name__ == '__main__':
    unittest.main()
