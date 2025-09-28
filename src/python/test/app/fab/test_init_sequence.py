#!/usr/bin/env python3
"""
Test suite for init_sequence module.

Tests the core functionality of adding sequence declarations to RDFLib Graphs.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.init_sequence import init_sequence


class TestInitSequence(unittest.TestCase):
    """Test cases for init_sequence core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_namespace = MagicMock()
        self.mock_rdf = MagicMock()

    @patch('app.fab.init_sequence.Graph')
    @patch('app.fab.init_sequence.Namespace')
    @patch('app.fab.init_sequence.RDF')
    def test_init_sequence_success(self, mock_rdf, mock_namespace, mock_graph):
        """Test successful sequence declaration."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_rdf.type = "rdf:type"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns
        }[uri]
        
        # Mock URI creation
        mock_sequence_uri = MagicMock()
        oeis_ns.__getitem__.return_value = mock_sequence_uri
        seqweb_ns.__getitem__.return_value = "seq:Sequence"
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = init_sequence(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertEqual(result['graph'], self.mock_graph)
        
        # Verify graph.add was called with correct triple
        self.mock_graph.add.assert_called_once()
        add_call = self.mock_graph.add.call_args[0][0]
        self.assertEqual(add_call[0], mock_sequence_uri)
        self.assertEqual(add_call[1], mock_rdf.type)
        self.assertEqual(add_call[2], "seq:Sequence")

    @patch('app.fab.init_sequence.Graph')
    @patch('app.fab.init_sequence.Namespace')
    @patch('app.fab.init_sequence.RDF')
    def test_init_sequence_with_noisy_output(self, mock_rdf, mock_namespace, mock_graph):
        """Test init_sequence with noisy output enabled."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_rdf.type = "rdf:type"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.__getitem__.return_value = "seq:Sequence"
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = init_sequence(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertIn('init_sequence: Added sequence declaration for A000001', print_args)

    @patch('app.fab.init_sequence.Graph')
    @patch('app.fab.init_sequence.Namespace')
    @patch('app.fab.init_sequence.RDF')
    def test_init_sequence_different_id(self, mock_rdf, mock_namespace, mock_graph):
        """Test init_sequence with different ID."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_rdf.type = "rdf:type"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.__getitem__.return_value = "seq:Sequence"
        
        box = {
            'id': 'A000123',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = init_sequence(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        
        # Verify URI was created with correct ID
        oeis_ns.__getitem__.assert_called_once_with('A000123')

    def test_init_sequence_rdflib_not_available(self):
        """Test init_sequence when RDFLib is not available."""
        # Arrange
        with patch('app.fab.init_sequence.Graph', None):
            box = {
                'id': 'A000001',
                'graph': MagicMock(),
                'noisy': False
            }
            
            # Act & Assert
            with self.assertRaises(ImportError) as context:
                init_sequence(box, **box)
            
            self.assertIn("RDFLib not available", str(context.exception))

    @patch('app.fab.init_sequence.Graph')
    def test_init_sequence_invalid_graph_type(self, mock_graph):
        """Test init_sequence with invalid graph type."""
        # Arrange
        mock_graph.return_value = MagicMock()  # Different type than expected
        
        box = {
            'id': 'A000001',
            'graph': "not a graph",  # Invalid graph type
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            init_sequence(box, **box)
        
        self.assertIn("Invalid graph type", str(context.exception))
        self.assertIn("expected RDFLib Graph", str(context.exception))

    @patch('app.fab.init_sequence.Graph')
    @patch('app.fab.init_sequence.Namespace')
    @patch('app.fab.init_sequence.RDF')
    def test_init_sequence_preserves_extra_keys(self, mock_rdf, mock_namespace, mock_graph):
        """Test that init_sequence preserves extra keys in the box."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_rdf.type = "rdf:type"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.__getitem__.return_value = "seq:Sequence"
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = init_sequence(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)

    @patch('app.fab.init_sequence.Graph')
    @patch('app.fab.init_sequence.Namespace')
    @patch('app.fab.init_sequence.RDF')
    def test_init_sequence_triple_creation(self, mock_rdf, mock_namespace, mock_graph):
        """Test that the correct triple is created."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_rdf.type = "rdf:type"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns
        }[uri]
        
        # Mock URI creation
        mock_sequence_uri = MagicMock()
        oeis_ns.__getitem__.return_value = mock_sequence_uri
        seqweb_ns.__getitem__.return_value = "seq:Sequence"
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = init_sequence(box, **box)
        
        # Assert
        # Verify the exact triple that was added
        self.mock_graph.add.assert_called_once()
        triple = self.mock_graph.add.call_args[0][0]
        
        # Triple should be: (sequence_uri, RDF.type, seq:Sequence)
        self.assertEqual(triple[0], mock_sequence_uri)
        self.assertEqual(triple[1], mock_rdf.type)
        self.assertEqual(triple[2], "seq:Sequence")

    @patch('app.fab.init_sequence.Graph')
    @patch('app.fab.init_sequence.Namespace')
    @patch('app.fab.init_sequence.RDF')
    def test_init_sequence_namespace_usage(self, mock_rdf, mock_namespace, mock_graph):
        """Test that namespaces are used correctly."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_rdf.type = "rdf:type"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns
        }[uri]
        
        # Mock URI creation
        oeis_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.__getitem__.return_value = "seq:Sequence"
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'noisy': False
        }
        
        # Act
        result = init_sequence(box, **box)
        
        # Assert
        # Verify namespaces were created with correct URIs
        mock_namespace.assert_any_call("http://www.seqweb.org/")
        mock_namespace.assert_any_call("http://www.oeis.org/")
        
        # Verify URIs were created correctly
        oeis_ns.__getitem__.assert_called_once_with('A000001')
        seqweb_ns.__getitem__.assert_called_once_with('Sequence')


if __name__ == '__main__':
    unittest.main()
