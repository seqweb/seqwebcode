#!/usr/bin/env python3
"""
Test suite for add_raw_text module.

Tests the core functionality of adding raw text triples to an RDFLib Graph.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.add_raw_text import add_raw_text


class TestAddRawText(unittest.TestCase):
    """Test cases for add_raw_text core function."""

    def setUp(self):
        """Set up test fixtures."""
        # Mock RDFLib components
        self.mock_graph = MagicMock()
        self.mock_namespace = MagicMock()
        self.mock_literal = MagicMock()
        self.mock_rdf = MagicMock()
        self.mock_collection = MagicMock()
        
        # Mock URIs
        self.mock_sequence_uri = MagicMock()
        self.mock_text_uri = MagicMock()
        self.mock_text_list_uri = MagicMock()
        
        # Set up mock return values
        self.mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": MagicMock(),
            "http://www.oeis.org/": MagicMock(),
            "http://www.w3.org/2011/content#": MagicMock()
        }[uri]
        
        self.mock_collection.return_value.uri = self.mock_text_list_uri

    @patch('app.fab.add_raw_text.Graph')
    @patch('app.fab.add_raw_text.Namespace')
    @patch('app.fab.add_raw_text.Literal')
    @patch('app.fab.add_raw_text.RDF')
    @patch('app.fab.add_raw_text.Collection')
    def test_add_raw_text_success(self, mock_collection, mock_rdf, mock_literal, 
                                 mock_namespace, mock_graph):
        """Test successful addition of raw text triples."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_namespace.side_effect = self.mock_namespace.side_effect
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_collection.return_value.uri = self.mock_text_list_uri
        
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
        oeis_ns.__getitem__.return_value = self.mock_sequence_uri
        seqweb_ns.__getitem__.side_effect = lambda key: {
            "A000001-raw-text": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'oeis_data': 'Test OEIS data',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = add_raw_text(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertEqual(result['extra_key'], 'extra_value')
        
        # Verify graph.add was called for the three expected triples
        self.assertEqual(self.mock_graph.add.call_count, 3)
        
        # Verify Collection was created and used
        mock_collection.assert_called_once_with(self.mock_graph, self.mock_text_uri)
        mock_collection.return_value.append.assert_called_once_with(self.mock_text_uri)

    @patch('app.fab.add_raw_text.Graph')
    @patch('app.fab.add_raw_text.Namespace')
    @patch('app.fab.add_raw_text.Literal')
    @patch('app.fab.add_raw_text.RDF')
    @patch('app.fab.add_raw_text.Collection')
    def test_add_raw_text_with_noisy_output(self, mock_collection, mock_rdf, mock_literal,
                                          mock_namespace, mock_graph):
        """Test add_raw_text with noisy output enabled."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_namespace.side_effect = self.mock_namespace.side_effect
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_collection.return_value.uri = self.mock_text_list_uri
        
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
        oeis_ns.__getitem__.return_value = self.mock_sequence_uri
        seqweb_ns.__getitem__.side_effect = lambda key: {
            "A000001-raw-text": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'oeis_data': 'Test OEIS data',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = add_raw_text(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)
        
        # Verify noisy output was printed
        mock_print.assert_called_once()
        print_args = mock_print.call_args[0][0]
        self.assertIn('add_raw_text: Added raw text triples for A000001', print_args)

    def test_add_raw_text_invalid_graph_type(self):
        """Test add_raw_text with invalid graph type."""
        # Arrange
        box = {
            'id': 'A000001',
            'graph': "not a graph",  # Invalid graph type
            'oeis_data': 'Test OEIS data',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ValueError) as context:
            add_raw_text(box, **box)
        
        self.assertIn("Invalid graph type", str(context.exception))
        self.assertIn("expected RDFLib Graph", str(context.exception))

    @patch('app.fab.add_raw_text.Graph', None)
    def test_add_raw_text_rdflib_not_available(self):
        """Test add_raw_text when RDFLib is not available."""
        # Arrange
        box = {
            'id': 'A000001',
            'graph': MagicMock(),
            'oeis_data': 'Test OEIS data',
            'noisy': False
        }
        
        # Act & Assert
        with self.assertRaises(ImportError) as context:
            add_raw_text(box, **box)
        
        self.assertIn("RDFLib not available", str(context.exception))

    @patch('app.fab.add_raw_text.Graph')
    @patch('app.fab.add_raw_text.Namespace')
    @patch('app.fab.add_raw_text.Literal')
    @patch('app.fab.add_raw_text.RDF')
    @patch('app.fab.add_raw_text.Collection')
    def test_add_raw_text_preserves_extra_keys(self, mock_collection, mock_rdf, mock_literal,
                                             mock_namespace, mock_graph):
        """Test that add_raw_text preserves extra keys in the box."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_namespace.side_effect = self.mock_namespace.side_effect
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_collection.return_value.uri = self.mock_text_list_uri
        
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
        oeis_ns.__getitem__.return_value = self.mock_sequence_uri
        seqweb_ns.__getitem__.side_effect = lambda key: {
            "A000001-raw-text": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'oeis_data': 'Test OEIS data',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = add_raw_text(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)


if __name__ == '__main__':
    unittest.main()
