#!/usr/bin/env python3
"""
Test suite for mod2 module.

Tests the core functionality of the RDF generation module.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.mod2 import mod2


class TestMod2(unittest.TestCase):
    """Test cases for mod2 core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_namespace = MagicMock()
        self.mock_literal = MagicMock()
        self.mock_rdf = MagicMock()
        self.mock_rdfs = MagicMock()

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_success(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test successful RDF generation."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        mock_sequence_uri = MagicMock()
        example_ns.__getitem__.return_value = mock_sequence_uri
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = mod2(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['extra_key'], 'extra_value')
        self.assertIn('rdf_turtle', result)
        self.assertEqual(result['rdf_turtle'], "@prefix ex: <http://example.org/> .\nex:test a ex:Test .")
        
        # Verify graph operations
        self.assertEqual(self.mock_graph.add.call_count, 4)  # 4 RDF statements
        self.mock_graph.serialize.assert_called_once_with(format='turtle')

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_with_noisy_output(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test mod2 with noisy output enabled."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        example_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000001',
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = mod2(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('mod2: Generating RDF for ID: A000001' in call for call in print_calls))
        self.assertTrue(any('mod2: Generated RDF/Turtle:' in call for call in print_calls))
        self.assertTrue(any('mod2: RDF generation complete for A000001' in call for call in print_calls))

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_different_id(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test mod2 with different ID."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        example_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000123',
            'noisy': False
        }
        
        # Act
        result = mod2(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000123')
        
        # Verify URI was created with correct ID
        example_ns.__getitem__.assert_called_once_with('sequence/A000123')

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_rdf_statements(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test that mod2 creates the correct RDF statements."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        mock_sequence_uri = MagicMock()
        example_ns.__getitem__.return_value = mock_sequence_uri
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = mod2(box, **box)
        
        # Assert
        # Verify 4 RDF statements were added
        self.assertEqual(self.mock_graph.add.call_count, 4)
        
        # Verify the specific statements
        add_calls = [call[0][0] for call in self.mock_graph.add.call_args_list]
        
        # Statement 1: (sequence_uri, RDF.type, seq:Sequence)
        self.assertEqual(add_calls[0][0], mock_sequence_uri)
        self.assertEqual(add_calls[0][1], mock_rdf.type)
        self.assertEqual(add_calls[0][2], "seq:Sequence")
        
        # Statement 2: (sequence_uri, RDFS.label, Literal("Sequence A000001"))
        self.assertEqual(add_calls[1][0], mock_sequence_uri)
        self.assertEqual(add_calls[1][1], mock_rdfs.label)
        self.assertEqual(add_calls[1][2], self.mock_literal)
        
        # Statement 3: (sequence_uri, seq:hasId, Literal("A000001"))
        self.assertEqual(add_calls[2][0], mock_sequence_uri)
        self.assertEqual(add_calls[2][1], "seq:hasId")
        self.assertEqual(add_calls[2][2], self.mock_literal)
        
        # Statement 4: (sequence_uri, seq:status, Literal("active"))
        self.assertEqual(add_calls[3][0], mock_sequence_uri)
        self.assertEqual(add_calls[3][1], "seq:status")
        self.assertEqual(add_calls[3][2], self.mock_literal)

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_preserves_extra_keys(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test that mod2 preserves extra keys in the box."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        example_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000001',
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = mod2(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertIn('rdf_turtle', result)

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_literal_creation(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test that literals are created with correct parameters."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        example_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = mod2(box, **box)
        
        # Assert
        # Verify literals were created with correct parameters
        self.assertEqual(mock_literal.call_count, 3)  # 3 literals created
        
        # Check the specific literal calls
        literal_calls = mock_literal.call_args_list
        self.assertEqual(literal_calls[0][0], ("Sequence A000001",))
        self.assertEqual(literal_calls[1][0], ("A000001",))
        self.assertEqual(literal_calls[2][0], ("active",))

    @patch('app.fab.mod2.Graph')
    @patch('app.fab.mod2.Namespace')
    @patch('app.fab.mod2.Literal')
    @patch('app.fab.mod2.RDF')
    @patch('app.fab.mod2.RDFS')
    def test_mod2_namespace_usage(self, mock_rdfs, mock_rdf, mock_literal, mock_namespace, mock_graph):
        """Test that namespaces are used correctly."""
        # Arrange
        mock_graph.return_value = self.mock_graph
        mock_literal.return_value = self.mock_literal
        mock_rdf.type = "rdf:type"
        mock_rdfs.label = "rdfs:label"
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        example_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://seqweb.org/ontology/": seqweb_ns,
            "http://example.org/": example_ns
        }[uri]
        
        # Mock URI creation
        example_ns.__getitem__.return_value = MagicMock()
        seqweb_ns.Sequence = "seq:Sequence"
        seqweb_ns.hasId = "seq:hasId"
        seqweb_ns.status = "seq:status"
        
        # Mock graph serialization
        self.mock_graph.serialize.return_value = "@prefix ex: <http://example.org/> .\nex:test a ex:Test ."
        
        box = {
            'id': 'A000001',
            'noisy': False
        }
        
        # Act
        result = mod2(box, **box)
        
        # Assert
        # Verify namespaces were created with correct URIs
        mock_namespace.assert_any_call("http://seqweb.org/ontology/")
        mock_namespace.assert_any_call("http://example.org/")
        
        # Verify URI was created correctly
        example_ns.__getitem__.assert_called_once_with('sequence/A000001')


if __name__ == '__main__':
    unittest.main()
