#!/usr/bin/env python3
"""
Test suite for add_sections module.

Tests the core functionality of adding RDF triples for text blocks in section_map.
"""

import unittest
import sys
import os
from unittest.mock import patch, MagicMock
from datetime import datetime

# Add the src/python directory to the path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from app.fab.add_sections import add_sections, _extract_stamp_from_i_section


class TestAddSections(unittest.TestCase):
    """Test cases for add_sections core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_graph = MagicMock()
        self.mock_namespace = MagicMock()
        self.mock_literal = MagicMock()
        self.mock_rdf = MagicMock()
        self.mock_collection = MagicMock()
        self.mock_bnode = MagicMock()
        
        # Mock URIs
        self.mock_sequence_uri = MagicMock()
        self.mock_text_uri = MagicMock()
        self.mock_list_head = MagicMock()

    @patch('app.fab.add_sections.datetime')
    @patch('app.fab.add_sections.BNode')
    @patch('app.fab.add_sections.Collection')
    @patch('app.fab.add_sections.RDF')
    @patch('app.fab.add_sections.Literal')
    @patch('app.fab.add_sections.Namespace')
    def test_add_sections_success(self, mock_namespace, mock_literal, mock_rdf, 
                                 mock_collection, mock_bnode, mock_datetime):
        """Test successful addition of section triples."""
        # Arrange
        mock_datetime.now.return_value.strftime.return_value = "20250121T120000"
        mock_bnode.return_value = self.mock_list_head
        mock_collection.return_value = MagicMock()
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
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
            "A000001_S_1_20250121T120000": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        section_map = {
            'S': ['First section content', 'Second section content'],
            'T': ['Third section content']
        }
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'section_map': section_map,
            'noisy': False,
            'extra_key': 'extra_value'
        }
        
        # Act
        result = add_sections(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)
        self.assertEqual(result['extra_key'], 'extra_value')
        
        # Verify graph.add was called for each text block (2 S + 1 T = 3 text blocks)
        # Each text block gets 2 triples (type + chars), plus 1 hasTextList triple = 7 total
        expected_calls = 7  # 3 text blocks * 2 triples each + 1 hasTextList
        self.assertEqual(self.mock_graph.add.call_count, expected_calls)
        
        # Verify Collection was created for the text list
        mock_collection.assert_called_once()

    @patch('app.fab.add_sections.datetime')
    @patch('app.fab.add_sections.BNode')
    @patch('app.fab.add_sections.Collection')
    @patch('app.fab.add_sections.RDF')
    @patch('app.fab.add_sections.Literal')
    @patch('app.fab.add_sections.Namespace')
    def test_add_sections_with_noisy_output(self, mock_namespace, mock_literal, mock_rdf,
                                          mock_collection, mock_bnode, mock_datetime):
        """Test add_sections with noisy output enabled."""
        # Arrange
        mock_datetime.now.return_value.strftime.return_value = "20250121T120000"
        mock_bnode.return_value = self.mock_list_head
        mock_collection.return_value = MagicMock()
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
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
            "A000001_S_1_20250121T120000": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        section_map = {'S': ['Test content']}
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'section_map': section_map,
            'noisy': True
        }
        
        # Act
        with patch('builtins.print') as mock_print:
            result = add_sections(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        
        # Verify noisy output was printed
        self.assertGreater(mock_print.call_count, 0)
        print_calls = [call[0][0] for call in mock_print.call_args_list]
        self.assertTrue(any('add_sections: Processing' in call for call in print_calls))

    @patch('app.fab.add_sections.datetime')
    @patch('app.fab.add_sections.BNode')
    @patch('app.fab.add_sections.Collection')
    @patch('app.fab.add_sections.RDF')
    @patch('app.fab.add_sections.Literal')
    @patch('app.fab.add_sections.Namespace')
    def test_add_sections_with_stamp_extraction(self, mock_namespace, mock_literal, mock_rdf,
                                              mock_collection, mock_bnode, mock_datetime):
        """Test add_sections with timestamp extraction from I section."""
        # Arrange
        mock_datetime.now.return_value.strftime.return_value = "20250121T120000"
        mock_bnode.return_value = self.mock_list_head
        mock_collection.return_value = MagicMock()
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
        # Mock namespace objects
        seqweb_ns = MagicMock()
        oeis_ns = MagicMock()
        cnt_ns = MagicMock()
        mock_namespace.side_effect = lambda uri: {
            "http://www.seqweb.org/": seqweb_ns,
            "http://www.oeis.org/": oeis_ns,
            "http://www.w3.org/2011/content#": cnt_ns
        }[uri]
        
        # Mock URI creation with extracted timestamp
        oeis_ns.__getitem__.return_value = self.mock_sequence_uri
        seqweb_ns.__getitem__.side_effect = lambda key: {
            "A000001_S_1_20250221T130704": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        section_map = {
            'I': ['Feb 21 2025 13:07:04'],  # Contains timestamp
            'S': ['Test content']
        }
        
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'section_map': section_map,
            'noisy': False
        }
        
        # Act
        result = add_sections(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        # Verify that the extracted timestamp was used in URI creation
        seqweb_ns.__getitem__.assert_called_with("A000001_S_1_20250221T130704")

    def test_add_sections_empty_section_map(self):
        """Test add_sections with empty section_map."""
        # Arrange
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'section_map': {},
            'noisy': False
        }
        
        # Act
        result = add_sections(box, **box)
        
        # Assert
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)
        # No triples should be added for empty section_map
        self.assertEqual(self.mock_graph.add.call_count, 0)

    def test_extract_stamp_from_i_section_month_name_format(self):
        """Test timestamp extraction with month name format."""
        # Arrange
        i_sections = ['Feb 21 2025 13:07:04']
        
        # Act
        result = _extract_stamp_from_i_section(i_sections)
        
        # Assert
        self.assertEqual(result, '20250221T130704')

    def test_extract_stamp_from_i_section_iso_format(self):
        """Test timestamp extraction with ISO format."""
        # Arrange
        i_sections = ['2025-02-21 13:07:04']
        
        # Act
        result = _extract_stamp_from_i_section(i_sections)
        
        # Assert
        self.assertEqual(result, '20250221T130704')

    def test_extract_stamp_from_i_section_target_format(self):
        """Test timestamp extraction with target format."""
        # Arrange
        i_sections = ['20250221T130704']
        
        # Act
        result = _extract_stamp_from_i_section(i_sections)
        
        # Assert
        self.assertEqual(result, '20250221T130704')

    def test_extract_stamp_from_i_section_no_match(self):
        """Test timestamp extraction with no matching pattern."""
        # Arrange
        i_sections = ['No timestamp here']
        
        # Act
        result = _extract_stamp_from_i_section(i_sections)
        
        # Assert
        self.assertEqual(result, '')

    def test_extract_stamp_from_i_section_empty(self):
        """Test timestamp extraction with empty I sections."""
        # Arrange
        i_sections = []
        
        # Act
        result = _extract_stamp_from_i_section(i_sections)
        
        # Assert
        self.assertEqual(result, '')

    @patch('app.fab.add_sections.datetime')
    @patch('app.fab.add_sections.BNode')
    @patch('app.fab.add_sections.Collection')
    @patch('app.fab.add_sections.RDF')
    @patch('app.fab.add_sections.Literal')
    @patch('app.fab.add_sections.Namespace')
    def test_add_sections_preserves_extra_keys(self, mock_namespace, mock_literal, mock_rdf,
                                             mock_collection, mock_bnode, mock_datetime):
        """Test that add_sections preserves extra keys in the box."""
        # Arrange
        mock_datetime.now.return_value.strftime.return_value = "20250121T120000"
        mock_bnode.return_value = self.mock_list_head
        mock_collection.return_value = MagicMock()
        mock_rdf.type = "rdf:type"
        mock_literal.return_value = self.mock_literal
        
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
            "A000001_S_1_20250121T120000": self.mock_text_uri,
            "Text": "seq:Text",
            "hasTextList": "seq:hasTextList"
        }[key]
        
        section_map = {'S': ['Test content']}
        box = {
            'id': 'A000001',
            'graph': self.mock_graph,
            'section_map': section_map,
            'noisy': False,
            'custom_key': 'custom_value',
            'another_key': 42
        }
        
        # Act
        result = add_sections(box, **box)
        
        # Assert
        self.assertEqual(result['custom_key'], 'custom_value')
        self.assertEqual(result['another_key'], 42)
        self.assertEqual(result['id'], 'A000001')
        self.assertEqual(result['graph'], self.mock_graph)


if __name__ == '__main__':
    unittest.main()
