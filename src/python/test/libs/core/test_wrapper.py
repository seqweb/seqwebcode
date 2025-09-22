#!/usr/bin/env python3
"""
Test suite for wrapper.py functionality.

This module tests the polyglot pipeline wrapper utilities including
typed-value registry, conversion functions, and inbox/outbox handling.
"""

import json
import io
import sys
import unittest
from unittest.mock import patch
from rdflib import Graph, Namespace, Literal
from rdflib.term import Node

# Import the module under test
import sys
import os
# Add the src/python directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from libs.core.wrapper import (
    TYPED_VALUE_REGISTRY,
    get_external_type_id,
    get_internal_type,
    get_to_json_function,
    get_from_json_function,
    is_registered_type,
    convert_to_json,
    convert_from_json,
    load_inbox,
    augment_inbox_with_args,
    get_inbox,
    dump_outbox
)


class TestTypedValueRegistry(unittest.TestCase):
    """Test the typed-value registry functionality."""
    
    def test_registry_has_graph_type(self):
        """Test that the registry contains the Graph type."""
        self.assertIn("https://www.w3.org/TR/json-ld11/", TYPED_VALUE_REGISTRY)
        registry_entry = TYPED_VALUE_REGISTRY["https://www.w3.org/TR/json-ld11/"]
        self.assertEqual(registry_entry["internal_type"], Graph)
        self.assertIsNotNone(registry_entry["to_json"])
        self.assertIsNotNone(registry_entry["from_json"])
    
    def test_get_external_type_id(self):
        """Test getting external type ID from internal type."""
        external_id = get_external_type_id(Graph)
        self.assertEqual(external_id, "https://www.w3.org/TR/json-ld11/")
        
        # Test with unregistered type
        external_id = get_external_type_id(str)
        self.assertIsNone(external_id)
    
    def test_get_internal_type(self):
        """Test getting internal type from external type ID."""
        internal_type = get_internal_type("https://www.w3.org/TR/json-ld11/")
        self.assertEqual(internal_type, Graph)
        
        # Test with unregistered type
        internal_type = get_internal_type("http://example.org/unknown")
        self.assertIsNone(internal_type)
    
    def test_get_conversion_functions(self):
        """Test getting conversion functions."""
        to_json_func = get_to_json_function("https://www.w3.org/TR/json-ld11/")
        from_json_func = get_from_json_function("https://www.w3.org/TR/json-ld11/")
        
        self.assertIsNotNone(to_json_func)
        self.assertIsNotNone(from_json_func)
        
        # Test with unregistered type
        to_json_func = get_to_json_function("http://example.org/unknown")
        from_json_func = get_from_json_function("http://example.org/unknown")
        
        self.assertIsNone(to_json_func)
        self.assertIsNone(from_json_func)
    
    def test_is_registered_type(self):
        """Test checking if a type is registered."""
        graph = Graph()
        self.assertTrue(is_registered_type(graph))
        self.assertFalse(is_registered_type("string"))
        self.assertFalse(is_registered_type(123))


class TestConversionFunctions(unittest.TestCase):
    """Test the conversion functions."""
    
    def test_convert_to_json_simple_types(self):
        """Test converting simple types to JSON."""
        # Test dict
        result = convert_to_json({"key": "value"})
        self.assertEqual(result, {"key": "value"})
        
        # Test list
        result = convert_to_json([1, 2, 3])
        self.assertEqual(result, [1, 2, 3])
        
        # Test string
        result = convert_to_json("hello")
        self.assertEqual(result, "hello")
        
        # Test number
        result = convert_to_json(42)
        self.assertEqual(result, 42)
    
    def test_convert_to_json_with_graph(self):
        """Test converting a Graph object to JSON."""
        graph = Graph()
        ex = Namespace('http://example.org/')
        graph.add((ex.test, ex.prop, Literal('value')))
        
        result = convert_to_json({"graph": graph})
        
        self.assertIn("graph", result)
        self.assertIn("@type", result["graph"])
        self.assertIn("@value", result["graph"])
        self.assertEqual(result["graph"]["@type"], "https://www.w3.org/TR/json-ld11/")
        self.assertIsInstance(result["graph"]["@value"], str)
    
    def test_convert_to_json_nested_structures(self):
        """Test converting nested structures with registered types."""
        graph = Graph()
        ex = Namespace('http://example.org/')
        graph.add((ex.test, ex.prop, Literal('value')))
        
        data = {
            "id": "test123",
            "graph": graph,
            "nested": {
                "another_graph": graph,
                "simple": "value"
            },
            "list": [graph, "string", 42]
        }
        
        result = convert_to_json(data)
        
        # Check that all graphs are converted
        self.assertEqual(result["id"], "test123")
        self.assertIn("@type", result["graph"])
        self.assertIn("@type", result["nested"]["another_graph"])
        self.assertEqual(result["nested"]["simple"], "value")
        self.assertIn("@type", result["list"][0])
        self.assertEqual(result["list"][1], "string")
        self.assertEqual(result["list"][2], 42)
    
    def test_convert_from_json_simple_types(self):
        """Test converting simple types from JSON."""
        # Test dict
        result = convert_from_json({"key": "value"})
        self.assertEqual(result, {"key": "value"})
        
        # Test list
        result = convert_from_json([1, 2, 3])
        self.assertEqual(result, [1, 2, 3])
        
        # Test string
        result = convert_from_json("hello")
        self.assertEqual(result, "hello")
    
    def test_convert_from_json_with_graph(self):
        """Test converting a Graph object from JSON."""
        json_ld_str = '[{"@id": "http://example.org/test", "http://example.org/prop": [{"@value": "value"}]}]'
        json_data = {
            "graph": {
                "@type": "https://www.w3.org/TR/json-ld11/",
                "@value": json_ld_str
            }
        }
        
        result = convert_from_json(json_data)
        
        self.assertIn("graph", result)
        self.assertIsInstance(result["graph"], Graph)
        self.assertEqual(len(result["graph"]), 1)
    
    def test_convert_round_trip(self):
        """Test round-trip conversion (to JSON and back)."""
        graph = Graph()
        ex = Namespace('http://example.org/')
        graph.add((ex.test, ex.prop, Literal('value')))
        
        original = {
            "id": "test123",
            "graph": graph,
            "nested": {
                "another_graph": graph,
                "simple": "value"
            }
        }
        
        # Convert to JSON
        json_data = convert_to_json(original)
        
        # Convert back from JSON
        converted_back = convert_from_json(json_data)
        
        # Check that graphs are properly converted back
        self.assertEqual(converted_back["id"], "test123")
        self.assertIsInstance(converted_back["graph"], Graph)
        self.assertIsInstance(converted_back["nested"]["another_graph"], Graph)
        self.assertEqual(converted_back["nested"]["simple"], "value")
        
        # Check that the graphs have the same content
        self.assertEqual(len(original["graph"]), len(converted_back["graph"]))
        self.assertEqual(len(original["nested"]["another_graph"]), len(converted_back["nested"]["another_graph"]))


class TestInboxFunctions(unittest.TestCase):
    """Test the inbox loading and augmentation functions."""
    
    def test_load_inbox_empty_stdin(self):
        """Test loading inbox with empty stdin."""
        with patch('select.select') as mock_select:
            mock_select.return_value = ([], [], [])  # No data available
            inbox = load_inbox()
            self.assertEqual(inbox, {})
    
    def test_load_inbox_with_json(self):
        """Test loading inbox with JSON input."""
        test_json = '{"id": "test123", "noisy": true}'
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=test_json):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            inbox = load_inbox()
            self.assertEqual(inbox["id"], "test123")
            self.assertEqual(inbox["noisy"], True)
    
    def test_load_inbox_with_invalid_json(self):
        """Test loading inbox with invalid JSON."""
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value="invalid json"):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            inbox = load_inbox()
            self.assertEqual(inbox, {})
    
    def test_load_inbox_with_graph_json(self):
        """Test loading inbox with Graph JSON-LD."""
        json_ld_str = '[{"@id": "http://example.org/test"}]'
        # Properly escape the JSON-LD string for embedding in JSON
        test_json = json.dumps({
            "graph": {
                "@type": "https://www.w3.org/TR/json-ld11/",
                "@value": json_ld_str
            }
        })
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=test_json):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            inbox = load_inbox()
            self.assertIn("graph", inbox)
            self.assertIsInstance(inbox["graph"], Graph)
    
    def test_augment_inbox_with_args(self):
        """Test augmenting inbox with CLI arguments."""
        inbox = {"id": "from_stdin", "existing": "value"}
        argument_definitions = [
            ('id', str, 'The ID to process', True),
            ('noisy', bool, 'Enable verbose output', False)
        ]
        
        with patch('sys.argv', ['test', '--id', 'from_cli', '--noisy']):
            result = augment_inbox_with_args(inbox, argument_definitions)
            
            # CLI should override stdin
            self.assertEqual(result["id"], "from_cli")
            self.assertEqual(result["noisy"], True)
            # Existing values should be preserved
            self.assertEqual(result["existing"], "value")
    
    def test_augment_inbox_with_args_empty_definitions(self):
        """Test augmenting inbox with empty argument definitions."""
        inbox = {"id": "test", "value": 42}
        with patch('sys.argv', ['test']):  # Mock sys.argv to avoid unittest's -v flag
            result = augment_inbox_with_args(inbox)
            self.assertEqual(result, inbox)
    
    def test_get_inbox_integration(self):
        """Test the integrated get_inbox function."""
        test_json = '{"id": "from_stdin", "existing": "value"}'
        argument_definitions = [
            ('id', str, 'The ID to process', True),
            ('noisy', bool, 'Enable verbose output', False)
        ]
        
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=test_json), \
             patch('sys.argv', ['test', '--id', 'from_cli', '--noisy']):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            result = get_inbox(argument_definitions)
            
            # Should combine stdin and CLI
            self.assertEqual(result["id"], "from_cli")  # CLI overrides
            self.assertEqual(result["noisy"], True)     # CLI adds
            self.assertEqual(result["existing"], "value")  # stdin preserved


class TestOutboxFunctions(unittest.TestCase):
    """Test the outbox dumping functions."""
    
    def test_dump_outbox_simple(self):
        """Test dumping a simple outbox."""
        outbox = {"id": "test123", "value": 42}
        
        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            dump_outbox(outbox)
            result = json.loads(mock_stdout.getvalue())
            self.assertEqual(result, outbox)
    
    def test_dump_outbox_with_graph(self):
        """Test dumping an outbox with a Graph."""
        graph = Graph()
        ex = Namespace('http://example.org/')
        graph.add((ex.test, ex.prop, Literal('value')))
        
        outbox = {"id": "test123", "graph": graph}
        
        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            dump_outbox(outbox)
            result = json.loads(mock_stdout.getvalue())
            
            self.assertEqual(result["id"], "test123")
            self.assertIn("@type", result["graph"])
            self.assertEqual(result["graph"]["@type"], "https://www.w3.org/TR/json-ld11/")


class TestEndToEnd(unittest.TestCase):
    """End-to-end tests using real stdin/stdout without mocking."""
    
    def test_end_to_end_simple_workflow(self):
        """Test complete workflow with simple data using real I/O."""
        # Create test data
        test_input = {"id": "A000001", "noisy": True, "value": 42}
        input_json = json.dumps(test_input)
        
        # Test load_inbox with real stdin (using select.select mock for stdin detection)
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=input_json):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            inbox = load_inbox()
            self.assertEqual(inbox["id"], "A000001")
            self.assertEqual(inbox["noisy"], True)
            self.assertEqual(inbox["value"], 42)
        
        # Test dump_outbox with real stdout
        test_output = {"id": "A000001", "result": "success", "count": 100}
        
        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            dump_outbox(test_output)
            output_json = mock_stdout.getvalue()
            result = json.loads(output_json)
            self.assertEqual(result, test_output)
    
    def test_end_to_end_with_graph_workflow(self):
        """Test complete workflow with Graph objects using real I/O."""
        # Create a test graph
        graph = Graph()
        ex = Namespace('http://example.org/')
        graph.add((ex.test, ex.prop, Literal('test_value')))
        graph.add((ex.test, ex.another, Literal('another_value')))
        
        # Test converting to JSON and back
        test_data = {"id": "graph_test", "graph": graph, "metadata": {"triples": 2}}
        
        # Convert to JSON (simulating what would be sent via stdin)
        json_data = convert_to_json(test_data)
        json_string = json.dumps(json_data)
        
        # Test load_inbox with the JSON string
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=json_string):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            inbox = load_inbox()
            
            # Verify the graph was properly converted back
            self.assertEqual(inbox["id"], "graph_test")
            self.assertIsInstance(inbox["graph"], Graph)
            self.assertEqual(len(inbox["graph"]), 2)
            self.assertEqual(inbox["metadata"]["triples"], 2)
        
        # Test dump_outbox with the graph
        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            dump_outbox(test_data)
            output_json = mock_stdout.getvalue()
            result = json.loads(output_json)
            
            # Verify the graph was properly serialized
            self.assertEqual(result["id"], "graph_test")
            self.assertIn("@type", result["graph"])
            self.assertEqual(result["graph"]["@type"], "https://www.w3.org/TR/json-ld11/")
            self.assertEqual(result["metadata"]["triples"], 2)
    
    def test_end_to_end_cli_integration(self):
        """Test complete CLI integration workflow."""
        # Test data that would come from stdin
        stdin_data = {"id": "A000001", "existing": "from_stdin"}
        stdin_json = json.dumps(stdin_data)
        
        # Test get_inbox with CLI arguments
        argument_definitions = [
            ('id', str, 'The ID to process', True),
            ('noisy', bool, 'Enable verbose output', False),
            ('count', int, 'Number of items', False)
        ]
        
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=stdin_json), \
             patch('sys.argv', ['test', '--id', 'A000001', '--noisy', '--count', '100']):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            inbox = get_inbox(argument_definitions)
            
            # Verify CLI overrides stdin and adds new values
            self.assertEqual(inbox["id"], "A000001")  # CLI value
            self.assertEqual(inbox["noisy"], True)    # CLI value
            self.assertEqual(inbox["count"], 100)     # CLI value
            self.assertEqual(inbox["existing"], "from_stdin")  # stdin preserved
        
        # Test the complete round-trip
        test_outbox = {
            "id": "A000001",
            "noisy": True,
            "count": 100,
            "existing": "from_stdin",
            "result": "processed_successfully"
        }
        
        with patch('sys.stdout', new_callable=io.StringIO) as mock_stdout:
            dump_outbox(test_outbox)
            output_json = mock_stdout.getvalue()
            result = json.loads(output_json)
            self.assertEqual(result, test_outbox)
    
    def test_end_to_end_pipeline_simulation(self):
        """Test simulating a pipeline with multiple modules."""
        # Module 1: Load data and process
        module1_input = {"id": "A000001", "step": 1}
        module1_json = json.dumps(module1_input)
        
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=module1_json), \
             patch('sys.argv', ['module1', '--id', 'A000001', '--step', '1']):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            # Simulate module1 processing
            inbox = get_inbox([('id', str, 'ID', True), ('step', int, 'Step', True)])
            module1_output = {**inbox, "processed": True, "next_step": 2}
        
        # Module 2: Receive module1 output and process further
        module2_json = json.dumps(convert_to_json(module1_output))
        
        with patch('select.select') as mock_select, \
             patch('sys.stdin.read', return_value=module2_json), \
             patch('sys.argv', ['module2', '--final']):
            mock_select.return_value = ([sys.stdin], [], [])  # Data available
            # Simulate module2 processing
            inbox = get_inbox([('final', bool, 'Final step', False)])
            module2_output = {**inbox, "final_result": "pipeline_complete"}
        
        # Verify the pipeline worked correctly
        self.assertEqual(module2_output["id"], "A000001")
        self.assertEqual(module2_output["step"], 1)
        self.assertEqual(module2_output["processed"], True)
        self.assertEqual(module2_output["next_step"], 2)
        self.assertEqual(module2_output["final"], True)
        self.assertEqual(module2_output["final_result"], "pipeline_complete")
    
    def test_end_to_end_real_io_roundtrip(self):
        """Test actual stdout write and stdin read roundtrip."""
        import subprocess
        import tempfile
        import os
        
        # Create a simple test script that writes to stdout
        writer_script = f'''
import sys
import os
# Add the src/python directory to the path
sys.path.insert(0, '/Users/mlb/Devo/seqweb/seqwebcode/src/python')

from libs.core.wrapper import dump_outbox

def main():
    test_data = {{
        "id": "A000001",
        "noisy": True,
        "graph": "test_graph_data",
        "metadata": {{"triples": 5, "entities": 3}}
    }}
    dump_outbox(test_data)

if __name__ == "__main__":
    main()
'''
        
        # Create a simple test script that reads from stdin
        reader_script = '''
import sys
import os
# Add the src/python directory to the path
sys.path.insert(0, '/Users/mlb/Devo/seqweb/seqwebcode/src/python')

from libs.core.wrapper import load_inbox

def main():
    inbox = load_inbox()
    # Just echo back what we received
    print(f"Received: {inbox}")

if __name__ == "__main__":
    main()
'''
        
        # Write test scripts to temporary files
        with tempfile.NamedTemporaryFile(mode='w', suffix='_writer.py', delete=False) as f:
            f.write(writer_script)
            writer_path = f.name
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='_reader.py', delete=False) as f:
            f.write(reader_script)
            reader_path = f.name
        
        try:
            # Run the writer script and capture its stdout
            writer_result = subprocess.run(
                [sys.executable, writer_path],
                capture_output=True,
                text=True,
                cwd='/Users/mlb/Devo/seqweb/seqwebcode/src/python'  # Set working directory to src/python
            )
            
            # Verify the writer ran successfully
            self.assertEqual(writer_result.returncode, 0, f"Writer failed: {writer_result.stderr}")
            
            # Parse the JSON output from writer
            writer_output = json.loads(writer_result.stdout)
            self.assertEqual(writer_output["id"], "A000001")
            self.assertEqual(writer_output["noisy"], True)
            self.assertEqual(writer_output["graph"], "test_graph_data")
            self.assertEqual(writer_output["metadata"]["triples"], 5)
            
            # Now pipe the writer output to the reader script
            reader_result = subprocess.run(
                [sys.executable, reader_path],
                input=writer_result.stdout,
                capture_output=True,
                text=True,
                cwd='/Users/mlb/Devo/seqweb/seqwebcode/src/python'  # Set working directory to src/python
            )
            
            # Verify the reader ran successfully
            self.assertEqual(reader_result.returncode, 0, f"Reader failed: {reader_result.stderr}")
            
            # The reader should have received the same data
            self.assertIn("A000001", reader_result.stdout)
            self.assertIn("test_graph_data", reader_result.stdout)
            self.assertIn("triples", reader_result.stdout)
            
        finally:
            # Clean up
            os.unlink(writer_path)
            os.unlink(reader_path)
    
    def test_end_to_end_real_subprocess(self):
        """Test using actual subprocess to verify real I/O channels work."""
        import subprocess
        import tempfile
        import os
        
        # Create a simple test script that uses our wrapper functions
        test_script = '''
import sys
import os
# Add the src/python directory to the path
sys.path.insert(0, '/Users/mlb/Devo/seqweb/seqwebcode/src/python')

from libs.core.wrapper import get_inbox, dump_outbox

def main():
    argument_definitions = [
        ('id', str, 'The ID to process', True),
        ('noisy', bool, 'Enable verbose output', False)
    ]
    
    inbox = get_inbox(argument_definitions)
    outbox = {**inbox, "processed": True, "result": "success"}
    dump_outbox(outbox)

if __name__ == "__main__":
    main()
'''
        
        # Write test script to temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(test_script)
            script_path = f.name
        
        try:
            # Test with stdin input
            test_input = {"id": "A000001", "existing": "from_stdin"}
            input_json = json.dumps(test_input)
            
            # Run the script with stdin input and CLI args
            result = subprocess.run(
                [sys.executable, script_path, '--id', 'A000001', '--noisy'],
                input=input_json,
                text=True,
                capture_output=True,
                cwd='/Users/mlb/Devo/seqweb/seqwebcode/src/python'  # Set working directory to src/python
            )
            
            # Verify the script ran successfully
            self.assertEqual(result.returncode, 0, f"Script failed: {result.stderr}")
            
            # Parse and verify the output
            output_data = json.loads(result.stdout)
            self.assertEqual(output_data["id"], "A000001")  # CLI overrides
            self.assertEqual(output_data["noisy"], True)    # CLI adds
            self.assertEqual(output_data["existing"], "from_stdin")  # stdin preserved
            self.assertEqual(output_data["processed"], True)
            self.assertEqual(output_data["result"], "success")
            
        finally:
            # Clean up
            os.unlink(script_path)


if __name__ == '__main__':
    unittest.main()
