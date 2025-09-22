"""
Wrapper utilities for polyglot pipeline modules.

This module provides standardized inbox/outbox handling for shell wrappers,
including typed-value registry for converting between native objects and
JSON-serializable formats for cross-language compatibility.
"""

import json
import sys
from typing import Dict, Any, Callable, Type, Union, List, Tuple
from rdflib import Graph


# Typed-value registry for polyglot pipeline compatibility
# Maps external type IDs to internal type information and conversion functions
TYPED_VALUE_REGISTRY = {
    "https://www.w3.org/TR/json-ld11/": {
        "internal_type": Graph,
        "to_json": lambda graph: graph.serialize(format='json-ld'),
        "from_json": lambda json_ld_str: Graph().parse(data=json_ld_str, format='json-ld')
    }
}


def get_external_type_id(internal_type: Type) -> str:
    """
    Get the external type ID for a given internal type.
    
    Args:
        internal_type: The internal Python type
        
    Returns:
        External type ID string, or None if not found
    """
    for external_id, type_info in TYPED_VALUE_REGISTRY.items():
        if type_info["internal_type"] == internal_type:
            return external_id
    return None


def get_internal_type(external_type_id: str) -> Type:
    """
    Get the internal type for a given external type ID.
    
    Args:
        external_type_id: The external type ID string
        
    Returns:
        Internal Python type, or None if not found
    """
    if external_type_id in TYPED_VALUE_REGISTRY:
        return TYPED_VALUE_REGISTRY[external_type_id]["internal_type"]
    return None


def get_to_json_function(external_type_id: str) -> Callable:
    """
    Get the to-JSON conversion function for a given external type ID.
    
    Args:
        external_type_id: The external type ID string
        
    Returns:
        Function that converts internal type to JSON-serializable format
    """
    if external_type_id in TYPED_VALUE_REGISTRY:
        return TYPED_VALUE_REGISTRY[external_type_id]["to_json"]
    return None


def get_from_json_function(external_type_id: str) -> Callable:
    """
    Get the from-JSON conversion function for a given external type ID.
    
    Args:
        external_type_id: The external type ID string
        
    Returns:
        Function that converts JSON-serializable format to internal type
    """
    if external_type_id in TYPED_VALUE_REGISTRY:
        return TYPED_VALUE_REGISTRY[external_type_id]["from_json"]
    return None


def is_registered_type(obj: Any) -> bool:
    """
    Check if an object's type is registered in the typed-value registry.
    
    Args:
        obj: Object to check
        
    Returns:
        True if the object's type is registered, False otherwise
    """
    return get_external_type_id(type(obj)) is not None


def convert_to_json(obj: Any) -> Union[Dict[str, Any], Any]:
    """
    Convert a registered internal type object to JSON-serializable format.
    
    This function recursively processes dictionaries, lists, and other containers
    to find and convert any registered internal type objects.
    
    Args:
        obj: Object to convert (may be a container with registered objects)
        
    Returns:
        JSON-serializable representation of the object
    """
    # Handle dictionaries
    if isinstance(obj, dict):
        result = {}
        for key, value in obj.items():
            result[key] = convert_to_json(value)
        return result
    
    # Handle lists
    elif isinstance(obj, list):
        return [convert_to_json(item) for item in obj]
    
    # Handle registered types
    elif is_registered_type(obj):
        external_type_id = get_external_type_id(type(obj))
        to_json_func = get_to_json_function(external_type_id)
        if to_json_func:
            return {
                "@type": external_type_id,
                "@value": to_json_func(obj)
            }
    
    # Return as-is for other types
    return obj


def convert_from_json(json_obj: Any) -> Any:
    """
    Convert a JSON-serializable format back to internal type object.
    
    This function recursively processes dictionaries, lists, and other containers
    to find and convert any @type/@value structures back to internal objects.
    
    Args:
        json_obj: JSON-serializable object (may contain @type/@value structure)
        
    Returns:
        Internal type object if conversion is possible, original object otherwise
    """
    # Handle dictionaries
    if isinstance(json_obj, dict):
        # Check if this is a @type/@value structure
        if "@type" in json_obj and "@value" in json_obj:
            external_type_id = json_obj["@type"]
            from_json_func = get_from_json_function(external_type_id)
            if from_json_func:
                try:
                    return from_json_func(json_obj["@value"])
                except Exception:
                    # If conversion fails, return original object
                    return json_obj
        
        # Otherwise, recursively process dictionary values
        result = {}
        for key, value in json_obj.items():
            result[key] = convert_from_json(value)
        return result
    
    # Handle lists
    elif isinstance(json_obj, list):
        return [convert_from_json(item) for item in json_obj]
    
    # Return as-is for other types
    return json_obj


def load_inbox() -> Dict[str, Any]:
    """
    Load JSON from stdin and convert to native objects.
    
    This function handles the non-blocking reading of JSON from stdin
    and converts any @type/@value structures back to native objects
    using the typed-value registry for polyglot pipeline compatibility.
    
    Returns:
        Dict containing the loaded and converted inbox
    """
    import select
    
    # Check if stdin has data available without blocking
    ready_to_read, _, _ = select.select([sys.stdin], [], [], 0)
    if ready_to_read:
        stdin_input = sys.stdin.read().strip()
        if stdin_input:
            try:
                inbox = json.loads(stdin_input)
            except json.JSONDecodeError:
                # If stdin is not valid JSON, treat as empty
                inbox = {}
        else:
            inbox = {}
    else:
        # No data available on stdin
        inbox = {}
    
    # Convert any @type/@value structures back to native objects
    return convert_from_json(inbox)


def augment_inbox_with_args(inbox: Dict[str, Any], argument_definitions: List[Tuple[str, Type, str, bool]] = None) -> Dict[str, Any]:
    """
    Augment an inbox with command-line arguments using destructuring merge.
    
    This function parses command-line arguments according to the provided
    definitions and merges them into the inbox using destructuring pattern.
    CLI arguments override inbox values on conflict.
    
    Args:
        inbox: Base inbox dictionary to augment
        argument_definitions: List of (name, type, help, required) tuples.
                             Defaults to empty list if not provided.
    
    Returns:
        Dict containing the merged inbox with CLI arguments
    """
    import argparse
    from typing import Type
    
    if argument_definitions is None:
        argument_definitions = []
    
    # Parse command-line arguments
    parser = argparse.ArgumentParser()
    
    for name, arg_type, help_text, required in argument_definitions:
        if arg_type == bool:
            parser.add_argument(f'--{name}', action='store_true', help=help_text)
        else:
            parser.add_argument(f'--{name}', type=arg_type, required=required, help=help_text)
    
    args = parser.parse_args()
    
    # Convert args to dict
    cli_args = {}
    for name, arg_type, _, _ in argument_definitions:
        value = getattr(args, name)
        if value is not None:
            cli_args[name] = value
    
    # Merge CLI args into inbox using destructuring (CLI overrides inbox)
    return {**inbox, **cli_args}


def get_inbox(argument_definitions: List[Tuple[str, Type, str, bool]] = None) -> Dict[str, Any]:
    """
    Get a complete inbox by loading from stdin and augmenting with CLI arguments.
    
    This function combines load_inbox() and augment_inbox_with_args() to provide
    a complete inbox building solution for polyglot pipeline wrappers.
    
    Args:
        argument_definitions: List of (name, type, help, required) tuples.
                             Defaults to empty list if not provided.
    
    Returns:
        Dict containing the complete inbox with stdin data and CLI arguments
    """
    # Load inbox from stdin and convert native objects
    inbox = load_inbox()
    
    # Augment with CLI arguments using destructuring merge
    return augment_inbox_with_args(inbox, argument_definitions)


def dump_outbox(outbox: Dict[str, Any]) -> None:
    """
    Convert an outbox to dumpable JSON and write it to stdout.
    
    This function handles the conversion of native objects in the outbox
    to JSON-serializable formats using the typed-value registry, then
    dumps the result as JSON to stdout for polyglot pipeline compatibility.
    
    Args:
        outbox: Output box from core function (may contain native objects)
    """
    # Convert native objects to JSON-serializable formats
    json_outbox = convert_to_json(outbox)
    
    # Dump as JSON to stdout
    json.dump(json_outbox, sys.stdout)
