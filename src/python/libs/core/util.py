#!/usr/bin/env python3
"""
Core utility functions for the SeqWeb project.

This module provides common utility functions used across the SeqWeb codebase.
"""

import inspect
import json
import sys
import argparse
from typing import List, Dict, Any, Tuple, Type


def get_call_trace() -> str:
    """
    Get a description of the call stack, with dotted import path for the root.
    
    Returns:
        String describing the call stack in the format "module.function>function>..."
    """
    call_stack = []
    stack_frames = []
    
    # Collect all frames first
    for frame_info in inspect.stack():
        # Skip this function and any internal Python frames
        if frame_info.function in ['get_call_trace', '<module>']:
            continue
        stack_frames.append(frame_info)
    
    # Reverse to get call order (outermost first)
    stack_frames.reverse()
    
    # Build call stack with dotted name for the first (outermost) function
    for i, frame_info in enumerate(stack_frames):
        if i == 0:  # First (outermost) function gets dotted name
            module_name = frame_info.frame.f_globals.get('__name__', 'unknown')
            function_name = frame_info.function
            
            if module_name != 'unknown' and module_name != '__main__':
                # Use the full module name as import path
                call_stack.append(f"{module_name}.{function_name}")
            elif module_name == '__main__':
                # For __main__, try to construct the actual file path
                filename = frame_info.frame.f_code.co_filename
                if 'seqwebcode/src/python/app/fab' in filename:
                    # Extract the module path from the file path
                    parts = filename.split('seqwebcode/src/python/')[-1].replace('.py', '').split('/')
                    module_path = '.'.join(parts)
                    call_stack.append(f"{module_path}.{function_name}")
                else:
                    call_stack.append(f"{module_name}.{function_name}")
            else:
                call_stack.append(function_name)
        else:
            call_stack.append(frame_info.function)
    
    return ">".join(call_stack)


def build_inbox_from_args(argument_definitions: List[Tuple[str, Type, str, bool]]) -> Dict[str, Any]:
    """
    Build an inbox from stdin (JSON) and command-line arguments.
    
    This function implements the unified wrapper contract:
    1. Read JSON from stdin (defaults to empty dict if empty)
    2. Parse command-line arguments according to definitions
    3. Merge CLI args into inbox (CLI overrides stdin)
    4. Return the merged inbox
    
    Args:
        argument_definitions: List of (name, type, help, required) tuples
        
    Returns:
        Dict containing the merged inbox
    """
    # Read JSON from stdin
    stdin_input = sys.stdin.read().strip()
    if stdin_input:
        try:
            inbox = json.loads(stdin_input)
        except json.JSONDecodeError:
            # If stdin is not valid JSON, treat as empty
            inbox = {}
    else:
        inbox = {}
    
    # Parse command-line arguments
    parser = argparse.ArgumentParser()
    
    for name, arg_type, help_text, required in argument_definitions:
        if arg_type == bool:
            parser.add_argument(f'--{name}', action='store_true', help=help_text)
        else:
            parser.add_argument(f'--{name}', type=arg_type, required=required, help=help_text)
    
    args = parser.parse_args()
    
    # Convert args to dict and merge with inbox
    cli_args = {}
    for name, arg_type, _, _ in argument_definitions:
        value = getattr(args, name)
        if value is not None:
            cli_args[name] = value
    
    # Merge CLI args into inbox (CLI overrides stdin)
    inbox.update(cli_args)
    
    return inbox
