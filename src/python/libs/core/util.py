#!/usr/bin/env python3
"""
Core utility functions for the SeqWeb project.

This module provides common utility functions used across the SeqWeb codebase.
"""

import inspect
from typing import List


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
