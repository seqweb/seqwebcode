#!/usr/bin/env python3
"""
Pipeline execution framework for SeqWeb fabricators.

This module implements the run_pipeline function that orchestrates
polyglot processing pipelines composed of individual modules.
"""

from typing import Callable, Dict, List, Any


def run_pipeline(
    modules: List[Callable[[Dict[str, Any]], Dict[str, Any]]],
    initial_inbox: Dict[str, Any]
) -> Dict[str, Any]:
    """
    Run a sequence of modules on an inbox, using the box-then-kwargs pattern.
    
    Each module takes an inbox (dictionary) and returns an outbox (dictionary).
    The outbox from one module becomes the inbox for the next module.
    
    Args:
        modules: List of module functions to execute in sequence
        initial_inbox: Starting inbox for the first module
        
    Returns:
        Final outbox from the last module in the pipeline
        
    Example:
        >>> from .modules import mod0
        >>> initial_inbox = {'id': 'A000001', 'noisy': False}
        >>> result = run_pipeline([mod0], initial_inbox)
        >>> print(result)
        {'id': 'A000001', 'noisy': False}
    """
    if not modules:
        return initial_inbox
    
    # Start with the initial inbox
    current_box = initial_inbox.copy()
    
    # Process through each module in sequence
    for i, module in enumerate(modules):
        try:
            # Call the module using the box-then-kwargs pattern
            current_box = module(current_box, **current_box)
            
            # Ensure we always have a dictionary back
            if not isinstance(current_box, dict):
                raise TypeError(f"Module {i} ({module.__name__}) must return a dictionary")
                
        except Exception as e:
            # For now, let Python fail naturally as specified
            # In the future, we might want more sophisticated error handling
            raise RuntimeError(f"Pipeline failed at module {i} ({module.__name__}): {e}") from e
    
    return current_box


if __name__ == "__main__":
    # Simple test when run directly
    test_inbox = {'id': 'A000001', 'noisy': True}
    print("Testing pipeline with empty module list:")
    result = run_pipeline([], test_inbox)
    print(f"Result: {result}")
    
    print("\nTesting pipeline with test module:")
    def test_module(inbox):
        print(f"Test module processing: {inbox}")
        return inbox
    
    result = run_pipeline([test_module], test_inbox)
    print(f"Result: {result}")
