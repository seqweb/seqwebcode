#!/usr/bin/env python3
"""
Run command for SeqWeb CLI - executes programs with proper environment setup
"""

import os
import subprocess
import sys
from pathlib import Path
from app.cli.base_command import BaseCommand


class RunCommand(BaseCommand):
    """Execute programs with proper SeqWeb environment setup"""
    
    has_subcommands: bool = False
    
    @property
    def name(self) -> str:
        return "run"
    
    @property
    def description(self) -> str:
        return "Execute a program with proper SeqWeb environment setup"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev run <file_path> [args...]
  Execute a program with proper SeqWeb environment setup.
  
  The command automatically detects the file type and runs it appropriately:
  - .py files: Executed with Python 3
  - Other file types: Currently unsupported
  
  The target program will have full access to SeqWeb functionality including:
  - seqvar functions (get, set, etc.)
  - All SeqWeb Python packages
  - Proper Python path configuration
  
  Examples:
    seqwebdev run my_script.py
    seqwebdev run my_script.py arg1 arg2
    seqwebdev run /path/to/script.py --noisy"""
    
    
    def do_command(self):
        """Execute the run command"""
        # Manual argument parsing to avoid consuming target program arguments
        if not self.args:
            print("❌ No file path provided")
            sys.exit(1)
        
        file_path = Path(self.args[0])
        program_args = self.args[1:]  # All remaining arguments go to the target program
        
        # Check if file exists
        if not file_path.exists():
            print(f"❌ File not found: {file_path}")
            sys.exit(1)
        
        # Check if it's a file (not directory)
        if not file_path.is_file():
            print(f"❌ Path is not a file: {file_path}")
            sys.exit(1)
        
        # Eavesdrop on arguments to detect --noisy flag
        noisy = '--noisy' in program_args
        
        # Get file extension to determine how to run it
        file_extension = file_path.suffix.lower()
        
        if file_extension == '.py':
            self._run_python_file(file_path, program_args, noisy)
        else:
            print(f"❌ Unsupported file type: {file_extension}")
            print("   Currently only .py files are supported")
            sys.exit(1)
    
    def _run_python_file(self, file_path: Path, program_args: list, noisy: bool = False):
        """Run a Python file with proper SeqWeb environment setup"""
        try:
            # Build the command to run
            cmd = [sys.executable, str(file_path)] + program_args
            
            # Set up environment with proper Python path
            env = os.environ.copy()
            # Add the current Python path to PYTHONPATH for subprocesses
            python_path = sys.path[0]  # This is already set up by _seqwebdev
            if 'PYTHONPATH' in env:
                env['PYTHONPATH'] = f"{python_path}:{env['PYTHONPATH']}"
            else:
                env['PYTHONPATH'] = python_path
            
            if noisy:
                print(f"Running Python file: {file_path}")
                print(f"  Python executable: {sys.executable}")
                print(f"  Arguments: {program_args if program_args else '(none)'}")
                print(f"  PYTHONPATH: {env.get('PYTHONPATH', 'not set')}")
                print("-" * 50)
            
            # Run the subprocess with proper environment
            result = subprocess.run(
                cmd,
                cwd=Path.cwd(),  # Run from current working directory
                env=env,  # Use modified environment
                check=False  # Don't raise exception on non-zero exit code
            )
            
            if noisy:
                print("-" * 50)
                if result.returncode == 0:
                    print(f"Program completed successfully (exit code: {result.returncode})")
                else:
                    print(f"⚠️  Program terminated with exit code: {result.returncode}")
            
            return result.returncode
            
        except FileNotFoundError:
            print(f"❌ Python executable not found: {sys.executable}")
            sys.exit(1)
        except PermissionError:
            print(f"❌ Permission denied running file: {file_path}")
            sys.exit(1)
        except Exception as e:
            print(f"❌ Error running Python file: {e}")
            sys.exit(1)
