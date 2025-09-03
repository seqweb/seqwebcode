#!/usr/bin/env python3
"""
Run command for SeqWeb CLI - executes programs with proper environment setup
"""

import os
import subprocess
import sys
from pathlib import Path
from app.cli.chain.base_command import BaseCommand


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
    
    def add_arguments(self, parser):
        parser.add_argument(
            'file_path',
            help='Path to the program file to execute'
        )
        parser.add_argument(
            '--noisy',
            action='store_true',
            help='Enable verbose output including progress messages'
        )
        # Allow unknown arguments to be passed to the target program
        parser.add_argument(
            'args',
            nargs='*',
            help='Arguments to pass to the target program'
        )
    
    def do_command(self):
        """Execute the run command"""
        # Use argparse for argument parsing
        parser = self.create_parser()
        args = parser.parse_args(self.args)
        
        file_path = Path(args.file_path)
        
        # Check if file exists
        if not file_path.exists():
            print(f"❌ File not found: {file_path}")
            sys.exit(1)
        
        # Check if it's a file (not directory)
        if not file_path.is_file():
            print(f"❌ Path is not a file: {file_path}")
            sys.exit(1)
        
        # Get file extension to determine how to run it
        file_extension = file_path.suffix.lower()
        
        if file_extension == '.py':
            self._run_python_file(file_path, args.args, args.noisy)
        else:
            print(f"❌ Unsupported file type: {file_extension}")
            print("   Currently only .py files are supported")
            sys.exit(1)
    
    def _run_python_file(self, file_path: Path, program_args: list, noisy: bool = False):
        """Run a Python file with proper SeqWeb environment setup"""
        try:
            # Get the absolute path to the Python source directory
            # This is the directory containing seqvar and other SeqWeb packages
            cli_dir = Path(__file__).parent
            python_src_dir = cli_dir.parent.parent.parent  # Go up from chain/seqwebdev/ to src/python/
            
            # Set up environment variables for the subprocess
            env = os.environ.copy()
            
            # Add the Python source directory to PYTHONPATH
            current_pythonpath = env.get('PYTHONPATH', '')
            if current_pythonpath:
                new_pythonpath = f"{python_src_dir}:{current_pythonpath}"
            else:
                new_pythonpath = str(python_src_dir)
            
            env['PYTHONPATH'] = new_pythonpath
            
            # Ensure SEQWEBDEV_HOME is available
            if not env.get('SEQWEBDEV_HOME'):
                print("❌ SEQWEBDEV_HOME environment variable is not set")
                print("   Please set SEQWEBDEV_HOME to your SeqWeb development home directory")
                sys.exit(1)
            
            # Build the command to run
            cmd = [sys.executable, str(file_path)] + program_args
            
            if noisy:
                print(f"Running Python file: {file_path}")
                print(f"  Python executable: {sys.executable}")
                print(f"  Python path: {new_pythonpath}")
                print(f"  Arguments: {program_args if program_args else '(none)'}")
                print("-" * 50)
            
            # Run the subprocess
            result = subprocess.run(
                cmd,
                env=env,
                cwd=Path.cwd(),  # Run from current working directory to avoid path issues
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
