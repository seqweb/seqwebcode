#!/usr/bin/env python3
"""
Help command for SeqWeb CLI
"""

from lib.base import BaseCommand
from lib.registry import get_available_commands


class HelpCommand(BaseCommand):
    """Show help information for SeqWeb commands"""
    
    @property
    def name(self) -> str:
        return "help"
    
    @property
    def description(self) -> str:
        return "Show help information for SeqWeb commands"
    
    @property
    def help_text(self) -> str:
        return """Usage: seqwebdev help [command]
  Show help information for all commands or a specific command.
     If a command is specified, shows detailed help for that command.
     Otherwise, lists all available commands."""
    
    def add_arguments(self, parser):
        parser.add_argument(
            "command", 
            nargs="?", 
            help="Command to show help for"
        )
    
    def execute(self, args):
        if args.command:
            self._show_command_help(args.command)
        else:
            self._show_all_commands()
    
    def _show_all_commands(self):
        """Show all available commands"""
        print("SeqWeb development CLI commands:")
        commands = get_available_commands()
        
        # Find the longest command name for proper column alignment
        max_name_length = (max(len(cmd_name) 
                              for cmd_name in commands.keys()) 
                          if commands else 0)
        
        # Print commands in two columns
        for cmd_name, cmd_class in sorted(commands.items()):
            cmd = cmd_class()
            # Format: "  cmd_name    description" with proper spacing
            padding = " " * (max_name_length - len(cmd_name) + 2)
            print(f"  {cmd_name}{padding}"
                  f"- {cmd.description}")
        
        print(" Run 'seqwebdev help <command>' for detailed help on specific commands.")
    
    def _show_command_help(self, command_name: str):
        """Show help for a specific command"""
        try:
            commands = get_available_commands()
            if command_name not in commands:
                print(f"❌ Unknown command: {command_name}")
                return
            
            cmd_class = commands[command_name]
            cmd = cmd_class()
            
            print(f" Help for command: {command_name}")
            print(f"  {cmd.help_text}")
            
        except Exception as e:
            print(f"❌ Error showing help for '{command_name}': {e}") 