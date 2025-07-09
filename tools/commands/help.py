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
        return """Usage: ./seqweb help [command]
  Show help information for all commands or a specific command.
  If no command is specified, lists all available commands.
  If a command is specified, shows detailed help for that command."""
    
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
        print("🧬 SeqWeb Development Environment")
        print()
        print("📜 Available commands:")
        print()
        
        commands = get_available_commands()
        for cmd_name, cmd_class in sorted(commands.items()):
            cmd = cmd_class()
            print(f"  {cmd_name}: {cmd.description}")
        
        print()
        print("💡 Run './seqweb help <command>' for detailed help on a specific command.")
    
    def _show_command_help(self, command_name: str):
        """Show help for a specific command"""
        try:
            commands = get_available_commands()
            if command_name not in commands:
                print(f"❌ Unknown command: {command_name}")
                return
            
            cmd_class = commands[command_name]
            cmd = cmd_class()
            
            print(f"🧬 Help for command: {command_name}")
            print()
            print(f"  {cmd.help_text}")
            print()
            
        except Exception as e:
            print(f"❌ Error showing help for '{command_name}': {e}") 