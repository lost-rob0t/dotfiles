{ lib, pkgs, config, ... }:

{
  options = with lib; {
    services.llm = {
      enable = mkEnableOption "Enable LLM and MCP services";
      mcpServers = {
        filesystem = mkEnableOption "Enable filesystem MCP server";
        mpris = mkEnableOption "Enable MPRIS media player control MCP server";
      };
    };
  };

  config = with lib; mkIf config.services.llm.enable {
    # Install required packages for MCP servers
    home.packages = with pkgs; [
      # Node.js for filesystem MCP server
      nodejs_22

      # Audio and media control utilities
      playerctl
      pavucontrol
      pulseaudio

      # Speech processing tools for voice interface
      espeak-ng
      sox
      alsa-utils

      # Additional utilities
      jq
      curl

      # Python with required packages for MCP development
      (python3.withPackages (ps: with ps; [
        dbus-python
        pygobject3
        requests
        websockets
        uvloop
      ]))
    ];

    # Create MCP servers directory
    home.file.".local/share/mcp-servers/.keep".text = "";

    # Custom MPRIS MCP Server
    home.file.".local/share/mcp-servers/mpris-server.py" = {
      text = ''
#!/usr/bin/env python3
"""
MPRIS MCP Server - Bridge between Model Context Protocol and MPRIS media players
Provides AI agents with media player control capabilities
"""

import asyncio
import json
import sys
import dbus
from dbus.mainloop.glib import DBusGMainLoop
import subprocess
import logging
from typing import Dict, List, Optional, Any

# MCP Protocol imports (placeholder - will be replaced with actual MCP Python SDK)
class MCPServer:
    def __init__(self):
        self.tools = {}
        self.resources = {}

    def tool(self, name: str, description: str):
        def decorator(func):
            self.tools[name] = {
                'function': func,
                'description': description,
                'name': name
            }
            return func
        return decorator

    async def run(self):
        """Main server loop - placeholder for MCP protocol implementation"""
        while True:
            try:
                # Read MCP messages from stdin
                line = await asyncio.get_event_loop().run_in_executor(None, sys.stdin.readline)
                if not line:
                    break

                message = json.loads(line.strip())
                response = await self.handle_message(message)

                if response:
                    print(json.dumps(response))
                    sys.stdout.flush()

            except Exception as e:
                logging.error(f"Error processing message: {e}")
                break

    async def handle_message(self, message: Dict) -> Optional[Dict]:
        """Handle incoming MCP messages"""
        method = message.get('method')
        params = message.get('params', {})

        if method == 'tools/list':
            return {
                'id': message.get('id'),
                'result': {
                    'tools': [
                        {
                            'name': name,
                            'description': info['description']
                        } for name, info in self.tools.items()
                    ]
                }
            }
        elif method == 'tools/call':
            tool_name = params.get('name')
            args = params.get('arguments', {})

            if tool_name in self.tools:
                try:
                    result = await self.tools[tool_name]['function'](**args)
                    return {
                        'id': message.get('id'),
                        'result': {'content': [{'type': 'text', 'text': str(result)}]}
                    }
                except Exception as e:
                    return {
                        'id': message.get('id'),
                        'error': {'code': -32000, 'message': str(e)}
                    }

        return None

class MPRISController:
    def __init__(self):
        DBusGMainLoop(set_as_default=True)
        self.bus = dbus.SessionBus()

    def get_players(self) -> List[str]:
        """Get list of available MPRIS players"""
        try:
            result = subprocess.run(['playerctl', '-l'], capture_output=True, text=True)
            return result.stdout.strip().split('\n') if result.stdout.strip() else []
        except:
            return []

    def get_current_player(self) -> Optional[str]:
        """Get currently active player"""
        players = self.get_players()
        return players[0] if players else None

    def execute_playerctl(self, command: str, player: Optional[str] = None) -> str:
        """Execute playerctl command"""
        try:
            cmd = ['playerctl']
            if player:
                cmd.extend(['-p', player])
            cmd.extend(command.split())

            result = subprocess.run(cmd, capture_output=True, text=True)
            return result.stdout.strip() if result.returncode == 0 else f"Error: {result.stderr.strip()}"
        except Exception as e:
            return f"Error executing command: {str(e)}"

# Initialize MCP server and MPRIS controller
server = MCPServer()
mpris = MPRISController()

# MCP Tools for media control
@server.tool("play", "Start or resume media playback")
async def play_media(player: Optional[str] = None):
    """Play media on specified player or current player"""
    return mpris.execute_playerctl("play", player)

@server.tool("pause", "Pause media playback")
async def pause_media(player: Optional[str] = None):
    """Pause media on specified player or current player"""
    return mpris.execute_playerctl("pause", player)

@server.tool("play_pause", "Toggle play/pause state")
async def play_pause_media(player: Optional[str] = None):
    """Toggle play/pause on specified player or current player"""
    return mpris.execute_playerctl("play-pause", player)

@server.tool("stop", "Stop media playback")
async def stop_media(player: Optional[str] = None):
    """Stop media on specified player or current player"""
    return mpris.execute_playerctl("stop", player)

@server.tool("next", "Skip to next track")
async def next_track(player: Optional[str] = None):
    """Skip to next track on specified player or current player"""
    return mpris.execute_playerctl("next", player)

@server.tool("previous", "Go to previous track")
async def previous_track(player: Optional[str] = None):
    """Go to previous track on specified player or current player"""
    return mpris.execute_playerctl("previous", player)

@server.tool("get_status", "Get current playback status and track information")
async def get_status(player: Optional[str] = None):
    """Get detailed playback status"""
    try:
        status = mpris.execute_playerctl("status", player)
        title = mpris.execute_playerctl("metadata title", player)
        artist = mpris.execute_playerctl("metadata artist", player)
        album = mpris.execute_playerctl("metadata album", player)

        return f"Status: {status}\nTitle: {title}\nArtist: {artist}\nAlbum: {album}"
    except Exception as e:
        return f"Error getting status: {str(e)}"

@server.tool("set_volume", "Set playback volume")
async def set_volume(volume: float, player: Optional[str] = None):
    """Set volume (0.0 to 1.0)"""
    if not 0.0 <= volume <= 1.0:
        return "Volume must be between 0.0 and 1.0"
    return mpris.execute_playerctl(f"volume {volume}", player)

@server.tool("get_volume", "Get current volume")
async def get_volume(player: Optional[str] = None):
    """Get current volume level"""
    return mpris.execute_playerctl("volume", player)

@server.tool("list_players", "List available media players")
async def list_players():
    """List all available MPRIS media players"""
    players = mpris.get_players()
    return f"Available players: {', '.join(players)}" if players else "No media players found"

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    try:
        asyncio.run(server.run())
    except KeyboardInterrupt:
        logging.info("MPRIS MCP Server shutting down")
    except Exception as e:
        logging.error(f"Server error: {e}")
        sys.exit(1)
      '';
      executable = true;
    };

    # Voice interface helper script
    home.file.".local/bin/llm-voice-capture" = {
      text = ''
#!/usr/bin/env bash
# Voice capture and processing script for LLM desktop assistant

TEMP_DIR="/tmp/llm-voice"
AUDIO_FILE="$TEMP_DIR/voice_input.wav"
TEXT_FILE="$TEMP_DIR/voice_text.txt"

mkdir -p "$TEMP_DIR"

# Function to speak text
speak_text() {
    echo "$1" | espeak-ng -s 150 -v en 2>/dev/null &
}

# Function to show notification
notify_user() {
    if command -v dunstify >/dev/null; then
        dunstify "LLM Assistant" "$1" -t 3000
    elif command -v notify-send >/dev/null; then
        notify-send "LLM Assistant" "$1"
    fi
}

# Record audio (3 seconds by default)
record_audio() {
    local duration=''${1:-3}
    notify_user "🎤 Listening..."
    speak_text "Listening"

    # Record audio using arecord
    timeout "$duration" arecord -f cd -t wav "$AUDIO_FILE" 2>/dev/null

    if [ ! -s "$AUDIO_FILE" ]; then
        notify_user "❌ No audio recorded"
        return 1
    fi

    return 0
}

# Process voice command (placeholder - would integrate with speech recognition)
process_command() {
    # For now, just simulate with a simple command
    # In a real implementation, this would use speech-to-text service

    notify_user "🤔 Processing..."

    # Placeholder: read a simple text command
    echo "What would you like me to do? (type command):"
    read -r user_command

    # Send to emacs/MCP for processing
    if command -v emacsclient >/dev/null; then
        emacsclient --eval "(message \"Voice command: $user_command\")" 2>/dev/null
    fi

    notify_user "✅ Command processed: $user_command"
    speak_text "Command processed"
}

# Main execution
case "''${1:-capture}" in
    "capture")
        if record_audio 3; then
            process_command
        fi
        ;;
    "test-speak")
        speak_text "LLM assistant is working correctly"
        ;;
    "test-notify")
        notify_user "LLM assistant notification test"
        ;;
    *)
        echo "Usage: $0 [capture|test-speak|test-notify]"
        exit 1
        ;;
esac

# Cleanup
rm -f "$AUDIO_FILE" "$TEXT_FILE"
      '';
      executable = true;
    };

    # MCP server startup script
    home.file.".local/bin/start-mcp-servers" = {
      text = ''
#!/usr/bin/env bash
# Start MCP servers for LLM desktop assistant

MCP_DIR="$HOME/.local/share/mcp-servers"
LOG_DIR="$HOME/.local/share/mcp-logs"

mkdir -p "$LOG_DIR"

# Function to check if server is running
is_server_running() {
    pgrep -f "$1" >/dev/null
}

# Start filesystem MCP server
start_filesystem_server() {
    if ! is_server_running "server-filesystem"; then
        echo "Starting filesystem MCP server..."
        nohup npx @modelcontextprotocol/server-filesystem "$HOME" > "$LOG_DIR/filesystem-server.log" 2>&1 &
        echo "Filesystem MCP server started (PID: $!)"
    else
        echo "Filesystem MCP server already running"
    fi
}

# Start MPRIS MCP server
start_mpris_server() {
    if ! is_server_running "mpris-server.py"; then
        echo "Starting MPRIS MCP server..."
        nohup python3 "$MCP_DIR/mpris-server.py" > "$LOG_DIR/mpris-server.log" 2>&1 &
        echo "MPRIS MCP server started (PID: $!)"
    else
        echo "MPRIS MCP server already running"
    fi
}

# Stop all MCP servers
stop_servers() {
    echo "Stopping MCP servers..."
    pkill -f "server-filesystem" && echo "Filesystem server stopped"
    pkill -f "mpris-server.py" && echo "MPRIS server stopped"
}

case "''${1:-start}" in
    "start")
        start_filesystem_server
        start_mpris_server
        ;;
    "stop")
        stop_servers
        ;;
    "restart")
        stop_servers
        sleep 2
        start_filesystem_server
        start_mpris_server
        ;;
    "status")
        echo "Checking MCP server status..."
        if is_server_running "server-filesystem"; then
            echo "✅ Filesystem MCP server: Running"
        else
            echo "❌ Filesystem MCP server: Not running"
        fi

        if is_server_running "mpris-server.py"; then
            echo "✅ MPRIS MCP server: Running"
        else
            echo "❌ MPRIS MCP server: Not running"
        fi
        ;;
    *)
        echo "Usage: $0 [start|stop|restart|status]"
        exit 1
        ;;
esac
      '';
      executable = true;
    };

    # Environment variables for MCP
    home.sessionVariables = {
      MCP_SERVERS_DIR = "$HOME/.local/share/mcp-servers";
      MCP_LOGS_DIR = "$HOME/.local/share/mcp-logs";
    };

    # Auto-start MCP servers (optional)
    # home.activation.startMcpServers = lib.hm.dag.entryAfter ["writeBoundary"] ''
    #   $DRY_RUN_CMD ${pkgs.bash}/bin/bash $HOME/.local/bin/start-mcp-servers start
    # '';
  };
}