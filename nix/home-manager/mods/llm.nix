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

      # GUI and notification utilities
      dunst
      libnotify
      scrot
      brightnessctl
      i3lock

      # AI/ML tools (optional)
      # whisper-ctranslate2  # Uncomment if available

      # Additional utilities
      jq
      curl

      # Python with packages for AI assistant
      (python3.withPackages (ps: with ps; [
        requests
        websockets
        tkinter
        # openai  # Uncomment if using OpenAI API
        # anthropic  # Uncomment if using Claude API
      ]))

      # Speech recognition
      openai-whisper
    ];

    # Create MCP servers directory
    home.file.".local/share/mcp-servers/.keep".text = "";

    # Custom MPRIS MCP Server
    home.file.".local/share/mcp-servers/mpris-server.py" = {
      text = ''
#!/usr/bin/env python3
"""
MPRIS MCP Server - Bridge between Model Context Protocol and MPRIS media players
Provides AI agents with media player control capabilities via playerctl
"""

import asyncio
import json
import sys
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
        # Using playerctl for MPRIS control - much simpler than D-Bus
        pass

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

    # Qtile AI Assistant - Enhanced Python-based desktop assistant
    home.file.".local/bin/qtile-ai-assistant" = {
      text = ''
#!/usr/bin/env python3
"""
Qtile AI Assistant - Comprehensive desktop AI interface
Integrates voice processing, LLM interactions, and system control
"""

import sys
import os
import json
import asyncio
import subprocess
import tempfile
import argparse
from pathlib import Path
from typing import Dict, List, Optional, Union
import logging

# Try importing whisper for speech recognition
try:
    import whisper
    WHISPER_AVAILABLE = True
except ImportError:
    WHISPER_AVAILABLE = False
    whisper = None

# Configure logging
log_dir = Path.home() / ".local/share/mcp-logs"
log_dir.mkdir(parents=True, exist_ok=True)
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_dir / "qtile-ai.log"),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class QtileAI:
    def __init__(self):
        self.home = Path.home()
        self.temp_dir = Path("/tmp/qtile-ai")
        self.temp_dir.mkdir(exist_ok=True)

        # Audio settings
        self.audio_file = self.temp_dir / "voice_input.wav"
        self.text_file = self.temp_dir / "voice_text.txt"

        # Whisper settings
        self.whisper_model_name = os.getenv('QTILE_AI_WHISPER_MODEL', 'tiny')
        self.whisper_model = None
        self.whisper_device = 'cpu'  # Can be changed to 'cuda' if available

        # MCP settings
        self.mcp_servers_dir = self.home / ".local/share/mcp-servers"

        # System commands
        self.commands = {
            "volume_up": "amixer set Master 10%+",
            "volume_down": "amixer set Master 10%-",
            "volume_mute": "amixer set Master toggle",
            "brightness_up": "brightnessctl s +10%",
            "brightness_down": "brightnessctl s 10%-",
            "screenshot": "scrot -s ~/Pictures/Screenshots/%Y-%m-%d_%H-%M-%S.png",
            "lock_screen": "i3lock -c 000000",
            "suspend": "systemctl suspend"
        }

        # AI prompt templates
        self.prompts = {
            "system": """You are a helpful desktop AI assistant integrated with qtile window manager.
You can control media playback, system settings, and assist with various tasks.
Be concise and actionable in your responses.""",
            "voice_context": "Process this voice command and provide a brief response: ",
            "chat_context": "Desktop assistance request: "
        }

    def notify(self, title: str, message: str, urgency: str = "normal"):
        """Send desktop notification"""
        try:
            # Try dunstify first (preferred)
            cmd = ["dunstify", "-t", "5000", "-u", urgency, "-i", "microphone-sensitivity-high", title, message]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            if result.returncode == 0:
                return
        except (subprocess.TimeoutExpired, subprocess.CalledProcessError, FileNotFoundError) as e:
            logger.debug(f"dunstify failed: {e}")

        try:
            # Fallback to notify-send
            cmd = ["notify-send", "-t", "5000", "-u", urgency, title, message]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            if result.returncode == 0:
                return
        except (subprocess.TimeoutExpired, subprocess.CalledProcessError, FileNotFoundError) as e:
            logger.debug(f"notify-send failed: {e}")

        # Final fallback to console
        print(f"🔔 {title}: {message}")
        logger.info(f"Notification: {title}: {message}")

    def speak(self, text: str):
        """Text-to-speech output"""
        try:
            # Clean text for speech
            clean_text = text.replace('\n', ' ').strip()
            if clean_text:
                subprocess.run(
                    ["espeak-ng", "-s", "150", "-v", "en", clean_text],
                    capture_output=True, timeout=10
                )
        except (subprocess.TimeoutExpired, subprocess.CalledProcessError) as e:
            logger.error(f"Speech failed: {e}")

    def record_audio(self, duration: int = 4) -> bool:
        """Record audio for voice input"""
        try:
            # Show recording countdown notification
            self.notify("AI Assistant", f"🎤 Recording in progress... ({duration}s)", "normal")
            self.speak("Start speaking now")

            # Small delay to let TTS finish
            import time
            time.sleep(0.5)

            # Show active recording notification
            self.notify("AI Assistant", f"🔴 RECORDING ({duration}s)", "normal")

            # Record using arecord
            cmd = ["timeout", str(duration), "arecord", "-f", "cd", "-t", "wav", str(self.audio_file)]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if self.audio_file.exists() and self.audio_file.stat().st_size > 1000:
                self.notify("AI Assistant", "✅ Recording complete", "normal")
                return True
            else:
                self.notify("AI Assistant", "❌ No audio recorded or too quiet", "critical")
                return False

        except Exception as e:
            logger.error(f"Audio recording failed: {e}")
            self.notify("AI Assistant", f"❌ Recording error: {str(e)}", "critical")
            return False

    def load_whisper_model(self):
        """Load whisper model on demand"""
        if not WHISPER_AVAILABLE:
            return False

        try:
            if self.whisper_model is None:
                logger.info(f"Loading whisper model: {self.whisper_model_name}")
                self.whisper_model = whisper.load_model(self.whisper_model_name)
            return True
        except Exception as e:
            logger.error(f"Failed to load whisper model: {e}")
            return False

    def transcribe_audio(self, audio_file: Path) -> str:
        """Transcribe audio to text using whisper or fallbacks"""
        try:
            # Show transcription start notification
            self.notify("AI Assistant", "🧠 Processing speech...", "normal")

            # First, try using Python whisper library
            if WHISPER_AVAILABLE and self.load_whisper_model():
                logger.info("Transcribing with Python whisper library")
                result = self.whisper_model.transcribe(str(audio_file))
                text = result["text"].strip()
                if text:
                    logger.info(f"Whisper transcription: {text}")
                    self.notify("AI Assistant", f"📝 Heard: \"{text}\"", "normal")
                    return text

            # Fallback: try command-line whisper
            if subprocess.run(["which", "whisper"], capture_output=True).returncode == 0:
                logger.info("Transcribing with command-line whisper")
                self.notify("AI Assistant", "🔄 Using command-line whisper...", "normal")
                result = subprocess.run(
                    ["whisper", str(audio_file), "--model", "tiny", "--output_format", "txt"],
                    capture_output=True, text=True, timeout=30
                )
                if result.returncode == 0:
                    txt_file = audio_file.with_suffix('.txt')
                    if txt_file.exists():
                        text = txt_file.read_text().strip()
                        if text:
                            logger.info(f"CLI whisper transcription: {text}")
                            self.notify("AI Assistant", f"📝 Heard: \"{text}\"", "normal")
                            return text

            # Final fallback to manual input
            logger.info("Falling back to manual input")
            self.notify("AI Assistant", "⌨️ Speech recognition unavailable - enter your command:", "normal")
            import tkinter as tk
            from tkinter import simpledialog

            root = tk.Tk()
            root.withdraw()
            command = simpledialog.askstring("AI Assistant", "What would you like me to do?")
            root.destroy()

            if command:
                self.notify("AI Assistant", f"📝 Input: \"{command}\"", "normal")

            return command or ""

        except Exception as e:
            logger.error(f"Transcription failed: {e}")
            self.notify("AI Assistant", f"❌ Transcription error: {str(e)}", "critical")
            return ""

    def execute_system_command(self, command_key: str) -> bool:
        """Execute predefined system commands"""
        if command_key not in self.commands:
            return False

        try:
            cmd = self.commands[command_key].split()
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            return result.returncode == 0
        except Exception as e:
            logger.error(f"System command failed: {e}")
            return False

    def call_mcp_tool(self, server: str, tool: str, args: Dict = None) -> str:
        """Call MCP server tool"""
        try:
            if server == "mpris":
                server_path = self.mcp_servers_dir / "mpris-server.py"
                if not server_path.exists():
                    return "MPRIS server not found"

                # For now, use playerctl directly
                if tool == "play":
                    result = subprocess.run(["playerctl", "play"], capture_output=True, text=True)
                elif tool == "pause":
                    result = subprocess.run(["playerctl", "pause"], capture_output=True, text=True)
                elif tool == "play_pause":
                    result = subprocess.run(["playerctl", "play-pause"], capture_output=True, text=True)
                elif tool == "next":
                    result = subprocess.run(["playerctl", "next"], capture_output=True, text=True)
                elif tool == "previous":
                    result = subprocess.run(["playerctl", "previous"], capture_output=True, text=True)
                elif tool == "get_status":
                    status_result = subprocess.run(["playerctl", "status"], capture_output=True, text=True)
                    title_result = subprocess.run(["playerctl", "metadata", "title"], capture_output=True, text=True)
                    artist_result = subprocess.run(["playerctl", "metadata", "artist"], capture_output=True, text=True)

                    return f"Status: {status_result.stdout.strip()}\nTitle: {title_result.stdout.strip()}\nArtist: {artist_result.stdout.strip()}"
                else:
                    return f"Unknown MPRIS tool: {tool}"

                return result.stdout.strip() if result.returncode == 0 else f"Error: {result.stderr.strip()}"

        except Exception as e:
            logger.error(f"MCP call failed: {e}")
            return f"Error calling {server}.{tool}: {str(e)}"

    def process_natural_language_command(self, text: str) -> str:
        """Process natural language commands and route to appropriate tools"""
        text_lower = text.lower().strip()

        # Media control commands
        if any(word in text_lower for word in ["play", "resume", "start"]):
            if "next" in text_lower:
                result = self.call_mcp_tool("mpris", "next")
                return f"Skipped to next track. {result}"
            elif "previous" in text_lower or "prev" in text_lower:
                result = self.call_mcp_tool("mpris", "previous")
                return f"Went to previous track. {result}"
            else:
                result = self.call_mcp_tool("mpris", "play")
                return f"Playing media. {result}"

        elif any(word in text_lower for word in ["pause", "stop"]):
            result = self.call_mcp_tool("mpris", "pause")
            return f"Paused media. {result}"

        elif "status" in text_lower or "what" in text_lower and "playing" in text_lower:
            return self.call_mcp_tool("mpris", "get_status")

        # Volume control
        elif "volume" in text_lower:
            if "up" in text_lower or "increase" in text_lower or "louder" in text_lower:
                if self.execute_system_command("volume_up"):
                    return "Volume increased"
                else:
                    return "Failed to increase volume"
            elif "down" in text_lower or "decrease" in text_lower or "quiet" in text_lower:
                if self.execute_system_command("volume_down"):
                    return "Volume decreased"
                else:
                    return "Failed to decrease volume"
            elif "mute" in text_lower or "silent" in text_lower:
                if self.execute_system_command("volume_mute"):
                    return "Volume toggled"
                else:
                    return "Failed to mute volume"

        # Brightness control
        elif "brightness" in text_lower or "screen" in text_lower:
            if "up" in text_lower or "increase" in text_lower or "brighter" in text_lower:
                if self.execute_system_command("brightness_up"):
                    return "Brightness increased"
                else:
                    return "Failed to increase brightness"
            elif "down" in text_lower or "decrease" in text_lower or "dimmer" in text_lower:
                if self.execute_system_command("brightness_down"):
                    return "Brightness decreased"
                else:
                    return "Failed to decrease brightness"

        # Screenshot
        elif "screenshot" in text_lower or "capture" in text_lower or "snap" in text_lower:
            if self.execute_system_command("screenshot"):
                return "Screenshot taken"
            else:
                return "Failed to take screenshot"

        # System control
        elif "lock" in text_lower:
            if self.execute_system_command("lock_screen"):
                return "Screen locked"
            else:
                return "Failed to lock screen"

        elif "suspend" in text_lower or "sleep" in text_lower:
            if self.execute_system_command("suspend"):
                return "System suspended"
            else:
                return "Failed to suspend system"

        # Default: pass to Emacs gptel for AI processing
        else:
            try:
                # Show AI processing notification
                self.notify("AI Assistant", "🤖 Thinking...", "normal")

                # Escape quotes for shell command
                escaped_text = text.replace('"', '\\"').replace("'", "\\'")

                # Send to Emacs for LLM processing using gptel
                emacs_cmd = 'emacsclient --eval "(let ((response nil)) (gptel-request \\"' + escaped_text + '\\" :system \\"You are a helpful desktop voice assistant. Be concise and actionable. The user is using a qtile window manager on Linux.\\" :callback (lambda (resp info) (when resp (message \\"QTILE_AI_RESPONSE: %s\\" resp))) :stream nil) \\"Processing with AI...\\")"'

                result = subprocess.run(emacs_cmd, shell=True, capture_output=True, text=True, timeout=30)
                if result.returncode == 0:
                    # Try to extract response from emacs messages
                    output = result.stdout.strip().strip('"')
                    if "QTILE_AI_RESPONSE:" in output:
                        response = output.split("QTILE_AI_RESPONSE:")[-1].strip()
                        self.notify("AI Assistant", "✅ AI response ready", "normal")
                        return response if response else "Command processed by AI assistant"
                    else:
                        self.notify("AI Assistant", "✅ AI processing completed", "normal")
                        return "AI processing completed"
                else:
                    logger.warning(f"Emacs command failed: {result.stderr}")
                    self.notify("AI Assistant", "⚠️ AI temporarily unavailable", "normal")
                    return "AI processing temporarily unavailable"
            except Exception as e:
                logger.error(f"Emacs AI processing failed: {e}")
                self.notify("AI Assistant", f"❌ AI error: {str(e)}", "critical")
                return f"I didn't understand: '{text}'. Try commands like 'play music', 'volume up', 'take screenshot'"

    def voice_mode(self, record_duration=4):
        """Handle voice input workflow"""
        try:
            # Initial activation notification
            self.notify("AI Assistant - Voice Mode", "🎤 Voice assistant activated")

            # Record audio (this will show the "start speaking" notification)
            if not self.record_audio(duration=record_duration):
                return

            # Transcribe audio
            transcribed_text = self.transcribe_audio(self.audio_file)
            if not transcribed_text:
                self.notify("AI Assistant", "❌ Could not understand audio", "critical")
                self.speak("I didn't understand that")
                return

            logger.info(f"Voice command: {transcribed_text}")

            # Process command
            response = self.process_natural_language_command(transcribed_text)

            # Respond
            self.notify("AI Assistant", f"✅ {response}")
            self.speak(response)

            logger.info(f"Response: {response}")

        except Exception as e:
            logger.error(f"Voice mode error: {e}")
            self.notify("AI Assistant", f"Voice processing error: {str(e)}", "critical")
            self.speak("Sorry, there was an error")

        finally:
            # Cleanup
            if self.audio_file.exists():
                self.audio_file.unlink()

    def chat_mode(self):
        """Handle text chat input workflow via Emacs gptel"""
        try:
            self.notify("AI Assistant - Chat Mode", "🗨️ Opening full-screen AI chat...")

            # Launch Emacs with gptel in full-frame mode
            # Use the +gptel/here function from doom emacs
            emacs_cmd = [
                "emacsclient",
                "-c",
                "-a", "emacs",
                "--eval", "(progn (delete-other-windows) (+gptel/here) (rename-buffer \"*AI Desktop Assistant*\"))"
            ]

            # Run emacs in background so script can continue
            subprocess.Popen(emacs_cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

            logger.info("Launched Emacs gptel chat interface")

        except Exception as e:
            logger.error(f"Chat mode error: {e}")
            self.notify("AI Assistant", f"Failed to open chat: {str(e)}", "critical")

            # Fallback: try basic emacs frame
            try:
                fallback_cmd = ["emacsclient", "-c", "-a", "emacs"]
                subprocess.Popen(fallback_cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                self.notify("AI Assistant", "Opened Emacs (gptel may not be available)", "normal")
            except Exception as fallback_error:
                logger.error(f"Fallback chat failed: {fallback_error}")
                self.notify("AI Assistant", "Unable to open chat interface", "critical")

    def status_mode(self):
        """Show AI assistant status"""
        try:
            # Check MCP servers
            mpris_status = "✅ Running" if (self.mcp_servers_dir / "mpris-server.py").exists() else "❌ Not found"

            # Check system tools
            tools_status = []
            for tool in ["playerctl", "amixer", "espeak-ng", "dunstify"]:
                status = "✅" if subprocess.run(["which", tool], capture_output=True).returncode == 0 else "❌"
                tools_status.append(f"{tool}: {status}")

            # Check whisper availability
            whisper_python = "✅" if WHISPER_AVAILABLE else "❌"
            whisper_cli = "✅" if subprocess.run(["which", "whisper"], capture_output=True).returncode == 0 else "❌"

            status_text = f"""AI Assistant Status:

MPRIS Server: {mpris_status}

Speech Recognition:
Python whisper: {whisper_python}
CLI whisper: {whisper_cli}
Model: {self.whisper_model_name}

System Tools:
{chr(10).join(tools_status)}

Logs: {log_dir / 'qtile-ai.log'}
"""

            self.notify("AI Assistant Status", status_text)
            print(status_text)

        except Exception as e:
            logger.error(f"Status check failed: {e}")

def main():
    parser = argparse.ArgumentParser(description="Qtile AI Assistant")
    parser.add_argument("mode", choices=["voice", "chat", "status"], help="Operation mode")
    parser.add_argument("--whisper-model", default="tiny",
                       choices=["tiny", "base", "small", "medium", "large"],
                       help="Whisper model size (default: tiny)")
    parser.add_argument("--record-duration", type=int, default=4,
                       help="Voice recording duration in seconds (default: 4)")
    parser.add_argument("--no-whisper", action="store_true",
                       help="Disable whisper and use manual input fallback")
    args = parser.parse_args()

    ai = QtileAI()

    # Configure whisper settings from CLI args
    ai.whisper_model_name = args.whisper_model
    if args.no_whisper:
        global WHISPER_AVAILABLE
        WHISPER_AVAILABLE = False

    if args.mode == "voice":
        ai.voice_mode(record_duration=args.record_duration)
    elif args.mode == "chat":
        ai.chat_mode()
    elif args.mode == "status":
        ai.status_mode()

if __name__ == "__main__":
    main()
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