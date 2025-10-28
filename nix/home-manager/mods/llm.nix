{ lib, pkgs, config, ... }:

{
  options = with lib; {
    services.llm = {
      enable = mkEnableOption "Enable LLM desktop assistant with gptel integration";
      whisperModel = mkOption {
        type = types.str;
        default = "tiny";
        description = "Whisper model size (tiny, base, small, medium, large)";
      };
      recordDuration = mkOption {
        type = types.int;
        default = 4;
        description = "Voice recording duration in seconds";
      };
    };
  };

  config = with lib; mkIf config.services.llm.enable {
    # Install required packages
    home.packages = with pkgs; [
      # Core tools
      nodejs_22
      playerctl
      pavucontrol
      pulseaudio

      # Voice interface
      espeak-ng
      sox
      alsa-utils
      openai-whisper

      # Notifications and UI
      dunst
      libnotify

      # Utilities
      jq
      curl

      # Python with AI packages
      (python3.withPackages (ps: with ps; [
        requests
        websockets
        tkinter
      ]))
    ];

    # Create directories
    home.file.".local/share/mcp-servers/.keep".text = "";
    home.file.".local/share/mcp-logs/.keep".text = "";

    # Qtile-to-Emacs Bridge Script
    home.file.".local/bin/qtile-llm-bridge" = {
      text = ''
#!/usr/bin/env python3
"""
Qtile LLM Bridge - Simple interface between Qtile and Emacs gptel
Handles voice input, system notifications, and IPC with Emacs
"""

import sys
import os
import subprocess
import tempfile
import json
import argparse
import logging
from pathlib import Path
from typing import Optional

# Configure logging
log_dir = Path.home() / ".local/share/mcp-logs"
log_dir.mkdir(parents=True, exist_ok=True)
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_dir / "qtile-llm-bridge.log"),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# Try importing whisper
try:
    import whisper
    WHISPER_AVAILABLE = True
except ImportError:
    WHISPER_AVAILABLE = False
    logger.warning("Whisper not available, will use fallback methods")

class QuileLLMBridge:
    def __init__(self, whisper_model: str = "${config.services.llm.whisperModel}"):
        self.home = Path.home()
        self.temp_dir = Path("/tmp/qtile-llm")
        self.temp_dir.mkdir(exist_ok=True)
        self.audio_file = self.temp_dir / "voice_input.wav"

        self.whisper_model_name = whisper_model
        self.whisper_model = None

    def notify(self, message: str, urgency: str = "normal"):
        """Send desktop notification"""
        try:
            subprocess.run(
                ["notify-send", "-t", "5000", "-u", urgency, "AI Assistant", message],
                capture_output=True, timeout=5
            )
        except Exception as e:
            logger.error(f"Notification failed: {e}")
            print(f"[NOTIFY] {message}")

    def speak(self, text: str):
        """Text-to-speech output"""
        try:
            clean_text = text.replace('\n', ' ').strip()[:500]  # Limit length
            if clean_text:
                subprocess.run(
                    ["espeak-ng", "-s", "150", "-v", "en", clean_text],
                    capture_output=True, timeout=10
                )
        except Exception as e:
            logger.error(f"TTS failed: {e}")

    def record_audio(self, duration: int = ${toString config.services.llm.recordDuration}) -> bool:
        """Record audio from microphone"""
        try:
            self.notify("🎤 Recording...", "normal")
            self.speak("Start speaking now")

            import time
            time.sleep(0.3)

            cmd = ["timeout", str(duration), "arecord", "-f", "cd", "-t", "wav", str(self.audio_file)]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if self.audio_file.exists() and self.audio_file.stat().st_size > 1000:
                self.notify("✅ Recording complete", "normal")
                return True
            else:
                self.notify("❌ No audio captured", "critical")
                return False

        except Exception as e:
            logger.error(f"Recording failed: {e}")
            self.notify(f"❌ Recording error", "critical")
            return False

    def transcribe_audio(self) -> str:
        """Transcribe audio to text"""
        try:
            self.notify("🧠 Processing speech...", "normal")

            # Try Python whisper
            if WHISPER_AVAILABLE:
                if self.whisper_model is None:
                    logger.info(f"Loading whisper model: {self.whisper_model_name}")
                    self.whisper_model = whisper.load_model(self.whisper_model_name)

                result = self.whisper_model.transcribe(str(self.audio_file))
                text = result["text"].strip()

                if text:
                    logger.info(f"Transcribed: {text}")
                    return text

            # Fallback: CLI whisper
            if subprocess.run(["which", "whisper"], capture_output=True).returncode == 0:
                self.notify("🔄 Using CLI whisper...", "normal")
                result = subprocess.run(
                    ["whisper", str(self.audio_file), "--model", "tiny", "--output_format", "txt"],
                    capture_output=True, text=True, timeout=30
                )
                if result.returncode == 0:
                    txt_file = self.audio_file.with_suffix('.txt')
                    if txt_file.exists():
                        text = txt_file.read_text().strip()
                        if text:
                            return text

            # Final fallback: manual input
            self.notify("⌨️ Enter command manually", "normal")
            import tkinter as tk
            from tkinter import simpledialog

            root = tk.Tk()
            root.withdraw()
            command = simpledialog.askstring("AI Assistant", "What would you like me to do?")
            root.destroy()

            return command or ""

        except Exception as e:
            logger.error(f"Transcription failed: {e}")
            self.notify("❌ Transcription error", "critical")
            return ""

    def call_emacs_gptel(self, prompt: str, mode: str = "query") -> str:
        """Send prompt to Emacs gptel and get response"""
        try:
            # Escape for elisp
            escaped_prompt = prompt.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')

            # Call elisp function
            elisp_cmd = f'(gptel-agent-process "{escaped_prompt}" "{mode}")'

            result = subprocess.run(
                ["emacsclient", "--eval", elisp_cmd],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                # Parse elisp string result
                response = result.stdout.strip().strip('"')
                # Unescape
                response = response.replace('\\n', '\n').replace('\\"', '"').replace('\\\\', '\\')
                return response
            else:
                logger.error(f"Emacs call failed: {result.stderr}")
                return "Error: Could not reach Emacs"

        except subprocess.TimeoutExpired:
            logger.error("Emacs call timed out")
            return "Error: Emacs request timed out"
        except Exception as e:
            logger.error(f"Emacs call failed: {e}")
            return f"Error: {str(e)}"

    def voice_mode(self):
        """Handle voice command workflow"""
        try:
            self.notify("🎤 Voice assistant activated")

            # Record audio
            if not self.record_audio():
                return

            # Transcribe
            text = self.transcribe_audio()
            if not text:
                self.notify("❌ No speech detected", "critical")
                self.speak("I didn't hear anything")
                return

            self.notify(f"📝 Heard: \"{text}\"")
            logger.info(f"Voice input: {text}")

            # Send to Emacs gptel
            self.notify("🤖 Thinking...")
            response = self.call_emacs_gptel(text, mode="voice")

            # Respond
            self.notify(f"✅ {response[:100]}...")
            self.speak(response)
            logger.info(f"Response: {response}")

        except Exception as e:
            logger.error(f"Voice mode error: {e}")
            self.notify(f"❌ Error: {str(e)}", "critical")
            self.speak("Sorry, there was an error")
        finally:
            # Cleanup
            if self.audio_file.exists():
                self.audio_file.unlink()

    def chat_mode(self):
        """Open full Emacs gptel interface"""
        try:
            self.notify("🗨️ Opening AI chat...")

            # Open Emacs with gptel
            elisp_cmd = "(gptel-agent-open-chat)"
            subprocess.Popen(
                ["emacsclient", "-c", "--eval", elisp_cmd],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

            logger.info("Opened Emacs gptel chat")

        except Exception as e:
            logger.error(f"Chat mode error: {e}")
            self.notify(f"❌ Failed to open chat", "critical")

    def quick_query(self, query: str):
        """Quick text query without voice"""
        try:
            self.notify(f"🤖 Processing: {query[:50]}...")

            response = self.call_emacs_gptel(query, mode="quick")

            self.notify(f"✅ {response[:100]}...")
            print(response)
            logger.info(f"Query: {query} -> Response: {response}")

        except Exception as e:
            logger.error(f"Quick query error: {e}")
            self.notify(f"❌ Error: {str(e)}", "critical")

def main():
    parser = argparse.ArgumentParser(description="Qtile LLM Bridge to Emacs gptel")
    parser.add_argument("mode", choices=["voice", "chat", "query"],
                       help="Operation mode")
    parser.add_argument("--query", type=str, help="Query text for 'query' mode")
    parser.add_argument("--whisper-model", default="${config.services.llm.whisperModel}",
                       help="Whisper model size")
    args = parser.parse_args()

    bridge = QuileLLMBridge(whisper_model=args.whisper_model)

    if args.mode == "voice":
        bridge.voice_mode()
    elif args.mode == "chat":
        bridge.chat_mode()
    elif args.mode == "query":
        if not args.query:
            print("Error: --query required for query mode")
            sys.exit(1)
        bridge.quick_query(args.query)

if __name__ == "__main__":
    main()
      '';
      executable = true;
    };

    # Emacs Lisp integration
    home.file.".config/doom/gptel-agent.el" = {
      text = ''
;;; gptel-agent.el --- Agentic AI assistant with MCP integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides integration between gptel and MCP servers for desktop AI assistant
;; Called by qtile-llm-bridge for voice and chat interactions

;;; Code:

(require 'gptel)
(require 'json)

;;; Configuration

(defgroup gptel-agent nil
  "Agentic AI assistant with MCP integration."
  :group 'gptel)

(defcustom gptel-agent-system-prompt
  "You are a helpful desktop AI assistant integrated with a Linux system running qtile window manager.

You have access to system control via MCP (Model Context Protocol) servers that provide:
- Media player control (MPRIS) - play/pause, volume, track info
- System commands - volume, brightness, screenshots, lock screen
- File system access

Be concise and actionable. When the user asks you to do something, use the available tools.
For system commands, respond with what you did. For queries, provide helpful information."
  "System prompt for the AI agent."
  :type 'string
  :group 'gptel-agent)

(defcustom gptel-agent-voice-prompt-prefix
  "Voice command: "
  "Prefix added to voice commands."
  :type 'string
  :group 'gptel-agent)

(defcustom gptel-agent-mcp-servers
  '((mpris . "~/.local/share/mcp-servers/mpris-server.py")
    (filesystem . "npx @modelcontextprotocol/server-filesystem ~"))
  "Alist of MCP server names and their launch commands."
  :type '(alist :key-type symbol :value-type string)
  :group 'gptel-agent)

;;; MCP Integration (placeholder - will be enhanced with actual MCP SDK)

(defun gptel-agent--call-system-command (command &rest args)
  "Execute a system COMMAND with ARGS.
Returns the output as a string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process command nil t nil args)))
      (if (= exit-code 0)
          (buffer-string)
        (error "Command failed: %s" (buffer-string))))))

(defun gptel-agent--playerctl (action &optional player)
  "Execute playerctl ACTION on PLAYER."
  (let ((cmd (if player
                 (list "playerctl" "-p" player action)
               (list "playerctl" action))))
    (apply #'gptel-agent--call-system-command cmd)))

(defun gptel-agent-play-music (&optional player)
  "Play media on PLAYER or current player."
  (interactive)
  (gptel-agent--playerctl "play" player)
  "Playing music")

(defun gptel-agent-pause-music (&optional player)
  "Pause media on PLAYER or current player."
  (interactive)
  (gptel-agent--playerctl "pause" player)
  "Paused music")

(defun gptel-agent-next-track (&optional player)
  "Skip to next track on PLAYER or current player."
  (interactive)
  (gptel-agent--playerctl "next" player)
  "Skipped to next track")

(defun gptel-agent-previous-track (&optional player)
  "Go to previous track on PLAYER or current player."
  (interactive)
  (gptel-agent--playerctl "previous" player)
  "Went to previous track")

(defun gptel-agent-get-playing ()
  "Get current playback status."
  (interactive)
  (condition-case err
      (let ((status (string-trim (gptel-agent--playerctl "status")))
            (title (string-trim (gptel-agent--playerctl "metadata" "title")))
            (artist (string-trim (gptel-agent--playerctl "metadata" "artist"))))
        (format "Status: %s\nPlaying: %s - %s" status title artist))
    (error "No media playing")))

(defun gptel-agent-volume-up ()
  "Increase system volume."
  (interactive)
  (gptel-agent--call-system-command "amixer" "set" "Master" "10%+")
  "Volume increased")

(defun gptel-agent-volume-down ()
  "Decrease system volume."
  (interactive)
  (gptel-agent--call-system-command "amixer" "set" "Master" "10%-")
  "Volume decreased")

(defun gptel-agent-volume-mute ()
  "Toggle system volume mute."
  (interactive)
  (gptel-agent--call-system-command "amixer" "set" "Master" "toggle")
  "Volume toggled")

(defun gptel-agent-brightness-up ()
  "Increase screen brightness."
  (interactive)
  (gptel-agent--call-system-command "brightnessctl" "s" "+10%")
  "Brightness increased")

(defun gptel-agent-brightness-down ()
  "Decrease screen brightness."
  (interactive)
  (gptel-agent--call-system-command "brightnessctl" "s" "10%-")
  "Brightness decreased")

(defun gptel-agent-take-screenshot ()
  "Take a screenshot."
  (interactive)
  (gptel-agent--call-system-command "scrot" "-s"
                                     (expand-file-name
                                      (format-time-string "~/Pictures/Screenshots/%Y-%m-%d_%H-%M-%S.png")))
  "Screenshot saved")

;;; Command Routing

(defun gptel-agent--parse-intent (input)
  "Parse user INPUT and determine intent.
Returns a cons of (intent . action-function)."
  (let ((input-lower (downcase input)))
    (cond
     ;; Media controls
     ((and (string-match-p "\\(play\\|resume\\|start\\)" input-lower)
           (string-match-p "next" input-lower))
      (cons 'media-next #'gptel-agent-next-track))

     ((and (string-match-p "\\(play\\|resume\\|start\\)" input-lower)
           (string-match-p "\\(previous\\|prev\\|back\\)" input-lower))
      (cons 'media-previous #'gptel-agent-previous-track))

     ((string-match-p "\\(play\\|resume\\|start\\)" input-lower)
      (cons 'media-play #'gptel-agent-play-music))

     ((string-match-p "\\(pause\\|stop\\)" input-lower)
      (cons 'media-pause #'gptel-agent-pause-music))

     ((string-match-p "\\(what.*playing\\|status\\|current\\|now playing\\)" input-lower)
      (cons 'media-status #'gptel-agent-get-playing))

     ;; Volume controls
     ((and (string-match-p "volume" input-lower)
           (string-match-p "\\(up\\|increase\\|louder\\|higher\\)" input-lower))
      (cons 'volume-up #'gptel-agent-volume-up))

     ((and (string-match-p "volume" input-lower)
           (string-match-p "\\(down\\|decrease\\|lower\\|quieter\\)" input-lower))
      (cons 'volume-down #'gptel-agent-volume-down))

     ((string-match-p "\\(mute\\|silent\\)" input-lower)
      (cons 'volume-mute #'gptel-agent-volume-mute))

     ;; Brightness controls
     ((and (string-match-p "\\(brightness\\|screen\\)" input-lower)
           (string-match-p "\\(up\\|increase\\|brighter\\)" input-lower))
      (cons 'brightness-up #'gptel-agent-brightness-up))

     ((and (string-match-p "\\(brightness\\|screen\\)" input-lower)
           (string-match-p "\\(down\\|decrease\\|dimmer\\|darker\\)" input-lower))
      (cons 'brightness-down #'gptel-agent-brightness-down))

     ;; Screenshot
     ((string-match-p "\\(screenshot\\|capture\\|snap\\)" input-lower)
      (cons 'screenshot #'gptel-agent-take-screenshot))

     ;; Default: use AI
     (t (cons 'ai-query nil)))))

;;; Main Processing Functions

(defun gptel-agent-process (prompt mode)
  "Process PROMPT in MODE (voice, quick, or chat).
Returns response as string (for programmatic use) or displays in buffer."
  (let* ((intent-action (gptel-agent--parse-intent prompt))
         (intent (car intent-action))
         (action (cdr intent-action)))

    ;; If we have a direct action, execute it
    (if action
        (condition-case err
            (funcall action)
          (error (format "Error: %s" (error-message-string err))))

      ;; Otherwise, use gptel for AI response
      (let ((full-prompt (concat
                          (when (string= mode "voice")
                            gptel-agent-voice-prompt-prefix)
                          prompt))
            (response-buffer (generate-new-buffer "*gptel-agent-response*")))

        (with-current-buffer response-buffer
          ;; Use gptel-request with synchronous-ish behavior
          (let ((response-text ""))
            (gptel-request full-prompt
              :system gptel-agent-system-prompt
              :callback (lambda (response info)
                          (when response
                            (setq response-text response)))
              :stream nil)

            ;; Wait a bit for response (not ideal, but works for now)
            (sleep-for 2)

            ;; Return or display response based on mode
            (if (member mode '("voice" "quick"))
                (progn
                  (kill-buffer response-buffer)
                  response-text)
              (progn
                (insert response-text)
                (switch-to-buffer response-buffer)
                response-text))))))))

(defun gptel-agent-open-chat ()
  "Open a full-screen gptel chat interface."
  (interactive)
  (delete-other-windows)
  (let ((buffer (get-buffer-create "*AI Desktop Assistant*")))
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'gptel-mode)
      (gptel-mode)
      (insert "# AI Desktop Assistant\n\n")
      (insert "Ask me anything or tell me to control your system.\n\n")
      (insert "Available commands:\n")
      (insert "- Media: play, pause, next track, what's playing\n")
      (insert "- Volume: volume up/down/mute\n")
      (insert "- Brightness: brightness up/down\n")
      (insert "- Screenshot: take screenshot\n\n")
      (insert "---\n\n"))
    buffer))

;;; Interactive Commands

;;;###autoload
(defun gptel-agent-voice-input ()
  "Trigger voice input mode (called by qtile-llm-bridge)."
  (interactive)
  (call-process "qtile-llm-bridge" nil 0 nil "voice"))

;;;###autoload
(defun gptel-agent-chat ()
  "Open AI chat interface."
  (interactive)
  (gptel-agent-open-chat))

;;;###autoload
(defun gptel-agent-quick-query (query)
  "Send a quick QUERY to the AI assistant."
  (interactive "sQuery: ")
  (let ((response (gptel-agent-process query "quick")))
    (message "AI: %s" response)))

;; Key bindings (optional - can be set in doom config)
(global-set-key (kbd "C-c a v") #'gptel-agent-voice-input)
(global-set-key (kbd "C-c a c") #'gptel-agent-chat)
(global-set-key (kbd "C-c a q") #'gptel-agent-quick-query)

(provide 'gptel-agent)
;;; gptel-agent.el ends here
      '';
    };

    # MCP Configuration
    home.file.".config/mcp/config.json" = {
      text = builtins.toJSON {
        mcpServers = {
          filesystem = {
            command = "npx";
            args = [ "@modelcontextprotocol/server-filesystem" "${config.home.homeDirectory}" ];
          };
          mpris = {
            command = "${config.home.homeDirectory}/.local/share/mcp-servers/mpris-server.py";
            args = [];
          };
        };
      };
    };

    # MPRIS MCP Server (simplified)
    home.file.".local/share/mcp-servers/mpris-server.py" = {
      text = ''
#!/usr/bin/env python3
"""
MPRIS MCP Server - Media player control via Model Context Protocol
"""

import asyncio
import json
import sys
import subprocess
import logging
from typing import Dict, Optional

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class MPRISServer:
    def __init__(self):
        self.tools = {
            "play": "Start or resume media playback",
            "pause": "Pause media playback",
            "play_pause": "Toggle play/pause state",
            "stop": "Stop media playback",
            "next": "Skip to next track",
            "previous": "Go to previous track",
            "get_status": "Get current playback status and track information",
            "set_volume": "Set playback volume (0.0 to 1.0)",
            "get_volume": "Get current volume",
            "list_players": "List available media players"
        }

    def playerctl(self, command: str, player: Optional[str] = None) -> str:
        """Execute playerctl command"""
        try:
            cmd = ['playerctl']
            if player:
                cmd.extend(['-p', player])
            cmd.extend(command.split())

            result = subprocess.run(cmd, capture_output=True, text=True)
            return result.stdout.strip() if result.returncode == 0 else f"Error: {result.stderr.strip()}"
        except Exception as e:
            return f"Error: {str(e)}"

    async def handle_tool_call(self, tool: str, args: Dict) -> str:
        """Handle tool call"""
        player = args.get('player')

        if tool == "play":
            return self.playerctl("play", player)
        elif tool == "pause":
            return self.playerctl("pause", player)
        elif tool == "play_pause":
            return self.playerctl("play-pause", player)
        elif tool == "stop":
            return self.playerctl("stop", player)
        elif tool == "next":
            return self.playerctl("next", player)
        elif tool == "previous":
            return self.playerctl("previous", player)
        elif tool == "get_status":
            status = self.playerctl("status", player)
            title = self.playerctl("metadata title", player)
            artist = self.playerctl("metadata artist", player)
            return f"Status: {status}\nTitle: {title}\nArtist: {artist}"
        elif tool == "set_volume":
            volume = args.get('volume', 0.5)
            return self.playerctl(f"volume {volume}", player)
        elif tool == "get_volume":
            return self.playerctl("volume", player)
        elif tool == "list_players":
            result = subprocess.run(['playerctl', '-l'], capture_output=True, text=True)
            players = result.stdout.strip().split('\n') if result.stdout.strip() else []
            return f"Available players: {', '.join(players)}" if players else "No players found"
        else:
            return f"Unknown tool: {tool}"

    async def run(self):
        """Main server loop"""
        while True:
            try:
                line = await asyncio.get_event_loop().run_in_executor(None, sys.stdin.readline)
                if not line:
                    break

                message = json.loads(line.strip())
                method = message.get('method')

                if method == 'tools/list':
                    response = {
                        'id': message.get('id'),
                        'result': {
                            'tools': [
                                {'name': name, 'description': desc}
                                for name, desc in self.tools.items()
                            ]
                        }
                    }
                elif method == 'tools/call':
                    tool = message.get('params', {}).get('name')
                    args = message.get('params', {}).get('arguments', {})
                    result = await self.handle_tool_call(tool, args)
                    response = {
                        'id': message.get('id'),
                        'result': {'content': [{'type': 'text', 'text': result}]}
                    }
                else:
                    response = None

                if response:
                    print(json.dumps(response))
                    sys.stdout.flush()

            except Exception as e:
                logger.error(f"Error: {e}")
                break

if __name__ == "__main__":
    try:
        asyncio.run(MPRISServer().run())
    except KeyboardInterrupt:
        logger.info("Server shutting down")
    except Exception as e:
        logger.error(f"Server error: {e}")
        sys.exit(1)
      '';
      executable = true;
    };

    # Environment variables
    home.sessionVariables = {
      QTILE_LLM_WHISPER_MODEL = config.services.llm.whisperModel;
      MCP_SERVERS_DIR = "${config.home.homeDirectory}/.local/share/mcp-servers";
      MCP_LOGS_DIR = "${config.home.homeDirectory}/.local/share/mcp-logs";
    };
  };
}
