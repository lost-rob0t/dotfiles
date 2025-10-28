# LLM Desktop Assistant - Qtile + Emacs gptel Integration

A clean, agentic AI assistant that bridges Qtile and Emacs gptel with MCP (Model Context Protocol) for system control.

## Architecture

```
┌─────────────┐         ┌──────────────────┐         ┌──────────────┐
│   Qtile     │         │  Python Bridge   │         │    Emacs     │
│  Keybind    ├────────>│  - Voice Input   ├────────>│    gptel     │
│             │         │  - Audio Rec     │         │   (AI Brain) │
└─────────────┘         │  - Notifications │         └───────┬──────┘
                        │  - TTS Output    │                 │
                        └──────────────────┘                 │
                                                              │
                                                      ┌───────v────────┐
                                                      │  MCP Servers   │
                                                      │  - Media (MPRIS)
                                                      │  - Filesystem  │
                                                      │  - System Ctrl │
                                                      └────────────────┘
```

## Components

1. **qtile-llm-bridge** (Python): Handles qtile side
   - Voice recording via `arecord`
   - Speech-to-text via Whisper
   - Desktop notifications
   - Text-to-speech via espeak
   - Simple IPC with Emacs

2. **gptel-agent.el** (Emacs Lisp): The "brain"
   - Intent parsing and routing
   - Direct system commands for simple tasks
   - gptel + MCP for complex queries
   - Tool orchestration

3. **MCP Servers**: The "hands"
   - MPRIS server for media control
   - Filesystem server for file operations
   - Extensible for more capabilities

## Setup

### 1. Enable in your home-manager configuration

```nix
# In your home.nix or system config
{
  imports = [
    ./mods/llm.nix
  ];

  services.llm = {
    enable = true;
    whisperModel = "tiny";  # or "base", "small", etc.
    recordDuration = 4;     # seconds
  };
}
```

### 2. Configure Doom Emacs

Add to your `config.el`:

```elisp
;; Load gptel-agent
(load! "~/.config/doom/gptel-agent.el")

;; Configure gptel with your API key
(use-package! gptel
  :config
  (setq gptel-model "claude-3-5-sonnet-20241022"
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key "your-api-key-here")))

;; Optional: Set custom keybindings
(map! :leader
      :desc "AI Voice" "a v" #'gptel-agent-voice-input
      :desc "AI Chat" "a c" #'gptel-agent-chat
      :desc "AI Query" "a q" #'gptel-agent-quick-query)
```

Or use environment variable for API key:

```nix
home.sessionVariables = {
  ANTHROPIC_API_KEY = "your-key-here";
};
```

### 3. Configure Qtile keybindings

Add to your `config.py`:

```python
from libqtile.config import Key
from libqtile.lazy import lazy

keys = [
    # AI Voice Assistant
    Key([mod, "shift"], "a",
        lazy.spawn("qtile-llm-bridge voice"),
        desc="Activate voice assistant"),

    # AI Chat Interface
    Key([mod, "shift"], "c",
        lazy.spawn("qtile-llm-bridge chat"),
        desc="Open AI chat"),

    # Quick AI query (via rofi/dmenu)
    Key([mod, "shift"], "q",
        lazy.spawn("sh -c 'query=$(echo | rofi -dmenu -p \"Ask AI:\") && qtile-llm-bridge query --query \"$query\"'"),
        desc="Quick AI query"),
]
```

### 4. Rebuild home-manager

```bash
home-manager switch --flake .#your-config
```

## Usage

### Voice Mode (Mod+Shift+A in Qtile)

1. Press the keybind
2. Speak your command (4 seconds by default)
3. Wait for processing
4. Hear/see the response

**Example commands:**
- "Play music"
- "Pause"
- "Next track"
- "What's playing?"
- "Volume up"
- "Increase brightness"
- "Take a screenshot"
- "What's the weather like?" (uses gptel AI)
- "Write a hello world in Python" (uses gptel AI)

### Chat Mode (Mod+Shift+C in Qtile)

Opens a full Emacs gptel buffer for extended conversation with full AI capabilities and tool access.

### Quick Query Mode (Mod+Shift+Q in Qtile)

Type a quick question, get a quick answer via notification and TTS.

## How It Works

### Simple Commands (Fast Path)

```
User: "Play music" → Intent Parser → Direct playerctl call → Done
```

The elisp `gptel-agent--parse-intent` function detects common commands and executes them directly without involving the AI. This is instant.

### Complex Queries (AI Path)

```
User: "What's a good playlist for coding?"
  ↓
gptel with system prompt
  ↓
AI decides to call MCP tools or just respond
  ↓
Response delivered
```

For complex queries, the full AI (gptel) processes the request and can use MCP tools if needed.

### MCP Tool Usage

When gptel needs to use tools (future enhancement with proper MCP SDK):

```
User: "Play my jazz playlist"
  ↓
gptel understands intent
  ↓
Calls MCP MPRIS server with "play" + search for "jazz"
  ↓
MPRIS server executes playerctl commands
  ↓
Returns status to gptel
  ↓
gptel responds: "Now playing your jazz playlist"
```

## Extending

### Add New System Commands

In `gptel-agent.el`:

```elisp
(defun gptel-agent-lock-screen ()
  "Lock the screen."
  (interactive)
  (gptel-agent--call-system-command "i3lock" "-c" "000000")
  "Screen locked")

;; Add to intent parser
(defun gptel-agent--parse-intent (input)
  (let ((input-lower (downcase input)))
    (cond
     ;; ... existing patterns ...
     ((string-match-p "lock" input-lower)
      (cons 'lock-screen #'gptel-agent-lock-screen))
     ;; ... more patterns ...
     )))
```

### Add New MCP Server

1. Create server script in `~/.local/share/mcp-servers/`
2. Add to MCP config in `llm.nix`:

```nix
home.file.".config/mcp/config.json" = {
  text = builtins.toJSON {
    mcpServers = {
      # ... existing servers ...
      myserver = {
        command = "${config.home.homeDirectory}/.local/share/mcp-servers/myserver.py";
        args = [];
      };
    };
  };
};
```

3. Use in gptel-agent.el or let gptel discover it automatically

## Customization

### Change Voice Recording Duration

```nix
services.llm.recordDuration = 6;  # 6 seconds instead of 4
```

### Change Whisper Model

```nix
services.llm.whisperModel = "base";  # Better accuracy, slower
```

Options: `tiny`, `base`, `small`, `medium`, `large`

### Customize AI System Prompt

In Doom `config.el`:

```elisp
(setq gptel-agent-system-prompt
      "You are my personal coding assistant specialized in Python...")
```

## Troubleshooting

### Voice not working

```bash
# Test microphone
arecord -f cd -t wav -d 3 test.wav
aplay test.wav

# Check whisper
python3 -c "import whisper; print(whisper.__version__)"
```

### Emacs not responding

```bash
# Check if Emacs server is running
emacsclient -e "(+ 1 1)"

# Start Emacs server
emacs --daemon
```

### MCP servers not working

```bash
# Check MPRIS server
playerctl status

# Test MCP server directly
python3 ~/.local/share/mcp-servers/mpris-server.py
```

### Check logs

```bash
# Bridge logs
tail -f ~/.local/share/mcp-logs/qtile-llm-bridge.log

# MCP server logs
tail -f ~/.local/share/mcp-logs/mpris-server.log
```

## Future Enhancements

- [ ] Proper MCP Python SDK integration
- [ ] More MCP servers (email, calendar, browser, etc.)
- [ ] Context awareness (active window, current workspace)
- [ ] Multi-turn conversation in voice mode
- [ ] Hotword detection for always-on mode
- [ ] Integration with Emacs org-mode for task management
- [ ] Screen context (OCR current window for AI)

## Philosophy

This system follows Unix philosophy:
- **Do one thing well**: Each component has a clear role
- **Composability**: Components communicate via simple interfaces
- **Extensibility**: Easy to add new capabilities
- **User choice**: Emacs (gptel) is the brain, you control the AI backend

The Python bridge is intentionally simple - it's just I/O (voice, notifications, TTS). All intelligence lives in Emacs where you have full control.
