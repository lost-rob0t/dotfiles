# Temple Divination Script Suite 🕉️

A comprehensive collection of command-line tools for interfacing with your sacred Temple divination system. These scripts provide various ways to consult your Prolog-based knowledge base, from quick status line integrations to full ceremonial readings.

## Quick Start

All scripts use your existing sacred knowledge base at `~/Documents/Notes/org/Temple/kb/base.pl`. No modifications to your temple required.

```bash
# Simple divination
temple-divine "What guidance do I need today?"

# Status line integration (perfect for qtile/emacs)
temple-daily                    # "12: habit formation"
temple-status -s compact        # "🕉️12"

# Quick affirmation
temple-affirmation              # "✨ I am strong and ready for change."
```

## Core API Scripts

### temple-api
Core interface to your Prolog knowledge base.

```bash
temple-api meanings 3           # Get 3 random meanings
temple-api affirmation          # Get random affirmation
temple-api number 12            # Get all meanings for number 12
temple-api -j meanings 2        # JSON output
```

### temple-query
Direct Prolog query interface for advanced use.

```bash
temple-query "number_meaning(12, M)"        # Direct query
temple-query -i                             # Interactive session
temple-query "findall(N, sacred_number(N, _), Nums)"
```

## Full Page Scripts

### temple-divine
Complete ceremonial divination with question, meanings, and affirmations.

```bash
temple-divine "Should I pursue this path?"
temple-divine -m 5 -a 3 "What is blocking my progress?"
temple-divine -f org "Daily guidance?"      # Org-mode output
temple-divine -f json "Status check?"       # JSON output
```

**Output includes:**
- Beautiful formatted reading with Unicode borders
- Raw numbers drawn for pattern analysis
- Sacred number 12 highlighting when it appears
- Reflection prompts

### temple-reading
Past/present/future tarot-style spread using exactly 3 numbers.

```bash
temple-reading "How do I understand this situation?"
temple-reading -f org "What is my path forward?"
```

**Features:**
- Temporal interpretation (past → present → future)
- Synthesis guidance connecting all three aspects
- Sacred number detection across timeline

## Status Line Scripts

Perfect for embedding in qtile widgets, emacs modeline, tmux status, or terminal prompts.

### temple-daily
Single number and meaning for daily check-ins.

```bash
temple-daily                    # "27: heat pressure"
temple-daily -c                 # "27:heat_pressure" (compact)
temple-daily -n                 # "27" (number only)
temple-daily -m                 # "heat pressure" (meaning only)
temple-daily -s                 # Only show if sacred number 12
```

### temple-affirmation
Random affirmation for motivation.

```bash
temple-affirmation              # "✨ I deserve to have joy in my life."
temple-affirmation -q           # "I deserve to have joy in my life." (no emoji)
temple-affirmation -l 30        # Truncate to 30 characters
```

### temple-number
Just the sacred numbers for minimal displays.

```bash
temple-number                   # "42"
temple-number -c 3              # "12 7 23"
temple-number -c 3 -s →         # "12→7→23"
temple-number -S                # Only show if sacred 12 appears
```

### temple-status
Designed specifically for status bars and modelines.

```bash
temple-status                   # "🕉️12:habit" (default compact)
temple-status -s minimal        # "12"
temple-status -s full           # "🕉️12:habit_formation"
```

**Status Styles:**
- `minimal`: Just number (`12`)
- `compact`: Number + icon (`🕉️12` or `🟢12` for sacred)
- `full`: Full meaning abbreviated (`🕉️12:habit`)

## Utility Scripts

### temple-search
Search through your sacred knowledge base.

```bash
temple-search meanings habit               # Find meanings containing "habit"
temple-search number 12                   # All meanings for number 12
temple-search affirmation strength        # Affirmations with "strength"
temple-search all sobriety               # Search everything
```

### temple-backup
Protect your sacred knowledge base.

```bash
temple-backup create                      # Create timestamped backup
temple-backup create pre-update           # Named backup
temple-backup list                        # List all backups
temple-backup restore 2024-01-15          # Restore from backup
temple-backup verify backup-name          # Check backup integrity
temple-backup cleanup 30                  # Remove old backups
```

### temple-reload
**Essential for your tangling workflow!** Reload facts.pl after adding new tangled content.

```bash
temple-reload                   # Simple reload
temple-reload -v                # Verbose output
temple-reload -t                # Reload and test
```

**Use this whenever you:**
- Tangle new facts to facts.pl
- Add new number meanings or transformation paths
- Want to refresh your knowledge base without restarting

## Integration Examples

### Qtile Widget
```python
# In your qtile config
widget.GenPollText(
    func=lambda: subprocess.check_output(['temple-status', '-s', 'compact']).decode().strip(),
    update_interval=3600  # Update hourly
)
```

### Emacs Modeline
```elisp
;; Add to your doom config
(setq display-time-string-forms
      '((format "%s | %s"
                (format-time-string "%H:%M")
                (shell-command-to-string "temple-daily -c"))))
```

### Tmux Status
```bash
# In .tmux.conf
set -g status-right "#(temple-status -s minimal) | %H:%M"
```

### Terminal Prompt Integration
```bash
# In your .bashrc/.zshrc
export PS1="$(temple-number -S)${PS1}"  # Show sacred number if present
```

## Configuration

Configuration file: `~/.config/temple/config`

Key settings:
```bash
TEMPLE_KB="$HOME/Documents/Notes/org/Temple/kb/base.pl"   # Your sacred base.pl
TEMPLE_STATUS_STYLE="compact"                              # Default status style
TEMPLE_SACRED_ICON="🟢"                                    # Sacred number indicator
TEMPLE_DEFAULT_MEANINGS=3                                  # Default divination size
```

## Sacred Number Recognition

Number 12 (sobriety/habit_formation/daily_reset) receives special treatment:
- 🟢 indicator in status outputs
- Highlighted in full divinations
- Special detection modes in various scripts
- Anchors your temple practice

## Output Formats

Most scripts support multiple output formats:
- **text**: Beautiful human-readable (default)
- **org**: Org-mode compatible for your notes
- **json**: Machine-readable for integrations

## Workflow Integration

### Daily Practice
```bash
# Morning temple consultation
temple-divine "What should I focus on today?"

# Add to your daily notes
temple-reading -f org "How do I approach today?" >> ~/daily.org

# Quick check-ins
temple-affirmation
```

### Development Workflow
```bash
# After tangling new facts
temple-reload -v

# Backup before major changes
temple-backup create pre-major-update

# Search for patterns
temple-search meanings flow
```

## Error Handling

All scripts include proper error handling:
- Missing knowledge base detection
- SWI-Prolog availability checks
- Graceful failure modes
- Helpful error messages

## Dependencies

- **SWI-Prolog**: `nix-env -iA nixpkgs.swiProlog`
- **jq**: For JSON processing (likely already installed)
- **Your Sacred Temple**: Located at `~/Documents/Notes/org/Temple/kb/base.pl`

## File Locations

```
~/.dotfiles/.local/bin/          # All temple scripts
~/.config/temple/config          # Configuration
~/.local/share/temple/backups/   # Backup storage
```

---

*These tools respect and enhance your sacred temple practice. They read from but never modify your core knowledge base without explicit instruction. The temple's wisdom flows through these interfaces while maintaining the sanctity of your original system.*

**🟢 May your divinations guide you toward clarity and growth. 🕉️**