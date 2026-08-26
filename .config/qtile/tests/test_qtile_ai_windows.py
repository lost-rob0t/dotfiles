#!/usr/bin/env python3
"""Regression tests for ActivityWatch-informed AI window routing."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / ".config" / "qtile"))

from qtile_ai_windows import (  # noqa: E402
    is_ai_window,
    is_game_window,
    is_messenger_window,
    is_video_window,
)


class AiWindowPatternTests(unittest.TestCase):
    def test_chatgpt_desktop_class_is_ai(self):
        self.assertTrue(is_ai_window(("chatgpt (/home/unseen/.config/Codex)", "Chatgpt"), "ChatGPT"))
        self.assertTrue(is_ai_window(("Codex",), "Codex"))

    def test_activitywatch_discovered_brave_titles_are_ai(self):
        for title in (
            "ChatGPT - Brave",
            "Agent Zero - Brave",
            "AI Chat Playground - OpenRouter - Brave",
            "Claude - Brave",
        ):
            with self.subTest(title=title):
                self.assertTrue(is_ai_window(("brave-browser", "Brave-browser"), title))

    def test_activitywatch_discovered_terminal_titles_are_ai(self):
        for title in ("OC | A0 RAGE prompt", "OpenCode", "⠂ Claude Code"):
            with self.subTest(title=title):
                self.assertTrue(is_ai_window(("terminator", "Terminator"), title))

    def test_emacs_llm_buffers_are_ai(self):
        self.assertTrue(is_ai_window(("emacs", "Emacs"), "*LLM Chat* – Doom Emacs"))

    def test_activitywatch_discovered_firefox_titles_are_ai(self):
        self.assertTrue(is_ai_window(("firefox",), "Welcome back - OpenAI — Mozilla Firefox"))

    def test_discord_is_a_messenger_window(self):
        self.assertTrue(is_messenger_window(("discord",), "Discord"))
        self.assertTrue(is_messenger_window(("brave-browser",), "Discord - Brave"))
        self.assertFalse(is_ai_window(("discord",), "#llms | Intel Feeds - Discord"))

    def test_activitywatch_discovered_games_are_games(self):
        for wm_class in (
            "War Thunder (Vulkan, 64bit)",
            "Minecraft* 1.20.1",
            "steam_app_671860",
            "PrismLauncher",
            "Terraria.bin.x86_64",
        ):
            with self.subTest(wm_class=wm_class):
                self.assertTrue(is_game_window((wm_class,)))

    def test_activitywatch_discovered_video_windows_are_video(self):
        for wm_class, title in (
            ("brave-browser", "YouTube - Brave"),
            ("brave-browser", "Cewpins - Twitch - Brave"),
            ("feishin", "Feishin"),
            ("vlc", "VLC media player"),
        ):
            with self.subTest(wm_class=wm_class, title=title):
                self.assertTrue(is_video_window((wm_class,), title))

    def test_regular_brave_and_terminal_windows_are_not_ai(self):
        self.assertFalse(is_ai_window(("brave-browser", "Brave-browser"), "GitHub - Brave"))
        self.assertFalse(is_ai_window(("terminator", "Terminator"), "unseen@flake:~/Documents/Projects"))

    def test_t3_code_class_is_ai(self):
        self.assertTrue(is_ai_window(("T3 Code (Alpha)",), "T3 Code (Alpha)"))


if __name__ == "__main__":
    unittest.main()
