"""Window metadata patterns for the Qtile AI workspace."""

from __future__ import annotations

import re
from typing import Iterable


AI_APPLICATION_CLASSES = frozenset({
    "claude",
    "claude code",
    "chatgpt",
    "codex",
    "copilot",
    "deepseek",
    "gemini",
    "openai",
    "opencode",
    "perplexity",
    "qwen",
    "t3 code (alpha)",
})

TITLE_MATCHING_CLASSES = frozenset({
    "brave",
    "brave-browser",
    "emacs",
    "firefox",
    "terminator",
})

MESSENGER_APPLICATION_CLASSES = frozenset({
    "discord",
    "discordcanary",
    "discordptb",
})

MESSENGER_TITLE_MATCHING_CLASSES = frozenset({"brave-browser"})

MESSENGER_TITLE_PATTERNS = tuple(
    re.compile(pattern, re.IGNORECASE)
    for pattern in (r"\bdiscord\b",)
)

GAME_CLASS_PATTERNS = tuple(
    re.compile(pattern, re.IGNORECASE)
    for pattern in (
        r"^minecraft",
        r"^war thunder",
        r"^steam(?:_app_.*)?$",
        r"^prismlauncher$",
        r"^terraria(?:\.bin\.x86_64)?$",
    )
)

VIDEO_APPLICATION_CLASSES = frozenset({
    "celluloid",
    "feishin",
    "jellyfin",
    "kodi",
    "mpv",
    "plex",
    "totem",
    "vlc",
})

VIDEO_TITLE_MATCHING_CLASSES = frozenset({"brave-browser"})

VIDEO_TITLE_PATTERNS = tuple(
    re.compile(pattern, re.IGNORECASE)
    for pattern in (
        r"\byoutube(?: music)?\b",
        r"\btwitch\b",
        r"\bnetflix\b",
        r"\bvimeo\b",
        r"\bxvideos\b",
        r"\bvideo\b",
    )
)

AI_TITLE_PATTERNS = tuple(
    re.compile(pattern, re.IGNORECASE)
    for pattern in (
        r"\bchatgpt\b",
        r"\bopenai\b",
        r"\bagent zero\b",
        r"\bopenrouter\b",
        r"\bclaude(?:\s+code)?\b",
        r"\bgemini\b",
        r"\bcopilot\b",
        r"\bcodex\b",
        r"\bopencode\b",
        r"\bmcp\b",
        r"\bllm\b",
        r"\bgptel\b",
        r"\bai\s+chat\b",
        r"\bai\s+models?\b",
        r"\bdeepseek\b",
        r"\bqwen\b",
        r"\bz\.ai\b",
        r"\bzero-forge\b",
        r"^oc\s*\|",
    )
)


def is_ai_window(wm_classes: Iterable[str] | None, title: str | None) -> bool:
    """Return whether X11 window metadata identifies an AI-related window."""
    classes = {value.casefold() for value in (wm_classes or ()) if value}
    if classes & AI_APPLICATION_CLASSES:
        return True
    if not classes & TITLE_MATCHING_CLASSES:
        return False
    return any(pattern.search(title or "") for pattern in AI_TITLE_PATTERNS)


def is_messenger_window(wm_classes: Iterable[str] | None, title: str | None) -> bool:
    """Return whether window metadata identifies Discord or a Discord tab."""
    classes = {value.casefold() for value in (wm_classes or ()) if value}
    if classes & MESSENGER_APPLICATION_CLASSES:
        return True
    if not classes & MESSENGER_TITLE_MATCHING_CLASSES:
        return False
    return any(pattern.search(title or "") for pattern in MESSENGER_TITLE_PATTERNS)


def is_game_window(wm_classes: Iterable[str] | None) -> bool:
    """Return whether window metadata identifies a game or game launcher."""
    classes = [value for value in (wm_classes or ()) if value]
    return any(pattern.search(value) for value in classes for pattern in GAME_CLASS_PATTERNS)


def is_video_window(wm_classes: Iterable[str] | None, title: str | None) -> bool:
    """Return whether window metadata identifies a video player or video site."""
    classes = {value.casefold() for value in (wm_classes or ()) if value}
    if classes & VIDEO_APPLICATION_CLASSES:
        return True
    if not classes & VIDEO_TITLE_MATCHING_CLASSES:
        return False
    return any(pattern.search(title or "") for pattern in VIDEO_TITLE_PATTERNS)
