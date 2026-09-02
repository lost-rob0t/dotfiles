#!/usr/bin/env python3
"""Exact source/generated parity checks for Qtile workflow literate sources."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def source_blocks(path: Path, language: str):
    text = path.read_text(encoding="utf-8")
    pattern = re.compile(
        rf"^#\+begin_src {re.escape(language)}(?P<header>[^\n]*)\n(?P<body>.*?)^#\+end_src\s*$",
        re.MULTILINE | re.DOTALL,
    )
    return list(pattern.finditer(text))


def tangle_for(path: Path, target: str, language: str) -> str:
    blocks = []
    default_target = None
    property_match = re.search(
        rf"^#\+property: header-args:{re.escape(language)} .*?:tangle\s+([^\s]+)",
        path.read_text(encoding="utf-8"),
        re.MULTILINE | re.IGNORECASE,
    )
    if property_match:
        default_target = property_match.group(1)

    for match in source_blocks(path, language):
        header = match.group("header")
        explicit = re.search(r":tangle\s+([^\s]+)", header)
        destination = explicit.group(1) if explicit else default_target
        if destination == target:
            blocks.append(match.group("body"))
    if not blocks:
        raise AssertionError(f"no {language} blocks tangle to {target} in {path.name}")
    return "".join(blocks)


class LiterateParityTests(unittest.TestCase):
    def test_ai_window_classifier_is_exact_tangle(self):
        source = ROOT / "qtile-ai-windows.org"
        generated = ROOT / "qtile_ai_windows.py"
        self.assertEqual(
            tangle_for(source, "./qtile_ai_windows.py", "python"),
            generated.read_text(encoding="utf-8"),
        )

    def test_workflow_runtime_is_exact_tangle(self):
        source = ROOT / "qtile-workflows.org"
        generated = ROOT / "qtile_workflows.py"
        self.assertEqual(
            tangle_for(source, "./qtile_workflows.py", "python"),
            generated.read_text(encoding="utf-8"),
        )

    def test_workflow_json_is_exact_tangle(self):
        source = ROOT / "qtile-workflows.org"
        generated = ROOT / "workflows.json"
        self.assertEqual(
            tangle_for(source, "./workflows.json", "json"),
            generated.read_text(encoding="utf-8"),
        )


if __name__ == "__main__":
    unittest.main()
