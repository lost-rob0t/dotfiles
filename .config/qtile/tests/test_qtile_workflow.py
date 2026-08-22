#!/usr/bin/env python3
"""Static regression tests for Emacs workflow frame placement."""

from __future__ import annotations

import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile-workflow.el"
TEXT = SOURCE.read_text(encoding="utf-8")


class WorkflowFrameTests(unittest.TestCase):
    def test_workflow_picker_is_right_aligned_and_top_aligned(self):
        self.assertIn("(left . 1.0)", TEXT)
        self.assertIn("(top . 0.0)", TEXT)
        self.assertIn("(user-position . t)", TEXT)
        self.assertIn('completing-read "Qtile workflow: "', TEXT)


if __name__ == "__main__":
    unittest.main()
