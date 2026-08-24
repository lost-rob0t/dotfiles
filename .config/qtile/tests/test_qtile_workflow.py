#!/usr/bin/env python3
"""Static regression tests for Emacs workflow frame placement."""

from __future__ import annotations

import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile-workflow.el"
TEXT = SOURCE.read_text(encoding="utf-8")


class WorkflowFrameTests(unittest.TestCase):
    def test_workflow_picker_uses_shared_popup_renderer(self):
        self.assertIn("(require 'qtile-ui)", TEXT)
        self.assertIn("(defun qtile-workflow-open (params)", TEXT)
        self.assertIn("qtile-ui-org-heading", TEXT)
        self.assertIn('completing-read "Workflow: " choices nil t', TEXT)


if __name__ == "__main__":
    unittest.main()
