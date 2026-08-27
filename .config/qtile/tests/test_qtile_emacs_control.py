#!/usr/bin/env python3
"""Static safety regressions for Qtile's Emacs control client."""

from __future__ import annotations

import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile-desktop.el"
TEXT = SOURCE.read_text(encoding="utf-8")
WORKFLOW_SOURCE = SOURCE.parent / "qtile-workflow.el"
WORKFLOW_TEXT = WORKFLOW_SOURCE.read_text(encoding="utf-8")


class QtileEmacsControlTests(unittest.TestCase):
    def test_agent_zero_uses_documented_external_api(self):
        self.assertIn('(concat host "/api_message")', TEXT)
        self.assertIn('("X-API-KEY" . ,key)', TEXT)
        self.assertIn('(message . ,prompt)', TEXT)
        self.assertIn('(context_id . ,qtile-agent-zero-context-id)', TEXT)

    def test_agent_zero_request_is_async(self):
        self.assertIn("url-retrieve", TEXT)
        self.assertNotIn("url-retrieve-synchronously", TEXT)
        self.assertIn("(defun qtile-agent-zero--finish (status target-buffer)", TEXT)

    def test_secrets_come_from_environment_or_private_file(self):
        self.assertIn('getenv "AGENT_ZERO_API_KEY"', TEXT)
        self.assertIn('getenv "AGENT_ZERO_API_KEY_FILE"', TEXT)
        self.assertIn('~/.config/qtile/private.env', TEXT)

    def test_org_todos_and_workflow_picker_are_emacs_native(self):
        self.assertIn('(org-agenda nil "t")', TEXT)
        self.assertIn('completing-read "Workflow: " picker-choices nil t', WORKFLOW_TEXT)
        self.assertNotIn('completing-read "Qtile workflow: "', TEXT)

    def test_shared_renderer_arguments_are_accepted_by_legacy_popups(self):
        self.assertIn("(defun qtile-org-todos-open (&optional _params)", TEXT)
        self.assertIn("(defun qtile-agent-zero-open (&optional _params)", TEXT)


if __name__ == "__main__":
    unittest.main()
