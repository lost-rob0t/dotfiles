#!/usr/bin/env python3
"""Source-level contracts for Qtile -> Emacs prompt-lib integration."""

from pathlib import Path
import py_compile
import unittest

ROOT = Path(__file__).resolve().parents[3]
QTILE = ROOT / ".config" / "qtile"
PROMPT_ORG = QTILE / "qtile-prompt-lib.org"
PROMPT_PY = QTILE / "qtile_prompt_lib.py"
CAPTURE_ORG = QTILE / "qtile-capture.org"
CAPTURE_PY = QTILE / "qtile_capture.py"
QTILE_CONFIG = QTILE / "config.py"
EMACS_CLIENT = ROOT / "lisp" / "llm" / "prompt-lib.el"
AI_INIT = ROOT / "lisp" / "llm" / "ai-init.el"


def single_python_block(path: Path) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    start = lines.index("#+begin_src python") + 1
    end = lines.index("#+end_src", start)
    return "\n".join(lines[start:end]) + "\n"


class PromptLibBindingTests(unittest.TestCase):
    def test_prompt_binding_helper_compiles(self):
        py_compile.compile(str(PROMPT_PY), doraise=True)

    def test_prompt_binding_literate_source_matches_runtime(self):
        self.assertEqual(
            single_python_block(PROMPT_ORG),
            PROMPT_PY.read_text(encoding="utf-8"),
        )

    def test_capture_extension_source_matches_runtime(self):
        self.assertEqual(
            single_python_block(CAPTURE_ORG),
            CAPTURE_PY.read_text(encoding="utf-8"),
        )

    def test_super_e_y_p_opens_prompt_library_and_preserves_existing_y_action(self):
        source = PROMPT_PY.read_text(encoding="utf-8")
        base_config = QTILE_CONFIG.read_text(encoding="utf-8")
        self.assertIn('KeyChord(', source)
        self.assertIn('"p"', source)
        self.assertIn("ai/prompt-lib-browse", source)
        self.assertIn('ai_submappings = [submappings[y_index], prompt_binding]', source)
        self.assertIn("+gptel/here", base_config)
        self.assertIn("Super e y p", source)
        self.assertIn("Super e y y", source)

    def test_auxiliary_installer_composes_prompt_binding_before_telemetry(self):
        source = CAPTURE_PY.read_text(encoding="utf-8")
        self.assertIn("from qtile_prompt_lib import install_prompt_lib_bindings", source)
        self.assertIn("install_prompt_lib_bindings(config_globals)", source)

    def test_emacs_client_is_loaded_and_exposes_copy_browser(self):
        client = EMACS_CLIENT.read_text(encoding="utf-8")
        init = AI_INIT.read_text(encoding="utf-8")
        self.assertIn("(require 'prompt-lib)", init)
        self.assertIn("(defun ai/prompt-lib-browse", client)
        self.assertIn("(defun ai/prompt-lib-copy", client)
        self.assertIn('(define-key map (kbd "c") #\'ai/prompt-lib-browser-copy)', client)


if __name__ == "__main__":
    unittest.main()
