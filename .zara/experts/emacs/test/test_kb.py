"""Deterministic compiler tests; no Emacs, network or Zara runtime required."""
from __future__ import annotations

import io
import json
import sys
import subprocess
import tempfile
import hashlib
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "kb"))
from emacs_kb import CorpusError, compile_corpus, read_corpus, quote_atom


def header():
    return {"type": "corpus", "schema": 1, "emacs_version": "30.2", "source_id": "fixture:emacs", "profile": "core-loaded"}


def symbol(name="forward-char", kind="function", doc="Move point forward.", interactive=True):
    return {"type": "symbol", "name": name, "kind": kind, "doc": doc, "interactive": interactive, "library": "simple.el"}


def encode(*rows):
    return io.BytesIO(("\n".join(json.dumps(row, ensure_ascii=False) for row in rows) + "\n").encode("utf-8"))


class EmacsKnowledgeCompilerTests(unittest.TestCase):
    def compile(self, *rows):
        return compile_corpus(read_corpus(encode(header(), *rows)))

    def test_documented_command_has_provenance(self):
        facts, manifest = self.compile(symbol())
        self.assertIn("emacs_symbol(", facts)
        self.assertIn("'forward-char','function',true,'simple.el',documented", facts)
        self.assertIn("emacs_documentation(", facts)
        self.assertEqual(manifest["counts"], {"symbols": 1, "documented": 1, "undocumented": 0, "commands": 1})
        self.assertEqual(len(manifest["corpus_id"]), 64)

    def test_order_independent_determinism(self):
        a, b = symbol("a"), symbol("b")
        self.assertEqual(self.compile(a, b), self.compile(b, a))

    def test_changed_text_changes_corpus_identity(self):
        self.assertNotEqual(self.compile(symbol())[1]["corpus_id"], self.compile(symbol(doc="Changed."))[1]["corpus_id"])

    def test_changed_source_changes_identity(self):
        original = self.compile(symbol())[1]["corpus_id"]
        changed = header() | {"source_id": "fixture:other"}
        other = compile_corpus(read_corpus(encode(changed, symbol())))[1]["corpus_id"]
        self.assertNotEqual(original, other)

    def test_undocumented_is_explicit(self):
        facts, manifest = self.compile(symbol(doc=None))
        self.assertIn("undocumented", facts)
        self.assertEqual(manifest["counts"]["undocumented"], 1)
        self.assertIn("unloaded-package-definitions", manifest["not_covered"])
        self.assertFalse(manifest["complete_emacs_coverage"])

    def test_symbol_can_have_multiple_roles(self):
        _, manifest = self.compile(symbol("same"), symbol("same", "variable", interactive=False))
        self.assertEqual(manifest["counts"]["symbols"], 2)

    def test_duplicate_role_rejected(self):
        with self.assertRaises(CorpusError):
            self.compile(symbol(), symbol())

    def test_duplicate_json_keys_rejected(self):
        with self.assertRaises(CorpusError):
            read_corpus(io.BytesIO(b'{"type":"corpus","type":"symbol"}\n'))

    def test_unknown_fields_rejected(self):
        with self.assertRaises(CorpusError):
            self.compile(symbol() | {"goal": "shell(rm)"})

    def test_unknown_kind_rejected(self):
        with self.assertRaises(CorpusError):
            self.compile(symbol(kind=":- initialization"))

    def test_strict_boolean(self):
        for value in (1, 0, "true", None):
            with self.subTest(value=value), self.assertRaises(CorpusError):
                self.compile(symbol(interactive=value))

    def test_nonfunction_cannot_be_command(self):
        with self.assertRaises(CorpusError):
            self.compile(symbol(kind="variable"))

    def test_strict_schema_version(self):
        for value in (True, "1", 2):
            with self.subTest(value=value), self.assertRaises(CorpusError):
                read_corpus(encode(header() | {"schema": value}, symbol()))

    def test_missing_header_and_empty_corpus_rejected(self):
        for rows in ((), (header(),), (symbol(),), (header(), header())):
            with self.subTest(rows=rows), self.assertRaises(CorpusError):
                read_corpus(encode(*rows))

    def test_bad_utf8_and_surrogate_rejected(self):
        with self.assertRaises(CorpusError):
            read_corpus(io.BytesIO(b'\xff\n'))
        with self.assertRaises(CorpusError):
            quote_atom("\ud800")

    def test_row_limit(self):
        with self.assertRaises(CorpusError):
            read_corpus(encode(header(), symbol("a"), symbol("b")), max_records=1)

    def test_byte_limit(self):
        with self.assertRaises(CorpusError):
            read_corpus(encode(header(), symbol()), max_bytes=10)

    def test_line_limit(self):
        with self.assertRaises(CorpusError):
            read_corpus(encode(header(), symbol()), max_line_bytes=10)

    def test_doc_length_limit(self):
        with self.assertRaises(CorpusError):
            self.compile(symbol(doc="x" * (256 * 1024 + 1)))

    def test_hostile_text_quoted_as_one_atom(self):
        self.assertEqual(quote_atom("x').\n:- halt.\\"), "'x\\').\\n:- halt.\\\\'")
        facts, _ = self.compile(symbol(doc="x').\n:- halt.\\"))
        self.assertNotIn("\n:- halt.", facts)

    def test_unicode_controls_and_quotes(self):
        self.assertEqual(quote_atom("λ🙂\x00\t\r\n'\\"), "'λ🙂\\x0\\\\t\\r\\n\\'\\\\'")

    def test_null_and_empty_documentation_are_distinct(self):
        self.assertEqual(self.compile(symbol(doc=""))[1]["counts"]["documented"], 1)

    def test_input_objects_not_mutated(self):
        corpus = read_corpus(encode(header(), symbol("z"), symbol("a")))
        before = json.dumps(corpus)
        compile_corpus(corpus)
        self.assertEqual(json.dumps(corpus), before)

    def test_input_has_no_executable_predicate_names(self):
        facts, _ = self.compile(symbol(name="x). shell(bad).", doc=":- shell(bad)."))
        declarations = [line for line in facts.splitlines() if line.startswith(":-")]
        self.assertEqual(len(declarations), 3)
        self.assertTrue(all("shell" not in line for line in declarations))


    def test_cli_writes_valid_manifest_and_facts(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, output = root / "input.jsonl", root / "generation"
            source.write_bytes(encode(header(), symbol()).getvalue())
            result = self.run_cli(source, output)
            self.assertEqual(result.returncode, 0, result.stderr)
            manifest = json.loads((output / "manifest.json").read_text())
            self.assertEqual(manifest["facts_sha256"], hashlib.sha256((output / "corpus.pl").read_bytes()).hexdigest())

    def test_cli_invalid_input_publishes_nothing(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, output = root / "bad.jsonl", root / "generation"
            source.write_text("not json")
            self.assertEqual(self.run_cli(source, output).returncode, 2)
            self.assertFalse(output.exists())
            self.assertEqual(list(root.glob(".emacs-kb-*")), [])

    def test_cli_existing_generation_is_preserved(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, output = root / "input.jsonl", root / "generation"
            source.write_bytes(encode(header(), symbol()).getvalue())
            output.mkdir()
            marker = output / "keep.txt"
            marker.write_text("keep")
            self.assertEqual(self.run_cli(source, output).returncode, 2)
            self.assertEqual(marker.read_text(), "keep")
            self.assertFalse((output / "corpus.pl").exists())

    def test_cli_missing_input_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.assertEqual(self.run_cli(root / "missing", root / "out").returncode, 2)
            self.assertFalse((root / "out").exists())

    def run_cli(self, source, output):
        compiler = Path(__file__).resolve().parents[1] / "kb" / "emacs_kb.py"
        if not compiler.exists():
            import emacs_kb
            compiler = Path(emacs_kb.__file__)
        return subprocess.run([sys.executable, str(compiler), "--input", str(source), "--out", str(output)],
                              capture_output=True, text=True, timeout=10)


if __name__ == "__main__":
    unittest.main()
