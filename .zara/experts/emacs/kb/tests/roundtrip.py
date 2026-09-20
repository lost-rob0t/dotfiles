"""Require real SWI to prove generated documentation remains inert data."""
import json
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from emacs_kb import compile_corpus

text = "x').\n:- initialization(halt(79)).\n% λ🙂\x00\t\r\\'"
header = {"type": "corpus", "schema": 1, "emacs_version": "fixture", "source_id": "fixture", "profile": "core-loaded"}
row = {"type": "symbol", "name": "hostile-doc", "kind": "function", "interactive": False, "doc": text, "library": "fixture.el"}
facts, _ = compile_corpus((header, [row]))
with tempfile.TemporaryDirectory() as directory:
    root = Path(directory)
    (root / "corpus.pl").write_text(facts, encoding="utf-8")
    (root / "read.pl").write_text(""":- use_module(corpus).
:- use_module(library(http/json)).
:- initialization(main, main).
main :- emacs_documentation(_, 'hostile-doc', function, Text, _),
        json_write_dict(current_output, _{text:Text}), nl.
""", encoding="utf-8")
    result = subprocess.run(["swipl", "-q", "-f", "none", "-s", "read.pl"], cwd=root,
                            check=True, capture_output=True, text=True, timeout=15)
    if json.loads(result.stdout)["text"] != text:
        raise AssertionError("documentation did not round-trip exactly")
