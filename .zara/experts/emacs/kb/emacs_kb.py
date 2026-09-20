"""Compile inert Emacs documentation JSONL into a versioned Prolog corpus."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import tempfile
from collections import Counter
from pathlib import Path
from typing import BinaryIO

MAX_BYTES = 64 * 1024 * 1024
MAX_LINE_BYTES = 1024 * 1024
MAX_RECORDS = 100_000
MAX_DOC_BYTES = 256 * 1024
HEADER_KEYS = {"type", "schema", "emacs_version", "source_id", "profile"}
SYMBOL_KEYS = {"type", "name", "kind", "interactive", "doc", "library"}
KINDS = {"function", "variable", "face"}


class CorpusError(ValueError):
    """The corpus violates its closed schema or finite resource limits."""


def _text(value: object, label: str, limit: int, *, empty: bool = False) -> str:
    if not isinstance(value, str) or (not empty and not value):
        raise CorpusError(f"{label} must be {'a' if empty else 'a nonempty'} string")
    try:
        size = len(value.encode("utf-8"))
    except UnicodeError as error:
        raise CorpusError(f"{label} contains invalid Unicode") from error
    if size > limit:
        raise CorpusError(f"{label} exceeds {limit} bytes")
    return value


def _object(pairs: list[tuple[str, object]]) -> dict:
    result = {}
    for key, value in pairs:
        if key in result:
            raise CorpusError("duplicate JSON key")
        result[key] = value
    return result


def _header(row: object) -> dict:
    if not isinstance(row, dict) or set(row) != HEADER_KEYS:
        raise CorpusError("invalid corpus header fields")
    if row["type"] != "corpus" or type(row["schema"]) is not int or row["schema"] != 1:
        raise CorpusError("unsupported corpus schema")
    if row["profile"] != "core-loaded":
        raise CorpusError("unsupported extraction profile")
    _text(row["emacs_version"], "emacs_version", 64)
    _text(row["source_id"], "source_id", 4096)
    return row


def _symbol(row: object) -> dict:
    if not isinstance(row, dict) or set(row) != SYMBOL_KEYS or row["type"] != "symbol":
        raise CorpusError("invalid symbol fields")
    _text(row["name"], "name", 1024)
    _text(row["kind"], "kind", 32)
    if row["kind"] not in KINDS:
        raise CorpusError("unsupported symbol kind")
    if type(row["interactive"]) is not bool:
        raise CorpusError("interactive must be a boolean")
    if row["interactive"] and row["kind"] != "function":
        raise CorpusError("only a function may be interactive")
    _text(row["library"], "library", 4096)
    if row["doc"] is not None:
        _text(row["doc"], "doc", MAX_DOC_BYTES, empty=True)
    return row


def read_corpus(
    stream: BinaryIO, *, max_bytes: int = MAX_BYTES,
    max_line_bytes: int = MAX_LINE_BYTES, max_records: int = MAX_RECORDS,
) -> tuple[dict, list[dict]]:
    """Read bounded UTF-8 JSONL; the first nonblank record must be a header."""
    for value, ceiling in ((max_bytes, MAX_BYTES), (max_line_bytes, MAX_LINE_BYTES), (max_records, MAX_RECORDS)):
        if type(value) is not int or not 1 <= value <= ceiling:
            raise CorpusError("limits must be positive integers within host ceilings")
    total = 0
    metadata = None
    rows = []
    seen = set()
    while True:
        line = stream.readline(min(max_line_bytes + 1, max_bytes - total + 1))
        if not line:
            break
        total += len(line)
        if total > max_bytes or len(line) > max_line_bytes:
            raise CorpusError("input byte limit exceeded")
        if not line.strip():
            continue
        try:
            row = json.loads(line.decode("utf-8"), object_pairs_hook=_object)
        except (UnicodeError, ValueError, RecursionError) as error:
            raise CorpusError("invalid UTF-8 JSON record") from error
        if metadata is None:
            metadata = _header(row)
            continue
        if len(rows) >= max_records:
            raise CorpusError("symbol count limit exceeded")
        row = _symbol(row)
        key = (row["kind"], row["name"])
        if key in seen:
            raise CorpusError("duplicate symbol role")
        seen.add(key)
        rows.append(row)
    if metadata is None or not rows:
        raise CorpusError("a header and at least one symbol are required")
    return metadata, rows


def quote_atom(value: str) -> str:
    """Quote one SWI-Prolog atom with character_escapes enabled."""
    _text(value, "atom", MAX_BYTES, empty=True)
    escaped = []
    substitutions = {"\\": "\\\\", "'": "\\'", "\n": "\\n", "\r": "\\r", "\t": "\\t"}
    for char in value:
        if char in substitutions:
            escaped.append(substitutions[char])
        elif ord(char) < 32 or ord(char) == 127:
            escaped.append(f"\\x{ord(char):x}\\")
        else:
            escaped.append(char)
    return "'" + "".join(escaped) + "'"


def _json(value: object) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def compile_corpus(corpus: tuple[dict, list[dict]]) -> tuple[str, dict]:
    """Compile a read_corpus result without modifying it or executing its text."""
    metadata, input_rows = corpus
    _header(metadata)
    if not isinstance(input_rows, list) or not 1 <= len(input_rows) <= MAX_RECORDS:
        raise CorpusError("invalid symbol count")
    rows = sorted((_symbol(row) for row in input_rows), key=lambda row: (row["kind"], row["name"]))
    if len({(row["kind"], row["name"]) for row in rows}) != len(rows):
        raise CorpusError("duplicate symbol role")
    normalized = (_json(metadata) + "\n" + "\n".join(_json(row) for row in rows) + "\n").encode("utf-8")
    if len(normalized) > MAX_BYTES:
        raise CorpusError("normalized corpus exceeds byte limit")
    corpus_id = hashlib.sha256(normalized).hexdigest()
    qid = quote_atom(corpus_id)
    lines = [
        ":- encoding(utf8).",
        ":- module(zara_emacs_corpus, [emacs_corpus/4, emacs_symbol/6, emacs_documentation/5]).",
        ":- set_prolog_flag(character_escapes, true).",
        f"emacs_corpus({qid},{quote_atom(metadata['emacs_version'])},{quote_atom(metadata['source_id'])},'core-loaded').",
    ]
    counts = Counter(symbols=len(rows), documented=0, undocumented=0, commands=0)
    docs = []
    for row in rows:
        name, kind, library = (quote_atom(row[key]) for key in ("name", "kind", "library"))
        state = "undocumented" if row["doc"] is None else "documented"
        counts[state] += 1
        counts["commands"] += int(row["interactive"])
        interactive = "true" if row["interactive"] else "false"
        lines.append(f"emacs_symbol({qid},{name},{kind},{interactive},{library},{state}).")
        if row["doc"] is not None:
            digest = hashlib.sha256(row["doc"].encode("utf-8")).hexdigest()
            docs.append(f"emacs_documentation({qid},{name},{kind},{quote_atom(row['doc'])},{quote_atom(digest)}).")
    lines.extend(docs or ["emacs_documentation(_, _, _, _, _) :- fail."])
    facts = "\n".join(lines) + "\n"
    encoded_facts = facts.encode("utf-8")
    if len(encoded_facts) > 128 * 1024 * 1024:
        raise CorpusError("compiled facts exceed byte limit")
    manifest = {
        "schema": 1, "corpus_id": corpus_id, "source": dict(metadata),
        "counts": dict(counts), "facts_sha256": hashlib.sha256(encoded_facts).hexdigest(),
        "complete_emacs_coverage": False,
        "not_covered": ["info-manuals", "unloaded-package-definitions", "source-declarations", "private-configuration", "live-keymaps"],
        "library_semantics": "filename hint, not an exact definition location",
    }
    return facts, manifest


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    stage = None
    try:
        with args.input.open("rb") as stream:
            facts, manifest = compile_corpus(read_corpus(stream))
        if args.out.exists():
            raise CorpusError("output already exists; choose a new generation directory")
        args.out.parent.mkdir(parents=True, exist_ok=True)
        stage = Path(tempfile.mkdtemp(prefix=".emacs-kb-", dir=args.out.parent))
        (stage / "corpus.pl").write_text(facts, encoding="utf-8")
        (stage / "manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, sort_keys=True, indent=2) + "\n", encoding="utf-8")
        os.rename(stage, args.out)
        stage = None
    except (CorpusError, OSError) as error:
        parser.exit(2, f"emacs-kb: {error}\n")
    finally:
        if stage is not None:
            shutil.rmtree(stage)


if __name__ == "__main__":
    main()
