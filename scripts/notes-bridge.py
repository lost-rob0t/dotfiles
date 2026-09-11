#!/usr/bin/env python3
"""Bidirectional Logseq <-> Org-roam bridge.

The bridge is conservative: it tracks last-synced hashes and never silently
overwrites a note that changed on both sides.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import sys
import uuid
from dataclasses import dataclass
from pathlib import Path

DEFAULT_LOGSEQ = Path(os.environ.get("NOTES_LOGSEQ_DIR", "~/Documents/Notes/log")).expanduser()
DEFAULT_ROAM = Path(os.environ.get("NOTES_ORG_ROAM_DIR", "~/Documents/Notes/org/roam")).expanduser()
DEFAULT_STATE = Path(os.environ.get("NOTES_BRIDGE_STATE", "~/.local/state/notes-bridge/state.json")).expanduser()

PAGE_PROP_RE = re.compile(r"^([A-Za-z0-9_-]+)::\s*(.*)$")
LOGSEQ_BULLET_RE = re.compile(r"^([ \t]*)-\s+(.*)$")
ORG_HEADING_RE = re.compile(r"^(\*+)\s+(.*)$")
ORG_TITLE_RE = re.compile(r"^#\+title:\s*(.*)$", re.I)
ORG_FILETAGS_RE = re.compile(r"^#\+filetags:\s*(.*)$", re.I)
ORG_ID_LINK_RE = re.compile(r"\[\[id:[^\]]+\]\[([^\]]+)\]\]")
ORG_FILE_LINK_RE = re.compile(r"\[\[file:[^\]]+\.org\]\[([^\]]+)\]\]")
LOGSEQ_REF_RE = re.compile(r"\[\[([^\]]+)\]\]")
MD_LINK_RE = re.compile(r"\[([^\]]+)\]\(([^)]+)\)")
ORG_EXT_LINK_RE = re.compile(r"\[\[(https?://[^\]]+)\]\[([^\]]+)\]\]")
DATE_RE = re.compile(r"^(\d{4})[-_](\d{2})[-_](\d{2})$")


@dataclass(frozen=True)
class Note:
    key: str
    title: str
    kind: str
    path: Path
    rel: str


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def deterministic_id(kind: str, title: str) -> str:
    return str(uuid.uuid5(uuid.NAMESPACE_URL, f"notes-bridge:{kind}:{title.casefold()}"))


def safe_page_filename(title: str) -> str:
    text = title.strip().replace("/", "___").replace("\\", "___")
    text = re.sub(r"[\x00-\x1f<>:\"|?*]", "_", text)
    return (text or "untitled") + ".md"


def safe_org_filename(title: str) -> str:
    text = title.strip().replace("/", "___").replace("\\", "___")
    text = re.sub(r"[\x00-\x1f<>:\"|?*]", "_", text)
    return (text or "untitled") + ".org"


def date_from_stem(stem: str) -> str | None:
    m = DATE_RE.fullmatch(stem)
    if not m:
        return None
    return "-".join(m.groups())


def logseq_title(path: Path, kind: str) -> str:
    if kind == "journal":
        return date_from_stem(path.stem) or path.stem
    try:
        for line in path.read_text(encoding="utf-8").splitlines()[:40]:
            m = PAGE_PROP_RE.match(line)
            if m and m.group(1).lower() == "title":
                return m.group(2).strip()
    except OSError:
        pass
    return path.stem.replace("___", "/")


def org_title(path: Path, kind: str) -> str:
    if kind == "journal":
        return date_from_stem(path.stem) or path.stem
    try:
        for line in path.read_text(encoding="utf-8").splitlines()[:80]:
            m = ORG_TITLE_RE.match(line)
            if m:
                return m.group(1).strip()
    except OSError:
        pass
    return path.stem.replace("___", "/")


def scan_logseq(root: Path) -> dict[str, Note]:
    notes: dict[str, Note] = {}
    for kind, dirname in (("page", "pages"), ("journal", "journals")):
        base = root / dirname
        if not base.exists():
            continue
        for path in sorted(base.rglob("*")):
            if not path.is_file() or path.suffix.lower() not in {".md", ".markdown"}:
                continue
            title = logseq_title(path, kind)
            key = f"{kind}:{title.casefold()}"
            notes[key] = Note(key, title, kind, path, str(path.relative_to(root)))
    return notes


def scan_roam(root: Path) -> dict[str, Note]:
    notes: dict[str, Note] = {}
    if not root.exists():
        return notes
    for path in sorted(root.rglob("*.org")):
        rel_path = path.relative_to(root)
        if any(part.startswith(".") for part in rel_path.parts):
            continue
        kind = "journal" if rel_path.parts and rel_path.parts[0] in {"daily", "dailies"} and date_from_stem(path.stem) else "page"
        title = org_title(path, kind)
        key = f"{kind}:{title.casefold()}"
        notes[key] = Note(key, title, kind, path, str(rel_path))
    return notes


def load_state(path: Path) -> dict:
    if not path.exists():
        return {"version": 1, "notes": {}}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise SystemExit(f"notes-bridge: cannot read state {path}: {exc}")
    if data.get("version") != 1 or not isinstance(data.get("notes"), dict):
        raise SystemExit(f"notes-bridge: unsupported state format in {path}")
    return data


def save_state(path: Path, state: dict, dry_run: bool) -> None:
    if dry_run:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(state, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    os.replace(tmp, path)


def parse_logseq_page_properties(lines: list[str]) -> tuple[dict[str, str], int]:
    props: dict[str, str] = {}
    i = 0
    while i < len(lines):
        line = lines[i]
        if not line.strip():
            i += 1
            continue
        m = PAGE_PROP_RE.match(line)
        if not m or line[:1].isspace():
            break
        props[m.group(1).lower()] = m.group(2).strip()
        i += 1
    return props, i


def logseq_refs_to_org(text: str, title_to_id: dict[str, str]) -> str:
    def repl(match: re.Match[str]) -> str:
        title = match.group(1).strip()
        if title.lower().startswith(("http://", "https://", "file:", "id:")):
            return match.group(0)
        node_id = title_to_id.get(title.casefold())
        if node_id:
            return f"[[id:{node_id}][{title}]]"
        return match.group(0)

    text = LOGSEQ_REF_RE.sub(repl, text)
    text = MD_LINK_RE.sub(lambda m: f"[[{m.group(2)}][{m.group(1)}]]", text)
    return text.replace("../assets/", "assets/")


def org_refs_to_logseq(text: str) -> str:
    text = ORG_ID_LINK_RE.sub(lambda m: f"[[{m.group(1)}]]", text)
    text = ORG_FILE_LINK_RE.sub(lambda m: f"[[{m.group(1)}]]", text)
    text = ORG_EXT_LINK_RE.sub(lambda m: f"[{m.group(2)}]({m.group(1)})", text)
    return text.replace("assets/", "../assets/")


def logseq_to_org_text(note: Note, title_to_id: dict[str, str]) -> str:
    lines = note.path.read_text(encoding="utf-8").splitlines()
    page_props, start = parse_logseq_page_properties(lines)

    node_id = page_props.get("id") or deterministic_id(note.kind, note.title)
    out = [":PROPERTIES:", f":ID: {node_id}"]
    for key, value in sorted(page_props.items()):
        if key in {"id", "title", "tags"}:
            continue
        prop_key = "LOGSEQ_" + re.sub(r"[^A-Za-z0-9_]", "_", key.upper())
        out.append(f":{prop_key}: {value}")
    out.append(":END:")
    out.append(f"#+title: {note.title}")
    if tags := page_props.get("tags"):
        tags_list = [t.strip().lstrip("#") for t in re.split(r"[, ]+", tags) if t.strip()]
        if tags_list:
            out.append("#+filetags: :" + ":".join(tags_list) + ":")
    out.append("")

    last_was_heading = False
    for raw in lines[start:]:
        bullet = LOGSEQ_BULLET_RE.match(raw)
        if bullet:
            indent = bullet.group(1).replace("\t", "  ")
            level = max(1, len(indent) // 2 + 1)
            content = logseq_refs_to_org(bullet.group(2), title_to_id)
            out.append("*" * level + " " + content)
            last_was_heading = True
            continue

        prop = PAGE_PROP_RE.match(raw.lstrip())
        if prop and raw[:1].isspace() and last_was_heading:
            key = prop.group(1).lower()
            value = prop.group(2).strip()
            prop_key = "ID" if key == "id" else "LOGSEQ_" + re.sub(r"[^A-Za-z0-9_]", "_", key.upper())
            out.extend([":PROPERTIES:", f":{prop_key}: {value}", ":END:"])
            last_was_heading = False
            continue

        last_was_heading = False
        out.append(logseq_refs_to_org(raw, title_to_id) if raw.strip() else "")

    return "\n".join(out).rstrip() + "\n"


def parse_org_file_properties(lines: list[str]) -> tuple[dict[str, str], int]:
    props: dict[str, str] = {}
    if not lines or lines[0].strip() != ":PROPERTIES:":
        return props, 0
    i = 1
    while i < len(lines):
        line = lines[i].strip()
        if line == ":END:":
            return props, i + 1
        m = re.match(r"^:([^:]+):\s*(.*)$", line)
        if m:
            props[m.group(1).upper()] = m.group(2).strip()
        i += 1
    return {}, 0


def org_to_logseq_text(note: Note) -> str:
    lines = note.path.read_text(encoding="utf-8").splitlines()
    props, start = parse_org_file_properties(lines)

    out: list[str] = []
    if note.kind == "page":
        out.append(f"title:: {note.title}")
    if node_id := props.get("ID"):
        out.append(f"id:: {node_id}")
    for key, value in sorted(props.items()):
        if key == "ID" or not key.startswith("LOGSEQ_"):
            continue
        out.append(f"{key[len('LOGSEQ_'):].lower()}:: {value}")

    filetags: list[str] = []
    body_start = start
    while body_start < len(lines):
        line = lines[body_start]
        if ORG_TITLE_RE.match(line):
            body_start += 1
            continue
        tag_match = ORG_FILETAGS_RE.match(line)
        if tag_match:
            filetags = [t for t in tag_match.group(1).strip(":").split(":") if t]
            body_start += 1
            continue
        if not line.strip():
            body_start += 1
            continue
        break
    if filetags:
        out.append("tags:: " + ", ".join(filetags))
    if out:
        out.append("")

    i = body_start
    while i < len(lines):
        line = lines[i]
        heading = ORG_HEADING_RE.match(line)
        if heading:
            level = len(heading.group(1))
            content = org_refs_to_logseq(heading.group(2))
            out.append("  " * (level - 1) + "- " + content)

            if i + 1 < len(lines) and lines[i + 1].strip() == ":PROPERTIES:":
                j = i + 2
                block_props: list[tuple[str, str]] = []
                while j < len(lines) and lines[j].strip() != ":END:":
                    m = re.match(r"^\s*:([^:]+):\s*(.*)$", lines[j])
                    if m:
                        block_props.append((m.group(1).upper(), m.group(2).strip()))
                    j += 1
                if j < len(lines):
                    for key, value in block_props:
                        if key == "ID":
                            out.append("  " * level + f"id:: {value}")
                        elif key.startswith("LOGSEQ_"):
                            out.append("  " * level + f"{key[len('LOGSEQ_'):].lower()}:: {value}")
                    i = j + 1
                    continue
            i += 1
            continue

        if line.startswith("#+"):
            i += 1
            continue
        out.append(org_refs_to_logseq(line))
        i += 1

    return "\n".join(out).rstrip() + "\n"


def destination_for_log(note: Note, roam_root: Path, state_entry: dict | None) -> Path:
    if state_entry and state_entry.get("org_path"):
        return roam_root / state_entry["org_path"]
    if note.kind == "journal":
        return roam_root / "daily" / f"{note.title}.org"
    return roam_root / safe_org_filename(note.title)


def destination_for_org(note: Note, log_root: Path, state_entry: dict | None) -> Path:
    if state_entry and state_entry.get("log_path"):
        return log_root / state_entry["log_path"]
    if note.kind == "journal":
        date = note.title.replace("-", "_")
        return log_root / "journals" / f"{date}.md"
    return log_root / "pages" / safe_page_filename(note.title)


def write_text(path: Path, text: str, dry_run: bool) -> None:
    if dry_run:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, path)


def copy_assets(src: Path, dst: Path, dry_run: bool, force: bool) -> tuple[int, list[str]]:
    src_assets = src / "assets"
    dst_assets = dst / "assets"
    copied = 0
    conflicts: list[str] = []
    if not src_assets.exists():
        return copied, conflicts

    for source in sorted(src_assets.rglob("*")):
        if not source.is_file():
            continue
        rel = source.relative_to(src_assets)
        target = dst_assets / rel
        if target.exists() and sha256(source) != sha256(target) and not force:
            conflicts.append(f"asset:{rel}")
            continue
        if target.exists() and sha256(source) == sha256(target):
            continue
        if not dry_run:
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
        copied += 1
    return copied, conflicts


def record_state(entry: dict, log_path: Path, org_path: Path, log_root: Path, roam_root: Path, dry_run: bool) -> None:
    entry["log_path"] = str(log_path.relative_to(log_root))
    entry["org_path"] = str(org_path.relative_to(roam_root))
    if not dry_run:
        entry["log_sha"] = sha256(log_path)
        entry["org_sha"] = sha256(org_path)


def convert_log_note(note: Note, log_root: Path, roam_root: Path, title_to_id: dict[str, str], entry: dict, *, dry_run: bool, force: bool) -> tuple[str, Path]:
    dest = destination_for_log(note, roam_root, entry)
    if dest.exists() and not force:
        old_sha = entry.get("org_sha")
        if not old_sha or sha256(dest) != old_sha:
            return "conflict", dest
    text = logseq_to_org_text(note, title_to_id)
    write_text(dest, text, dry_run)
    if not dry_run:
        record_state(entry, note.path, dest, log_root, roam_root, dry_run=False)
    return "write", dest


def convert_org_note(note: Note, log_root: Path, roam_root: Path, entry: dict, *, dry_run: bool, force: bool) -> tuple[str, Path]:
    dest = destination_for_org(note, log_root, entry)
    if dest.exists() and not force:
        old_sha = entry.get("log_sha")
        if not old_sha or sha256(dest) != old_sha:
            return "conflict", dest
    text = org_to_logseq_text(note)
    write_text(dest, text, dry_run)
    if not dry_run:
        record_state(entry, dest, note.path, log_root, roam_root, dry_run=False)
    return "write", dest


def title_id_map(log_notes: dict[str, Note]) -> dict[str, str]:
    result: dict[str, str] = {}
    for note in log_notes.values():
        try:
            props, _ = parse_logseq_page_properties(note.path.read_text(encoding="utf-8").splitlines())
        except OSError:
            props = {}
        result[note.title.casefold()] = props.get("id") or deterministic_id(note.kind, note.title)
    return result


def cmd_one_way(args: argparse.Namespace, direction: str) -> int:
    log_root = args.logseq.expanduser()
    roam_root = args.roam.expanduser()
    state = load_state(args.state.expanduser())
    log_notes = scan_logseq(log_root)
    roam_notes = scan_roam(roam_root)
    ids = title_id_map(log_notes)

    source = log_notes if direction == "logseq-to-roam" else roam_notes
    writes = 0
    conflicts: list[str] = []

    for key, note in source.items():
        entry = state["notes"].setdefault(key, {})
        if direction == "logseq-to-roam":
            entry.setdefault("log_path", note.rel)
            if existing := roam_notes.get(key):
                entry.setdefault("org_path", existing.rel)
            status, dest = convert_log_note(note, log_root, roam_root, ids, entry, dry_run=args.dry_run, force=args.force)
        else:
            entry.setdefault("org_path", note.rel)
            if existing := log_notes.get(key):
                entry.setdefault("log_path", existing.rel)
            status, dest = convert_org_note(note, log_root, roam_root, entry, dry_run=args.dry_run, force=args.force)
        if status == "conflict":
            conflicts.append(f"{key} -> {dest}")
        else:
            writes += 1

    asset_writes, asset_conflicts = copy_assets(
        log_root if direction == "logseq-to-roam" else roam_root,
        roam_root if direction == "logseq-to-roam" else log_root,
        args.dry_run,
        args.force,
    )
    conflicts.extend(asset_conflicts)
    save_state(args.state.expanduser(), state, args.dry_run)

    print(f"{direction}: {writes} notes, {asset_writes} assets, {len(conflicts)} conflicts")
    for conflict in conflicts:
        print(f"CONFLICT {conflict}", file=sys.stderr)
    return 2 if conflicts else 0


def cmd_sync(args: argparse.Namespace) -> int:
    log_root = args.logseq.expanduser()
    roam_root = args.roam.expanduser()
    state = load_state(args.state.expanduser())
    log_notes = scan_logseq(log_root)
    roam_notes = scan_roam(roam_root)
    ids = title_id_map(log_notes)

    writes = 0
    conflicts: list[str] = []

    for key in sorted(set(log_notes) | set(roam_notes)):
        log_note = log_notes.get(key)
        org_note = roam_notes.get(key)
        entry = state["notes"].setdefault(key, {})
        if log_note:
            entry.setdefault("log_path", log_note.rel)
        if org_note:
            entry.setdefault("org_path", org_note.rel)

        if log_note and not org_note:
            status, _ = convert_log_note(log_note, log_root, roam_root, ids, entry, dry_run=args.dry_run, force=True)
            writes += int(status == "write")
            continue

        if org_note and not log_note:
            status, _ = convert_org_note(org_note, log_root, roam_root, entry, dry_run=args.dry_run, force=True)
            writes += int(status == "write")
            continue

        assert log_note and org_note
        log_hash = sha256(log_note.path)
        org_hash = sha256(org_note.path)
        old_log = entry.get("log_sha")
        old_org = entry.get("org_sha")

        if not old_log or not old_org:
            if args.prefer == "logseq":
                status, _ = convert_log_note(log_note, log_root, roam_root, ids, entry, dry_run=args.dry_run, force=True)
                writes += int(status == "write")
            elif args.prefer == "roam":
                status, _ = convert_org_note(org_note, log_root, roam_root, entry, dry_run=args.dry_run, force=True)
                writes += int(status == "write")
            else:
                conflicts.append(f"{key}: exists on both sides with no sync baseline; use --prefer logseq|roam once")
            continue

        log_changed = log_hash != old_log
        org_changed = org_hash != old_org
        if log_changed and org_changed:
            if args.prefer == "logseq":
                status, _ = convert_log_note(log_note, log_root, roam_root, ids, entry, dry_run=args.dry_run, force=True)
                writes += int(status == "write")
            elif args.prefer == "roam":
                status, _ = convert_org_note(org_note, log_root, roam_root, entry, dry_run=args.dry_run, force=True)
                writes += int(status == "write")
            else:
                conflicts.append(f"{key}: changed in both Logseq and Org-roam")
        elif log_changed:
            status, _ = convert_log_note(log_note, log_root, roam_root, ids, entry, dry_run=args.dry_run, force=True)
            writes += int(status == "write")
        elif org_changed:
            status, _ = convert_org_note(org_note, log_root, roam_root, entry, dry_run=args.dry_run, force=True)
            writes += int(status == "write")
        elif not args.dry_run:
            record_state(entry, log_note.path, org_note.path, log_root, roam_root, dry_run=False)

    a1, c1 = copy_assets(log_root, roam_root, args.dry_run, force=False)
    a2, c2 = copy_assets(roam_root, log_root, args.dry_run, force=False)
    conflicts.extend(c1)
    conflicts.extend(c2)
    save_state(args.state.expanduser(), state, args.dry_run)

    print(f"sync: {writes} notes, {a1 + a2} assets, {len(conflicts)} conflicts")
    for conflict in conflicts:
        print(f"CONFLICT {conflict}", file=sys.stderr)
    return 2 if conflicts else 0


def cmd_status(args: argparse.Namespace) -> int:
    log_root = args.logseq.expanduser()
    roam_root = args.roam.expanduser()
    state = load_state(args.state.expanduser())
    log_notes = scan_logseq(log_root)
    roam_notes = scan_roam(roam_root)

    for key in sorted(set(log_notes) | set(roam_notes)):
        entry = state["notes"].get(key, {})
        l = log_notes.get(key)
        r = roam_notes.get(key)
        if l and r and entry.get("log_sha") and entry.get("org_sha"):
            lc = sha256(l.path) != entry["log_sha"]
            rc = sha256(r.path) != entry["org_sha"]
            status = "conflict" if lc and rc else "logseq-changed" if lc else "roam-changed" if rc else "clean"
        elif l and r:
            status = "unbaselined"
        elif l:
            status = "logseq-only"
        else:
            status = "roam-only"
        print(f"{status:16} {key}")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Conservative Logseq <-> Org-roam bridge")
    parser.add_argument("--logseq", type=Path, default=DEFAULT_LOGSEQ, help=f"Logseq graph (default: {DEFAULT_LOGSEQ})")
    parser.add_argument("--roam", type=Path, default=DEFAULT_ROAM, help=f"Org-roam directory (default: {DEFAULT_ROAM})")
    parser.add_argument("--state", type=Path, default=DEFAULT_STATE, help=f"sync state (default: {DEFAULT_STATE})")

    sub = parser.add_subparsers(dest="command", required=True)
    for name in ("logseq-to-roam", "roam-to-logseq"):
        p = sub.add_parser(name)
        p.add_argument("--dry-run", action="store_true")
        p.add_argument("--force", action="store_true", help="overwrite destination notes/assets even when changed")

    p = sub.add_parser("sync")
    p.add_argument("--dry-run", action="store_true")
    p.add_argument("--prefer", choices=("logseq", "roam"), help="resolve unbaselined or two-sided conflicts from one side")

    sub.add_parser("status")
    return parser


def main() -> int:
    args = build_parser().parse_args()
    if args.command in {"logseq-to-roam", "roam-to-logseq"}:
        return cmd_one_way(args, args.command)
    if args.command == "sync":
        return cmd_sync(args)
    if args.command == "status":
        return cmd_status(args)
    raise AssertionError(args.command)


if __name__ == "__main__":
    raise SystemExit(main())
