#!/usr/bin/env python3

from __future__ import annotations

import os
from pathlib import Path
import stat
import sys
import tempfile

import tomlkit


LANGUAGE_EXPERTS = (
    "prolog",
    "python",
    "nim",
    "javascript",
    "typescript",
    "java",
    "kotlin",
    "nix",
    "bash",
)

LISP_EXPERTS = (
    "lisp",
    "common-lisp",
    "emacs-lisp",
)


def _table(parent, key: str):
    value = parent.get(key)
    if value is None:
        value = tomlkit.table()
        parent[key] = value
    if not hasattr(value, "__setitem__"):
        raise ValueError(f"{key} must be a TOML table")
    return value


def _source_map(expert_root: Path, names: tuple[str, ...]) -> dict[str, list[str]]:
    result: dict[str, list[str]] = {}
    for name in names:
        source = expert_root / name / "kb" / "expert.pl"
        if not source.is_file():
            raise FileNotFoundError(f"missing canonical Zara expert source: {source}")
        result[name] = [str(source)]
    return result


def _replace_table(parent, key: str, values: dict[str, list[str]]) -> None:
    table = tomlkit.table()
    for name in sorted(values):
        table[name] = values[name]
    parent[key] = table


def main() -> int:
    if len(sys.argv) != 3:
        print(
            "usage: apply-zara-expert-config CONFIG_TOML EXPERT_ROOT",
            file=sys.stderr,
        )
        return 2

    config_path = Path(sys.argv[1]).expanduser()
    expert_root = Path(sys.argv[2]).expanduser().resolve()

    if not expert_root.is_dir():
        raise FileNotFoundError(f"canonical Zara expert root is unavailable: {expert_root}")

    language_sources = _source_map(expert_root, LANGUAGE_EXPERTS)
    lisp_sources = _source_map(expert_root, LISP_EXPERTS)

    if config_path.exists():
        if config_path.is_symlink():
            raise RuntimeError(
                "refusing to mutate a symlinked config.toml; enable zara.nixManaged "
                "or make the file user-owned"
            )
        original = config_path.read_text(encoding="utf-8")
        document = tomlkit.parse(original)
        mode = stat.S_IMODE(config_path.stat().st_mode)
    else:
        document = tomlkit.document()
        mode = 0o600

    plugins = _table(document, "plugins")
    expert = _table(plugins, "zara-expert")
    _replace_table(expert, "language_expert_sources", language_sources)
    _replace_table(expert, "lisp_family_sources", lisp_sources)

    config_path.parent.mkdir(parents=True, exist_ok=True)
    rendered = tomlkit.dumps(document)
    fd, tmp_name = tempfile.mkstemp(
        prefix=".config.toml.",
        dir=config_path.parent,
        text=True,
    )
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            handle.write(rendered)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(tmp_name, mode)
        os.replace(tmp_name, config_path)
    except BaseException:
        try:
            os.unlink(tmp_name)
        except FileNotFoundError:
            pass
        raise

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
