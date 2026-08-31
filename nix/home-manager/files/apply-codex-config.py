#!/usr/bin/env python3

import json
import os
import stat
import sys
import tempfile
from collections.abc import MutableMapping
from pathlib import Path

import tomlkit


def main() -> int:
    if len(sys.argv) != 3:
        raise SystemExit("usage: apply-codex-config CONFIG_TOML PATCH_JSON")

    config_path = Path(sys.argv[1])
    patch_path = Path(sys.argv[2])

    if config_path.is_symlink():
        raise SystemExit(
            f"refusing to edit Home Manager-owned symlink: {config_path}"
        )

    if config_path.exists():
        original = config_path.read_text(encoding="utf-8")
        document = tomlkit.parse(original)
        mode = stat.S_IMODE(config_path.stat().st_mode)
    else:
        original = ""
        document = tomlkit.document()
        mode = 0o600

    patch = json.loads(patch_path.read_text(encoding="utf-8"))
    for section_name, values in patch.items():
        section = document.get(section_name)
        if section is None:
            section = tomlkit.table()
            document[section_name] = section
        elif not isinstance(section, MutableMapping):
            raise SystemExit(f"Codex config section is not a table: {section_name}")

        for key, value in values.items():
            current = section.get(key)
            if hasattr(current, "unwrap"):
                current = current.unwrap()
            if current != value:
                section[key] = value

    rendered = tomlkit.dumps(document)
    if rendered == original:
        return 0

    config_path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        dir=config_path.parent,
        prefix=f".{config_path.name}.",
    )
    try:
        os.fchmod(descriptor, mode)
        with os.fdopen(descriptor, "w", encoding="utf-8") as output:
            output.write(rendered)
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary_name, config_path)
    except BaseException:
        try:
            os.unlink(temporary_name)
        except FileNotFoundError:
            pass
        raise

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
