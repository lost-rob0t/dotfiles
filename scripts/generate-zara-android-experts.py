#!/usr/bin/env python3

from __future__ import annotations

import argparse
from pathlib import Path
import re
import sys


EXPERT_ID = re.compile(r"(?m)^expert_id\('([^']+)'\)\.\s*$")
PURE_SYMBOLIC_MARKERS = (
    "provider_policy(disabled).",
    "max_model_calls(0).",
)


def _atom(value: str) -> str:
    return "'" + value.replace("\\", "\\\\").replace("'", "\\'") + "'"


def _experts(root: Path) -> list[tuple[str, str, str]]:
    result: list[tuple[str, str, str]] = []
    seen: set[str] = set()

    for source in sorted((root / ".zara" / "experts").glob("*/kb/expert.pl")):
        text = source.read_text(encoding="utf-8")
        match = EXPERT_ID.search(text)
        if match is None or not all(marker in text for marker in PURE_SYMBOLIC_MARKERS):
            continue

        expert_id = match.group(1)
        prefix = "zara:expert/"
        if not expert_id.startswith(prefix):
            raise ValueError(f"unsupported expert id in {source}: {expert_id}")

        package = expert_id.removeprefix(prefix)
        if not re.fullmatch(r"[a-z][a-z0-9-]{0,63}", package):
            raise ValueError(f"unsupported expert package in {source}: {package}")

        local_name = package.replace("-", "_")
        if local_name in seen:
            raise ValueError(f"Android expert projection collision: {local_name}")
        seen.add(local_name)

        relative = source.parent.parent.relative_to(root).as_posix()
        result.append((local_name, expert_id, relative))

    if not result:
        raise ValueError("no pure-symbolic Dotfiles experts found")

    return result


def render(root: Path) -> str:
    lines = [
        "% GENERATED FILE. DO NOT EDIT.",
        "% Source of truth: .zara/experts/*/kb/expert.pl",
        "% Regenerate with: python3 scripts/generate-zara-android-experts.py",
        "%",
        "% Android's Git-template workspace is intentionally data-only. This",
        "% projection exposes identity and local explain routing for canonical",
        "% zero-model experts without creating another ZARA-EXPERT registry.",
        "",
    ]

    for local_name, expert_id, relative in _experts(root):
        lines.extend(
            [
                f"android_expert_source({local_name}, {_atom(expert_id)}, {_atom(relative)}).",
                f"expert_activation({local_name}, {local_name}).",
                f"{local_name}_explain(Entity, Result) :-",
                "    Result = expert_projection(",
                f"        id({_atom(expert_id)}),",
                "        entity(Entity),",
                f"        source({_atom(relative)}),",
                "        policy(pure_symbolic),",
                "        model_calls(0)",
                "    ).",
                "",
            ]
        )

    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()

    root = Path(__file__).resolve().parents[1]
    output = root / ".config" / "zarathushtra" / "android" / "dotfiles_experts.pl"
    generated = render(root)

    if args.check:
        try:
            current = output.read_text(encoding="utf-8")
        except FileNotFoundError:
            print(f"missing generated Android expert projection: {output}", file=sys.stderr)
            return 1
        if current != generated:
            print(
                "Android expert projection is stale; run "
                "python3 scripts/generate-zara-android-experts.py",
                file=sys.stderr,
            )
            return 1
        return 0

    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(generated, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
