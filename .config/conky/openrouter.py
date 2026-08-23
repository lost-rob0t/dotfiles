#!/usr/bin/env python3
"""Render the existing Qtile OpenRouter collector as Conky markup."""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys

HOME = Path.home()
COLLECTOR = HOME / ".config" / "qtile" / "scripts" / "openrouter_status.py"


def compact(value: object) -> str:
    try:
        number = float(value or 0)
    except (TypeError, ValueError):
        return "--"
    for suffix, divisor in (("B", 1_000_000_000), ("M", 1_000_000), ("k", 1_000)):
        if abs(number) >= divisor:
            return f"{number / divisor:.1f}".rstrip("0").rstrip(".") + suffix
    return str(int(round(number)))


def collect() -> dict[str, object]:
    try:
        proc = subprocess.run(
            [sys.executable, str(COLLECTOR), "--json"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=8,
        )
        payload = json.loads(proc.stdout or "{}")
    except (OSError, subprocess.TimeoutExpired, json.JSONDecodeError):
        return {"error": "collector unavailable"}
    if not isinstance(payload, dict):
        return {"error": "collector malformed"}
    return payload


def number(payload: dict[str, object], key: str) -> float:
    try:
        return float(payload.get(key) or 0)
    except (TypeError, ValueError):
        return 0.0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--metric", choices=("tpm", "tpm-percent", "input", "output"))
    args = parser.parse_args()
    payload = collect()

    input_tpm = number(payload, "input_tokens_per_minute")
    output_tpm = number(payload, "output_tokens_per_minute")
    total_tpm = number(payload, "total_tokens_per_minute") or input_tpm + output_tpm

    if args.metric:
        if args.metric == "input":
            value = input_tpm
        elif args.metric == "output":
            value = output_tpm
        elif args.metric == "tpm":
            value = total_tpm
        else:
            try:
                ceiling = max(float(os.environ.get("OPENROUTER_TPM_MAX", "10000000")), 1.0)
            except ValueError:
                ceiling = 10_000_000.0
            value = min(max(total_tpm / ceiling * 100.0, 0.0), 100.0)
        print(f"{value:.3f}")
        return 0

    if payload.get("error"):
        print("${color9}OpenRouter telemetry unavailable${color}")
        print("${color3}" + str(payload["error"]) + "${color}")
        return 1

    balance = payload.get("balance_usd")
    try:
        balance_value = float(balance) if balance is not None else None
    except (TypeError, ValueError):
        balance_value = None
    if balance_value is None:
        balance_text = "--"
        balance_color = "${color3}"
    elif balance_value < 5:
        balance_text = f"${balance_value:.2f}"
        balance_color = "${color9}"
    elif balance_value < 10:
        balance_text = f"${balance_value:.2f}"
        balance_color = "${color8}"
    else:
        balance_text = f"${balance_value:.2f}"
        balance_color = "${color4}"

    stale = " ~" if payload.get("stale") else ""
    print(f"${{color2}}balance${{alignr}}{balance_color}{balance_text}{stale}${{color}}")
    print(f"${{color2}}input / min${{alignr}}${{color}}{compact(input_tpm)}")
    print(f"${{color2}}output / min${{alignr}}${{color}}{compact(output_tpm)}")
    print(f"${{color1}}TOTAL TPM${{alignr}}${{color}}{compact(total_tpm)}")
    print("${color3}${hr 1}${color}")
    print(f"${{color2}}hour${{alignr}}${{color}}{compact(payload.get('tokens_hour'))} tok")
    print(f"${{color2}}day${{alignr}}${{color}}{compact(payload.get('tokens_day'))} tok  ${compact(number(payload, 'spend_day'))}")
    print(f"${{color2}}week${{alignr}}${{color}}{compact(payload.get('tokens_week'))} tok  ${compact(number(payload, 'spend_week'))}")
    print(f"${{color2}}month${{alignr}}${{color}}{compact(payload.get('tokens_month'))} tok  ${compact(number(payload, 'spend_month'))}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
