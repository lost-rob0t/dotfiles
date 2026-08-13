#!/usr/bin/env bash
set -euo pipefail

request=$(printf '{"operation":"status","root":"%s"}' "$PWD/.ci/prolog-rlm")
response=$(printf '%s' "$request" | swipl -q -s lisp/llm/prolog-rlm-bridge.pl)

RESPONSE="$response" python3 - <<'PY'
import json
import os

value = json.loads(os.environ["RESPONSE"])
assert value["ok"] is True, value
assert value["ready"] is True, value
assert value["version"], value
print("prolog-rlm bridge status:", value["version"])
PY
