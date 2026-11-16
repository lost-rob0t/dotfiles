#!/usr/bin/env sh

exec python3 - <<'PYTHON'
import hashlib
import re
import subprocess

# Physical left, middle, right: EDIDs stay stable when DP names change.
monitors = [
    "cd2f589f7f8ba879f497cfdb2de1ebef05db55094d1f52f434472c2e8d246968",
    "a417bb79251762b2df332326d0068f7fa6fb16f48b6d88ff81f405f4356b9762",
    "1d54e1c1ca9e6cb05a2cb2275a3a57a77e970d4fb3b57db2960140f4a9d28e57",
]
properties = subprocess.check_output(["xrandr", "--props"], text=True)
outputs = {}
for block in re.split(r"(?=^\S+ (?:connected|disconnected))", properties, flags=re.M):
    if not re.match(r"\S+ connected", block):
        continue
    edid = re.search(r"\n\s+EDID:\s*\n((?:\s+[0-9a-fA-F]{32}\n)+)", block)
    if edid:
        identity = hashlib.sha256(bytes.fromhex(edid[1])).hexdigest()
        outputs[identity] = block.split()[0]

if not all(identity in outputs for identity in monitors):
    raise SystemExit("Flake monitor profile: expected three monitors not connected; leaving layout unchanged")

command = ["xrandr"]
for index, identity in enumerate(monitors):
    command += ["--output", outputs[identity], "--mode", "1920x1080",
                "--rate", "60.00", "--pos", f"{index * 1920}x0", "--rotate", "normal"]
    if index == 1:
        command += ["--primary"]
subprocess.run(command, check=True)
PYTHON
