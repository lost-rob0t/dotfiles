"""Verify exact Org-to-generated parity for the StarIntel module and templates."""
from pathlib import Path
import re
import sys

root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parents[1]
source = root / 'nix/home-manager/mods/starintel.org'
blocks = re.findall(r'^#\+begin_src \S+ :tangle (\S+)\n(.*?)^#\+end_src\s*$', source.read_text(), re.M | re.S | re.I)
assert len(blocks) == 4, 'Expected the module, two init templates and runtime adapter'
for destination, contents in blocks:
    output = (source.parent / destination).resolve()
    assert output.is_relative_to(root.resolve())
    assert output.read_text() == contents, f'Literate drift: {destination}'
print('StarIntel literate/generated parity passed')
