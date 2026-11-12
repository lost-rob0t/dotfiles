"""Verify the targeted installed release, login stanza and idempotent startup."""
import json
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
source = Path(sys.argv[1]).resolve()
pin = json.loads((ROOT / '.config/starintel-conky-release.json').read_text())
data = Path(os.environ.get('XDG_DATA_HOME', str(Path.home() / '.local/share')))
release = (data / 'starintel-conky/current').resolve()
for relative in ('bin/starintel-desktop.py', 'bin/starintel-conky', 'etc/outrun.json'):
    expected = subprocess.check_output(['git', '-C', str(source), 'show', pin['revision'] + ':' + relative])
    assert (release / relative).read_bytes() == expected, relative + ' differs from pinned Git source'
launcher = Path.home() / '.local/bin/starintel-conky'
assert launcher.read_bytes() == (ROOT / '.local/bin/starintel-conky').read_bytes()

def stanza(text):
    return text.split('# BEGIN StarIntel desktop', 1)[1].split('# END StarIntel desktop', 1)[0]

live_startup = Path.home() / '.config/qtile/scripts/autostart.sh'
assert stanza(live_startup.read_text()) == stanza((ROOT / '.config/qtile/scripts/autostart.sh').read_text())
state = Path(os.environ.get('XDG_RUNTIME_DIR', str(data))) / 'starintel-desktop'
before = {path.name: path.read_text() for path in state.glob('*.pid')}
focus = subprocess.check_output(['xdotool', 'getwindowfocus'])
subprocess.run([str(launcher), 'start'], check=True)
after = {path.name: path.read_text() for path in state.glob('*.pid')}
assert before == after, 'Repeated startup spawned new processes'
assert focus == subprocess.check_output(['xdotool', 'getwindowfocus']), 'Startup stole focus'
subprocess.run(['/usr/bin/python3', str(source / 'tests/check_live.py')], check=True)
print('PASS: installed runtime matches immutable pin; live login stanza matches source; startup is idempotent and does not steal focus.')
