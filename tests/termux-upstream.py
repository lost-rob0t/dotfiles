"""Run the pinned upstream installer body with Android/package/network test doubles.

Only in this harness, replace its hardcoded Android HOME/PREFIX/getprop gate so
it can use a disposable HOME. Production sources and their guard are unchanged.
Venv/pip/package operations are stubbed; the real upstream setup, receipt, config,
launcher, and status code execute. This is a Linux contract test, not Android.
"""
import os
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import textwrap

ROOT = Path(__file__).resolve().parents[1]
UPSTREAM = Path(sys.argv[1]).resolve()
PIN = '5d8d9659acea6349adc6f264b35964ef7309f004'
assert subprocess.check_output(['git', '-C', str(UPSTREAM), 'rev-parse', 'HEAD'], text=True).strip() == PIN
REAL_PYTHON = sys.executable
REAL_GIT = shutil.which('git')

with tempfile.TemporaryDirectory() as tmp:
    home = Path(tmp)
    prefix = home / 'prefix'
    bin_dir = prefix / 'bin'
    bin_dir.mkdir(parents=True)
    (prefix / 'var/service/crond/supervise').mkdir(parents=True)
    (prefix / 'var/service/crond/run').write_text('packaged fixture')
    os.mkfifo(prefix / 'var/service/crond/supervise/ok')
    (home / 'cron').write_text('0 2 * * * unrelated-backup\n')
    env = dict(os.environ, HOME=tmp, PREFIX=str(prefix), PATH=str(bin_dir) + ':' + os.environ['PATH'],
               TEST_REAL_PYTHON=REAL_PYTHON, TEST_REAL_GIT=REAL_GIT, TEST_SOURCE=str(UPSTREAM))

    def shell(name, body):
        path = bin_dir / name
        path.write_text('#!/bin/bash\nset -euo pipefail\n' + body + '\n')
        path.chmod(0o755)

    def python_stub(name, body):
        path = bin_dir / name
        path.write_text('#!' + REAL_PYTHON + '\n' + textwrap.dedent(body))
        path.chmod(0o755)

    shell('crontab', 'if [[ "$1" == -l ]]; then cat "$HOME/cron"; else cp "$1" "$HOME/cron"; fi')
    shell('pgrep', 'exit 1')
    shell('svlogd', 'cat > /dev/null')
    shell('sv', 'echo "$*" >> "$HOME/services"')
    shell('sv-enable', 'echo enable >> "$HOME/services"')
    shell('service-daemon', '[[ ! -e /proc/self/fd/8 && ! -e /proc/self/fd/9 ]]; echo daemon >> "$HOME/services"')
    shell('dpkg-query', '[[ -e "$HOME/packages-installed" ]] || exit 1; printf "install ok installed"')
    shell('apt-get', 'echo "$*" >> "$HOME/packages"; if [[ "$1" == install ]]; then touch "$HOME/packages-installed"; fi')
    python_stub('git', '''
        import os, sys
        args = sys.argv[1:]
        args = [os.environ['TEST_SOURCE'] if x == 'https://github.com/lost-rob0t/election26.git' else x for x in args]
        os.execv(os.environ['TEST_REAL_GIT'], ['git', *args])
    ''')
    python_stub('bash', r'''
        import os, subprocess, sys
        from pathlib import Path
        script, *args = sys.argv[1:]
        assert script.endswith('/scripts/install-termux.sh')
        assert args == ['--install', '--if-needed']
        text = Path(script).read_text()
        gate = 'if [[ "$prefix" != /data/data/com.termux/files/usr ]] || [[ "$home" != /data/data/com.termux/files/home ]] || ! /system/bin/getprop ro.build.version.sdk >/dev/null 2>&1; then'
        assert text.count(gate) == 1
        text = text.replace(gate, 'if false; then')
        # $0 remains the real script path, so its source-root calculation is real.
        sys.exit(subprocess.run(['/bin/bash', '-c', text, script, *args]).returncode)
    ''')
    python_stub('python', r'''
        import importlib.util, os, runpy, shlex, shutil, subprocess, sys
        from pathlib import Path
        from unittest.mock import patch
        args = sys.argv[1:]
        venv = os.environ.get('TEST_VENV')
        if venv:
            sys.path.insert(0, str(Path(venv) / 'lib'))
        if args[:2] == ['-m', 'pip']:
            assert venv and args[2:6] == ['install', '--disable-pip-version-check', '--no-deps', args[-1]]
            source = Path(args[-1])
            target = Path(venv) / 'lib/election26'
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copytree(source / 'src/election26', target, dirs_exist_ok=True)
            cli = Path(venv) / 'bin/election26'
            cli.write_text('#!/bin/bash\nexec env PYTHONPATH=' + shlex.quote(str(target.parent)) + ' ' + shlex.quote(os.environ['TEST_REAL_PYTHON']) + ' -m election26.cli "$@"\n')
            cli.chmod(0o700)
            with open(Path(os.environ['HOME']) / 'pip-calls', 'a') as log:
                log.write('pip\n')
            sys.exit(0)
        if args and args[0].endswith('/scripts/termux_setup.py'):
            def create_venv(command, **kwargs):
                assert command[1:3] == ['-m', 'venv']
                root = Path(command[3]); (root / 'bin').mkdir(parents=True, exist_ok=True)
                wrapper = root / 'bin/python'
                wrapper.write_text('#!/bin/bash\nexec env TEST_VENV=' + shlex.quote(str(root)) + ' ' + shlex.quote(str(Path(os.environ['PREFIX']) / 'bin/python')) + ' "$@"\n')
                wrapper.chmod(0o700)
                return subprocess.CompletedProcess(command, 0)
            sys.argv = args
            with patch('subprocess.run', side_effect=create_venv):
                runpy.run_path(args[0], run_name='__main__')
        os.execv(os.environ['TEST_REAL_PYTHON'], [os.environ['TEST_REAL_PYTHON'], *args])
    ''')

    def install():
        result = subprocess.run(['/bin/bash', str(ROOT / 'scripts/termux-starintel'), 'install'],
                                env=env, capture_output=True, text=True)
        assert result.returncode == 0, (result.stdout, result.stderr)
        return result

    first = install()
    config = home / '.config/election26/config.json'
    original = config.read_bytes()
    defaults = json.loads(original)
    assert defaults['allowed_hosts'] == [] and defaults['feeds'] == []
    assert defaults['brave_enabled'] is False and defaults['discord_enabled'] is False
    assert config.stat().st_mode & 0o777 == 0o600
    archive = home / '.local/share/election26/archive'
    (archive / 'preserved.fixture').write_bytes(b'raw data\x00')
    cron = (home / 'cron').read_bytes()
    second = install()
    assert 'no package download or reinstall needed' in second.stdout
    assert config.read_bytes() == original
    assert (archive / 'preserved.fixture').read_bytes() == b'raw data\x00'
    assert (home / 'cron').read_bytes() == cron
    assert cron.count(b' collect ') == 1 and b'unrelated-backup' in cron
    assert (home / 'pip-calls').read_text() == 'pip\n'
    packages = (home / 'packages').read_text()
    assert 'python python-pip python-ensurepip-wheels' in packages and len(packages.splitlines()) == 2
    assert not (home / 'starintel').exists()
    print('PASS: actual pinned upstream installer body, setup, receipt, config, launcher and status; fresh install + verified rerun')
    print('STUBBED: Android environment gate, package manager, Git network source, venv creation and pip installation; no network collection')
