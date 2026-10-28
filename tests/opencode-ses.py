#!/usr/bin/env python3
"""Focused regressions for opencode-ses; fake binaries never contact an LLM provider."""
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
from datetime import datetime
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
LIB = ROOT / '.local/lib/dotfiles/opencode_ses.py'
spec = importlib.util.spec_from_file_location('opencode_ses', LIB)
app = importlib.util.module_from_spec(spec)
spec.loader.exec_module(app)


def record(ident="ses_e853fa9bbffek0fnTQuj3AHNpb", title="Pick me", directory="/home/unseen/.dotfiles",
           created=1793060787793, updated=1793060808833, **extra):
    value = {"id": ident, "title": title,
             "location": {"directory": directory},
             "time": {"created": created, "updated": updated}}
    value.update(extra)
    return value


FAKE_OPENCODE = '''#!/usr/bin/env python3
import json, os, sys
from pathlib import Path
base = Path(os.environ["FAKE_BASE"])
calls = base / "api-calls"
count = len(list(calls.glob("call-*"))) + 1
calls.mkdir(exist_ok=True)
payload = os.environ.get("FAKE_PAYLOAD_%d" % count, os.environ.get("FAKE_PAYLOAD", '{"data": []}'))
(calls / ("call-%d" % count)).write_text(payload)
if sys.argv[1:4] == ["api", "get", "/api/session"]:
    print(payload)
else:
    (base / "resume.json").write_text(json.dumps([os.getcwd(), sys.argv[1:]]))
'''

FAKE_FZF = '''#!/usr/bin/env python3
import os, sys
from pathlib import Path
base = Path(os.environ["FAKE_BASE"])
data = sys.stdin.buffer.read()
(base / "fzf-input").write_bytes(data)
(base / "fzf-invoked").write_text("")
records = [row for row in data.split(b"\\0") if row]
sys.stdout.buffer.write(records[int(os.environ.get("FAKE_FZF_PICK", "0"))] + b"\\0")
sys.exit(int(os.environ.get("FAKE_FZF_EXIT", "0")))
'''


class ContractTests(unittest.TestCase):
    def test_records_require_supported_shape(self):
        records = [
            record(),
            record(ident="ses_untitled0", title=""),
            record(ident="bad-id"),
            record(ident="ses_relative1", directory="relative/path"),
            record(ident="ses_noloc123", location={}),
            record(ident="ses_notime123", time={}),
            record(ident="ses_strtime12", time={"created": "x", "updated": 1}),
            record(ident="ses_booltime1", time={"created": True, "updated": 2}),
            "not a dict",
        ]
        parsed = app.parse_sessions(records)
        self.assertEqual([s["id"] for s in parsed], ["ses_e853fa9bbffek0fnTQuj3AHNpb", "ses_untitled0"])
        self.assertEqual(parsed[1]["title"], "(untitled)")

    def test_newest_first_and_deterministic_ties(self):
        records = [record(ident="ses_aaaaaaaa1", updated=100),
                   record(ident="ses_cccccccc3", updated=300),
                   record(ident="ses_bbbbbbbb2", updated=300)]
        self.assertEqual([s["id"] for s in app.parse_sessions(records)],
                         ["ses_bbbbbbbb2", "ses_cccccccc3", "ses_aaaaaaaa1"])

    def test_rows_show_timestamps_title_directory_and_id(self):
        stamp = datetime.fromtimestamp(1793060808.833).strftime("%Y-%m-%d %H:%M")
        title = 'pick "me" $(touch PWNED) λ'
        sessions = app.parse_sessions([record(title=title, directory="/tmp/one two")])
        self.assertEqual(app.rows(sessions),
                         [f"{stamp}\tcreated {stamp}\t{title}\t/tmp/one two\tses_e853fa9bbffek0fnTQuj3AHNpb"])
        both = app.rows(app.parse_sessions([record(), record(ident="ses_older0001", updated=1, created=1)]))
        self.assertEqual(len(both), len(set(both)))

    def test_listing_validates_payload_exit_timeout(self):
        ok = subprocess.CompletedProcess([], 0, json.dumps({"data": [record()]}).encode(), b'')
        with patch.object(subprocess, 'run', return_value=ok):
            self.assertEqual([s["id"] for s in app.parse_sessions(app.listing('/opencode'))],
                             ["ses_e853fa9bbffek0fnTQuj3AHNpb"])
        for stdout in (b'not json', b'[]', b'{"data": 3}'):
            with self.subTest(stdout=stdout), \
                 patch.object(subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], 0, stdout, b'')), \
                 self.assertRaises(app.Failure) as error:
                app.listing('/opencode')
            self.assertEqual(error.exception.code, 70)
        failed = subprocess.CompletedProcess([], 1, b'', b'connection refused')
        with patch.object(subprocess, 'run', return_value=failed), \
             self.assertRaises(app.Failure) as error:
            app.listing('/opencode')
        self.assertIn('connection refused', str(error.exception))
        with patch.object(subprocess, 'run',
                          side_effect=subprocess.TimeoutExpired(['opencode'], 60)), \
             self.assertRaises(app.Failure) as error:
            app.listing('/opencode')
        self.assertEqual(error.exception.code, 124)

    def test_fzf_protocol_cannot_be_reconfigured(self):
        def fzf(argv, **kwargs):
            self.assertIn('--no-multi', argv)
            self.assertIn('--read0', argv)
            self.assertIn('--print0', argv)
            self.assertEqual(kwargs['input'], b'row one\0')
            self.assertEqual(kwargs['env']['FZF_DEFAULT_OPTS'], '')
            self.assertEqual(kwargs['env']['FZF_DEFAULT_OPTS_FILE'], '')
            return subprocess.CompletedProcess(argv, 0, b'row one\0', b'')
        with patch.dict(os.environ, {'FZF_DEFAULT_OPTS': '--multi --print-query',
                                     'FZF_DEFAULT_OPTS_FILE': '/bad'}), \
             patch.object(app, 'executable', return_value='/fzf'), \
             patch.object(subprocess, 'run', fzf):
            self.assertEqual(app.fzf_select(['row one']), 'row one')

    def test_fzf_must_return_one_original_record(self):
        with patch.object(app, 'executable', return_value='/fzf'), \
             patch.object(subprocess, 'run',
                          return_value=subprocess.CompletedProcess([], 0, b'row\0', b'')):
            self.assertEqual(app.fzf_select(['row']), 'row')
        for value in (b'row', b'a\0b\0', b'forged\0', b'\xff\xfe\0'):
            with self.subTest(value=value), \
                 patch.object(app, 'executable', return_value='/fzf'), \
                 patch.object(subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], 0, value, b'')), \
                 self.assertRaises(app.Failure) as error:
                app.fzf_select(['row'])
            self.assertEqual(error.exception.code, 70)

    def test_cancel_and_infrastructure_failure_differ(self):
        for code in (1, 130):
            with self.subTest(code=code), \
                 patch.object(app, 'executable', return_value='/fzf'), \
                 patch.object(subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], code, b'', b'')), \
                 self.assertRaises(app.Failure) as error:
                app.fzf_select(['row'])
            self.assertEqual(error.exception.code, 130)
        with patch.object(app, 'executable', return_value='/fzf'), \
             patch.object(subprocess, 'run',
                          return_value=subprocess.CompletedProcess([], 2, b'', b'boom')), \
             self.assertRaises(app.Failure) as error:
            app.fzf_select(['row'])
        self.assertEqual(error.exception.code, 1)

    def test_missing_dependencies_are_named(self):
        for name in ('opencode', 'fzf'):
            with self.subTest(name=name), patch.object(app.shutil, 'which', return_value=None), \
                 self.assertRaises(app.Failure) as error:
                app.executable(name)
            self.assertEqual(error.exception.code, 127)
            self.assertIn(name, str(error.exception))

    def test_resume_execs_supported_interface_in_directory(self):
        session = app.parse_sessions([record(directory=str(ROOT))])[0]
        with patch.object(app.os, 'chdir') as chdir, patch.object(app.os, 'execv') as execute:
            app.resume('/opencode', session)
            chdir.assert_called_once_with(Path(session['directory']))
            execute.assert_called_once_with('/opencode',
                                            ['/opencode', '--session', session['id'], session['directory']])

    def test_resume_fails_closed_when_directory_is_gone(self):
        session = app.parse_sessions([record(directory="/definitely/not/here")])[0]
        with patch.object(app.os, 'execv') as execute, self.assertRaises(app.Failure) as error:
            app.resume('/opencode', session)
        execute.assert_not_called()
        self.assertIn('no longer exists', str(error.exception))

    def test_empty_inventory_never_opens_fzf(self):
        with patch.object(app, 'executable', return_value='/bin'), \
             patch.object(app, 'listing', return_value=[]), patch.object(app, 'fzf_select') as pick:
            self.assertEqual(app.main([]), 0)
            pick.assert_not_called()

    def test_cancel_is_a_successful_no_op(self):
        with patch.object(app, 'executable', return_value='/bin'), \
             patch.object(app, 'listing', return_value=[record()]), \
             patch.object(app, 'fzf_select', side_effect=app.Failure('', 130)), \
             patch.object(app, 'resume') as resume:
            self.assertEqual(app.main([]), 0)
            resume.assert_not_called()

    def test_stale_selection_never_creates_a_replacement_session(self):
        selected = app.rows(app.parse_sessions([record()]))[0]
        with patch.object(app, 'executable', return_value='/bin'), \
             patch.object(app, 'listing', side_effect=[[record()], []]), \
             patch.object(app, 'fzf_select', return_value=selected), \
             patch.object(app, 'resume') as resume, self.assertRaises(app.Failure) as error:
            app.main([])
        resume.assert_not_called()
        self.assertIn('disappeared', str(error.exception))

    def test_cli_help(self):
        launcher = str(ROOT / '.local/bin/opencode-ses')
        for argv in ([launcher, '--help'], ['bash', launcher, '--help']):
            result = subprocess.run(argv, capture_output=True, timeout=10)
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_literate_generated_parity(self):
        text = (ROOT / 'scripts/opencode-ses.org').read_text()
        blocks = re.findall(r'^#\+begin_src \S+ :tangle (\S+)[^\n]*\n(.*?)^#\+end_src\s*$', text, re.M | re.S)
        self.assertGreaterEqual(len(blocks), 3)
        for target, body in blocks:
            self.assertEqual((ROOT / 'scripts' / target).resolve().read_text(), body, target)
        self.assertIn('./opencode-ses.nix', (ROOT / 'nix/home-manager/mods/default.nix').read_text())


class EndToEndTests(unittest.TestCase):
    """Real launcher with fake opencode/fzf binaries; no provider, tty, or user sessions touched."""

    def run_picker(self, payloads, pick="0", fzf_exit="0", with_opencode=True):
        """payloads maps api-call number to payload, or is a callable(base) -> such a map.

        Returns (snapshot, result); snapshot maps each produced file's
        repository-relative path to its bytes, because the temporary directory
        is removed before assertions run.
        """
        with tempfile.TemporaryDirectory(prefix='opencode-ses-') as temporary:
            base = Path(temporary)
            fake = base / 'bin'
            fake.mkdir()
            if with_opencode:
                (fake / 'opencode').write_text(FAKE_OPENCODE)
            (fake / 'fzf').write_text(FAKE_FZF)
            for name in ('opencode', 'fzf'):
                if (fake / name).exists():
                    (fake / name).chmod(0o755)
            env = os.environ.copy()
            env.update(FAKE_BASE=str(base), FAKE_FZF_PICK=pick, FAKE_FZF_EXIT=fzf_exit)
            env['PATH'] = f"{fake}:{Path(sys.executable).parent}:/usr/bin:/bin"
            payloads = payloads(base) if callable(payloads) else payloads
            for index, payload in payloads.items():
                env[f'FAKE_PAYLOAD_{index}'] = payload
            result = subprocess.run([str(ROOT / '.local/bin/opencode-ses')],
                                    capture_output=True, timeout=60, env=env, cwd=base)
            snapshot = {str(path.relative_to(base)): path.read_bytes()
                        for path in sorted(base.rglob('*')) if path.is_file()}
            return snapshot, result

    def test_resume_passes_exact_session_and_directory(self):
        title = 'pick "me" $(touch PWNED) λ'

        def payloads(base):
            project = base / "pro ject 'quote'"
            project.mkdir()
            payload = json.dumps({"data": [record(title=title, directory=str(project)),
                                           record(ident="ses_older0001", title="old",
                                                  created=1, updated=1, directory=str(base))]})
            return {"1": payload, "2": payload}

        snapshot, result = self.run_picker(payloads)
        self.assertEqual(result.returncode, 0, result.stderr)
        rows_seen = [row for row in snapshot['fzf-input'].split(b'\0') if row]
        self.assertEqual(len(rows_seen), 2)
        self.assertIn(b'ses_e853fa9bbffek0fnTQuj3AHNpb', rows_seen[0])
        self.assertIn(b'ses_older0001', rows_seen[1])
        self.assertIn(title.encode(), rows_seen[0])
        self.assertEqual(json.loads(snapshot['resume.json'])[1],
                         ["--session", "ses_e853fa9bbffek0fnTQuj3AHNpb",
                          json.loads(snapshot['resume.json'])[0]])
        self.assertTrue(json.loads(snapshot['resume.json'])[0].endswith("pro ject 'quote'"))
        self.assertNotIn("PWNED", " ".join(snapshot))

    def test_second_row_resumes_that_session(self):
        def payloads(base):
            project = base / 'second project'
            project.mkdir()
            payload = json.dumps({"data": [record(),
                                           record(ident="ses_older0001", created=1, updated=1,
                                                  directory=str(project))]})
            return {"1": payload, "2": payload}

        snapshot, result = self.run_picker(payloads, pick="1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(snapshot['resume.json'])[1],
                         ["--session", "ses_older0001", json.loads(snapshot['resume.json'])[0]])
        self.assertTrue(json.loads(snapshot['resume.json'])[0].endswith('second project'))

    def test_cancel_is_successful_silent_no_op(self):
        snapshot, result = self.run_picker({"1": json.dumps({"data": [record(directory=str(ROOT))]})},
                                           fzf_exit="130")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('resume.json', snapshot)

    def test_empty_inventory_reports_without_opening_fzf(self):
        snapshot, result = self.run_picker({"1": '{"data": []}'})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('No OpenCode sessions', result.stderr.decode())
        self.assertNotIn('fzf-invoked', snapshot)
        self.assertNotIn('resume.json', snapshot)

    def test_stale_selection_fails_closed(self):
        payload = json.dumps({"data": [record(directory=str(ROOT))]})
        snapshot, result = self.run_picker({"1": payload, "2": '{"data": []}'})
        self.assertEqual(result.returncode, 1, result.stdout)
        self.assertIn('disappeared', result.stderr.decode())
        self.assertNotIn('resume.json', snapshot)

    def test_missing_opencode_is_reported_with_127(self):
        snapshot, result = self.run_picker({}, with_opencode=False)
        self.assertEqual(result.returncode, 127, result.stdout)
        self.assertIn('opencode', result.stderr.decode())
        self.assertNotIn('fzf-invoked', snapshot)


if __name__ == '__main__':
    unittest.main()
