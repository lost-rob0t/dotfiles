#!/usr/bin/env python3
"""Local regressions for the OpenCode commands; subprocess doubles never contact an LLM provider."""
import importlib.util
import io
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile
from datetime import datetime
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMUX_LIB = ROOT / '.local/lib/dotfiles/opencode_tmux.py'
SES_LIB = ROOT / '.local/lib/dotfiles/opencode_ses.py'
tmux_spec = importlib.util.spec_from_file_location('opencode_tmux', TMUX_LIB)
tmux = importlib.util.module_from_spec(tmux_spec)
tmux_spec.loader.exec_module(tmux)
ses_spec = importlib.util.spec_from_file_location('opencode_ses', SES_LIB)
ses = importlib.util.module_from_spec(ses_spec)
ses_spec.loader.exec_module(ses)


class TmuxContractTests(unittest.TestCase):
    def test_names_are_bounded_and_unambiguous(self):
        for name in ('a', 'starintel-api', 'pr_42', 'X' * 64):
            self.assertEqual(tmux.session_name(name), 'opencode-' + name)
        for name in ('', '-bad', 'a:b', 'a.b', 'a b', 'a\nb', '$(id)', 'x;id', 'a' * 65):
            with self.subTest(name=name), self.assertRaises(tmux.Failure):
                tmux.session_name(name)

    def test_list_rejects_foreign_and_malformed_records(self):
        text = ('$3\topencode-api\t1\n$4\tother\t1\n'
                '$5\topencode-api-more\t\n$6\topencode-bad:name\t1\n'
                'x\topencode-forged\t1\n$7\topencode-worker_2\t1\n')
        self.assertEqual(tmux.parse_sessions(text), {'opencode-api': '$3', 'opencode-worker_2': '$7'})

    def test_picker_requires_one_exact_record(self):
        rows = ['api\t$3', 'api-more\t$4']
        self.assertEqual(tmux.decode_selection(b'api\t$3\0', rows), 'api\t$3')
        for value in (b'', b'api\t$3', b'api\t$3\0api-more\t$4\0', b'forged\t$0\0'):
            with self.subTest(value=value), self.assertRaises(tmux.Failure):
                tmux.decode_selection(value, rows)

    def test_ezf_records_preserve_data(self):
        values = ['nil', 'two words', 'a"b', "a'b", r'a\b', 'a,b', '$(touch nope)', 'λ']
        raw = ('\0'.join(values) + '\0').encode()
        self.assertEqual(tmux.read_candidates(io.BytesIO(raw), True), values)
        self.assertEqual(tmux.read_candidates(io.BytesIO(b''), False), [])
        self.assertEqual(tmux.read_candidates(io.BytesIO(b'a\nb\n'), False), ['a', 'b'])
        with self.assertRaises(tmux.Failure):
            tmux.read_candidates(io.BytesIO(b'a\0b'), False)
        with self.assertRaises(tmux.Failure):
            tmux.read_candidates(io.BytesIO(b'\xff'), False)
        with patch.object(tmux, 'MAX_INPUT', 8), self.assertRaises(tmux.Failure):
            tmux.read_candidates(io.BytesIO(b'a' * 9), False)

    def test_field_is_zero_based_and_bounded(self):
        self.assertEqual(tmux.fields(['one  two three', 'x y z'], 1), ['two', 'y'])
        self.assertEqual(tmux.fields(['a b'], 0), ['a'])
        for field in (-1, 10):
            with self.assertRaises(tmux.Failure):
                tmux.fields(['a b'], field)

    def test_lisp_paths_use_only_base64_data(self):
        payload = 'x\") (delete-file \"/tmp/no\") ; λ'
        expression = tmux.lisp_data(payload)
        encoded = re.fullmatch(r'\(decode-coding-string \(base64-decode-string "([A-Za-z0-9+/=]+)"\) \'utf-8\)', expression)
        self.assertIsNotNone(encoded)
        import base64
        self.assertEqual(base64.b64decode(encoded[1]).decode(), payload)

    def test_shell_transport_preserves_every_argument(self):
        args = ['two words', "quote'and\"double", '', ';', 'suffix;', '$(touch INJECTION)', '#{pane_id}', 'a\nb']
        with tempfile.TemporaryDirectory() as directory:
            out = Path(directory) / 'argv.json'
            executable = Path(directory) / "fake opencode's"
            executable.write_text('#!/usr/bin/env python3\nimport json,sys\n'
                                  'from pathlib import Path\nPath(sys.argv[1]).write_text(json.dumps(sys.argv[2:]))\n')
            executable.chmod(0o755)
            command = tmux.shell_command(str(executable), str(out), args)
            self.assertFalse(command.endswith(';'))
            subprocess.run(['sh', '-c', command], check=True, cwd=directory)
            self.assertEqual(json.loads(out.read_text()), args)
            self.assertFalse((Path(directory) / 'INJECTION').exists())

    def test_ezf_response_is_not_executable_and_nil_is_data(self):
        self.assertEqual(tmux.validate_ezf_response({'status': 'ok', 'values': ['nil']}, ['nil'], True), ['nil'])
        for response in ({'status': 'ok', 'values': ['forged']},
                         {'status': 'ok', 'values': ['a', 'b']},
                         {'status': 'ok', 'values': 'a'},
                         {'status': 'wat'}, []):
            with self.assertRaises(tmux.Failure):
                tmux.validate_ezf_response(response, ['a', 'b'], True)
        for state, code in (('cancel', 130), ('unavailable', 69), ('timeout', 124), ('error', 70)):
            with self.assertRaises(tmux.Failure) as result:
                tmux.validate_ezf_response({'status': state, 'message': 'test'}, ['a'], True)
            self.assertEqual(result.exception.code, code)

    def test_empty_ezf_input_never_starts_emacs(self):
        with patch.object(tmux, 'executable', side_effect=AssertionError('must not run')):
            self.assertEqual(tmux.ezf_select([], single=True), [])

    def test_no_server_and_permission_failure_are_different(self):
        absent = subprocess.CompletedProcess([], 1, b'', b'no server running on /tmp/test\n')
        denied = subprocess.CompletedProcess([], 1, b'', b'error connecting to /tmp/test (Permission denied)\n')
        with patch.object(tmux, 'tmux', return_value=absent):
            self.assertEqual(tmux.sessions(), {})
        with patch.object(tmux, 'tmux', return_value=denied), self.assertRaises(tmux.Failure):
            tmux.sessions()

    def test_create_reuses_race_winner_without_starting_again(self):
        failure = subprocess.CompletedProcess([], 1, b'', b'duplicate session\n')
        with patch.object(tmux, 'sessions', side_effect=[{}, {'opencode-api': '$9'}]), \
             patch.object(tmux, 'tmux', return_value=failure) as tmux_mock, \
             patch.object(tmux, 'executable', return_value='/usr/bin/true'):
            self.assertEqual(tmux.ensure_session('api', ROOT, []), '$9')
            self.assertEqual(tmux_mock.call_count, 1)

    def test_reusing_session_does_not_require_opencode(self):
        with patch.object(tmux, 'sessions', return_value={'opencode-api': '$9'}), \
             patch.object(tmux, 'executable', side_effect=AssertionError('must not run')):
            self.assertEqual(tmux.ensure_session('api', ROOT, []), '$9')

    def test_new_session_receives_atomic_marker_and_literal_cwd(self):
        created = subprocess.CompletedProcess([], 0, b'$9\n', b'')
        with tempfile.TemporaryDirectory(prefix='cwd-#{pane_id}-') as directory:
            with patch.object(tmux, 'sessions', side_effect=[{}, {'opencode-api': '$9'}]), \
                 patch.object(tmux, 'tmux', return_value=created) as tmux_mock, \
                 patch.object(tmux, 'executable', return_value='/usr/bin/true'):
                self.assertEqual(tmux.ensure_session('api', Path(directory), ['--model', 'a/b']), '$9')
                argv = tmux_mock.call_args.args
                self.assertIn('DOTFILES_OPENCODE_SESSION=1', argv)
                self.assertIn(directory.replace('#', '##') + '/.', argv)
                self.assertIn('-d', argv)

    def test_attach_switches_inside_tmux_and_execs_outside(self):
        with patch.dict(os.environ, {'TMUX': 'active'}), patch.object(tmux, 'tmux') as tmux_mock:
            tmux_mock.return_value = subprocess.CompletedProcess([], 0, b'', b'')
            tmux.attach('$4')
            tmux_mock.assert_called_once_with('switch-client', '-t', '$4')
        with patch.dict(os.environ, {}, clear=True), patch.object(tmux, 'executable', return_value='/tmux'), \
             patch.object(os, 'execv') as execute:
            tmux.attach('$4')
            execute.assert_called_once_with('/tmux', ['/tmux', 'attach-session', '-t', '$4'])

    def test_auto_does_not_fall_back_on_emacs_cancel(self):
        with patch.dict(os.environ, {'INSIDE_EMACS': 'vterm'}), \
             patch.object(tmux, 'ezf_select', side_effect=tmux.Failure('', 130)), \
             patch.object(tmux, 'fzf_select') as fzf:
            with self.assertRaises(tmux.Failure):
                tmux.choose(['a'], 'auto')
            fzf.assert_not_called()

    def test_auto_falls_back_only_when_emacs_unavailable(self):
        with patch.dict(os.environ, {'INSIDE_EMACS': 'vterm'}), \
             patch.object(tmux, 'ezf_select', side_effect=tmux.Failure('no frame', 69)), \
             patch.object(tmux, 'fzf_select', return_value='a') as fzf:
            self.assertEqual(tmux.choose(['a'], 'auto'), 'a')
            fzf.assert_called_once()

    def test_cancel_and_stale_pick_never_attach(self):
        with patch.object(tmux, 'sessions', return_value={'opencode-api': '$3'}), \
             patch.object(tmux, 'choose', side_effect=tmux.Failure('', 130)), \
             patch.object(tmux, 'attach') as attach:
            self.assertEqual(tmux.pick('fzf'), 0)
            attach.assert_not_called()
        with patch.object(tmux, 'sessions', side_effect=[{'opencode-api': '$3'}, {'opencode-api': '$4'}]), \
             patch.object(tmux, 'choose', return_value='api\t$3'), \
             patch.object(tmux, 'attach') as attach:
            with self.assertRaises(tmux.Failure):
                tmux.pick('fzf')
            attach.assert_not_called()

    def test_cli_help_and_invalid_input(self):
        for command in ('opencode-tmux', 'opencode-pick', 'ezf', 'ezf.sh'):
            path = ROOT / '.local/bin' / command
            for argv in ([str(path), '--help'], ['bash', str(path), '--help']):
                result = subprocess.run(argv, capture_output=True, timeout=10)
                self.assertEqual(result.returncode, 0, result.stderr)
        for argv in (['launch', 'bad:name'], ['ezf', '-f', '-1'], ['ezf', '-c', '(error "oops")'],
                     ['ezf', '--timeout', '0'], ['ezf', '--single', '-c', 'my-completer']):
            result = subprocess.run(['python3', str(TMUX_LIB), *argv], input=b'a\n', capture_output=True, timeout=10)
            self.assertNotEqual(result.returncode, 0)



class TransportTests(unittest.TestCase):
    def test_private_json_roundtrip_and_cleanup(self):
        candidates = ["nil", "a,b", "space and ' quote", "newline\nin record", "λ", "$(touch nope)"]
        temporary_paths = []
        def client(argv, **kwargs):
            import base64
            encoded = re.findall(r'base64-decode-string "([A-Za-z0-9+/=]+)"', argv[-1])
            self.assertEqual(len(encoded), 2)
            request_path = Path(base64.b64decode(encoded[-1]).decode())
            request = json.loads(request_path.read_text())
            self.assertEqual(request['candidates'], candidates)
            self.assertIn('--alternate-editor=false', argv)
            self.assertEqual(stat.S_IMODE(request_path.parent.stat().st_mode), 0o700)
            for name in ('request.json', 'result.json', 'candidates'):
                path = request_path.parent / name
                self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)
                temporary_paths.append(path)
            Path(request['output']).write_text(json.dumps({'status': 'ok', 'values': candidates}))
            return subprocess.CompletedProcess(argv, 0, b'"irrelevant Lisp printer output"', b'')
        with patch.object(tmux, 'executable', return_value='/emacsclient'), patch.object(tmux, 'run', client):
            self.assertEqual(tmux.ezf_select(candidates), candidates)
        self.assertTrue(all(not path.exists() for path in temporary_paths))

    def test_client_failures_and_timeout_clean_up(self):
        import base64
        for code, message, expected in ((1, b"can't find socket; have you started the server?", 69),
                                         (1, b'ERROR: invalid Lisp library', 70),
                                         (0, b'', 70)):
            paths = []
            def client(argv, **kwargs):
                token = re.findall(r'base64-decode-string "([A-Za-z0-9+/=]+)"', argv[-1])[-1]
                paths.append(Path(base64.b64decode(token).decode()).parent)
                return subprocess.CompletedProcess(argv, code, b'', message)
            with self.subTest(expected=expected), patch.object(tmux, 'executable', return_value='/emacsclient'), \
                 patch.object(tmux, 'run', client), self.assertRaises(tmux.Failure) as error:
                tmux.ezf_select(['a'])
            self.assertEqual(error.exception.code, expected)
            self.assertTrue(all(not path.exists() for path in paths))
        with patch.object(tmux, 'executable', return_value='/emacsclient'), \
             patch.object(tmux, 'run', side_effect=tmux.Failure('timed out', 124)), self.assertRaises(tmux.Failure) as error:
            tmux.ezf_select(['a'])
        self.assertEqual(error.exception.code, 124)

    def test_fzf_config_cannot_change_record_protocol(self):
        def fzf(argv, **kwargs):
            self.assertIn('--no-multi', argv)
            self.assertIn('--read0', argv)
            self.assertIn('--print0', argv)
            self.assertEqual(kwargs['input'], b'api\t$3\0')
            self.assertEqual(kwargs['env']['FZF_DEFAULT_OPTS'], '')
            self.assertEqual(kwargs['env']['FZF_DEFAULT_OPTS_FILE'], '')
            return subprocess.CompletedProcess(argv, 0, b'api\t$3\0', b'')
        with patch.dict(os.environ, {'FZF_DEFAULT_OPTS': '--multi --print-query', 'FZF_DEFAULT_OPTS_FILE': '/bad'}), \
             patch.object(tmux, 'executable', return_value='/fzf'), patch.object(subprocess, 'run', fzf):
            self.assertEqual(tmux.fzf_select(['api\t$3']), 'api\t$3')

    def test_picker_infrastructure_failure_is_not_cancel(self):
        with patch.object(tmux, 'sessions', return_value={'opencode-api': '$3'}), \
             patch.object(tmux, 'choose', side_effect=tmux.Failure('fzf broke', 1)), \
             patch.object(tmux, 'attach') as attach:
            with self.assertRaises(tmux.Failure):
                tmux.pick('fzf')
            attach.assert_not_called()

    def test_unmarked_collision_and_bad_creation_id_fail_closed(self):
        for result in (subprocess.CompletedProcess([], 1, b'', b'duplicate session'),
                       subprocess.CompletedProcess([], 0, b'$4;kill-server\n', b'')):
            with patch.object(tmux, 'sessions', side_effect=[{}, {}]), patch.object(tmux, 'tmux', return_value=result), \
                 patch.object(tmux, 'executable', return_value='/usr/bin/true'), self.assertRaises(tmux.Failure):
                tmux.ensure_session('api', ROOT, [])

    def test_no_sessions_does_not_open_picker(self):
        with patch.object(tmux, 'sessions', return_value={}), patch.object(tmux, 'choose') as choose:
            self.assertEqual(tmux.pick('fzf'), 0)
            choose.assert_not_called()

    def test_control_timeout_is_explicit(self):
        with patch.object(subprocess, 'run', side_effect=subprocess.TimeoutExpired(['tmux'], 10)), \
             self.assertRaises(tmux.Failure) as error:
            tmux.run(['tmux'])
        self.assertEqual(error.exception.code, 124)


@unittest.skipUnless(shutil.which('tmux'), 'real tmux is not installed')
class RealTmuxTests(unittest.TestCase):
    def test_session_identity_cwd_and_argument_boundaries(self):
        # Isolated socket/config; never accesses the user's tmux server or OpenCode.
        with tempfile.TemporaryDirectory() as directory:
            base = Path(directory)
            project = base / "space ' quote #{pane_id};"
            project.mkdir()
            socket = base / 'tmux.sock'
            binary = base / 'opencode'
            binary.write_text('#!/usr/bin/env python3\nimport os,json,sys,time\n'
                              'from pathlib import Path\nPath("result.json").write_text(json.dumps([os.getcwd(),sys.argv[1:]]))\ntime.sleep(30)\n')
            binary.chmod(0o755)
            original = tmux.tmux
            real_executable = tmux.executable
            real_tmux = shutil.which('tmux')
            def run_tmux(*args):
                return subprocess.run([real_tmux, '-S', str(socket), '-f', '/dev/null', *args], capture_output=True, timeout=10)
            def executable(name):
                return str(binary) if name == 'opencode' else real_executable(name)
            try:
                with patch.object(tmux, 'tmux', run_tmux), patch.object(tmux, 'executable', executable):
                    ident = tmux.ensure_session('test', project, ['--prompt', ';', "a'b", '$(touch PWNED)'])
                    self.assertEqual(tmux.sessions(), {'opencode-test': ident})
                    self.assertEqual(tmux.ensure_session('test', project, []), ident)
                    import time
                    for _ in range(100):
                        if (project / 'result.json').exists():
                            break
                        time.sleep(.02)
                    result = json.loads((project / 'result.json').read_text())
                    self.assertEqual(result, [str(project), [str(project), '--prompt', ';', "a'b", '$(touch PWNED)']])
                    self.assertFalse((project / 'PWNED').exists())
            finally:
                subprocess.run([real_tmux, '-S', str(socket), 'kill-server'], capture_output=True)


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


class SesContractTests(unittest.TestCase):
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
        parsed = ses.parse_sessions(records)
        self.assertEqual([s["id"] for s in parsed], ["ses_e853fa9bbffek0fnTQuj3AHNpb", "ses_untitled0"])
        self.assertEqual(parsed[1]["title"], "(untitled)")

    def test_newest_first_and_deterministic_ties(self):
        records = [record(ident="ses_aaaaaaaa1", updated=100),
                   record(ident="ses_cccccccc3", updated=300),
                   record(ident="ses_bbbbbbbb2", updated=300)]
        self.assertEqual([s["id"] for s in ses.parse_sessions(records)],
                         ["ses_bbbbbbbb2", "ses_cccccccc3", "ses_aaaaaaaa1"])

    def test_rows_show_timestamps_title_directory_and_id(self):
        stamp = datetime.fromtimestamp(1793060808.833).strftime("%Y-%m-%d %H:%M")
        title = 'pick "me" $(touch PWNED) λ'
        sessions = ses.parse_sessions([record(title=title, directory="/tmp/one two")])
        self.assertEqual(ses.rows(sessions),
                         [f"{stamp}\tcreated {stamp}\t{title}\t/tmp/one two\tses_e853fa9bbffek0fnTQuj3AHNpb"])
        both = ses.rows(ses.parse_sessions([record(), record(ident="ses_older0001", updated=1, created=1)]))
        self.assertEqual(len(both), len(set(both)))

    def test_listing_validates_payload_exit_timeout(self):
        ok = subprocess.CompletedProcess([], 0, json.dumps({"data": [record()]}).encode(), b'')
        with patch.object(subprocess, 'run', return_value=ok):
            self.assertEqual([s["id"] for s in ses.parse_sessions(ses.listing('/opencode'))],
                             ["ses_e853fa9bbffek0fnTQuj3AHNpb"])
        for stdout in (b'not json', b'[]', b'{"data": 3}'):
            with self.subTest(stdout=stdout), \
                 patch.object(subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], 0, stdout, b'')), \
                 self.assertRaises(ses.Failure) as error:
                ses.listing('/opencode')
            self.assertEqual(error.exception.code, 70)
        failed = subprocess.CompletedProcess([], 1, b'', b'connection refused')
        with patch.object(subprocess, 'run', return_value=failed), \
             self.assertRaises(ses.Failure) as error:
            ses.listing('/opencode')
        self.assertIn('connection refused', str(error.exception))
        with patch.object(subprocess, 'run',
                          side_effect=subprocess.TimeoutExpired(['opencode'], 60)), \
             self.assertRaises(ses.Failure) as error:
            ses.listing('/opencode')
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
             patch.object(ses, 'executable', return_value='/fzf'), \
             patch.object(subprocess, 'run', fzf):
            self.assertEqual(ses.fzf_select(['row one']), 'row one')

    def test_fzf_must_return_one_original_record(self):
        with patch.object(ses, 'executable', return_value='/fzf'), \
             patch.object(subprocess, 'run',
                          return_value=subprocess.CompletedProcess([], 0, b'row\0', b'')):
            self.assertEqual(ses.fzf_select(['row']), 'row')
        for value in (b'row', b'a\0b\0', b'forged\0', b'\xff\xfe\0'):
            with self.subTest(value=value), \
                 patch.object(ses, 'executable', return_value='/fzf'), \
                 patch.object(subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], 0, value, b'')), \
                 self.assertRaises(ses.Failure) as error:
                ses.fzf_select(['row'])
            self.assertEqual(error.exception.code, 70)

    def test_cancel_and_infrastructure_failure_differ(self):
        for code in (1, 130):
            with self.subTest(code=code), \
                 patch.object(ses, 'executable', return_value='/fzf'), \
                 patch.object(subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], code, b'', b'')), \
                 self.assertRaises(ses.Failure) as error:
                ses.fzf_select(['row'])
            self.assertEqual(error.exception.code, 130)
        with patch.object(ses, 'executable', return_value='/fzf'), \
             patch.object(subprocess, 'run',
                          return_value=subprocess.CompletedProcess([], 2, b'', b'boom')), \
             self.assertRaises(ses.Failure) as error:
            ses.fzf_select(['row'])
        self.assertEqual(error.exception.code, 1)

    def test_missing_dependencies_are_named(self):
        for name in ('opencode', 'fzf'):
            with self.subTest(name=name), patch.object(ses.shutil, 'which', return_value=None), \
                 self.assertRaises(ses.Failure) as error:
                ses.executable(name)
            self.assertEqual(error.exception.code, 127)
            self.assertIn(name, str(error.exception))

    def test_resume_execs_supported_interface_in_directory(self):
        session = ses.parse_sessions([record(directory=str(ROOT))])[0]
        with patch.object(ses.os, 'chdir') as chdir, patch.object(ses.os, 'execv') as execute:
            ses.resume('/opencode', session)
            chdir.assert_called_once_with(Path(session['directory']))
            execute.assert_called_once_with('/opencode',
                                            ['/opencode', '--session', session['id'], session['directory']])

    def test_resume_fails_closed_when_directory_is_gone(self):
        session = ses.parse_sessions([record(directory="/definitely/not/here")])[0]
        with patch.object(ses.os, 'execv') as execute, self.assertRaises(ses.Failure) as error:
            ses.resume('/opencode', session)
        execute.assert_not_called()
        self.assertIn('no longer exists', str(error.exception))

    def test_empty_inventory_never_opens_fzf(self):
        with patch.object(ses, 'executable', return_value='/bin'), \
             patch.object(ses, 'listing', return_value=[]), patch.object(ses, 'fzf_select') as pick:
            self.assertEqual(ses.main([]), 0)
            pick.assert_not_called()

    def test_cancel_is_a_successful_no_op(self):
        with patch.object(ses, 'executable', return_value='/bin'), \
             patch.object(ses, 'listing', return_value=[record()]), \
             patch.object(ses, 'fzf_select', side_effect=ses.Failure('', 130)), \
             patch.object(ses, 'resume') as resume:
            self.assertEqual(ses.main([]), 0)
            resume.assert_not_called()

    def test_stale_selection_never_creates_a_replacement_session(self):
        selected = ses.rows(ses.parse_sessions([record()]))[0]
        with patch.object(ses, 'executable', return_value='/bin'), \
             patch.object(ses, 'listing', side_effect=[[record()], []]), \
             patch.object(ses, 'fzf_select', return_value=selected), \
             patch.object(ses, 'resume') as resume, self.assertRaises(ses.Failure) as error:
            ses.main([])
        resume.assert_not_called()
        self.assertIn('disappeared', str(error.exception))

    def test_cli_help(self):
        launcher = str(ROOT / '.local/bin/opencode-ses')
        for argv in ([launcher, '--help'], ['bash', launcher, '--help']):
            result = subprocess.run(argv, capture_output=True, timeout=10)
            self.assertEqual(result.returncode, 0, result.stderr)



class SesEndToEndTests(unittest.TestCase):
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


class ParityTests(unittest.TestCase):
    def test_literate_generated_parity(self):
        text = (ROOT / 'scripts/opencode.org').read_text()
        blocks = re.findall(r'^#\+begin_src \S+ :tangle (\S+)[^\n]*\n(.*?)^#\+end_src\s*$', text, re.M | re.S)
        self.assertEqual(len(blocks), 10)
        for target, body in blocks:
            self.assertEqual((ROOT / 'scripts' / target).resolve().read_text(), body, target)
        mods = (ROOT / 'nix/home-manager/mods/default.nix').read_text()
        self.assertIn('./opencode-tmux.nix', mods)
        self.assertIn('./opencode-ses.nix', mods)


if __name__ == '__main__':
    unittest.main()
