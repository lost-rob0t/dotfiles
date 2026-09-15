#!/usr/bin/env python3
"""Local regressions; subprocess doubles never contact an LLM provider."""
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
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
LIB = ROOT / '.local/lib/dotfiles/opencode_tmux.py'
spec = importlib.util.spec_from_file_location('opencode_tmux', LIB)
app = importlib.util.module_from_spec(spec)
spec.loader.exec_module(app)


class ContractTests(unittest.TestCase):
    def test_names_are_bounded_and_unambiguous(self):
        for name in ('a', 'starintel-api', 'pr_42', 'X' * 64):
            self.assertEqual(app.session_name(name), 'opencode-' + name)
        for name in ('', '-bad', 'a:b', 'a.b', 'a b', 'a\nb', '$(id)', 'x;id', 'a' * 65):
            with self.subTest(name=name), self.assertRaises(app.Failure):
                app.session_name(name)

    def test_list_rejects_foreign_and_malformed_records(self):
        text = ('$3\topencode-api\t1\n$4\tother\t1\n'
                '$5\topencode-api-more\t\n$6\topencode-bad:name\t1\n'
                'x\topencode-forged\t1\n$7\topencode-worker_2\t1\n')
        self.assertEqual(app.parse_sessions(text), {'opencode-api': '$3', 'opencode-worker_2': '$7'})

    def test_picker_requires_one_exact_record(self):
        rows = ['api\t$3', 'api-more\t$4']
        self.assertEqual(app.decode_selection(b'api\t$3\0', rows), 'api\t$3')
        for value in (b'', b'api\t$3', b'api\t$3\0api-more\t$4\0', b'forged\t$0\0'):
            with self.subTest(value=value), self.assertRaises(app.Failure):
                app.decode_selection(value, rows)

    def test_ezf_records_preserve_data(self):
        values = ['nil', 'two words', 'a"b', "a'b", r'a\b', 'a,b', '$(touch nope)', 'λ']
        raw = ('\0'.join(values) + '\0').encode()
        self.assertEqual(app.read_candidates(io.BytesIO(raw), True), values)
        self.assertEqual(app.read_candidates(io.BytesIO(b''), False), [])
        self.assertEqual(app.read_candidates(io.BytesIO(b'a\nb\n'), False), ['a', 'b'])
        with self.assertRaises(app.Failure):
            app.read_candidates(io.BytesIO(b'a\0b'), False)
        with self.assertRaises(app.Failure):
            app.read_candidates(io.BytesIO(b'\xff'), False)
        with patch.object(app, 'MAX_INPUT', 8), self.assertRaises(app.Failure):
            app.read_candidates(io.BytesIO(b'a' * 9), False)

    def test_field_is_zero_based_and_bounded(self):
        self.assertEqual(app.fields(['one  two three', 'x y z'], 1), ['two', 'y'])
        self.assertEqual(app.fields(['a b'], 0), ['a'])
        for field in (-1, 10):
            with self.assertRaises(app.Failure):
                app.fields(['a b'], field)

    def test_lisp_paths_use_only_base64_data(self):
        payload = 'x\") (delete-file \"/tmp/no\") ; λ'
        expression = app.lisp_data(payload)
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
            command = app.shell_command(str(executable), str(out), args)
            self.assertFalse(command.endswith(';'))
            subprocess.run(['sh', '-c', command], check=True, cwd=directory)
            self.assertEqual(json.loads(out.read_text()), args)
            self.assertFalse((Path(directory) / 'INJECTION').exists())

    def test_ezf_response_is_not_executable_and_nil_is_data(self):
        self.assertEqual(app.validate_ezf_response({'status': 'ok', 'values': ['nil']}, ['nil'], True), ['nil'])
        for response in ({'status': 'ok', 'values': ['forged']},
                         {'status': 'ok', 'values': ['a', 'b']},
                         {'status': 'ok', 'values': 'a'},
                         {'status': 'wat'}, []):
            with self.assertRaises(app.Failure):
                app.validate_ezf_response(response, ['a', 'b'], True)
        for state, code in (('cancel', 130), ('unavailable', 69), ('timeout', 124), ('error', 70)):
            with self.assertRaises(app.Failure) as result:
                app.validate_ezf_response({'status': state, 'message': 'test'}, ['a'], True)
            self.assertEqual(result.exception.code, code)

    def test_empty_ezf_input_never_starts_emacs(self):
        with patch.object(app, 'executable', side_effect=AssertionError('must not run')):
            self.assertEqual(app.ezf_select([], single=True), [])

    def test_no_server_and_permission_failure_are_different(self):
        absent = subprocess.CompletedProcess([], 1, b'', b'no server running on /tmp/test\n')
        denied = subprocess.CompletedProcess([], 1, b'', b'error connecting to /tmp/test (Permission denied)\n')
        with patch.object(app, 'tmux', return_value=absent):
            self.assertEqual(app.sessions(), {})
        with patch.object(app, 'tmux', return_value=denied), self.assertRaises(app.Failure):
            app.sessions()

    def test_create_reuses_race_winner_without_starting_again(self):
        failure = subprocess.CompletedProcess([], 1, b'', b'duplicate session\n')
        with patch.object(app, 'sessions', side_effect=[{}, {'opencode-api': '$9'}]), \
             patch.object(app, 'tmux', return_value=failure) as tmux, \
             patch.object(app, 'executable', return_value='/usr/bin/true'):
            self.assertEqual(app.ensure_session('api', ROOT, []), '$9')
            self.assertEqual(tmux.call_count, 1)

    def test_reusing_session_does_not_require_opencode(self):
        with patch.object(app, 'sessions', return_value={'opencode-api': '$9'}), \
             patch.object(app, 'executable', side_effect=AssertionError('must not run')):
            self.assertEqual(app.ensure_session('api', ROOT, []), '$9')

    def test_new_session_receives_atomic_marker_and_literal_cwd(self):
        created = subprocess.CompletedProcess([], 0, b'$9\n', b'')
        with tempfile.TemporaryDirectory(prefix='cwd-#{pane_id}-') as directory:
            with patch.object(app, 'sessions', side_effect=[{}, {'opencode-api': '$9'}]), \
                 patch.object(app, 'tmux', return_value=created) as tmux, \
                 patch.object(app, 'executable', return_value='/usr/bin/true'):
                self.assertEqual(app.ensure_session('api', Path(directory), ['--model', 'a/b']), '$9')
                argv = tmux.call_args.args
                self.assertIn('DOTFILES_OPENCODE_SESSION=1', argv)
                self.assertIn(directory.replace('#', '##') + '/.', argv)
                self.assertIn('-d', argv)

    def test_attach_switches_inside_tmux_and_execs_outside(self):
        with patch.dict(os.environ, {'TMUX': 'active'}), patch.object(app, 'tmux') as tmux:
            tmux.return_value = subprocess.CompletedProcess([], 0, b'', b'')
            app.attach('$4')
            tmux.assert_called_once_with('switch-client', '-t', '$4')
        with patch.dict(os.environ, {}, clear=True), patch.object(app, 'executable', return_value='/tmux'), \
             patch.object(os, 'execv') as execute:
            app.attach('$4')
            execute.assert_called_once_with('/tmux', ['/tmux', 'attach-session', '-t', '$4'])

    def test_auto_does_not_fall_back_on_emacs_cancel(self):
        with patch.dict(os.environ, {'INSIDE_EMACS': 'vterm'}), \
             patch.object(app, 'ezf_select', side_effect=app.Failure('', 130)), \
             patch.object(app, 'fzf_select') as fzf:
            with self.assertRaises(app.Failure):
                app.choose(['a'], 'auto')
            fzf.assert_not_called()

    def test_auto_falls_back_only_when_emacs_unavailable(self):
        with patch.dict(os.environ, {'INSIDE_EMACS': 'vterm'}), \
             patch.object(app, 'ezf_select', side_effect=app.Failure('no frame', 69)), \
             patch.object(app, 'fzf_select', return_value='a') as fzf:
            self.assertEqual(app.choose(['a'], 'auto'), 'a')
            fzf.assert_called_once()

    def test_cancel_and_stale_pick_never_attach(self):
        with patch.object(app, 'sessions', return_value={'opencode-api': '$3'}), \
             patch.object(app, 'choose', side_effect=app.Failure('', 130)), \
             patch.object(app, 'attach') as attach:
            self.assertEqual(app.pick('fzf'), 0)
            attach.assert_not_called()
        with patch.object(app, 'sessions', side_effect=[{'opencode-api': '$3'}, {'opencode-api': '$4'}]), \
             patch.object(app, 'choose', return_value='api\t$3'), \
             patch.object(app, 'attach') as attach:
            with self.assertRaises(app.Failure):
                app.pick('fzf')
            attach.assert_not_called()

    def test_cli_help_and_invalid_input(self):
        for command in ('opencode-tmux', 'opencode-pick', 'ezf', 'ezf.sh'):
            path = ROOT / '.local/bin' / command
            for argv in ([str(path), '--help'], ['bash', str(path), '--help']):
                result = subprocess.run(argv, capture_output=True, timeout=10)
                self.assertEqual(result.returncode, 0, result.stderr)
        for argv in (['launch', 'bad:name'], ['ezf', '-f', '-1'], ['ezf', '-c', '(error "oops")'],
                     ['ezf', '--timeout', '0'], ['ezf', '--single', '-c', 'my-completer']):
            result = subprocess.run(['python3', str(LIB), *argv], input=b'a\n', capture_output=True, timeout=10)
            self.assertNotEqual(result.returncode, 0)

    def test_literate_generated_parity(self):
        text = (ROOT / 'scripts/opencode-tmux.org').read_text()
        blocks = re.findall(r'^#\+begin_src \S+ :tangle (\S+)[^\n]*\n(.*?)^#\+end_src\s*$', text, re.M | re.S)
        self.assertGreaterEqual(len(blocks), 7)
        for target, body in blocks:
            self.assertEqual((ROOT / 'scripts' / target).resolve().read_text(), body, target)
        self.assertIn('./opencode-tmux.nix', (ROOT / 'nix/home-manager/mods/default.nix').read_text())


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
        with patch.object(app, 'executable', return_value='/emacsclient'), patch.object(app, 'run', client):
            self.assertEqual(app.ezf_select(candidates), candidates)
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
            with self.subTest(expected=expected), patch.object(app, 'executable', return_value='/emacsclient'), \
                 patch.object(app, 'run', client), self.assertRaises(app.Failure) as error:
                app.ezf_select(['a'])
            self.assertEqual(error.exception.code, expected)
            self.assertTrue(all(not path.exists() for path in paths))
        with patch.object(app, 'executable', return_value='/emacsclient'), \
             patch.object(app, 'run', side_effect=app.Failure('timed out', 124)), self.assertRaises(app.Failure) as error:
            app.ezf_select(['a'])
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
             patch.object(app, 'executable', return_value='/fzf'), patch.object(subprocess, 'run', fzf):
            self.assertEqual(app.fzf_select(['api\t$3']), 'api\t$3')

    def test_picker_infrastructure_failure_is_not_cancel(self):
        with patch.object(app, 'sessions', return_value={'opencode-api': '$3'}), \
             patch.object(app, 'choose', side_effect=app.Failure('fzf broke', 1)), \
             patch.object(app, 'attach') as attach:
            with self.assertRaises(app.Failure):
                app.pick('fzf')
            attach.assert_not_called()

    def test_unmarked_collision_and_bad_creation_id_fail_closed(self):
        for result in (subprocess.CompletedProcess([], 1, b'', b'duplicate session'),
                       subprocess.CompletedProcess([], 0, b'$4;kill-server\n', b'')):
            with patch.object(app, 'sessions', side_effect=[{}, {}]), patch.object(app, 'tmux', return_value=result), \
                 patch.object(app, 'executable', return_value='/usr/bin/true'), self.assertRaises(app.Failure):
                app.ensure_session('api', ROOT, [])

    def test_no_sessions_does_not_open_picker(self):
        with patch.object(app, 'sessions', return_value={}), patch.object(app, 'choose') as choose:
            self.assertEqual(app.pick('fzf'), 0)
            choose.assert_not_called()

    def test_control_timeout_is_explicit(self):
        with patch.object(subprocess, 'run', side_effect=subprocess.TimeoutExpired(['tmux'], 10)), \
             self.assertRaises(app.Failure) as error:
            app.run(['tmux'])
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
            original = app.tmux
            real_executable = app.executable
            real_tmux = shutil.which('tmux')
            def tmux(*args):
                return subprocess.run([real_tmux, '-S', str(socket), '-f', '/dev/null', *args], capture_output=True, timeout=10)
            def executable(name):
                return str(binary) if name == 'opencode' else real_executable(name)
            try:
                with patch.object(app, 'tmux', tmux), patch.object(app, 'executable', executable):
                    ident = app.ensure_session('test', project, ['--prompt', ';', "a'b", '$(touch PWNED)'])
                    self.assertEqual(app.sessions(), {'opencode-test': ident})
                    self.assertEqual(app.ensure_session('test', project, []), ident)
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


if __name__ == '__main__':
    unittest.main()
