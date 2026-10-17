"""Contract checks; real PRoot acceptance is a separate explicit command."""
import importlib.machinery
import importlib.util
from pathlib import Path
import tempfile
import socket
import sys
import subprocess
import time
import signal
import os
import json
from unittest import mock
import unittest

ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader('probe', str(ROOT / 'scripts/termux-couchdb-proof'))
spec = importlib.util.spec_from_loader(loader.name, loader)
probe = importlib.util.module_from_spec(spec)
loader.exec_module(probe)

class Contracts(unittest.TestCase):
    def setUp(self):
        old = os.umask(0o077)
        self.addCleanup(os.umask, old)

    def test_parity(self):
        org = (ROOT / 'scripts/termux-couchdb-proof.org').read_text()
        source = org.split('#+begin_src python :tangle termux-couchdb-proof\n', 1)[1].split('#+end_src', 1)[0]
        self.assertEqual(source, (ROOT / 'scripts/termux-couchdb-proof').read_text())

    def test_prepare_rerun_preserves_private_data(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            before = {p.name: p.read_bytes() for p in root.iterdir() if p.is_file()}
            (root / 'data/sentinel').write_bytes(b'old data\0')
            probe.prepare(root)
            self.assertEqual(before, {p.name: p.read_bytes() for p in root.iterdir() if p.is_file()})
            self.assertEqual((root / 'data/sentinel').read_bytes(), b'old data\0')
            self.assertEqual((root / 'password').stat().st_mode & 0o777, 0o600)
            self.assertNotIn(before['password'].strip(), before['couch.ini'])
            self.assertIn(b'bind_address = 127.0.0.1', before['couch.ini'])

    def test_refuse_existing_unowned_or_symlink_state(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            root.mkdir()
            with self.assertRaises(RuntimeError): probe.prepare(root)
            link = Path(tmp) / 'link'
            link.symlink_to(root)
            with self.assertRaises(RuntimeError): probe.prepare(link)

    def test_guest_pin(self):
        manifest = {'arch': 'x86_64', 'manifest': {'config': {'digest': probe.CONFIGS['x86_64']}}}
        self.assertEqual(probe.verify_manifest(manifest, 'x86_64'), 'x86_64')
        for bad in ({}, manifest | {'arch': 'aarch64'}, manifest | {'manifest': {}}):
            with self.assertRaises(RuntimeError): probe.verify_manifest(bad, 'x86_64')

    def test_listener_refused_but_time_wait_allowed(self):
        with socket.socket() as server:
            server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            server.bind(('127.0.0.1', 0))
            port = server.getsockname()[1]
            server.listen()
            with self.assertRaises(OSError): probe.check_ports([port])
            with socket.create_connection(('127.0.0.1', port)) as client:
                accepted, _ = server.accept()
                accepted.close()
                client.recv(1)
        probe.check_ports([port])

    def test_rotated_logs_and_child_cleanup(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            runner = probe.Probe(root, 'unused', 'unused')
            runner.argv = [sys.executable, '-c']
            child = runner.start(['import sys; sys.stdout.write("x" * 4000000)'], 'bounded')
            self.assertEqual(child.wait(timeout=10), 0)
            runner.stop(child, timeout=.1)
            runner.logs[0].join(timeout=10)
            files = list(root.glob('bounded.log*'))
            self.assertLessEqual(len(files), 3)
            self.assertLessEqual(sum(p.stat().st_size for p in files), 3 * (1024**2 + 4096))
            # Stop only the separately created process group, without a PID file.
            child = probe.OwnedProcess([sys.executable, '-c', 'import time; time.sleep(60)'], stdout=subprocess.DEVNULL)
            child.start()
            try:
                runner.stop(child, timeout=.1)
                self.assertTrue(child.stopped)
            finally:
                if not child.stopped: child.stop(timeout=.1)

    def test_budget_refuses_without_starting_processes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            with (root / 'data/large').open('wb') as stream:
                stream.truncate(129 * 1024**2)
            runner = probe.Probe(root, 'unused', 'unused')
            with self.assertRaises(RuntimeError): runner.budget()
            self.assertEqual(runner.children, [])

    def test_cleanup_reaps_group_after_launcher_exits(self):
        with tempfile.TemporaryDirectory() as tmp:
            ready, stopped = Path(tmp) / 'ready', Path(tmp) / 'stopped'
            worker = ('import signal,time; from pathlib import Path; '
                      'signal.signal(signal.SIGTERM, lambda *_: (Path(' + repr(str(stopped)) + ').touch(), exit(0))); '
                      'Path(' + repr(str(ready)) + ').touch(); time.sleep(60)')
            launcher = 'import subprocess; subprocess.Popen(' + repr([sys.executable, '-c', worker]) + ')'
            proc = probe.OwnedProcess([sys.executable, '-c', launcher], stdout=subprocess.DEVNULL)
            proc.start()
            try:
                proc.wait(timeout=5)
                deadline = time.monotonic() + 5
                while not ready.exists() and time.monotonic() < deadline: time.sleep(.01)
                self.assertTrue(ready.exists())
                probe.Probe.stop(proc, timeout=.2)
                deadline = time.monotonic() + 3
                while not stopped.exists() and time.monotonic() < deadline: time.sleep(.01)
                self.assertTrue(stopped.exists(), 'orphaned child must receive shutdown')
            finally:
                if not proc.stopped: proc.stop(timeout=.1)

    def test_log_symlink_refused_without_touching_target(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            outside = Path(tmp) / 'preserve'
            outside.write_bytes(b'unchanged')
            (root / 'couch.log').symlink_to(outside)
            runner = probe.Probe(root, 'unused', 'unused')
            runner.argv = [sys.executable, '-c']
            try:
                with self.assertRaises(RuntimeError):
                    runner.start(['print("test output")'], 'couch')
            finally:
                for child in runner.children:
                    child.stop(timeout=.1)
                for thread in runner.logs: thread.join(timeout=5)
            self.assertEqual(outside.read_bytes(), b'unchanged')
            self.assertEqual(runner.children, [])

    def test_term_ignoring_descendant_loses_listener_after_kill(self):
        with tempfile.TemporaryDirectory() as tmp:
            ready = Path(tmp) / 'listener'
            worker = ('import socket,signal,time,json; from pathlib import Path; '
                      'signal.signal(signal.SIGTERM, signal.SIG_IGN); '
                      'sock=socket.socket(); sock.bind(("127.0.0.1",0)); sock.listen(); '
                      'Path(' + repr(str(ready)) + ').write_text(json.dumps(sock.getsockname()[1])); time.sleep(60)')
            launcher = 'import subprocess; subprocess.Popen(' + repr([sys.executable, '-c', worker]) + ')'
            proc = probe.OwnedProcess([sys.executable, '-c', launcher], stdout=subprocess.DEVNULL)
            proc.start()
            try:
                self.assertEqual(proc.wait(timeout=5), 0)
                deadline = time.monotonic() + 5
                while not ready.exists() and time.monotonic() < deadline: time.sleep(.01)
                self.assertTrue(ready.exists())
                port = json.loads(ready.read_text())
                with self.assertRaises(OSError): probe.check_ports([port])
                self.assertIsNone(proc.guard.poll(), 'guardian must pin group after launcher exit')
                proc.stop(timeout=.2)
                deadline = time.monotonic() + 3
                while True:
                    try:
                        probe.check_ports([port])
                        break
                    except OSError:
                        if time.monotonic() >= deadline: self.fail('descendant listener leaked')
                        time.sleep(.01)
                with mock.patch.object(probe.os, 'killpg') as kill:
                    proc.stop(timeout=.1)
                    kill.assert_not_called()
            finally:
                if not proc.stopped: proc.stop(timeout=.1)

    def test_dead_guardian_prevents_group_signals(self):
        proc = probe.OwnedProcess([sys.executable, '-c', 'pass'], stdout=subprocess.DEVNULL)
        proc.start()
        proc.wait(timeout=5)
        proc.guard.kill()
        proc.guard.wait(timeout=5)
        try:
            with mock.patch.object(probe.os, 'killpg') as kill:
                with self.assertRaises(RuntimeError): proc.stop(timeout=.1)
                kill.assert_not_called()
        finally:
            os.close(proc.status_fd)

    def test_linked_outputs_and_rotations_refused(self):
        for name in ('lock', 'couch.log', 'epmd.log', 'couch.log.1', 'couch.log.2', 'epmd.log.1', 'epmd.log.2'):
            for kind in ('symlink', 'hardlink'):
                with self.subTest(name=name, kind=kind), tempfile.TemporaryDirectory() as tmp:
                    root = Path(tmp) / 'proof'
                    probe.prepare(root)
                    outside = Path(tmp) / 'preserve'
                    outside.write_bytes(b'outside data')
                    if kind == 'hardlink': os.link(outside, root / name)
                    else: (root / name).symlink_to(outside)
                    with self.assertRaises(RuntimeError): probe.safe_outputs(root)
                    self.assertEqual(outside.read_bytes(), b'outside data')

    def test_log_failure_is_reported(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            runner = probe.Probe(root, 'unused', 'unused')
            runner.argv = [sys.executable, '-c']
            with mock.patch.object(probe.PrivateLogHandler, 'emit', side_effect=OSError('disk full')):
                child = runner.start(['print("log data")'], 'couch')
                try:
                    child.wait(timeout=5)
                finally:
                    child.stop(timeout=.1)
                    runner.logs[0].join(timeout=5)
                self.assertTrue(runner.log_errors)
                with self.assertRaises(RuntimeError): runner.budget()

    def test_wrong_output_owner_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'log'
            path.write_text('private')
            with mock.patch.object(probe.os, 'getuid', return_value=os.getuid() + 1):
                with self.assertRaises(RuntimeError): probe.safe_file(path)
            self.assertEqual(path.read_text(), 'private')

    def test_nonregular_output_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            (root / 'lock').mkdir()
            with self.assertRaises(RuntimeError): probe.safe_outputs(root)

    def test_unowned_process_is_never_signaled(self):
        process = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'], start_new_session=True)
        try:
            with self.assertRaises(RuntimeError): probe.Probe.stop(process, timeout=.1)
            self.assertIsNone(process.poll())
        finally:
            process.terminate()
            process.wait(timeout=5)

    def test_failed_and_interrupted_handshakes_leave_no_processes(self):
        for interrupted in (False, True):
            with self.subTest(interrupted=interrupted), tempfile.TemporaryDirectory() as tmp:
                ready = Path(tmp) / 'child-started'
                worker = ('from pathlib import Path; import time; Path(' + repr(str(ready)) + ').touch(); time.sleep(60)')
                created = []
                before_fds = len(os.listdir('/proc/self/fd'))
                popen = subprocess.Popen
                def capture(*args, **kwargs):
                    child = popen(*args, **kwargs)
                    created.append(child)
                    return child
                def handshake(*args):
                    deadline = time.monotonic() + .5
                    while not ready.exists() and time.monotonic() < deadline: time.sleep(.01)
                    if interrupted: raise KeyboardInterrupt
                    return ([], [], [])
                try:
                    with mock.patch.object(probe.subprocess, 'Popen', side_effect=capture), mock.patch.object(probe.select, 'select', side_effect=handshake):
                        with self.assertRaises(KeyboardInterrupt if interrupted else RuntimeError):
                            probe.OwnedProcess([sys.executable, '-c', worker], stdout=subprocess.DEVNULL)
                    self.assertFalse(ready.exists(), 'guest must wait for committed ownership')
                    self.assertIsNotNone(created[0].poll(), 'guardian must be reaped')
                    self.assertEqual(len(os.listdir('/proc/self/fd')), before_fds, 'startup pipes must close')
                finally:
                    for child in created:
                        try: os.killpg(child.pid, signal.SIGKILL)
                        except ProcessLookupError: pass
                        child.wait(timeout=5)

    def test_term_during_spawn_is_cleaned_after_registration(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            runner = probe.Probe(root, 'unused', 'unused')
            runner.argv = [sys.executable, '-c']
            created = []
            popen = subprocess.Popen
            stop = probe.OwnedProcess.stop
            previous = signal.signal(signal.SIGTERM, lambda *_: (_ for _ in ()).throw(KeyboardInterrupt()))
            def spawn(*args, **kwargs):
                child = popen(*args, **kwargs)
                created.append(child)
                os.kill(os.getpid(), signal.SIGTERM)
                return child
            try:
                with mock.patch.object(probe.subprocess, 'Popen', side_effect=spawn), mock.patch.object(probe.OwnedProcess, 'stop', autospec=True, side_effect=lambda proc, timeout=15: stop(proc, timeout=.1)):
                    with self.assertRaises(KeyboardInterrupt): runner.start(['import time; time.sleep(60)'], 'couch')
                self.assertIsNotNone(created[0].poll(), 'interrupted startup must reap its guardian')
            finally:
                signal.signal(signal.SIGTERM, previous)
                for child in created:
                    try: os.killpg(child.pid, signal.SIGKILL)
                    except ProcessLookupError: pass
                    child.wait(timeout=5)
                for thread in runner.logs: thread.join(timeout=5)

    def test_redirects_refused(self):
        self.assertIsNone(probe.NoRedirect().redirect_request(None, None, 302, '', {}, 'http://example.invalid'))

if __name__ == '__main__': unittest.main()

