"""Contract checks; real PRoot acceptance is a separate explicit command."""
import importlib.machinery
import importlib.util
from pathlib import Path
import tempfile
import socket
import sys
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader('probe', str(ROOT / 'scripts/termux-couchdb-proof'))
spec = importlib.util.spec_from_loader(loader.name, loader)
probe = importlib.util.module_from_spec(spec)
loader.exec_module(probe)

class Contracts(unittest.TestCase):
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
            runner.logs[0].join(timeout=10)
            files = list(root.glob('bounded.log*'))
            self.assertLessEqual(len(files), 3)
            self.assertLessEqual(sum(p.stat().st_size for p in files), 3 * (1024**2 + 4096))
            # Stop only the separately created process group, without a PID file.
            child = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'], start_new_session=True)
            try:
                runner.stop(child)
                self.assertIsNotNone(child.poll())
            finally:
                if child.poll() is None: child.kill()

    def test_budget_refuses_without_starting_processes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / 'proof'
            probe.prepare(root)
            with (root / 'data/large').open('wb') as stream:
                stream.truncate(129 * 1024**2)
            runner = probe.Probe(root, 'unused', 'unused')
            with self.assertRaises(RuntimeError): runner.budget()
            self.assertEqual(runner.children, [])

    def test_redirects_refused(self):
        self.assertIsNone(probe.NoRedirect().redirect_request(None, None, 302, '', {}, 'http://example.invalid'))

if __name__ == '__main__': unittest.main()

