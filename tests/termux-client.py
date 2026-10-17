"""Termux client contracts with temporary HOME and a local curl double only."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / 'scripts/termux-starintel-client'

class ClientTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='termux client ')
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name)
        self.prefix = self.home / 'prefix'
        self.bin = self.prefix / 'bin'
        self.bin.mkdir(parents=True)
        (self.bin / 'bash').symlink_to('/bin/bash')
        self.env = dict(os.environ, HOME=str(self.home), PREFIX=str(self.prefix),
                        PATH=str(self.bin) + ':' + os.environ['PATH'])
        for key in ('STARINTEL_ADMIN_URL', 'STAR_SERVER_URL', 'STARINTEL_ADMIN_TOKEN',
                    'STAR_SERVER_TOKEN', 'STAR_SERVER_API_KEY', 'STARINTEL_ADMIN_TOKEN_FILE',
                    'STARINTEL_ADMIN_CURL'):
            self.env.pop(key, None)
        curl = self.bin / 'curl'
        curl.write_text('''#!/bin/bash
set -euo pipefail
printf '%s\\n' "$@" >> "$HOME/requests"
[[ "${FAIL_CURL:-0}" == 0 ]] || { echo sensitive-response >&2; exit 22; }
if [[ "${HUGE_RESPONSE:-0}" == 1 ]]; then head -c 1100000 /dev/zero; exit; fi
[[ "${*: -1}" == 'https://star.test/auth/credentials' ]]
printf '[{"id":"fixture-key","status":"active"}]'
''')
        curl.chmod(0o755)

    def run_helper(self, *args, ok=True, **env):
        result = subprocess.run(['/bin/bash', str(SCRIPT), *args], env=self.env | env,
                                capture_output=True, text=True)
        if ok:
            self.assertEqual(result.returncode, 0, result.stderr)
        return result

    def online(self, *args, **env):
        return self.run_helper(*args, STARINTEL_ADMIN_URL='https://star.test',
                               STARINTEL_ADMIN_TOKEN='fixture-secret', **env)

    def test_install_rerun_preserves_external_configuration_and_has_no_network(self):
        cfg = self.home / '.config/starintel-admin'
        cfg.mkdir(parents=True)
        (cfg / 'datasets.json').write_bytes(b'operator policy')
        secret = self.home / 'private-token'
        secret.write_text('external-secret\n'); secret.chmod(0o600)
        self.run_helper('install', STARINTEL_ADMIN_URL='https://star.test',
                        STARINTEL_ADMIN_TOKEN_FILE=str(secret))
        first = (self.bin / 'starintel-admin').read_bytes()
        self.run_helper('install')
        self.assertEqual(first, (self.bin / 'starintel-admin').read_bytes())
        self.assertNotIn(b'external-secret', first)
        self.assertNotIn(b'https://star.test', first)
        self.assertEqual(secret.read_text(), 'external-secret\n')
        self.assertEqual((cfg / 'datasets.json').read_bytes(), b'operator policy')
        self.assertFalse((self.home / 'requests').exists())
        self.assertFalse((self.home / '.termux').exists())

    def test_installed_launcher_reuses_canonical_cli(self):
        self.run_helper('install')
        result = subprocess.run(['/bin/bash', str(self.bin / 'starintel-admin'), '--version'],
                                env=self.env, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), 'starintel-admin 0.1.0')
        self.assertFalse((self.home / 'requests').exists())

    def test_offline_doctor_and_help_never_connect(self):
        self.run_helper('doctor')
        self.run_helper('run', '--help')
        self.assertFalse((self.home / 'requests').exists())

    def test_explicit_remote_diagnostic_uses_only_credential_list_get(self):
        result = self.online('doctor', '--connect')
        self.assertIn('reachable', result.stdout)
        self.assertNotIn('fixture-key', result.stdout)
        self.assertNotIn('fixture-secret', result.stdout + result.stderr)
        requests = (self.home / 'requests').read_text()
        self.assertIn('\nGET\n', requests)
        self.assertIn('/auth/credentials', requests)
        self.assertNotIn('fixture-secret', requests)
        self.assertFalse((self.home / '.config/starintel-admin/datasets.json').exists())

    def test_normal_cli_dispatch_preserves_tracked_command_behavior(self):
        result = self.online('run', 'key', 'list')
        self.assertIn('fixture-key', result.stdout)
        self.assertNotIn('fixture-secret', result.stdout + result.stderr)
        self.assertEqual((self.home / 'requests').read_text().count('/auth/credentials'), 1)

    def test_missing_endpoint_or_credential_refuses_without_network(self):
        for env in ({}, {'STARINTEL_ADMIN_URL': 'https://star.test'}):
            result = self.run_helper('doctor', '--connect', ok=False, **env)
            self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.home / 'requests').exists())

    def test_url_credentials_query_fragment_and_non_https_are_rejected(self):
        for url in ('http://star.test', 'https://user:secret@star.test',
                    'https://star.test/?token=secret', 'https://star.test/#secret',
                    'https://star.test\nheader = bad'):
            with self.subTest(url=url):
                result = self.run_helper('doctor', '--connect', ok=False,
                    STARINTEL_ADMIN_URL=url, STARINTEL_ADMIN_TOKEN='fixture-secret')
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn(url, result.stderr)
        self.assertFalse((self.home / 'requests').exists())

    def test_supported_private_external_token_file(self):
        secret = self.home / 'external-token'
        secret.write_text('fixture-secret\n'); secret.chmod(0o600)
        self.run_helper('doctor', '--connect', STAR_SERVER_URL='https://star.test/',
                        STARINTEL_ADMIN_TOKEN_FILE=str(secret))
        secret.chmod(0o644)
        self.assertNotEqual(self.run_helper('doctor', '--connect', ok=False,
            STAR_SERVER_URL='https://star.test', STARINTEL_ADMIN_TOKEN_FILE=str(secret)).returncode, 0)

    def test_token_control_characters_cannot_inject_curl_configuration(self):
        for token in ('bad\nheader = secret', 'bad"quote', 'bad\\escape'):
            self.assertNotEqual(self.run_helper('doctor', '--connect', ok=False,
                STARINTEL_ADMIN_URL='https://star.test', STARINTEL_ADMIN_TOKEN=token).returncode, 0)
        self.assertFalse((self.home / 'requests').exists())

    def test_diagnostic_failure_is_redacted_and_temporary_files_removed(self):
        result = self.online('doctor', '--connect', ok=False, FAIL_CURL='1', TMPDIR=str(self.home))
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn('sensitive-response', result.stdout + result.stderr)
        self.assertFalse(list(self.home.glob('starintel-doctor.*')))

    def test_regular_cli_failure_preserves_status_and_cleans_credential_tempfiles(self):
        result = self.online('run', 'key', 'list', ok=False, FAIL_CURL='1', TMPDIR=str(self.home))
        self.assertEqual(result.returncode, 22)
        self.assertFalse(list(self.home.glob('starintel-client.*')))

    def test_symlink_credential_file_is_not_followed(self):
        secret = self.home / 'private-token'
        secret.write_text('fixture-secret\n'); secret.chmod(0o600)
        link = self.home / 'credential-link'; link.symlink_to(secret)
        result = self.run_helper('doctor', '--connect', ok=False,
            STARINTEL_ADMIN_URL='https://star.test', STARINTEL_ADMIN_TOKEN_FILE=str(link))
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.home / 'requests').exists())

    def test_diagnostic_output_is_bounded(self):
        result = self.online('doctor', '--connect', ok=False, HUGE_RESPONSE='1')
        self.assertNotEqual(result.returncode, 0)
        self.assertLess(len(result.stdout) + len(result.stderr), 1000)

    def test_diagnostic_deadline_and_read_only_dispatch_are_fixed(self):
        timeout = self.bin / 'timeout'
        timeout.write_text('#!/bin/bash\nprintf "%s\\n" "$@" > "$HOME/deadline"\nexit 124\n')
        timeout.chmod(0o755)
        result = self.online('doctor', '--connect', ok=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual((self.home / 'deadline').read_text().splitlines()[:3], ['-k', '2', '20'])
        self.assertFalse((self.home / 'requests').exists())
        self.assertNotEqual(self.online('doctor', '--connect', 'user', 'revoke', ok=False).returncode, 0)

    def test_unmanaged_launcher_or_symlink_is_never_overwritten(self):
        target = self.bin / 'starintel-admin'
        target.write_text('existing command')
        self.assertNotEqual(self.run_helper('install', ok=False).returncode, 0)
        self.assertEqual(target.read_text(), 'existing command')
        self.assertFalse((self.bin / 'starintel-admin-doctor').exists())
        target.unlink()
        outside = self.home / 'outside'; outside.write_text('keep')
        target.symlink_to(outside)
        self.assertNotEqual(self.run_helper('install', ok=False).returncode, 0)
        self.assertEqual(outside.read_text(), 'keep')

    def test_missing_dependency_is_reported_without_network(self):
        self.env['PATH'] = str(self.bin)
        self.assertNotEqual(self.run_helper('doctor', ok=False).returncode, 0)
        self.assertFalse((self.home / 'requests').exists())

    def test_literate_parity(self):
        for org, name in [('scripts/termux-starintel-client.org', 'termux-starintel-client'),
                          ('bootstrap-termux.org', 'bootstrap-termux.sh')]:
            target = ROOT / ('scripts/' + name if name == 'termux-starintel-client' else name)
            code = (ROOT / org).read_text().split('#+begin_src sh :tangle ' + name + '\n', 1)[1].split('#+end_src', 1)[0]
            self.assertEqual(target.read_text(), code)
            subprocess.run(['/bin/bash', '-n', str(target)], check=True)

if __name__ == '__main__':
    unittest.main()
