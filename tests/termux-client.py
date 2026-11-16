"""Termux client contracts: temporary HOME, curl doubles and loopback HTTPS fixtures."""
import os
import contextlib
import http.server
import ssl
import socket
import threading
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / 'scripts/termux-starintel-client'

@contextlib.contextmanager
def local_https(home, ipv6=False):
    """Real curl/TLS test target on loopback only; never uses an external server."""
    certificate, key = home / 'fixture.crt', home / 'fixture.key'
    subprocess.run(['openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes',
                    '-keyout', str(key), '-out', str(certificate), '-days', '1',
                    '-subj', '/CN=localhost', '-addext', 'subjectAltName=DNS:localhost,IP:127.0.0.1,IP:::1'],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    requests = []
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            requests.append((self.path, self.headers.get('Authorization')))
            if self.server.redirect and self.path == '/auth/credentials':
                self.send_response(302)
                self.send_header('Location', f'https://localhost:{self.server.server_port}/unexpected-redirect')
            else:
                self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', '2')
            self.end_headers()
            self.wfile.write(b'[]')
        def log_message(self, *args):
            pass
    class Server(http.server.ThreadingHTTPServer):
        address_family = socket.AF_INET6 if ipv6 else socket.AF_INET
    server = Server(('::1' if ipv6 else '127.0.0.1', 0), Handler)
    server.redirect = False
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain(certificate, key)
    server.socket = context.wrap_socket(server.socket, server_side=True)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield server, requests, certificate
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)

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

    def test_curl_glob_endpoints_are_rejected_but_ipv6_is_valid(self):
        for url in ('https://{one,two}.example', 'https://one.example/[1-3]',
                    'https://one.example/{a,b}'):
            with self.subTest(url=url):
                self.assertNotEqual(self.run_helper('doctor', ok=False, STARINTEL_ADMIN_URL=url).returncode, 0)
        self.run_helper('doctor', STARINTEL_ADMIN_URL='https://[::1]:8443')
        self.assertFalse((self.home / 'requests').exists())

    def test_both_online_paths_disable_curl_config_and_globbing(self):
        for args in (('run', 'key', 'list'), ('doctor', '--connect')):
            with self.subTest(args=args):
                (self.home / 'requests').unlink(missing_ok=True)
                self.online(*args)
                flags = (self.home / 'requests').read_text().splitlines()
                self.assertEqual(flags[0], '--disable')
                self.assertIn('--globoff', flags)
                self.assertIn('--location', flags)
                self.assertEqual(flags[flags.index('--max-redirs') + 1], '0')

    def test_real_curl_ignores_hostile_curlrc_for_both_online_paths(self):
        (self.bin / 'curl').unlink()
        with local_https(self.home) as (server, requests, certificate):
            endpoint = f'https://localhost:{server.server_port}'
            curlrc = self.home / '.curlrc'
            contents = (f'trace-ascii = "{self.home / "trace"}"\n'
                        f'output = "{self.home / "stolen-output"}"\n'
                        'location-trusted\n'
                        f'url = "{endpoint}/unexpected-extra-url"\n')
            curlrc.write_text(contents)
            for args in (('run', 'key', 'list'), ('doctor', '--connect')):
                with self.subTest(args=args):
                    self.run_helper(*args, STARINTEL_ADMIN_URL=endpoint,
                        STARINTEL_ADMIN_TOKEN='fixture-secret', CURL_HOME=str(self.home),
                        CURL_CA_BUNDLE=str(certificate), NO_PROXY='*', no_proxy='*')
                    self.assertFalse((self.home / 'trace').exists())
                    self.assertFalse((self.home / 'stolen-output').exists())
                    self.assertEqual(curlrc.read_text(), contents)
            self.assertEqual(requests, [('/auth/credentials', 'Bearer fixture-secret')] * 2)

    def test_real_curl_redirects_are_blocked_without_forwarding_bearer(self):
        (self.bin / 'curl').unlink()
        with local_https(self.home) as (server, requests, certificate):
            server.redirect = True
            (self.home / '.curlrc').write_text('location-trusted\n')
            for args in (('run', 'key', 'list'), ('doctor', '--connect')):
                with self.subTest(args=args):
                    result = self.run_helper(*args, ok=False,
                        STARINTEL_ADMIN_URL=f'https://localhost:{server.server_port}',
                        STARINTEL_ADMIN_TOKEN='fixture-secret', CURL_HOME=str(self.home),
                        CURL_CA_BUNDLE=str(certificate), NO_PROXY='*', no_proxy='*')
                    self.assertNotEqual(result.returncode, 0)
            self.assertEqual(requests, [('/auth/credentials', 'Bearer fixture-secret')] * 2)

    def test_ipv6_literal_with_real_curl_when_loopback_is_available(self):
        (self.bin / 'curl').unlink()
        try:
            with local_https(self.home, ipv6=True) as (server, requests, certificate):
                self.run_helper('doctor', '--connect',
                    STARINTEL_ADMIN_URL=f'https://[::1]:{server.server_port}',
                    STARINTEL_ADMIN_TOKEN='fixture-secret', CURL_CA_BUNDLE=str(certificate),
                    NO_PROXY='*', no_proxy='*')
                self.assertEqual(requests, [('/auth/credentials', 'Bearer fixture-secret')])
        except OSError as error:
            self.skipTest(f'IPv6 loopback unavailable: {error.__class__.__name__}')

    def test_external_curl_override_is_not_used_on_either_online_path(self):
        other = self.bin / 'untrusted-curl'
        other.write_text('#!/bin/bash\ntouch "$HOME/untrusted-used"\nexit 1\n')
        other.chmod(0o755)
        for args in (('run', 'key', 'list'), ('doctor', '--connect')):
            self.online(*args, STARINTEL_ADMIN_CURL=str(other))
        self.assertFalse((self.home / 'untrusted-used').exists())

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
