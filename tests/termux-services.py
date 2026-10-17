"""Disposable-HOME integration tests; never contact Android or run a collector."""
import os
import fcntl
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / 'scripts/termux-starintel'

class ServicesTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name)
        self.prefix = self.home / 'prefix'
        self.bin = self.prefix / 'bin'
        self.bin.mkdir(parents=True)
        (self.prefix / 'var/service/crond/log').mkdir(parents=True)
        (self.prefix / 'var/service/crond/supervise').mkdir()
        os.mkfifo(self.prefix / 'var/service/crond/supervise/ok')
        (self.prefix / 'var/service/crond/run').write_text('# packaged service\n')
        self.env = dict(os.environ, HOME=str(self.home), PREFIX=str(self.prefix),
                        PATH=str(self.bin) + ':' + os.environ['PATH'])
        self.cron = self.home / 'cron'
        self.cron.write_text('0 2 * * * backup-important\n*/5 * * * * ' + str(self.home) + '/.local/bin/election26 --config ' + str(self.home) + '/starintel/ops/archive-config.json --archive ' + str(self.home) + '/starintel/ops/archive run --mode rss >> old.log 2>&1\n')
        self.fake('crontab', 'if [[ "$1" == -l ]]; then cat "$HOME/cron"; else cp "$1" "$HOME/cron"; fi')
        self.fake('service-daemon', 'echo daemon >> "$HOME/actions"')
        self.fake('sv-enable', 'echo enable >> "$HOME/actions"')
        self.fake('sv', 'echo "$*" >> "$HOME/actions"')
        self.fake('pgrep', 'exit 1')
        self.fake('svlogd', 'cat >> "${@: -1}/current"')
        collector = self.home / '.local/bin/election26'
        collector.parent.mkdir(parents=True)
        collector.write_text('#!/bin/bash\necho called >> "$HOME/calls"\necho result\nexit "${COLLECT_EXIT:-0}"\n')
        collector.chmod(0o755)
        archive = self.home / 'starintel/ops/archive'
        archive.mkdir(parents=True)
        (archive / 'keep').write_bytes(b'original archive\x00')
        (archive.parent / 'archive-config.json').write_text('{}')
        (self.home / '2026').mkdir()
        (self.home / '2026/keep').write_text('earlier data')

    def fake(self, name, body):
        path = self.bin / name
        path.write_text('#!/bin/bash\nset -eu\n' + body + '\n')
        path.chmod(0o755)

    def run_script(self, *args, ok=True, **env):
        result = subprocess.run(['bash', str(SCRIPT), *args], env=self.env | env,
                                text=True, capture_output=True)
        if ok:
            self.assertEqual(result.returncode, 0, result.stderr)
        return result

    def test_install_rerun_preserves_data_schedule_and_other_jobs(self):
        before = self.cron.read_text()
        self.run_script('install')
        first = self.cron.read_text()
        self.run_script('install')
        self.assertEqual(first, self.cron.read_text())
        self.assertIn('0 2 * * * backup-important\n', first)
        self.assertEqual(first.count(' collect '), 1)
        self.assertIn('>> old.log 2>&1', first)
        self.assertEqual((self.home / 'starintel/ops/archive/keep').read_bytes(), b'original archive\x00')
        self.assertEqual((self.home / '2026/keep').read_text(), 'earlier data')
        backups = list((self.home / '.local/state/termux-starintel').glob('crontab.*'))
        self.assertEqual(len(backups), 1)
        self.assertEqual(backups[0].read_text(), before)
        self.assertTrue((self.home / '.termux/boot/20-starintel-services').is_file())

    def test_ambiguous_collector_fails_without_cron_edit(self):
        self.cron.write_text(self.cron.read_text() + '*/5 * * * * election26 run\n')
        before = self.cron.read_text()
        self.assertNotEqual(self.run_script('install', ok=False).returncode, 0)
        self.assertEqual(before, self.cron.read_text())

    def test_known_executable_spellings(self):
        original = self.cron.read_text()
        executable = str(self.home / '.local/bin/election26')
        for spelling in ('election26', '~/.local/bin/election26', '$HOME/.local/bin/election26'):
            with self.subTest(spelling=spelling):
                self.cron.write_text(original.replace(executable, spelling))
                self.run_script('install')
                self.assertEqual(self.cron.read_text().count(' collect '), 1)

    def test_no_schedule_adds_exactly_one(self):
        self.cron.write_text('0 2 * * * backup-important\n')
        self.run_script('install')
        self.run_script('install')
        self.assertEqual(self.cron.read_text().count(' collect '), 1)

    def test_missing_config_and_collector_fail_before_cron_edit(self):
        before = self.cron.read_text()
        (self.home / 'starintel/ops/archive-config.json').unlink()
        self.assertNotEqual(self.run_script('install', ok=False).returncode, 0)
        self.assertEqual(before, self.cron.read_text())
        self.assertFalse((self.home / '.termux/boot/20-starintel-services').exists())

    def test_collect_failure_status_and_log(self):
        self.run_script('install')
        result = self.run_script('collect', '--config', str(self.home / 'starintel/ops/archive-config.json'), '--archive', str(self.home / 'starintel/ops/archive'), 'run', '--mode', 'rss', ok=False, COLLECT_EXIT='7')
        self.assertEqual(result.returncode, 7)
        self.assertIn('result', (self.prefix / 'var/log/sv/election26/current').read_text())

    def test_success_and_bounded_log_configuration(self):
        self.run_script('install')
        self.run_script('collect', '--config', str(self.home / 'starintel/ops/archive-config.json'),
                        '--archive', str(self.home / 'starintel/ops/archive'), 'run', '--mode', 'rss')
        self.assertEqual((self.home / 'calls').read_text(), 'called\n')
        for service in ('crond', 'election26'):
            self.assertEqual((self.prefix / 'var/log/sv' / service / 'config').read_text(), 's1000000\nn3\n')

    def test_missing_runtime_config_is_logged_and_fails(self):
        self.run_script('install')
        (self.home / 'starintel/ops/archive-config.json').unlink()
        self.assertNotEqual(self.run_script('collect', ok=False).returncode, 0)
        self.assertIn('not ready', (self.prefix / 'var/log/sv/election26/current').read_text())
        self.assertFalse((self.home / 'calls').exists())

    def test_overlap_skips_collector(self):
        self.run_script('install')
        lock = open(self.home / '.local/state/termux-starintel/collector.lock', 'w')
        self.addCleanup(lock.close)
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        self.assertEqual(self.run_script('collect', ok=False).returncode, 75)
        self.assertFalse((self.home / 'calls').exists())

    def test_start_existing_daemon_does_not_launch_second(self):
        self.fake('pgrep', 'exit 0')
        self.run_script('start')
        self.assertNotIn('enable', (self.home / 'actions').read_text())

    def test_running_collector_aborts_before_cron_edit(self):
        self.fake('pgrep', 'exit 0')
        before = self.cron.read_bytes()
        self.assertNotEqual(self.run_script('install', ok=False).returncode, 0)
        self.assertEqual(before, self.cron.read_bytes())

    def test_timeout_failure_is_preserved(self):
        self.run_script('install')
        self.fake('timeout', 'exit 124')
        self.assertEqual(self.run_script('collect', ok=False).returncode, 124)

    def test_crontab_read_failure_is_not_treated_as_empty(self):
        self.fake('crontab', 'echo permission-denied >&2; exit 1')
        before = self.cron.read_bytes()
        self.assertNotEqual(self.run_script('install', ok=False).returncode, 0)
        self.assertEqual(before, self.cron.read_bytes())

    def test_existing_supervisor_is_not_restarted(self):
        run = self.prefix / 'var/run'
        run.mkdir(parents=True)
        (run / 'service-daemon.pid').write_text(str(os.getpid()) + '\n')
        self.run_script('start')
        self.assertNotIn('daemon', (self.home / 'actions').read_text())

    def test_pause_preserved_by_boot_and_rerun(self):
        self.run_script('install')
        cron = self.cron.read_bytes()
        self.run_script('stop')
        self.run_script('boot')
        self.run_script('install')
        self.run_script('collect')
        self.assertFalse((self.home / 'calls').exists())
        self.assertEqual(cron, self.cron.read_bytes())
        self.run_script('start')
        self.assertFalse((self.home / '.local/state/termux-starintel/paused').exists())

    def test_literate_parity_and_shell_syntax(self):
        for org, target in [('bootstrap-termux.org', 'bootstrap-termux.sh'),
                            ('scripts/termux-starintel.org', 'scripts/termux-starintel')]:
            text = (ROOT / org).read_text()
            code = text.split('#+begin_src sh :tangle ' + Path(target).name + '\n', 1)[1].split('#+end_src', 1)[0]
            self.assertEqual(code, (ROOT / target).read_text())
            subprocess.run(['bash', '-n', str(ROOT / target)], check=True)

if __name__ == '__main__':
    unittest.main()
