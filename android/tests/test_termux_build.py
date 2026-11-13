"""Host-side regression tests; no Android SDK or signing key required."""
import importlib.util
import argparse
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("termux_build", ROOT / "build.py")
build = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(build)


def manifest(package="org.gnu.emacs", uid="com.termux", extra=""):
    return (f'E: manifest (line=1)\n'
            f'  A: package="{package}" (Raw: "{package}")\n'
            f'  A: android:sharedUserId(0x0101000b)="{uid}" (Raw: "{uid}")\n'
            f'{extra}'
            '  E: application (line=4)\n'
            '    A: android:label="ignored"\n')


class ManifestTests(unittest.TestCase):
    def test_expected_manifest(self):
        self.assertEqual(build.parse_manifest(manifest()), ("org.gnu.emacs", "com.termux"))

    def test_missing_uid_is_not_assumed(self):
        with self.assertRaises(build.BuildError):
            build.parse_manifest('E: manifest\n  A: package="org.gnu.emacs"\n')

    def test_child_attribute_is_not_manifest_uid(self):
        with self.assertRaises(build.BuildError):
            build.parse_manifest('E: manifest\n  A: package="org.gnu.emacs"\n'
                                 '  E: application\n  A: android:sharedUserId="com.termux"\n')

    def test_conditional_shared_uid_is_rejected(self):
        with self.assertRaises(build.BuildError):
            build.parse_manifest(manifest(extra='  A: android:sharedUserMaxSdkVersion=0x20\n'))

    def test_certificate(self):
        self.assertEqual(build.parse_certificate('Signer #1 certificate SHA-256 digest: ' + 'ab' * 32), 'ab' * 32)

    def test_empty_or_malformed_certificate_is_rejected(self):
        for text in ('', 'Signer #1 certificate SHA-256 digest: nope'):
            with self.subTest(text=text), self.assertRaises(build.BuildError):
                build.parse_certificate(text)

    def test_multiple_signers_even_with_same_key_are_rejected(self):
        text = '\n'.join(f'Signer #{n} certificate SHA-256 digest: ' + 'ab' * 32 for n in (1, 2))
        with self.assertRaises(build.BuildError):
            build.parse_certificate(text)


class FamilyTests(unittest.TestCase):
    def test_companion_packages_are_part_of_family(self):
        expected = {
            "org.gnu.emacs",
            "com.termux",
            "com.termux.api",
            "com.termux.widget",
            "com.termux.boot",
            "com.termux.window",
            "com.termux.styling",
            "com.termux.tasker",
        }
        self.assertEqual(set(build.FAMILY), expected)

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.inputs = {}
        for package, name in build.FAMILY.items():
            path = self.root / name
            path.write_bytes(b'test apk')
            self.inputs[package] = path
        self.commands = []
        self.bad_uid = False
        self.bad_package = False
        self.bad_signer = False

    def fake_run(self, argv, **kwargs):
        self.commands.append([str(arg) for arg in argv])
        args = [str(arg) for arg in argv]
        if 'xmltree' in args:
            package = next(p for p, name in build.FAMILY.items() if Path(args[-2]).name == name)
            return manifest('wrong.package' if self.bad_package else package,
                            'wrong.uid' if self.bad_uid else 'com.termux')
        if 'verify' in args:
            digest = 'cd' * 32 if self.bad_signer and args[-1].endswith('termux-api-starintel.apk') else 'ab' * 32
            return 'Signer #1 certificate SHA-256 digest: ' + digest
        return ''

    def test_valid_family_produces_hashes_and_one_signer(self):
        with patch.object(build, 'run', side_effect=self.fake_run):
            result = build.verify_family(self.root, self.root)
        self.assertEqual(len(result['apks']), len(build.FAMILY))
        self.assertEqual(result['shared_user_id'], 'com.termux')
        self.assertEqual(result['certificate_sha256'], 'ab' * 32)
        self.assertTrue(all(len(row['sha256']) == 64 for row in result['apks']))
        self.assertEqual(sum('-c' in command for command in self.commands), len(build.FAMILY))

    def test_wrong_uid_package_and_signer_fail_closed(self):
        for flag in ('bad_uid', 'bad_package', 'bad_signer'):
            with self.subTest(flag=flag):
                setattr(self, flag, True)
                with patch.object(build, 'run', side_effect=self.fake_run), self.assertRaises(build.BuildError):
                    build.verify_family(self.root, self.root)
                setattr(self, flag, False)

    def test_missing_apk_is_rejected(self):
        next(iter(self.inputs.values())).unlink()
        with patch.object(build, 'run', side_effect=self.fake_run), self.assertRaises(build.BuildError):
            build.verify_family(self.root, self.root)

    def test_duplicate_or_missing_emacs_output_is_rejected(self):
        directory = self.root / 'java'
        directory.mkdir()
        with self.assertRaises(build.BuildError):
            build.single_apk(directory)
        (directory / 'emacs-one.apk').touch()
        self.assertEqual(build.single_apk(directory).name, 'emacs-one.apk')
        (directory / 'emacs-stale.apk').touch()
        with self.assertRaises(build.BuildError):
            build.single_apk(directory)

    def test_signing_needs_existing_private_material(self):
        with self.assertRaises(build.BuildError):
            build.sign_family(self.inputs, self.root / 'out', self.root,
                              self.root / 'missing.keystore', self.root / 'missing.pass')

    def test_sign_uses_password_file_and_preserves_inputs(self):
        key = self.root / 'key'; key.write_text('fixture key')
        password = self.root / 'password'; password.write_text('not-on-command-line')
        before = {p: path.read_bytes() for p, path in self.inputs.items()}
        def fake_sign(argv, **kwargs):
            args = [str(arg) for arg in argv]
            self.commands.append(args)
            if 'sign' in args:
                Path(args[args.index('--out') + 1]).write_bytes(b'signed fixture')
            return ''
        with patch.object(build, 'run', side_effect=fake_sign), \
             patch.object(build, 'inspect_apk', side_effect=lambda path, tools: next(p for p, f in self.inputs.items() if f == path)), \
             patch.object(build, 'verify_family', return_value={'apks': []}):
            build.sign_family(self.inputs, self.root / 'out', self.root, key, password)
        self.assertEqual(before, {p: path.read_bytes() for p, path in self.inputs.items()})
        self.assertTrue((self.root / 'out' / 'PAIRING.json').is_file())
        self.assertFalse(any('not-on-command-line' in arg for command in self.commands for arg in command))
        self.assertEqual(sum('sign' in command for command in self.commands), len(build.FAMILY))
        self.assertEqual(sum('-f' in command for command in self.commands), len(build.FAMILY))

    def test_failed_verification_does_not_publish(self):
        key = self.root / 'key'; key.touch()
        password = self.root / 'password'; password.touch()
        with patch.object(build, 'run', return_value=''), \
             patch.object(build, 'inspect_apk', side_effect=lambda path, tools: next(p for p, f in self.inputs.items() if f == path)), \
             patch.object(build, 'verify_family', side_effect=build.BuildError('bad signer')), \
             self.assertRaises(build.BuildError):
            build.sign_family(self.inputs, self.root / 'out', self.root, key, password)
        self.assertFalse((self.root / 'out' / 'PAIRING.json').exists())
        self.assertFalse(list((self.root / 'out').glob('*.apk')))


class BuildTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name)
        self.args = argparse.Namespace(source=root / 'source', sdk=root / 'sdk',
                                      build_dir=root / 'build', jobs=2, min_api=29,
                                      ndk='26.1.10909125', ndk_path=None)
        self.args.source.mkdir()
        for name in ('autogen.sh', 'configure'):
            (self.args.source / name).touch()
        jar = self.args.sdk / 'platforms/android-34/android.jar'
        jar.parent.mkdir(parents=True); jar.touch()
        cc = self.args.sdk / 'ndk/26.1.10909125/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android29-clang'
        cc.parent.mkdir(parents=True); cc.touch(); cc.chmod(0o700)
        self.tools = self.args.sdk / 'build-tools/34.0.0'
        self.commands = []

    def fake_build(self, argv, **kwargs):
        self.commands.append([str(arg) for arg in argv])
        if argv[0] == 'make':
            java = self.args.build_dir / 'java'
            java.mkdir()
            (java / 'emacs-test.apk').touch()
        return ''

    def test_cross_build_uses_shared_uid_exact_headers_and_bounded_jobs(self):
        with patch.object(build.platform, 'system', return_value='Linux'), \
             patch.object(build.platform, 'machine', return_value='x86_64'), \
             patch.object(build, 'run', side_effect=self.fake_build), \
             patch.object(build, 'inspect_apk', return_value='org.gnu.emacs'):
            apk = build.build_emacs(self.args, self.tools)
        self.assertEqual(apk.name, 'emacs-test.apk')
        self.assertIn('--with-shared-user-id=com.termux', self.commands[0])
        self.assertIn('--without-android-debug', self.commands[0])
        self.assertTrue(any('android-34/android.jar' in arg for arg in self.commands[0]))
        self.assertEqual(self.commands[1], ['make', '-j2', 'all'])

    def test_stale_tree_is_rejected_before_build(self):
        self.args.build_dir.mkdir()
        with patch.object(build.platform, 'system', return_value='Linux'), \
             patch.object(build.platform, 'machine', return_value='x86_64'), \
             patch.object(build, 'run') as run, self.assertRaises(build.BuildError):
            build.build_emacs(self.args, self.tools)
        run.assert_not_called()

    def test_invalid_parallelism_is_rejected(self):
        self.args.jobs = 0
        with patch.object(build.platform, 'system', return_value='Linux'), \
             patch.object(build.platform, 'machine', return_value='x86_64'), \
             self.assertRaises(build.BuildError):
            build.build_emacs(self.args, self.tools)

    def test_non_linux_host_is_rejected(self):
        with patch.object(build.platform, 'system', return_value='Darwin'), \
             self.assertRaises(build.BuildError):
            build.build_emacs(self.args, self.tools)


class WorkflowTests(unittest.TestCase):
    def test_release_workflow_covers_full_companion_family(self):
        workflow = (ROOT.parent / ".github/workflows/android-emacs-release.yml").read_text()
        repositories = {
            "TERMUX_APP_REF": "termux/termux-app",
            "TERMUX_API_REF": "termux/termux-api",
            "TERMUX_WIDGET_REF": "termux/termux-widget",
            "TERMUX_BOOT_REF": "termux/termux-boot",
            "TERMUX_FLOAT_REF": "termux/termux-float",
            "TERMUX_STYLING_REF": "termux/termux-styling",
            "TERMUX_TASKER_REF": "termux/termux-tasker",
        }
        for variable, repository in repositories.items():
            with self.subTest(repository=repository):
                self.assertIn(repository, workflow)
                self.assertIn(variable, workflow)

        for flag in (
            "--emacs-apk",
            "--termux-apk",
            "--api-apk",
            "--widget-apk",
            "--boot-apk",
            "--float-apk",
            "--styling-apk",
            "--tasker-apk",
        ):
            with self.subTest(flag=flag):
                self.assertIn(flag, workflow)

        for name in (
            "termux_app_sha",
            "termux_api_sha",
            "termux_widget_sha",
            "termux_boot_sha",
            "termux_float_sha",
            "termux_styling_sha",
            "termux_tasker_sha",
        ):
            with self.subTest(source_lock=name):
                self.assertIn(name, workflow)


class BootstrapTests(unittest.TestCase):
    def test_bootstrap_source_is_exact(self):
        org = (ROOT.parent / "bootstrap-termux.org").read_text()
        code = org.split("#+begin_src sh :tangle bootstrap-termux.sh\n", 1)[1].split("#+end_src", 1)[0]
        self.assertEqual(code, (ROOT.parent / "bootstrap-termux.sh").read_text())

    def test_bootstrap_installs_native_emacs_doctor(self):
        script = (ROOT.parent / "bootstrap-termux.sh").read_text()
        for package in (
            "org.gnu.emacs",
            "com.termux",
            "com.termux.api",
            "com.termux.widget",
            "com.termux.boot",
            "com.termux.window",
            "com.termux.styling",
            "com.termux.tasker",
        ):
            with self.subTest(package=package):
                self.assertIn(package, script)
        self.assertIn('TERMUX_HOME="/data/data/com.termux/files"', script)
        self.assertIn('NATIVE_HOME="/data/data/org.gnu.emacs/files"', script)
        self.assertIn("stat -c '%u'", script)
        self.assertIn('android-emacs-stow', script)
        self.assertIn('08-Emacs-Doctor', script)



class LiterateTests(unittest.TestCase):
    def test_build_source_is_exact(self):
        org = (ROOT / 'build.org').read_text()
        code = org.split('#+begin_src python :tangle build.py\n', 1)[1].split('#+end_src', 1)[0]
        self.assertEqual(code, (ROOT / 'build.py').read_text())

    def test_early_init_source_is_exact(self):
        org = (ROOT / 'early-init.org').read_text()
        code = org.split('#+begin_src emacs-lisp :tangle early-init.el\n', 1)[1].split('#+end_src', 1)[0]
        self.assertEqual(code, (ROOT / 'early-init.el').read_text())


if __name__ == '__main__':
    unittest.main()
