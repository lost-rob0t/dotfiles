#!/usr/bin/env python3
"""Build native Android Emacs and verify/sign its shared-UID Termux family.

Generated from android/build.org.  Edit the Org source, then tangle.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import subprocess
import tempfile

FAMILY = {
    "org.gnu.emacs": "emacs-android-starintel.apk",
    "com.termux": "termux-starintel-arm64.apk",
    "com.termux.api": "termux-api-starintel.apk",
    "com.termux.widget": "termux-widget-starintel.apk",
}


class BuildError(RuntimeError):
    """A precondition or Android verification gate failed."""


def run(argv: list, *, cwd: Path | None = None, timeout: int = 180,
        capture: bool = True) -> str:
    result = subprocess.run([str(arg) for arg in argv], cwd=cwd, check=True,
                            text=True, timeout=timeout,
                            stdout=subprocess.PIPE if capture else None)
    return result.stdout or ""


def require_file(path: Path) -> None:
    if not path.is_file():
        raise BuildError(f"Required file missing: {path}")


def parse_manifest(text: str) -> tuple[str, str]:
    # Only manifest attributes count, never similarly named child attributes.
    root = re.search(r"(?m)^\s*E: manifest\b[^\n]*\n", text)
    if root is None:
        raise BuildError("aapt did not return a manifest root")
    attrs = re.split(r"(?m)^\s*E:", text[root.end():], maxsplit=1)[0]
    if "sharedUserMaxSdkVersion" in attrs:
        raise BuildError("Conditional shared UID is not supported by this family")
    values = []
    for name in ("package", "android:sharedUserId"):
        matches = re.findall(r'\bA: ' + re.escape(name) + r'(?:\([^)]*\))?="([^"]+)"', attrs)
        if len(matches) != 1:
            raise BuildError(f"Expected exactly one manifest attribute: {name}")
        values.append(matches[0])
    return values[0], values[1]


def parse_certificate(text: str) -> str:
    matches = re.findall(r"(?m)^Signer #\d+ certificate SHA-256 digest: ([0-9a-fA-F]{64})\s*$", text)
    if len(matches) != 1:
        raise BuildError("Expected exactly one valid APK signer certificate")
    return matches[0].lower()


def inspect_apk(path: Path, tools: Path) -> str:
    require_file(path)
    package, uid = parse_manifest(run([tools / "aapt", "dump", "xmltree", path, "AndroidManifest.xml"]))
    if uid != "com.termux":
        raise BuildError(f"{path.name}: sharedUserId must be com.termux, got {uid}")
    return package


def verify_family(directory: Path, tools: Path) -> dict:
    rows = []
    certificates = set()
    for package, name in FAMILY.items():
        apk = directory / name
        if inspect_apk(apk, tools) != package:
            raise BuildError(f"{name}: unexpected package identity (expected {package})")
        run([tools / "zipalign", "-c", "-p", "4", apk])
        certificates.add(parse_certificate(run([tools / "apksigner", "verify", "--print-certs", apk])))
        with apk.open("rb") as stream:
            digest = hashlib.sha256()
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
        rows.append({"file": name, "package": package, "sha256": digest.hexdigest()})
    if len(certificates) != 1:
        raise BuildError("The APKs do not share one signing certificate")
    return {"shared_user_id": "com.termux", "certificate_sha256": certificates.pop(), "apks": rows}


def sign_family(inputs: dict[str, Path], output: Path, tools: Path,
                keystore: Path, password_file: Path) -> None:
    require_file(keystore)
    require_file(password_file)
    if set(inputs) != set(FAMILY):
        raise BuildError("Supply exactly Emacs, Termux, Termux:API, and Termux:Widget")
    for package, apk in inputs.items():
        if inspect_apk(apk, tools) != package:
            raise BuildError(f"Wrong input APK for {package}: {apk}")
    output.mkdir(parents=True, exist_ok=True)
    destinations = {(output / name).resolve() for name in FAMILY.values()}
    if any(path.resolve() in destinations for path in inputs.values()):
        raise BuildError("Input APKs must not be output APKs; originals are preserved")
    with tempfile.TemporaryDirectory(prefix=".sign-", dir=output) as work:
        stage = Path(work)
        for package, apk in inputs.items():
            aligned = stage / (FAMILY[package] + ".aligned")
            signed = stage / FAMILY[package]
            run([tools / "zipalign", "-f", "-p", "4", apk, aligned])
            run([tools / "apksigner", "sign", "--ks", keystore,
                 "--ks-pass", f"file:{password_file}", "--out", signed, aligned])
            aligned.unlink(missing_ok=True)
        report = verify_family(stage, tools)
        (stage / "PAIRING.json").write_text(json.dumps(report, indent=2) + "\n")
        for name in (*FAMILY.values(), "PAIRING.json"):
            os.replace(stage / name, output / name)


def single_apk(directory: Path) -> Path:
    apks = list(directory.glob("emacs-*.apk"))
    if len(apks) != 1 or not apks[0].is_file():
        raise BuildError(f"Expected one Emacs APK in {directory}, got {len(apks)}")
    return apks[0]


def build_emacs(args: argparse.Namespace, tools: Path) -> Path:
    if platform.system() != "Linux" or platform.machine() != "x86_64":
        raise BuildError("This arm64 cross-build entry point requires a Linux x86_64 host")
    if not 1 <= args.jobs <= 32 or not 21 <= args.min_api <= 34:
        raise BuildError("jobs must be 1..32 and minimum API must be 21..34")
    source = args.source.resolve()
    require_file(source / "autogen.sh")
    jar = args.sdk / "platforms" / "android-34" / "android.jar"
    cc = (args.sdk / "ndk" / args.ndk / "toolchains/llvm/prebuilt/linux-x86_64/bin" /
          f"aarch64-linux-android{args.min_api}-clang")
    require_file(jar)
    if not os.access(cc, os.X_OK):
        raise BuildError(f"NDK compiler is missing or not executable: {cc}")
    # Do not reuse a failed or differently configured build tree.
    build_dir = args.build_dir.resolve()
    if build_dir.exists():
        raise BuildError(f"Build directory exists; select a fresh --build-dir: {build_dir}")
    if not (source / "configure").is_file():
        run([source / "autogen.sh"], cwd=source, timeout=600, capture=False)
    build_dir.mkdir(parents=True)
    # GNU Emacs' Android port supports optional NDK-packaged libraries.  A bare
    # SDK/NDK build has no Android GnuTLS module, so do not make that optional
    # feature block creation of the baseline APK.  When --ndk-path contains the
    # upstream Android GnuTLS port, configure still detects and enables it.
    command = [source / "configure", f"--with-android={jar}",
               "--with-shared-user-id=com.termux", "--without-android-debug",
               "--with-gnutls=ifavailable",
               f"ANDROID_CC={cc}", f"SDK_BUILD_TOOLS={tools}"]
    if args.ndk_path:
        command.append(f"--with-ndk-path={args.ndk_path}")
    run(command, cwd=build_dir, timeout=900, capture=False)
    run(["make", f"-j{args.jobs}", "all"], cwd=build_dir, timeout=7200, capture=False)
    apk = single_apk(build_dir / "java")
    if inspect_apk(apk, tools) != "org.gnu.emacs":
        raise BuildError("Emacs build returned an unexpected package")
    return apk


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--sdk", type=Path, default=os.environ.get("ANDROID_HOME") or os.environ.get("ANDROID_SDK_ROOT"))
    common.add_argument("--build-tools", default="34.0.0")
    commands = parser.add_subparsers(dest="command", required=True)
    emacs = commands.add_parser("emacs", parents=[common], help="Cross-build GNU Emacs for arm64 Android")
    emacs.add_argument("--source", type=Path, required=True)
    emacs.add_argument("--build-dir", type=Path, default=Path("emacs-build"))
    emacs.add_argument("--ndk", default="26.1.10909125")
    emacs.add_argument("--min-api", type=int, default=29)
    emacs.add_argument("--jobs", type=int, default=2)
    emacs.add_argument("--ndk-path", help="Prepared Emacs Android dependency directories (upstream --with-ndk-path)")
    sign = commands.add_parser("sign", parents=[common], help="Sign the four APKs with one provided key")
    for flag in ("emacs-apk", "termux-apk", "api-apk", "widget-apk", "keystore", "password-file"):
        sign.add_argument(f"--{flag}", type=Path, required=True)
    sign.add_argument("--out", type=Path, default=Path("dist"))
    verify = commands.add_parser("verify", parents=[common], help="Verify an existing four-APK family")
    verify.add_argument("--out", type=Path, default=Path("dist"))
    args = parser.parse_args()
    try:
        if args.sdk is None:
            raise BuildError("Set ANDROID_HOME or provide --sdk")
        args.sdk = Path(args.sdk).resolve()
        tools = args.sdk / "build-tools" / args.build_tools
        for name in ("aapt", "apksigner", "zipalign"):
            if not os.access(tools / name, os.X_OK):
                raise BuildError(f"Missing executable SDK tool: {tools / name}")
        if args.command == "emacs":
            print(build_emacs(args, tools))
        elif args.command == "sign":
            inputs = dict(zip(FAMILY, (args.emacs_apk, args.termux_apk, args.api_apk, args.widget_apk)))
            sign_family(inputs, args.out, tools, args.keystore, args.password_file)
            print(f"Verified paired APKs: {args.out / 'PAIRING.json'}")
        else:
            print(json.dumps(verify_family(args.out, tools), indent=2))
    except (BuildError, OSError, subprocess.SubprocessError) as error:
        parser.exit(1, f"android-build: {error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
