#!/usr/bin/env python3
"""Build and smoke-test the actual StarIntel stack on a disposable Docker host.

Run: python3 tests/starintel-live.py --artifacts "$RUNNER_TEMP/starintel-live-logs"
Requires Nix, a local rootful Docker engine, free module ports and passwordless
sudo (unless root). The module owns startup; this runner only provisions NEW
empty state. It never reads ~/.starintel or starts an actor. Fetch/build errors,
including unavailable Forgejo inputs, fail the check instead of being skipped.
"""

import argparse
import base64
import hashlib
import json
import os
from pathlib import Path
import re
import secrets
import shlex
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
import urllib.parse


PORTS = (5000, 5672, 5984, 6000, 6001, 6379, 8080, 8081, 15672)
INFRA = ("couchdb", "clouseau", "rabbitmq", "valkey")
SECRET_NAMES = (
    "couchdb_password", "couchdb_secret", "erlang_cookie", "rabbitmq_password",
    "valkey_password", "auth_pepper", "auth_bootstrap_secret", "initial_password",
)


def one_command(service, field):
    value = service[field]
    commands = value if isinstance(value, list) else [value]
    if len(commands) != 1:
        raise RuntimeError(f"Expected one {field} command, got {commands!r}")
    return shlex.split(commands[0])


def check_ports_free():
    # Never kill a process or stop another stack to make room for this test.
    for port in PORTS:
        with socket.socket() as sock:
            try:
                sock.bind(("127.0.0.1", port))
            except OSError as exc:
                raise RuntimeError(f"Port {port} is occupied; refusing to touch its owner") from exc


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, message, headers, new_url):
        return None


class Smoke:
    def __init__(self, artifacts):
        self.artifacts = artifacts
        self.root = Path(tempfile.mkdtemp(prefix="starintel-live-", dir="/tmp"))
        self.identity = (self.root.stat().st_dev, self.root.stat().st_ino)
        self.marker = secrets.token_hex(24)
        (self.root / ".disposable-smoke").write_text(self.marker)
        self.log_dir = self.root / "logs"
        self.log_dir.mkdir()
        self.values = {name: secrets.token_hex(24) for name in SECRET_NAMES}
        self.processes = []
        self.services = {}
        self.infrastructure_attempted = False
        self.project = "starintel-" + hashlib.sha256(str(self.root).encode()).hexdigest()[:16]
        self.environment = {
            "HOME": str(self.root / "home"), "PATH": os.defpath,
            "USER": "starintel-ci", "LOGNAME": "starintel-ci", "LANG": "C.UTF-8",
            "XDG_CONFIG_HOME": str(self.root / "config"),
        }
        self.docker = shutil.which("docker")
        self.sudo = [] if os.geteuid() == 0 else ["sudo", "-n"]
        self.report = {"status": "failed", "stage": "preflight", "checks": []}
        self.http = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())

    def record(self, message):
        print(message, flush=True)
        self.report["checks"].append(message)

    def run(self, command, log, *, env=None, timeout=240):
        with (self.log_dir / f"{log}.log").open("ab") as output:
            process = subprocess.Popen(command, cwd=self.root, env=env, stdin=subprocess.DEVNULL,
                                       stdout=output, stderr=subprocess.STDOUT, start_new_session=True)
            try:
                status = process.wait(timeout=timeout)
            except BaseException:
                self.stop_process(process)
                raise
            if status:
                raise subprocess.CalledProcessError(status, command)

    def preflight(self):
        for tool in ("nix", "docker"):
            if not shutil.which(tool):
                raise RuntimeError(f"Required executable is unavailable: {tool}")
        # Do not send bind mounts or teardown commands to an unexpected daemon.
        endpoint = os.environ.get("DOCKER_HOST")
        if not endpoint or os.environ.get("DOCKER_CONTEXT"):
            endpoint = subprocess.check_output(
                [self.docker, "context", "inspect", "--format", "{{.Endpoints.docker.Host}}"],
                text=True, timeout=20).strip()
        if endpoint != "unix:///var/run/docker.sock":
            raise RuntimeError(f"Smoke requires local /var/run/docker.sock; selected endpoint is {endpoint!r}")
        security = subprocess.check_output(
            [self.docker, "info", "--format", "{{json .SecurityOptions}}"],
            env=self.environment, text=True, timeout=20)
        if "rootless" in security or "userns" in security:
            raise RuntimeError("Smoke requires rootful Docker without user namespace remapping")
        self.run(self.sudo + ["true"], "preflight", timeout=20)
        check_ports_free()
        self.record("Preflight: local Docker and all nine module ports are available")

    def build(self):
        self.report["stage"] = "pinned-package-build"
        expression = Path(__file__).resolve().with_suffix(".nix")
        print("Building pinned native applications and four image archives", flush=True)
        with (self.log_dir / "nix-build.log").open("wb") as output:
            result = subprocess.run(
                ["nix", "--extra-experimental-features", "nix-command flakes", "build",
                 "--impure", "--no-link", "--print-out-paths", "--print-build-logs",
                 "--show-trace", "--file", str(expression), "--argstr", "root", str(self.root)],
                stdout=subprocess.PIPE, stderr=output, text=True, check=True, timeout=7200)
        outputs = result.stdout.strip().splitlines()
        if len(outputs) != 1 or not outputs[0].startswith("/nix/store/"):
            raise RuntimeError(f"Unexpected Nix build output: {result.stdout!r}")
        manifest = json.loads((Path(outputs[0]) / "manifest.json").read_text())
        if manifest["root"] != str(self.root):
            raise RuntimeError("Built fixture does not target this disposable root")
        expected = {"starintel-prepare", "starintel-infrastructure", "starintel-server",
                    "starintel-quasar", "starintel-router"}
        if set(manifest["services"]) != expected:
            raise RuntimeError("Unexpected services or actors in smoke fixture")
        self.services = manifest["services"]
        self.authority = manifest["authority"]
        self.schema = manifest["schema"]
        self.python = manifest["python"]
        self.report["schema_authority"] = self.authority
        self.docker = manifest["docker"]
        self.report.update({key: manifest[key] for key in ("sources", "packages", "images")})
        script = Path(self.command("starintel-infrastructure")[0]).read_text()
        paths = set(re.findall(r'/nix/store/[^\s"\']+-starintel-compose\.json', script))
        if len(paths) != 1:
            raise RuntimeError("Cannot identify the real module's Compose configuration")
        compose = json.loads(Path(paths.pop()).read_text())
        if compose["name"] != self.project or set(compose["services"]) != set(INFRA):
            raise RuntimeError("Unexpected Compose project or services")
        for name, service in compose["services"].items():
            for mount in service["volumes"]:
                if mount["type"] != "bind" or mount["source"] != str(self.root / "data" / name):
                    raise RuntimeError("Compose mount escapes disposable infrastructure state")
        (self.log_dir / "compose.json").write_text(json.dumps(compose, indent=2) + "\n")
        self.record("Built the real pinned server, Quasar, router and all four infrastructure images")

    def command(self, name, field="ExecStart"):
        return one_command(self.services[name], field)

    def unit_environment(self, name):
        service = self.services[name]
        if service.get("EnvironmentFile"):
            raise RuntimeError("Smoke must not consume external environment files")
        environment = self.environment.copy()
        for assignment in service.get("Environment", []):
            key, value = assignment.split("=", 1)
            environment[key] = value
        return environment

    def provision(self):
        self.report["stage"] = "fresh-state-provisioning"
        (self.root / "home").mkdir()
        # Do not materialize credentials to work around file-only upstream APIs.
        # Fail before any startup if the actual evaluated units still need files.
        file_variables = [assignment.split("=", 1)[0]
                          for service in self.services.values()
                          for assignment in service.get("Environment", [])
                          if assignment.split("=", 1)[0].endswith("_FILE")
                          and any(word in assignment for word in ("PASSWORD", "PEPPER", "SECRET"))]
        if file_variables:
            raise RuntimeError("Runtime-environment-only secret transport is blocked by "
                               "file-only unit settings: " + ", ".join(sorted(file_variables)))
        self.environment.update({
            "COUCHDB_PASSWORD": self.values["couchdb_password"],
            "COUCHDB_SECRET": self.values["couchdb_secret"],
            "COUCHDB_ERLANG_COOKIE": self.values["erlang_cookie"],
            "RABBITMQ_PASSWORD": self.values["rabbitmq_password"],
            "VALKEY_PASSWORD": self.values["valkey_password"],
            "STAR_AUTH_PEPPER": self.values["auth_pepper"],
            "STAR_AUTH_BOOTSTRAP_SECRET": self.values["auth_bootstrap_secret"],
            "STAR_AUTH_INITIAL_PASSWORD": self.values["initial_password"],
        })
        for name in INFRA:
            directory = self.root / "data" / name
            directory.mkdir(parents=True)
            # Only new, empty directories are ever chowned. These two upstream
            # entrypoints drop privilege without repairing bind-mount ownership.
            if name in ("clouseau", "valkey"):
                if directory.is_symlink() or any(directory.iterdir()):
                    raise RuntimeError(f"Refusing to change nonempty state: {directory}")
                self.run(self.sudo + ["chown", "65532:65532", str(directory)], "provision")
                stat = directory.stat()
                if (stat.st_uid, stat.st_gid) != (65532, 65532):
                    raise RuntimeError(f"Incorrect fresh bind-mount ownership: {directory}")
        self.run(self.command("starintel-prepare"), "prepare", env=self.environment)
        self.record("Prepared runtime environment and fresh UID/GID 65532 state")

    def start_infrastructure(self):
        self.report["stage"] = "infrastructure-start"
        self.infrastructure_attempted = True
        self.run(self.command("starintel-infrastructure"), "infrastructure", env=self.environment,
                 timeout=600)
        ids = self.container_ids()
        if len(ids) != 4:
            raise RuntimeError(f"Expected four infrastructure containers, found {len(ids)}")
        state_lines = subprocess.check_output(
            [self.docker, "inspect", "--format", "{{json .State}}", *ids],
            env=self.environment, text=True, timeout=20).splitlines()
        states = [json.loads(line) for line in state_lines]
        if not all(state.get("Running") and state.get("Health", {}).get("Status") == "healthy"
                   for state in states):
            raise RuntimeError("Infrastructure is not uniformly running and healthy")
        self.record("CouchDB, Clouseau, RabbitMQ and Valkey are healthy")

    def check_processes(self):
        for name, process in self.processes:
            if process.poll() is not None:
                raise RuntimeError(f"{name} exited early with status {process.returncode}")

    def request(self, port, path, *, method="GET", body=None, auth=False, api_auth=False, bootstrap=False):
        headers = {"Content-Type": "application/json"}
        if auth:
            credential = base64.b64encode(f"starintel:{self.values['couchdb_password']}".encode()).decode()
            headers["Authorization"] = "Basic " + credential
        if api_auth:
            headers["Authorization"] = "Bearer " + self.values["api_key"]
        if bootstrap:
            headers["X-Star-Bootstrap-Secret"] = self.values["auth_bootstrap_secret"]
        request = urllib.request.Request(f"http://127.0.0.1:{port}{path}",
                                         data=None if body is None else json.dumps(body).encode(),
                                         headers=headers, method=method)
        with self.http.open(request, timeout=3) as response:
            return response.status, response.read(1024 * 1024)

    def await_http(self, port, path):
        deadline = time.monotonic() + 180
        while time.monotonic() < deadline:
            self.check_processes()
            try:
                status, body = self.request(port, path)
                if status == 200 and body:
                    return
            except (OSError, urllib.error.URLError):
                pass
            time.sleep(1)
        raise RuntimeError(f"Timed out waiting for HTTP 200 at 127.0.0.1:{port}{path}")

    def start_apps(self):
        self.report["stage"] = "native-application-start"
        for name in ("starintel-server", "starintel-quasar", "starintel-router"):
            with (self.log_dir / f"{name}.log").open("ab") as output:
                process = subprocess.Popen(self.command(name), cwd=self.root,
                                           env=self.unit_environment(name), stdin=subprocess.DEVNULL,
                                           stdout=output, stderr=subprocess.STDOUT, start_new_session=True)
            self.processes.append((name, process))
            if name == "starintel-server":
                self.run(self.command(name, "ExecStartPost"), "server-readiness",
                         env=self.unit_environment(name), timeout=150)
                self.await_http(5000, "/health")
            elif name == "starintel-quasar":
                self.await_http(8080, "/")
        for port in (6000, 6001, 8081):
            deadline = time.monotonic() + 60
            while True:
                self.check_processes()
                try:
                    with socket.create_connection(("127.0.0.1", port), timeout=1):
                        break
                except OSError:
                    if time.monotonic() >= deadline:
                        raise RuntimeError(f"Native service did not listen on port {port}")
                    time.sleep(1)
        self.check_processes()
        self.record("Native server health, Quasar HTTP/WebSocket and both router listeners passed")

    @staticmethod
    def stop_process(process):
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            process.wait(timeout=20)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait(timeout=10)

    def stop_apps(self):
        for _name, process in reversed(self.processes):
            self.stop_process(process)
        self.processes.clear()

    def container_ids(self):
        return subprocess.check_output(
            [self.docker, "ps", "--all", "--quiet", "--filter",
             f"label=com.docker.compose.project={self.project}"],
            env=self.environment, text=True, timeout=20).split()

    def stop_infrastructure(self):
        self.run(self.command("starintel-infrastructure", "ExecStopPost"), "teardown",
                 env=self.environment, timeout=150)
        if self.container_ids():
            raise RuntimeError("Disposable Compose project still has containers after teardown")
        self.infrastructure_attempted = False

    def validate_canonical(self, document):
        # The generated schema is a definitions library: select the Person
        # definition explicitly instead of accidentally accepting the empty root.
        program = ("import json,sys,jsonschema; "
                   "schema=json.load(open(sys.argv[1])); "
                   "schema['$ref']='#/$defs/Person'; "
                   "jsonschema.Draft202012Validator(schema).validate(json.load(sys.stdin))")
        subprocess.run([self.python, "-c", program, self.schema],
                       input=json.dumps(document), text=True, check=True, timeout=30,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    def persistence(self):
        self.report["stage"] = "canonical-ingestion"
        status, body = self.request(5000, "/auth/bootstrap", method="POST", bootstrap=True,
                                    body={"owner": "starintel-ci"})
        credential = json.loads(body).get("api_key")
        if status != 201 or not isinstance(credential, str) or not credential:
            raise RuntimeError("Initial administrator API bootstrap failed")
        # Keep the one-time bearer only in memory, and redact it from diagnostics.
        self.values["api_key"] = credential
        document = {"id": "ci:person:" + secrets.token_hex(12), "dataset": "starintel-ci",
                    "dtype": "person", "schemaVersion": self.authority["schema_version"],
                    "fname": "Synthetic CI", "lname": secrets.token_hex(12)}
        self.validate_canonical(document)
        invalid = dict(document, id=document["id"] + "-invalid", unexpectedCiField=True)
        try:
            self.validate_canonical(invalid)
        except subprocess.CalledProcessError:
            pass
        else:
            raise RuntimeError("Pinned schema unexpectedly accepts the invalid fixture")
        try:
            self.request(5000, "/api/v1/documents", method="POST", body=invalid, api_auth=True)
        except urllib.error.HTTPError as exc:
            if exc.code != 422:
                raise
        else:
            raise RuntimeError("Canonical ingress accepted an undeclared field")
        status, _ = self.request(5000, "/api/v1/documents", method="POST", body=document, api_auth=True)
        if status not in (200, 201, 202):
            raise RuntimeError("Canonical ingress did not accept the valid fixture")
        path = "/api/v1/documents/" + urllib.parse.quote(document["id"], safe="")

        def readback():
            deadline = time.monotonic() + 120
            while time.monotonic() < deadline:
                self.check_processes()
                try:
                    _, payload = self.request(5000, path, api_auth=True)
                    saved = json.loads(payload)
                    self.validate_canonical(saved)
                    if any(saved.get(key) != value for key, value in document.items()):
                        raise RuntimeError("Canonical readback changed fixture fields")
                    if any(key in saved for key in ("_id", "_rev", "tenant_id")):
                        raise RuntimeError("Canonical readback exposed storage-only fields")
                    return
                except urllib.error.HTTPError as exc:
                    if exc.code != 404:
                        raise
                time.sleep(1)
            raise RuntimeError("Canonical HTTP ingestion did not reach durable readback")

        readback()
        self.record("Canonical HTTP ingress rejected invalid data and persisted a schema-valid person through RabbitMQ")
        self.report["stage"] = "canonical-restart-persistence"
        self.stop_apps()
        self.stop_infrastructure()
        self.start_infrastructure()
        self.start_apps()
        readback()
        invalid_path = "/api/v1/documents/" + urllib.parse.quote(invalid["id"], safe="")
        try:
            self.request(5000, invalid_path, api_auth=True)
        except urllib.error.HTTPError as exc:
            if exc.code != 404:
                raise
        else:
            raise RuntimeError("Rejected canonical document was nevertheless persisted")
        self.record("Canonical document and API credential survived full application/infrastructure recreation; invalid document remains absent")

    def redact(self, text):
        for value in self.values.values():
            text = text.replace(value, "[ephemeral secret redacted]")
            text = text.replace(base64.b64encode(f"starintel:{value}".encode()).decode(), "[redacted]")
        return text

    def diagnostics(self):
        if self.infrastructure_attempted:
            for identifier in self.container_ids():
                self.run([self.docker, "logs", "--tail", "400", identifier],
                         f"container-{identifier}", env=self.environment, timeout=30)

    def remove_root(self):
        stat = self.root.lstat()
        if (self.root.is_symlink() or (stat.st_dev, stat.st_ino) != self.identity
                or not re.fullmatch(r"/tmp/starintel-live-[a-zA-Z0-9_-]+", str(self.root))
                or (self.root / ".disposable-smoke").read_text() != self.marker):
            raise RuntimeError("Disposable root identity changed; refusing cleanup")
        # Docker-created database files can be root-owned. Never prune images,
        # volumes, networks, unrelated containers, or any user's home directory.
        subprocess.run(self.sudo + ["rm", "-rf", "--", str(self.root)], check=True, timeout=60)

    def execute(self):
        ok = False
        try:
            self.preflight()
            self.build()
            check_ports_free()
            self.provision()
            self.start_infrastructure()
            self.start_apps()
            self.persistence()
            ok = True
        except (Exception, KeyboardInterrupt) as exc:
            self.report["error"] = self.redact(f"{type(exc).__name__}: {exc}")
            print(f"FAILED at {self.report['stage']}: {self.report['error']}", file=sys.stderr)
            if self.report["stage"] == "pinned-package-build":
                build_log = self.log_dir / "nix-build.log"
                if build_log.exists():
                    print(self.redact("\n".join(build_log.read_text(errors="replace").splitlines()[-60:])),
                          file=sys.stderr)
        finally:
            cleanup_errors = []
            for action in (self.diagnostics, self.stop_apps):
                try:
                    action()
                except Exception as exc:
                    cleanup_errors.append(str(exc))
            if self.infrastructure_attempted:
                try:
                    self.stop_infrastructure()
                except Exception as exc:
                    cleanup_errors.append(str(exc))
            for log in self.log_dir.iterdir():
                if log.is_file():
                    (self.artifacts / log.name).write_text(self.redact(log.read_text(errors="replace")))
            if not cleanup_errors:
                try:
                    self.remove_root()
                except Exception as exc:
                    cleanup_errors.append(str(exc))
            if cleanup_errors:
                self.report["cleanup_errors"] = cleanup_errors
                self.report["retained_disposable_root"] = str(self.root)
                ok = False
            self.report["status"] = "passed" if ok else "failed"
            (self.artifacts / "summary.json").write_text(json.dumps(self.report, indent=2) + "\n")
        print(f"StarIntel live smoke: {self.report['status']}; evidence: {self.artifacts}", flush=True)
        return 0 if ok else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifacts", type=Path,
                        help="New directory for sanitized logs and summary.json")
    args = parser.parse_args()
    os.umask(0o077)
    def interrupted(_signum, _frame):
        raise KeyboardInterrupt("Smoke runner was interrupted")
    signal.signal(signal.SIGTERM, interrupted)
    if args.artifacts is None:
        artifacts = Path(tempfile.mkdtemp(prefix="starintel-smoke-evidence-", dir="/tmp"))
    else:
        artifacts = args.artifacts.resolve()
        artifacts.mkdir(parents=True, exist_ok=False)
    return Smoke(artifacts).execute()


if __name__ == "__main__":
    sys.exit(main())
