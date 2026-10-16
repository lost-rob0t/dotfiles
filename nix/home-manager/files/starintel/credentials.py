"""Configure the fixed set of local StarIntel credential files safely."""
import argparse
import getpass
import os
from pathlib import Path
import stat
import tempfile


CREDENTIALS = (
    "couchdb_password",
    "couchdb_secret",
    "erlang_cookie",
    "rabbitmq_password",
    "valkey_password",
    "auth_pepper",
    "auth_bootstrap_secret",
    "initial_password",
)


def private_directory(path):
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    details = path.lstat()
    if not stat.S_ISDIR(details.st_mode) or path.is_symlink():
        raise SystemExit(f"Refusing non-directory credential path: {path}")
    if details.st_uid != os.geteuid():
        raise SystemExit(f"Credential directory must be owned by this user: {path}")
    if details.st_mode & 0o077:
        path.chmod(0o700)


def credential_path(root, name):
    if name not in CREDENTIALS:
        raise SystemExit(f"Unknown StarIntel credential name: {name}")
    return root / "config" / "secrets" / name


def configure(root, name, from_stdin):
    destination = credential_path(root, name)
    private_directory(root)
    private_directory(root / "config")
    private_directory(destination.parent)
    if destination.exists() and not destination.is_file():
        raise SystemExit(f"Refusing non-file credential path: {destination}")
    if from_stdin:
        value = os.read(0, 1024 * 1024)
        if os.read(0, 1):
            raise SystemExit("Credential input is too large")
    else:
        first = getpass.getpass(f"New {name}: ").encode()
        second = getpass.getpass(f"Confirm {name}: ").encode()
        if first != second:
            raise SystemExit("Credential entries do not match")
        value = first
    if not value:
        raise SystemExit("Credential must be nonempty")
    descriptor, temporary = tempfile.mkstemp(prefix=f".{name}.", dir=destination.parent)
    try:
        os.fchmod(descriptor, 0o600)
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(value)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, destination)
        directory = os.open(destination.parent, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    except BaseException:
        try:
            os.close(descriptor)
        except OSError:
            pass
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise
    print(f"Configured {name} in the private StarIntel credential directory")


def list_credentials(root):
    for name in CREDENTIALS:
        path = credential_path(root, name)
        status = "missing"
        if path.is_file() and not path.is_symlink():
            details = path.stat()
            status = "configured" if details.st_size and not details.st_mode & 0o077 else "insecure"
        elif path.exists() or path.is_symlink():
            status = "invalid"
        print(f"{name}\t{status}")


def main():
    parser = argparse.ArgumentParser(description="Configure local StarIntel credentials")
    parser.add_argument("root", type=Path, help=argparse.SUPPRESS)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("list", help="list credential status without values")
    setter = commands.add_parser("set", help="configure one credential")
    setter.add_argument("name", choices=CREDENTIALS)
    setter.add_argument("--stdin", action="store_true", help="read the value from standard input")
    arguments = parser.parse_args()
    if arguments.command == "list":
        list_credentials(arguments.root)
    else:
        configure(arguments.root, arguments.name, arguments.stdin)


if __name__ == "__main__":
    main()
