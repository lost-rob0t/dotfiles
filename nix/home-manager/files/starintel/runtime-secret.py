"""Bridge a runtime environment credential to the pinned Lisp file reader."""
import fcntl
import os
import sys


def main():
    password = os.environ.pop("VALKEY_PASSWORD", None)
    if password is not None:
        if not password:
            raise SystemExit("VALKEY_PASSWORD must be nonempty")
        descriptor = os.memfd_create("starintel-valkey", os.MFD_ALLOW_SEALING)
        with os.fdopen(os.dup(descriptor), "wb") as stream:
            stream.write(password.encode())
        os.lseek(descriptor, 0, os.SEEK_SET)
        fcntl.fcntl(descriptor, fcntl.F_ADD_SEALS,
                    fcntl.F_SEAL_WRITE | fcntl.F_SEAL_GROW |
                    fcntl.F_SEAL_SHRINK | fcntl.F_SEAL_SEAL)
        os.set_inheritable(descriptor, True)
        os.environ["VALKEY_PASSWORD_FILE"] = f"/proc/self/fd/{descriptor}"
    os.execv(sys.argv[1], sys.argv[1:])


if __name__ == "__main__":
    main()
