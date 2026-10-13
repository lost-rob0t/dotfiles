"""Check that the pinned Lisp runtime can read a sealed anonymous credential.

The value never has a filesystem pathname: /proc/self/fd only references the
inherited anonymous memory descriptor. This exercises the same probe-file and
UIOP reader used by the upstream Valkey client, without starting any service.
"""
import fcntl
import os
import subprocess


fd = os.memfd_create("starintel-test", os.MFD_ALLOW_SEALING)
try:
    os.write(fd, b"synthetic-memory-only-value")
    os.lseek(fd, 0, os.SEEK_SET)
    fcntl.fcntl(fd, fcntl.F_ADD_SEALS,
                fcntl.F_SEAL_WRITE | fcntl.F_SEAL_GROW | fcntl.F_SEAL_SHRINK | fcntl.F_SEAL_SEAL)
    assert os.readlink(f"/proc/self/fd/{fd}").startswith("/memfd:")
    environment = dict(os.environ, VALKEY_PASSWORD_FILE=f"/proc/self/fd/{fd}")
    subprocess.run([
        os.environ["sbcl"], "--non-interactive", "--no-userinit", "--no-sysinit",
        "--eval", "(require :asdf)",
        "--eval", '(let ((path (uiop:getenv "VALKEY_PASSWORD_FILE"))) '
                  '(assert (probe-file path)) '
                  '(assert (string= "synthetic-memory-only-value" (uiop:read-file-string path))))',
    ], env=environment, pass_fds=(fd,), check=True, timeout=30)
finally:
    os.close(fd)
print("PASS: pinned SBCL reads a sealed anonymous credential without a filesystem secret")
