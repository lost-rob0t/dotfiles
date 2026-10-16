"""Exercise the production environment-to-memory adapter with the pinned Lisp."""
import os
import subprocess
import sys


environment = dict(os.environ, VALKEY_PASSWORD="synthetic-memory-only-value")
subprocess.run([
    sys.executable, sys.argv[1], os.environ["sbcl"],
    "--non-interactive", "--no-userinit", "--no-sysinit",
    "--eval", "(require :asdf)",
    "--eval", '(let ((path (uiop:getenv "VALKEY_PASSWORD_FILE"))) '
              '(assert (not (uiop:getenv "VALKEY_PASSWORD"))) '
              '(assert (probe-file path)) '
              '(assert (string= "synthetic-memory-only-value" (uiop:read-file-string path))))',
], env=environment, check=True, timeout=30)
# Check the descriptor is anonymous, sealed, and survives exec independently of Lisp.
subprocess.run([
    sys.executable, sys.argv[1], sys.executable, "-c",
    'import os, fcntl; p=os.environ["VALKEY_PASSWORD_FILE"]; '
    'assert os.readlink(p).startswith("/memfd:"); '
    'fd=int(p.rsplit("/", 1)[1]); '
    'assert fcntl.fcntl(fd, fcntl.F_GET_SEALS) & fcntl.F_SEAL_WRITE; '
    'assert "VALKEY_PASSWORD" not in os.environ',
], env=environment, check=True, timeout=30)
print("PASS: production launcher supplies pinned SBCL a sealed anonymous credential")
