"""Generated from scripts/opencode-tmux.org.  Python 3.10+, standard library only."""
from __future__ import annotations

import argparse
import base64
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
from typing import BinaryIO

MAX_INPUT = 8 * 1024 * 1024
MAX_ROWS = 10_000
NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]{0,63}\Z")
SYMBOL = re.compile(r"[A-Za-z_][A-Za-z0-9_+*/<>=!?$%-]{0,127}\Z")
SESSION_ID = re.compile(r"\$[0-9]+\Z")
PREFIX = "opencode-"
MARKER = "DOTFILES_OPENCODE_SESSION"
LIBRARY = Path(__file__).resolve().with_name("ezf.el")


class Failure(Exception):
    def __init__(self, message: str, code: int = 1):
        super().__init__(message)
        self.code = code


def executable(name: str) -> str:
    path = shutil.which(name)
    if path is None:
        raise Failure(f"required command not found: {name}", 127)
    return path


def run(argv: list[str], *, data: bytes | None = None,
        env: dict[str, str] | None = None, timeout: int = 10) -> subprocess.CompletedProcess:
    try:
        return subprocess.run(argv, input=data, capture_output=True, env=env, timeout=timeout)
    except subprocess.TimeoutExpired as error:
        raise Failure(f"{Path(argv[0]).name} timed out", 124) from error
    except OSError as error:
        raise Failure(f"could not run {Path(argv[0]).name}: {error}", 69) from error


def checked(result: subprocess.CompletedProcess, operation: str) -> None:
    if result.returncode != 0:
        message = result.stderr.decode("utf-8", errors="replace").strip()
        raise Failure(f"{operation}: {message[:2048] or 'command failed'}")


def tmux(*args: str) -> subprocess.CompletedProcess:
    return run([executable("tmux"), *args])


def session_name(name: str) -> str:
    if NAME.fullmatch(name) is None:
        raise Failure("name must be 1-64 letters/digits/_/-, starting with a letter or digit", 2)
    return PREFIX + name


def parse_sessions(text: str) -> dict[str, str]:
    result = {}
    for row in text.splitlines():
        parts = row.split("\t")
        if len(parts) != 3:
            continue
        ident, name, marker = parts
        if (SESSION_ID.fullmatch(ident) and name.startswith(PREFIX)
                and NAME.fullmatch(name[len(PREFIX):]) and marker == "1"):
            result[name] = ident
    return result


def sessions() -> dict[str, str]:
    result = tmux("list-sessions", "-F", f"#{{session_id}}\t#{{session_name}}\t#{{{MARKER}}}")
    if result.returncode:
        message = result.stderr.decode("utf-8", errors="replace")
        absent = (message.startswith("no server running on ")
                  or (message.startswith("error connecting to ") and "(No such file or directory)" in message))
        if absent:
            return {}
        checked(result, "listing tmux sessions")
    return parse_sessions(result.stdout.decode("utf-8"))


def shell_command(program: str, directory: str, args: list[str]) -> str:
    # Force a POSIX sh below, not the user's potentially non-POSIX default shell.
    # One quoted command argument also prevents tmux interpreting a literal ';'.
    return "exec " + shlex.join([program, directory, *args])


def ensure_session(name: str, directory: Path, args: list[str]) -> str:
    target = session_name(name)
    existing = sessions().get(target)
    if existing is not None:
        print(f"Reusing {target}; its existing process is unchanged.", file=sys.stderr)
        return existing
    directory = directory.expanduser().resolve(strict=True)
    if not directory.is_dir():
        raise Failure(f"not a directory: {directory}", 2)
    command = shell_command(executable("opencode"), str(directory), args)
    # The /. suffix keeps a directory ending in ; out of tmux command syntax.
    result = tmux("new-session", "-d", "-P", "-F", "#{session_id}", "-s", target,
                  "-n", "opencode", "-c", str(directory).replace("#", "##") + "/.",
                  "-e", f"{MARKER}=1", executable("sh"), "-c", command)
    current = sessions().get(target)
    if result.returncode:
        # A concurrent creator may have won.  Never retry creation or adopt an
        # unmarked collision; only return the exact, marked winning session.
        if current is not None:
            return current
        checked(result, f"creating {target}")
    ident = result.stdout.decode("utf-8").strip()
    if not SESSION_ID.fullmatch(ident) or current != ident:
        raise Failure(f"{target} exited or changed during startup; inspect OpenCode's launch/configuration")
    return ident


def attach(ident: str) -> None:
    if not SESSION_ID.fullmatch(ident):
        raise Failure("invalid tmux session identity", 2)
    if os.environ.get("TMUX"):
        checked(tmux("switch-client", "-t", ident), "switching tmux client")
    else:
        binary = executable("tmux")
        os.execv(binary, [binary, "attach-session", "-t", ident])


def decode_selection(data: bytes, rows: list[str]) -> str:
    try:
        values = data.decode("utf-8").split("\0")
    except UnicodeError as error:
        raise Failure("picker returned invalid UTF-8", 70) from error
    if len(values) != 2 or values[-1] != "" or values[0] not in rows:
        raise Failure("picker did not return exactly one original record", 70)
    return values[0]


def fzf_select(rows: list[str]) -> str:
    env = os.environ.copy()
    env.update(FZF_DEFAULT_OPTS="", FZF_DEFAULT_OPTS_FILE="", FZF_DEFAULT_COMMAND="")
    # fzf owns the interaction and can remain open until the user chooses.
    result = subprocess.run([executable("fzf"), "--read0", "--print0", "--no-multi",
                             "--exit-0", "--height=40%", "--layout=reverse", "--prompt=OpenCode> "],
                            input=("\0".join(rows) + "\0").encode("utf-8"),
                            capture_output=True, env=env)
    if result.returncode in (1, 130) or (result.returncode == 0 and not result.stdout):
        raise Failure("", 130)
    checked(result, "fzf")
    return decode_selection(result.stdout, rows)


def lisp_data(value: str) -> str:
    encoded = base64.b64encode(value.encode("utf-8")).decode("ascii")
    return f'(decode-coding-string (base64-decode-string "{encoded}") \'utf-8)'


def read_candidates(stream: BinaryIO, read0: bool) -> list[str]:
    data = stream.read(MAX_INPUT + 1)
    if len(data) > MAX_INPUT:
        raise Failure("input exceeds 8 MiB", 2)
    try:
        text = data.decode("utf-8")
    except UnicodeError as error:
        raise Failure("input must be valid UTF-8", 2) from error
    if not read0 and "\0" in text:
        raise Failure("NUL in line input; use --read0", 2)
    rows = [row for row in text.split("\0" if read0 else "\n") if row != ""]
    if len(rows) > MAX_ROWS:
        raise Failure("input exceeds 10,000 candidates", 2)
    return rows


def fields(rows: list[str], field: int | None) -> list[str]:
    if field is None:
        return rows
    if not 0 <= field < 1024:
        raise Failure("field must be between 0 and 1023", 2)
    result = []
    for row in rows:
        parts = row.split()
        if field >= len(parts):
            raise Failure(f"selected candidate has no field {field}", 2)
        result.append(parts[field])
    return result


def validate_ezf_response(response: object, candidates: list[str], single: bool) -> list[str]:
    if not isinstance(response, dict):
        raise Failure("invalid EZF response", 70)
    status = response.get("status")
    if status != "ok":
        code = {"cancel": 130, "unavailable": 69, "timeout": 124, "error": 70}.get(status, 70)
        message = response.get("message", "EZF did not complete")
        raise Failure("" if code == 130 else str(message)[:2048], code)
    values = response.get("values")
    allowed = set(candidates)
    if (not isinstance(values, list) or len(values) > MAX_ROWS
            or (single and len(values) != 1)
            or any(not isinstance(value, str) or value not in allowed for value in values)):
        raise Failure("EZF returned values outside its candidate contract", 70)
    return values


def write_private(path: Path, data: str) -> None:
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
        stream.write(data)


def ezf_select(candidates: list[str], *, single: bool = False,
               candidate_fn: str = "ezf-default", timeout: int = 300) -> list[str]:
    if not candidates:
        return []
    if SYMBOL.fullmatch(candidate_fn) is None or (single and candidate_fn != "ezf-default"):
        raise Failure("invalid custom function, or -c combined with --single", 2)
    if not 1 <= timeout <= 3600:
        raise Failure("timeout must be between 1 and 3600 seconds", 2)
    if candidate_fn != "ezf-default" and any("\n" in value for value in candidates):
        raise Failure("legacy filename completers cannot represent embedded newlines", 2)
    try:
        client = executable("emacsclient")
    except Failure as error:
        raise Failure(str(error), 69) from error
    with tempfile.TemporaryDirectory(prefix="ezf-") as temporary:
        directory = Path(temporary)
        request, output, source = (directory / name for name in ("request.json", "result.json", "candidates"))
        write_private(source, "\n".join(candidates) + "\n")
        write_private(output, "")
        write_private(request, json.dumps({"candidates": candidates, "single": single,
                                          "function": candidate_fn, "timeout": timeout,
                                          "source": str(source), "output": str(output)}, ensure_ascii=False))
        expression = f"(progn (load {lisp_data(str(LIBRARY))} nil t) (dotfiles-ezf-main {lisp_data(str(request))}))"
        result = run([client, "--alternate-editor=false", "--eval", expression], timeout=timeout + 5)
        if result.returncode:
            message = result.stderr.decode("utf-8", errors="replace")
            unavailable = re.search(r"can't find socket|can't connect|cannot connect|could not connect|connection refused|no socket or alternate editor|no such file or directory", message, re.I)
            raise Failure(message.strip()[:2048] or "emacsclient failed", 69 if unavailable else 70)
        with output.open("rb") as stream:
            raw = stream.read(MAX_INPUT * 2 + 4097)
        if len(raw) > MAX_INPUT * 2 + 4096:
            raise Failure("EZF response is too large", 70)
        try:
            response = json.loads(raw)
        except (ValueError, UnicodeError) as error:
            raise Failure("EZF did not return a valid JSON result", 70) from error
        return validate_ezf_response(response, candidates, single)


def choose(rows: list[str], picker: str) -> str:
    if picker == "fzf" or (picker == "auto" and not os.environ.get("INSIDE_EMACS")):
        return fzf_select(rows)
    try:
        values = ezf_select(rows, single=True)
    except Failure as error:
        if picker == "auto" and error.code == 69:
            return fzf_select(rows)
        raise
    return decode_selection(("\0".join(values) + "\0").encode("utf-8"), rows)


def pick(picker: str) -> int:
    inventory = sessions()
    if not inventory:
        print("No OpenCode tmux sessions. Start one with: opencode-tmux NAME [DIR]", file=sys.stderr)
        return 0
    rows = [f"{name[len(PREFIX):]}\t{ident}" for name, ident in sorted(inventory.items())]
    try:
        selected = choose(rows, picker)
    except Failure as error:
        if error.code == 130:
            return 0
        raise
    if selected not in rows:
        raise Failure("selection was not in the original inventory", 70)
    name, ident = selected.split("\t")
    if sessions().get(PREFIX + name) != ident:
        raise Failure("selected session disappeared or changed; run opencode-pick again")
    attach(ident)
    return 0


def main(argv: list[str]) -> int:
    if not argv or argv[0] not in ("launch", "pick", "ezf"):
        raise Failure("internal command must be launch, pick, or ezf", 2)
    action, rest = argv[0], argv[1:]
    parser = argparse.ArgumentParser(prog={"launch": "opencode-tmux", "pick": "opencode-pick", "ezf": "ezf"}[action])
    if action == "launch":
        parser.add_argument("-d", "--detach", action="store_true", help="create/reuse without attaching")
        parser.add_argument("name")
        parser.add_argument("directory", nargs="?", default=".")
        extra = []
        if "--" in rest:
            separator = rest.index("--")
            rest, extra = rest[:separator], rest[separator + 1:]
        options = parser.parse_args(rest)
        session_name(options.name)
        if not options.detach and not os.environ.get("TMUX") and not sys.stdin.isatty():
            raise Failure("attaching needs a terminal; use -d for detached creation", 2)
        ident = ensure_session(options.name, Path(options.directory), extra)
        if options.detach:
            print(session_name(options.name))
        else:
            attach(ident)
        return 0
    if action == "pick":
        parser.add_argument("--picker", choices=("auto", "fzf", "ezf"), default=os.environ.get("OPENCODE_TMUX_PICKER", "auto"))
        options = parser.parse_args(rest)
        if options.picker not in ("auto", "fzf", "ezf"):
            parser.error("OPENCODE_TMUX_PICKER must be auto, fzf, or ezf")
        return pick(options.picker)
    parser.add_argument("--single", action="store_true")
    parser.add_argument("--read0", action="store_true")
    output_format = parser.add_mutually_exclusive_group()
    output_format.add_argument("--print0", action="store_true")
    output_format.add_argument("--shell-quote", action="store_true")
    parser.add_argument("-f", "--field", type=int)
    parser.add_argument("-c", "--candidate-fn", default="ezf-default")
    parser.add_argument("--timeout", type=int, default=300)
    options = parser.parse_args(rest)
    if options.field is not None and not 0 <= options.field < 1024:
        parser.error("field must be between 0 and 1023")
    if not SYMBOL.fullmatch(options.candidate_fn) or (options.single and options.candidate_fn != "ezf-default"):
        parser.error("invalid custom function, or -c combined with --single")
    if not 1 <= options.timeout <= 3600:
        parser.error("timeout must be between 1 and 3600 seconds")
    candidates = read_candidates(sys.stdin.buffer, options.read0)
    values = fields(ezf_select(candidates, single=options.single, candidate_fn=options.candidate_fn,
                              timeout=options.timeout), options.field)
    if not values:
        return 1
    if not options.print0 and not options.shell_quote and any("\n" in value for value in values):
        raise Failure("selection contains a newline; use --print0", 2)
    if options.shell_quote:
        data = shlex.join(values) + "\n"
    else:
        separator = "\0" if options.print0 else "\n"
        data = separator.join(values) + separator
    sys.stdout.buffer.write(data.encode("utf-8"))
    sys.stdout.buffer.flush()
    return 0


def interrupted(signum: int, _frame: object) -> None:
    raise Failure("", 128 + signum)


if __name__ == "__main__":
    for termination_signal in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(termination_signal, interrupted)
    try:
        status = main(sys.argv[1:])
    except Failure as error:
        if str(error):
            print(f"{Path(sys.argv[0]).stem}: {error}", file=sys.stderr)
        status = error.code
    except KeyboardInterrupt:
        status = 130
    except BrokenPipeError:
        # Avoid an additional flush exception while the interpreter shuts down.
        os.dup2(os.open(os.devnull, os.O_WRONLY), sys.stdout.fileno())
        status = 141
    except (OSError, UnicodeError) as error:
        print(f"opencode-tmux: {error}", file=sys.stderr)
        status = 1
    sys.exit(status)
