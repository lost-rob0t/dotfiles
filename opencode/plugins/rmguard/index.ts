// rmguard — vetoes destructive shell commands that permission wildcards miss
// (flags in any argument order, xargs pipelines, find -exec rm).
//
// Born from the 2026-11-13 incident: an agent ran `rm -rf ~/starintel`
// without listing it. Layers: opencode permissions/policies (dotfiles
// opencode-security.nix) do the coarse gating; this hook is the semantic
// backstop; llm-log tags the attempt; prolog-rlm prolog/rlm_shell_guard.pl
// is the shared classifier spec. Keep the four in sync.
//
// Classification is command-word based, not substring based: a command is
// destructive only when a destructive file utility (rm, find -delete,
// find -exec rm, dd of=/dev/*, mkfs, shred) is the actual command word of a
// shell segment. Non-file "delete" verbs such as `tea labels delete` or
// `gh label delete` must never trip the guard (2026-11-16 false positive).
//
// Deliberately import-free and duck-typed so it loads without package
// resolution in any plugin runtime.

const SEGMENT_SPLIT = /;+|\|\||\||&&|&+|\n+/
const TOKEN_SPLIT = /\s+/

// prefixes that wrap a real command; walk past them to find the command word
const WRAPPERS = new Set([
  "sudo",
  "nohup",
  "nice",
  "time",
  "env",
  "command",
  "exec",
  "xargs",
  "stdbuf",
  "watch",
])

const DESTRUCTIVE_HEADS = new Set(["rm", "shred"])

function stripPath(token) {
  return token.slice(token.lastIndexOf("/") + 1)
}

// resolve the effective command word of one shell segment, walking past
// wrappers like sudo/xargs and leading VAR=value assignments
function commandWord(segment) {
  const tokens = segment.split(TOKEN_SPLIT).filter(Boolean)
  let i = 0
  while (i < tokens.length) {
    const token = tokens[i]
    if (/^[A-Za-z_][A-Za-z0-9_]*=/.test(token)) {
      i += 1
      continue
    }
    const head = stripPath(token)
    if (WRAPPERS.has(head) || head === "\\") {
      i += 1
      continue
    }
    return { head, args: tokens.slice(i + 1) }
  }
  return { head: "", args: [] }
}

function rmIsRecursive(args) {
  return args.some((arg) => arg === "--recursive" || arg === "-R" || (arg.startsWith("-") && !arg.startsWith("--") && /[rfR]/.test(arg.slice(1))))
}

function findIsDestructive(args) {
  for (let i = 0; i < args.length; i += 1) {
    if (args[i] === "-delete" || args[i] === "-delete;") return true
    if (args[i] === "-exec" && args[i + 1] !== undefined && stripPath(args[i + 1]) === "rm") return true
  }
  return false
}

function ddTargetsRawDevice(args) {
  return args.some((arg) => /^of=\/dev\/(sd|nvme|mapper\/)/.test(arg))
}

function classifySegment(segment) {
  const { head, args } = commandWord(segment)
  if (DESTRUCTIVE_HEADS.has(head)) {
    if (head === "rm") return rmIsRecursive(args)
    return true // shred
  }
  if (head === "find") return findIsDestructive(args)
  if (head === "dd") return ddTargetsRawDevice(args)
  if (head.startsWith("mkfs")) return true
  return false
}

const HOME_TARGET = /(?:~|\$HOME|\/home\/\w+)\/[^\s"']*/

function hitsHomeRoot(command) {
  const m = command.match(HOME_TARGET)
  if (m === null) return false
  const t = m[0]
  return t.startsWith("~") || t.startsWith("$HOME") || /^\/home\/\w+/.test(t)
}

// strip quoted spans (commit messages, prose) before classification so
// describing a destructive command does not trip the guard
function unquoted(command) {
  return command.replace(/"(?:[^"\\]|\\.)*"/g, ' "" ').replace(/'(?:[^'\\]|\\.)*'/g, " '' ")
}

export default {
  id: "rmguard",
  async setup(ctx) {
    await ctx.tool.hook("execute.before", (event) => {
      if (event.tool !== "bash" && event.tool !== "shell") return
      const raw = String((event.input ?? {}).command ?? "")
      if (!raw) return
      const command = unquoted(raw)
      const destructive = command
        .split(SEGMENT_SPLIT)
        .map((segment) => segment.trim())
        .filter(Boolean)
        .some(classifySegment)
      if (!destructive) return
      if (hitsHomeRoot(command)) {
        throw new Error(
          "rmguard BLOCKED: recursive deletion aimed at a home root. Refusing; " +
            "the operator must run such deletions manually. Rule: never rm -rf " +
            "a directory you have not listed; never destroy outside your task's " +
            "own outputs. (incident 2026-11-13: rm -rf ~/starintel)",
        )
      }
      throw new Error(
        "rmguard HOLD: destructive command requires the operator. List the " +
          "target directory first and confirm before retrying. (incident " +
          "2026-11-13)",
      )
    })
  },
}
