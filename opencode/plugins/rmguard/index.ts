// rmguard — vetoes destructive shell commands that permission wildcards miss
// (flags in any argument order, xargs pipelines, find -exec rm).
//
// Born from the 2026-11-13 incident: an agent ran `rm -rf ~/starintel`
// without listing it. Layers: opencode permissions/policies (dotfiles
// opencode.nix) do the coarse gating; this hook is the semantic backstop;
// llm-log tags the attempt; prolog-rlm prolog/rlm_shell_guard.pl is the
// shared classifier spec. Keep the four in sync.
//
// Deliberately import-free and duck-typed so it loads without package
// resolution in any plugin runtime.

const DENY = [
  /\brm\b[^;|&]*\s-{1,2}[a-z]*[rfR]/,
  /\brm\b[^;|&]*\s--recursive\b/,
  /\bsudo\b[^;|&]*\brm\b/,
  /\bfind\b[^;|&]*\s-delete\b/,
  /\bfind\b[^;|&]*\s-exec\s+rm\b/,
  /\bxargs\b[^;|&]*\brm\b/,
  /\bdd\b[^;|&]*\bof=\/dev\/(?:sd|nvme|mapper\/)/,
  /\bmkfs(?:\.\w+)?\b/,
  /\bshred\b/,
]

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
      if (!DENY.some((re) => re.test(command))) return
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
