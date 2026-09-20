import { tool } from "@opencode-ai/plugin"
import { spawn } from "node:child_process"

// No shell expansion, arbitrary executable, or sender identity supplied by the model.
export default tool({
  description: "Send a ZARA-CREW/1 peer message or propose a bounded role subagent. Admission is not completion.",
  args: {
    action: tool.schema.enum(["message", "spawn"]),
    text: tool.schema.string().min(1).max(8192),
    target: tool.schema.string().optional(),
    role: tool.schema.string().optional(),
    turns: tool.schema.number().int().min(1).max(100000).optional(),
    request_id: tool.schema.string().optional(),
  },
  async execute(args) {
    const script = process.env.CREW_CLIENT_SCRIPT
    if (!script) throw new Error("Crew host bridge unavailable")
    return await new Promise<string>((resolve, reject) => {
      const child = spawn("python3", [script, "--request"], { stdio: ["pipe", "pipe", "pipe"] })
      let output = ""
      let bytes = 0
      let done = false
      const finish = (error?: Error) => {
        if (done) return
        done = true
        clearTimeout(timer)
        if (error) { child.kill("SIGTERM"); reject(error) }
        else resolve(output)
      }
      const timer = setTimeout(() => finish(new Error("Crew delivery outcome unknown; do not blindly retry")), 17000)
      child.on("error", () => finish(new Error("Crew bridge failed to start")))
      child.stdin.on("error", () => finish(new Error("Crew input closed")))
      child.stdout.on("data", (chunk: Buffer) => {
        bytes += chunk.length
        if (bytes > 65536) finish(new Error("Crew receipt exceeds limit"))
        else output += chunk.toString("utf8")
      })
      child.stderr.on("data", (chunk: Buffer) => {
        bytes += chunk.length
        if (bytes > 65536) finish(new Error("Crew diagnostic exceeds limit"))
      })
      child.on("close", (code) => finish(code === 0 ? undefined : new Error("Crew request rejected")))
      child.stdin.end(JSON.stringify(args))
    })
  },
})
