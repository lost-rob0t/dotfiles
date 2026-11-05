/**
 * Sub-agent spawning: headless pi children that load the same extension
 * (so they share the kb/agent/verify toolset), share the orchestrator's
 * blackboard through PI_RLM_OPS_KB, and report their final answer on
 * stdout, which arrives in the orchestrator as a background finish event.
 */

import type { BgManager } from "./bg.ts";

const CHILD_CONTRACT = `You are sub-agent {ID} working for an orchestrator pi session.
- Shared state: the kb_* tools read and write a shared Prolog blackboard all agents can see.
- Post partial results or questions with kb_post (to: "main"); read messages with kb_take("{ID}") or kb_query.
- kb_assert/kb_retract share durable facts (no messages).
- When your task is complete, put the FINAL answer in your last message (stdout): the orchestrator receives it verbatim as a finish event.

TASK:
`;

export interface AgentSpawnOpts {
	prompt: string;
	cwd?: string;
	model?: string;
	boardPath: string;
	label?: string;
	notify?: boolean;
	timeoutMs?: number;
}

export function spawnAgent(bg: BgManager, opts: AgentSpawnOpts): { id: string; agentId: string } {
	const depth = Number(process.env.PI_RLM_OPS_DEPTH ?? "0");
	if (depth >= 1) {
		throw new Error("nested sub-agents are disabled (depth cap 1): coordinate through the shared blackboard instead");
	}

	const agentId = `agent-${bg.list().filter((t) => t.kind === "agent").length + 1}`;
	const prompt = CHILD_CONTRACT.replace(/\{ID\}/g, agentId) + opts.prompt;

	const argv = [
		"pi",
		"--print",
		"--no-mcp",
		...(opts.model ? ["--model", opts.model] : []),
		prompt,
	];

	const task = bg.start({
		command: argv.join(" "),
		argv,
		cwd: opts.cwd,
		env: {
			PI_RLM_OPS_KB: opts.boardPath,
			PI_AGENT_ID: agentId,
			PI_RLM_OPS_DEPTH: "1",
		},
		kind: "agent",
		label: opts.label ?? agentId,
		notify: opts.notify ?? true,
		timeoutMs: opts.timeoutMs,
	});

	return { id: task.id, agentId };
}
