/**
 * rlm-ops — background processes with finish events, sub-agents with a
 * shared Prolog blackboard, and an enforced symbolic verification mode.
 *
 * - bg_* tools run shell commands detached from the turn; completion is
 *   delivered back into the session as an event message.
 * - agent_spawn runs headless pi children that share this extension and
 *   the blackboard (PI_RLM_OPS_KB), giving multi-agent communication via
 *   Prolog facts and mailboxes (kb_* tools).
 * - /symbolic enables observation recording (obs/5 facts from real tool
 *   executions) plus a fail-closed gate at settle time: every declared
 *   requirement must be proven from recorded observations, and rlm_ask /
 *   rlm_status front the Prolog-RLM bridge for recursive large-context
 *   analysis.
 */

import { join } from "node:path";
import { Type } from "@earendil-works/pi-ai";
import { defineTool, type ExtensionAPI, type ExtensionContext } from "@earendil-works/pi-coding-agent";
import { BgManager, type BgTask } from "./bg.ts";
import { spawnAgent } from "./agents.ts";
import { KbStore } from "./kb.ts";
import { SymbolicStore, type GateReport } from "./symbolic.ts";
import { findBridge, findRoot, rlmCompletion, rlmStatus } from "./rlm.ts";
import { newSessionHome, sharedSessionHome, truncate, validateTerm } from "./prolog.ts";

const MAX_GATE_REJECTIONS = 4;

const SYMBOLIC_GUIDE = `Symbolic mode is active for this session. Contract:
- Every meaningful tool execution is recorded as an observation fact: obs(N, tool(Tool), summary(Text), ok(Bool), stamp(Ms)). Observations are machine-written; never fabricate them.
- Before the session may settle, every declared requirement requirement(Name, Goal) must be provable in Prolog from recorded observations plus your declared rules. The gate runs automatically and blocks settling on failure (max ${MAX_GATE_REJECTIONS} rejections, then it gives up and reports failure).
- Declare invariants with verify_require(name, goal_text). Add derivation clauses with verify_rules (facts or Head :- Body rules, no directives).
- Work the Prolog-RLM loop: query the symbolic state, form the smallest hypothesis, act and observe, record, re-query. Use kb_query/kb_assert for shared symbolic state, kb_post/kb_take for agent mailboxes, rlm_ask for bounded recursive analysis of large context.`;

const OBSERVED_TOOLS = new Set([
	"bash",
	"edit",
	"write",
	"bg_run",
	"bg_kill",
	"agent_spawn",
	"kb_post",
	"kb_assert",
	"kb_retract",
	"verify_require",
	"verify_drop",
	"verify_rules",
	"rlm_ask",
]);

function summarizeToolCall(toolName: string, args: Record<string, unknown> | undefined): string {
	const a = args ?? {};
	switch (toolName) {
		case "bash":
			return `bash ${String(a.command ?? "").slice(0, 160)}`;
		case "edit":
		case "write":
			return `${toolName} ${String(a.path ?? "")}`;
		case "agent_spawn":
			return `agent_spawn ${String(a.prompt ?? "").slice(0, 120)}`;
		case "bg_run":
			return `bg_run ${String(a.command ?? "").slice(0, 160)}`;
		case "bg_kill":
			return `bg_kill ${String(a.id ?? "")}`;
		case "kb_post":
			return `kb_post to=${String(a.to ?? "")} kind=${String(a.kind ?? "")} ${String(a.payload ?? "").slice(0, 100)}`;
		case "kb_assert":
		case "kb_retract":
			return `${toolName} ${String(a.term ?? "").slice(0, 140)}`;
		case "verify_require":
			return `verify_require ${String(a.name ?? "")}: ${String(a.goal ?? "").slice(0, 140)}`;
		case "verify_drop":
			return `verify_drop ${String(a.name ?? "")}`;
		case "verify_rules":
			return `verify_rules ${String(a.clauses ?? "").slice(0, 140)}`;
		case "rlm_ask":
			return `rlm_ask ${String(a.query ?? "").slice(0, 140)}`;
		default:
			return toolName;
	}
}

function fmtDuration(ms: number): string {
	const s = Math.round(ms / 100) / 10;
	return s >= 60 ? `${Math.round(s / 60)}m${Math.round(s % 60)}s` : `${s}s`;
}

export default function rlmOps(pi: ExtensionAPI): void {
	let sessionHome = sharedSessionHome();
	let boardPath = "";
	let kb!: KbStore;
	let symbolic!: SymbolicStore;
	let agentId = process.env.PI_AGENT_ID ?? "main";
	let symbolicOn = false;
	let gateFailStreak = 0;
	let closed = false;
	let ctxRef: ExtensionContext | undefined;

	const bg = new BgManager((task) => onTaskExit(task));

	function updateStatus(): void {
		if (!ctxRef) return;
		const running = bg.runningCount();
		if (!symbolicOn) {
			ctxRef.ui.setStatus("rlm-ops", running > 0 ? `bg ${running}` : undefined);
			return;
		}
		ctxRef.ui.setStatus("rlm-ops", `∑ symbolic · bg ${running}`);
	}

	function deliverEvent(text: string): void {
		if (closed) return;
		try {
			if (ctxRef?.isIdle()) {
				pi.sendUserMessage(text);
			} else {
				pi.sendUserMessage(text, { deliverAs: "followUp" });
			}
		} catch (err) {
			if (ctxRef) {
				ctxRef.ui.notify(`rlm-ops event delivery failed: ${String(err)}`, "warning");
			}
		}
	}

	function onTaskExit(task: BgTask): void {
		if (closed) return;
		if (symbolicOn) {
			symbolic.recordObservation(
				task.kind === "agent" ? "agent_exit" : "bg_exit",
				`${task.label} status=${task.status} exit=${task.exitCode ?? "signal"}`,
				task.status === "done",
			);
		}
		updateStatus();
		if (!task.notify) return;

		const duration = fmtDuration(task.endedAt && task.endedAt > task.startedAt ? task.endedAt - task.startedAt : 0);
		const tail = truncate(task.output.trimEnd(), 1200);
		const icon = task.status === "done" ? "✅" : task.status === "timeout" ? "⏱" : "❌";
		deliverEvent(
			`[event ${task.id}] ${icon} ${task.kind} "${task.label}" finished: ${task.status}` +
				`${task.exitCode !== null ? ` (exit ${task.exitCode})` : ""} after ${duration}\n` +
				`final output:\n${tail || "(no output)"}`,
		);
	}

	function gateReportText(report: GateReport): string {
		const lines = [
			`symbolic gate: ${report.ok ? "PASS" : "FAIL"} — ${report.passed}/${report.requirements} requirements proven, ${report.observations} observations recorded`,
		];
		for (const f of report.failed) {
			lines.push(`  ✗ ${f.name}: ${f.goal}${f.reason ? ` — ${f.reason}` : ""}`);
		}
		return lines.join("\n");
	}

	// ------------------------------------------------------------ lifecycle

	pi.on("session_start", (_event, ctx) => {
		ctxRef = ctx;
		sessionHome = process.env.PI_RLM_OPS_KB ? sharedSessionHome() : newSessionHome();
		boardPath = join(sessionHome, "shared.pl");
		kb = new KbStore(boardPath);
		symbolic = new SymbolicStore(sessionHome);
		agentId = process.env.PI_AGENT_ID ?? "main";
		closed = false;
		if (pi.getFlag("symbolic") === true) {
			symbolicOn = true;
		}
		updateStatus();
	});

	pi.on("session_shutdown", () => {
		closed = true;
		bg.killAll();
	});

	pi.on("before_agent_start", (event) => {
		if (event.messageType !== "user") return undefined;
		gateFailStreak = 0;
		if (!symbolicOn) return undefined;
		return {
			message: { customType: "rlm-ops-guide", content: SYMBOLIC_GUIDE, display: false },
		};
	});

	const pendingInputs = new Map<string, Record<string, unknown>>();

	pi.on("tool_call", (event) => {
		// Observe only: capture the call input for the observation recorder;
		// tool_execution_end no longer carries argument data.
		pendingInputs.set(event.toolCallId, (event.input ?? {}) as Record<string, unknown>);
		return undefined;
	});

	pi.on("tool_execution_end", (event) => {
		if (!symbolicOn || closed) return;
		if (!OBSERVED_TOOLS.has(event.toolName)) {
			pendingInputs.delete(event.toolCallId);
			return;
		}
		symbolic.recordObservation(
			event.toolName,
			summarizeToolCall(event.toolName, pendingInputs.get(event.toolCallId)),
			!event.isError,
		);
		pendingInputs.delete(event.toolCallId);
	});

	pi.on("agent_before_settle", async (event) => {
		if (!symbolicOn || closed) return undefined;
		if (event.outcome === "aborted") return undefined;

		let report: GateReport;
		try {
			report = await symbolic.gate();
		} catch (err) {
			report = {
				ok: false,
				requirements: 0,
				passed: 0,
				observations: 0,
				failed: [{ name: "gate", goal: "-", outcome: "fail", reason: String(err) }],
			};
		}

		if (report.ok) {
			gateFailStreak = 0;
			return undefined;
		}

		gateFailStreak += 1;
		const text = gateReportText(report);
		if (gateFailStreak > MAX_GATE_REJECTIONS) {
			ctxRef?.ui.notify(`symbolic gate still failing after ${MAX_GATE_REJECTIONS} rejections; settling with failures`, "warning");
			return undefined;
		}
		return {
			continue: true,
			entries: [
				{
					type: "custom_message" as const,
					customType: "rlm-ops-gate",
					content: `The symbolic verification gate rejected settling (${gateFailStreak}/${MAX_GATE_REJECTIONS}).\n${text}\nProve the remaining requirements from real observations (run the actual checks, then verify_require proven goals), or remove unprovable requirements.`,
					display: true,
					details: report,
				},
			],
		};
	});

	// -------------------------------------------------------------- commands

	pi.registerCommand("symbolic", {
		description: "Toggle enforced symbolic verification mode (on|off|status)",
		handler: async (args, ctx) => {
			const arg = args.trim().toLowerCase();
			if (arg === "on" || (arg === "" && !symbolicOn)) {
				symbolicOn = true;
				gateFailStreak = 0;
				updateStatus();
				ctx.ui.notify(
					"symbolic mode ON — observations recorded, settle gate enforced",
					"info",
				);
			} else if (arg === "off" || (arg === "" && symbolicOn)) {
				symbolicOn = false;
				updateStatus();
				ctx.ui.notify("symbolic mode OFF", "info");
			} else if (arg === "status") {
				try {
					ctx.ui.notify(symbolic ? gateReportText(await symbolic.gate()) : "no session", "info");
				} catch (err) {
					ctx.ui.notify(`gate failed closed: ${String(err)}`, "error");
				}
			} else {
				ctx.ui.notify("usage: /symbolic on|off|status", "warning");
			}
		},
	});

	pi.registerCommand("bg", {
		description: "List background tasks and sub-agents",
		handler: async (_args, ctx) => {
			const lines = bg.list().map(fmtTaskLine);
			ctx.ui.notify(lines.length ? lines.join("\n") : "no background tasks", "info");
		},
	});

	pi.registerFlag("symbolic", {
		description: "Start with enforced symbolic verification mode enabled",
		type: "boolean",
		default: false,
	});

	function fmtTaskLine(t: BgTask): string {
		const dur = t.endedAt ? fmtDuration(t.endedAt - t.startedAt) : "…";
		return `${t.id} [${t.kind}] ${t.status}${t.exitCode !== null ? ` exit=${t.exitCode}` : ""} (${dur}) ${t.label}`;
	}

	// ---------------------------------------------------------------- tools

	pi.registerTool(
		defineTool({
			name: "bg_run",
			label: "Background run",
			description:
				"Run a shell command in the background and return immediately. When the process finishes, a finish event with status, exit code and output tail is delivered back into the session. Use for long builds, tests, servers, watches. Use bg_wait to block on a task, bg_logs/bg_list/... to inspect.",
			parameters: Type.Object({
				command: Type.String({ description: "Shell command to run" }),
				cwd: Type.Optional(Type.String({ description: "Working directory (default: session cwd)" })),
				env: Type.Optional(Type.Record(Type.String(), Type.String({ description: "env value" }), { description: "Extra environment variables" })),
				label: Type.Optional(Type.String({ description: "Human label shown in events and lists" })),
				notify: Type.Optional(Type.Boolean({ description: "Deliver a finish event into the session (default true)" })),
				timeoutMs: Type.Optional(Type.Number({ description: "Kill the task after this many milliseconds" })),
			}),
			async execute(_id, params) {
				const task = bg.start({
					command: params.command,
					cwd: params.cwd,
					env: params.env,
					label: params.label,
					notify: params.notify,
					timeoutMs: params.timeoutMs ? Math.min(params.timeoutMs, 3_600_000) : undefined,
				});
				updateStatus();
				return {
					content: [{ type: "text", text: `started ${task.id} [process] "${task.label}"` }],
					details: { id: task.id, kind: "process" },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "bg_list",
			label: "Background list",
			description: "List background tasks and sub-agents with status, exit codes and durations.",
			parameters: Type.Object({}),
			async execute() {
				const lines = bg.list().map(fmtTaskLine);
				return {
					content: [{ type: "text", text: lines.length ? lines.join("\n") : "no background tasks" }],
					details: { count: lines.length },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "bg_logs",
			label: "Background logs",
			description: "Read the captured output of a background task (stdout+stderr interleaved).",
			parameters: Type.Object({
				id: Type.String({ description: "Task id from bg_run/agent_spawn" }),
				tailBytes: Type.Optional(Type.Number({ description: "Return the last N bytes (default 4096)" })),
			}),
			async execute(_id, params) {
				return {
					content: [{ type: "text", text: bg.logs(params.id, params.tailBytes ?? 4096) || "(no output)" }],
					details: { id: params.id },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "bg_kill",
			label: "Background kill",
			description: "Terminate a running background task (SIGTERM, then SIGKILL after 3s).",
			parameters: Type.Object({ id: Type.String({ description: "Task id" }) }),
			async execute(_id, params) {
				const ok = bg.kill(params.id);
				return {
					content: [{ type: "text", text: ok ? `killing ${params.id}` : `${params.id} is not running` }],
					details: { id: params.id, killed: ok },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "bg_wait",
			label: "Background wait",
			description: "Block until a background task finishes and return its final status and output tail.",
			parameters: Type.Object({
				id: Type.String({ description: "Task id" }),
				timeoutMs: Type.Optional(Type.Number({ description: "Give up waiting after this many milliseconds (default 60000)" })),
			}),
			async execute(_id, params, signal) {
				const task = await bg.wait(params.id, params.timeoutMs ?? 60_000).catch(async (err) => {
					if (signal?.aborted) throw new Error("bg_wait aborted");
					throw err;
				});
				return {
					content: [
						{
							type: "text",
							text: `${fmtTaskLine(task)}\n${truncate(task.output.trimEnd(), 3000) || "(no output)"}`,
						},
					],
					details: { id: task.id, status: task.status, exitCode: task.exitCode },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "agent_spawn",
			label: "Spawn sub-agent",
			description:
				"Spawn a headless pi sub-agent with the given task prompt. It runs in the background, shares this extension and the shared Prolog blackboard (kb_* tools), and its final answer is delivered as a finish event. Sub-agents cannot spawn their own sub-agents; coordinate through the blackboard.",
			parameters: Type.Object({
				prompt: Type.String({ description: "Task for the sub-agent (context, goal, expected output)" }),
				model: Type.Optional(Type.String({ description: "Model pattern for the child (default: session default)" })),
				cwd: Type.Optional(Type.String({ description: "Working directory (default: session cwd)" })),
				label: Type.Optional(Type.String({ description: "Human label" })),
				timeoutMs: Type.Optional(Type.Number({ description: "Kill the sub-agent after this many milliseconds" })),
				notify: Type.Optional(Type.Boolean({ description: "Deliver finish event (default true)" })),
			}),
			async execute(_id, params) {
				const { id, agentId: child } = spawnAgent(bg, {
					prompt: params.prompt,
					cwd: params.cwd,
					model: params.model,
					boardPath,
					label: params.label,
					notify: params.notify,
					timeoutMs: params.timeoutMs ? Math.min(params.timeoutMs, 3_600_000) : undefined,
				});
				updateStatus();
				return {
					content: [
						{
							type: "text",
							text: `spawned ${id} [agent] "${params.label ?? child}" — it shares the blackboard; post to it with kb_post(to:"${child}") and read replies with kb_take("${child}")`,
						},
					],
					details: { id, agentId: child },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "kb_post",
			label: "Blackboard post",
			description:
				'Post a message to a mailbox on the shared Prolog blackboard: msg(From, To, Kind, Payload, Seq, Stamp). Use to="main" for the orchestrator or a sub-agent id. The receiver drains messages with kb_take.',
			parameters: Type.Object({
				to: Type.String({ description: "Recipient id (e.g. main, agent-1) or all" }),
				kind: Type.String({ description: "Message kind tag (e.g. result, question, partial)" }),
				payload: Type.String({ description: "Message body; a Prolog term if it parses, else plain text" }),
			}),
			async execute(_id, params) {
				const seq = await kb.post(agentId, params.to, params.kind, params.payload);
				return {
					content: [{ type: "text", text: `posted seq=${seq} to ${params.to} (${params.kind})` }],
					details: { seq, to: params.to },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "kb_take",
			label: "Blackboard take",
			description:
				"Atomically drain (destructively remove) mailbox messages addressed to an id (default: this agent) from the shared blackboard. Returns the taken msg/6 terms in sequence order. kb_query first if the facts must remain visible afterwards.",
			parameters: Type.Object({
				who: Type.Optional(Type.String({ description: "Mailbox owner (default: this agent id)" })),
			}),
			async execute(_id, params) {
				const msgs = await kb.take(params.who ?? agentId);
				return {
					content: [
						{
							type: "text",
							text: msgs.length ? msgs.join("\n") : `(mailbox empty for ${params.who ?? agentId})`,
						},
					],
					details: { count: msgs.length },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "kb_query",
			label: "Blackboard query",
			description:
				"Run a read-only Prolog goal over the shared blackboard (mailbox msg/6 facts plus any shared facts). Example goal: msg(From, main, result, P, Seq, _)",
			parameters: Type.Object({
				goal: Type.String({ description: "Prolog goal text" }),
				maxSolutions: Type.Optional(Type.Number({ description: "Cap on reported solutions (default 20)" })),
			}),
			async execute(_id, params) {
				const res = await kb.query(params.goal, params.maxSolutions ?? 20);
				return {
					content: [
						{
							type: "text",
							text:
								res.count === 0
									? "no solutions"
									: truncate(res.solutions.join("\n"), 6000),
						},
					],
					details: res,
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "kb_assert",
			label: "Blackboard assert",
			description:
				"Assert a shared fact on the blackboard visible to all agents (plain facts only, no rules). Example: analysis_complete(agent-1, 42)",
			parameters: Type.Object({ term: Type.String({ description: "Prolog fact" }) }),
			async execute(_id, params) {
				await kb.assert(params.term);
				return {
					content: [{ type: "text", text: `asserted ${params.term}` }],
					details: { term: params.term },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "kb_retract",
			label: "Blackboard retract",
			description: "Remove all shared facts matching a term from the blackboard.",
			parameters: Type.Object({ term: Type.String({ description: "Prolog fact pattern" }) }),
			async execute(_id, params) {
				const n = await kb.retract(params.term);
				return {
					content: [{ type: "text", text: `retracted ${n} fact(s) matching ${params.term}` }],
					details: { count: n },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "verify_require",
			label: "Declare verification requirement",
			description:
				"Declare (or replace) a named verification requirement. In symbolic mode the settle gate requires the goal to be provable from recorded obs/5 facts plus your declared rules. Goals should reference observations, e.g. tests_green :- obs(_, tool(bash), summary(S), ok(true), _), sub_atom(S, _, _, _, pytest).",
			parameters: Type.Object({
				name: Type.String({ description: "Lowercase atom name for the requirement" }),
				goal: Type.String({ description: "Prolog goal text that must be proven" }),
			}),
			async execute(_id, params) {
				await validateTerm(params.goal);
				symbolic.setRequirement(params.name, params.goal);
				return {
					content: [{ type: "text", text: `requirement ${params.name} recorded: ${params.goal}` }],
					details: { name: params.name, goal: params.goal },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "verify_drop",
			label: "Drop verification requirement",
			description: "Remove a named requirement from the symbolic verification gate. Use for requirements that turned out to be wrong, not to dodge failures.",
			parameters: Type.Object({
				name: Type.String({ description: "Requirement name to remove" }),
			}),
			async execute(_id, params) {
				const removed = symbolic.removeRequirement(params.name);
				return {
					content: [{ type: "text", text: removed ? `requirement ${params.name} removed` : `no requirement named ${params.name}` }],
					details: { name: params.name, removed },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "verify_rules",
			label: "Add verification rules",
			description:
				"Append derivation clauses (facts or Head :- Body rules) used to prove requirements from observations. No directives allowed; invalid clauses are rejected fail-closed.",
			parameters: Type.Object({
				clauses: Type.String({ description: "Prolog clauses (one clause per line or dot-terminated)" }),
			}),
			async execute(_id, params) {
				await symbolic.addRules(params.clauses);
				return {
					content: [{ type: "text", text: "rules accepted" }],
					details: { bytes: params.clauses.length },
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "verify_status",
			label: "Verification status",
			description: "Run the symbolic verification gate now and report each requirement's status.",
			parameters: Type.Object({}),
			async execute() {
				const report = await symbolic.gate();
				return {
					content: [{ type: "text", text: gateReportText(report) }],
					details: report,
					isError: !report.ok,
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "rlm_ask",
			label: "Prolog-RLM query",
			description:
				"Run a bounded recursive analysis through the Prolog-RLM bridge: the inner runtime inspects/searches/slices the provided context with bounded OpenRouter subcalls without loading it into this conversation. Use for large-context analysis; prefer ordinary tools for small questions.",
			parameters: Type.Object({
				query: Type.String({ description: "Question for the RLM" }),
				context: Type.String({ description: "Context text (or a file path) to analyze" }),
				model: Type.Optional(Type.String({ description: "OpenRouter model for inner subcalls (default openrouter/auto or PI_RLM_MODEL)" })),
				timeoutMs: Type.Optional(Type.Number({ description: "Budget in milliseconds (default 120000)" })),
			}),
			async execute(_id, params, _signal, onUpdate) {
				onUpdate?.({ content: [{ type: "text", text: "rlm running (bounded recursive analysis)…" }] });
				const res = await rlmCompletion({
					query: params.query,
					context: params.context,
					model: params.model,
					timeoutMs: params.timeoutMs ? Math.min(params.timeoutMs, 300_000) : undefined,
				});
				return {
					content: [{ type: "text", text: truncate(JSON.stringify(res.result ?? res, null, 1), 8000) }],
					details: res,
					isError: !res.ok,
				};
			},
		}),
	);

	pi.registerTool(
		defineTool({
			name: "rlm_status",
			label: "Prolog-RLM status",
			description: "Report Prolog-RLM bridge/runtime readiness (root, bridge path, version).",
			parameters: Type.Object({}),
			async execute() {
				const res = await rlmStatus();
				const lines = [
					`bridge: ${findBridge() ?? "not found"}`,
					`root: ${findRoot() ?? "not found (set PI_RLM_ROOT)"}`,
					`status: ${JSON.stringify(res)}`,
				];
				return {
					content: [{ type: "text", text: lines.join("\n") }],
					details: res,
					isError: !res.ok,
				};
			},
		}),
	);
}
