/**
 * Shared Prolog blackboard: the communication medium between the main
 * agent and its sub-agents. All operations are flock-guarded one-shot
 * swipl runs against the same board file, so any pi process that knows
 * the board path (orchestrator or child, passed via PI_RLM_OPS_KB)
 * shares one session state.
 */

import { KB_DRIVER, parseDriverJson, prologQuote, runProc, swiplBin, truncate } from "./prolog.ts";

export interface KbQueryResult {
	ok: boolean;
	count: number;
	solutions: string[];
	error?: string;
}

export class KbStore {
	public readonly boardPath: string;

	constructor(boardPath: string) {
		this.boardPath = boardPath;
	}

	private async op(args: string[]): Promise<Record<string, unknown>> {
		const res = await runProc(
			"flock",
			["-x", "-w", "30", `${this.boardPath}.lock`, swiplBin(), "-q", KB_DRIVER],
			{ env: { ...process.env, KB_ARGS: JSON.stringify(args) }, timeoutMs: 45_000 },
		);
		const parsed = parseDriverJson(res.stdout);
		if (!parsed) {
			throw new Error(
				`kb ${args[0]} produced no JSON (exit=${res.exitCode} timedOut=${res.timedOut}) stderr: ${truncate(res.stderr, 400)}`,
			);
		}
		if (parsed.ok !== true) {
			throw new Error(`kb ${args[0]} failed: ${String(parsed.error ?? "unknown error")}`);
		}
		return parsed;
	}

	public async post(from: string, to: string, kind: string, payload: string): Promise<number> {
		const res = await this.op(["post", this.boardPath, from, to, kind, payload]);
		return Number(res.seq);
	}

	public async take(who: string): Promise<string[]> {
		const res = await this.op(["take", this.boardPath, who]);
		return (res.messages as string[]) ?? [];
	}

	public async query(goal: string, maxSolutions = 20): Promise<KbQueryResult> {
		const res = await this.op(["query", this.boardPath, goal, String(Math.min(maxSolutions, 100))]);
		return {
			ok: true,
			count: Number(res.count),
			solutions: (res.solutions as string[]) ?? [],
		};
	}

	public async assert(term: string): Promise<void> {
		await this.op(["assert", this.boardPath, term]);
	}

	public async retract(term: string): Promise<number> {
		const res = await this.op(["retract", this.boardPath, term]);
		return Number(res.retracted);
	}

	public async dump(): Promise<string> {
		const res = await this.op(["dump", this.boardPath]);
		return String(res.text ?? "");
	}

	/** Machine-recorded observation fact text (single writer, no lock needed). */
	public static observationFact(n: number, tool: string, summary: string, ok: boolean): string {
		const s = summary.replace(/[\r\n\t]+/g, " ").slice(0, 240);
		return `obs(${n}, tool(${prologQuote(tool)}), summary(${prologQuote(s)}), ok(${ok}), stamp(${Date.now()})).`;
	}
}
