/**
 * Prolog-RLM frontend: drives the same trusted SWI bridge the Emacs gptel
 * tool uses (lisp/llm/prolog-rlm-bridge.pl in this dotfiles checkout).
 * The bridge reads one JSON request from stdin and writes one JSON
 * response; the inner RLM runtime (prolog/rlm.pl from the prolog-rlm
 * checkout) does bounded recursive decomposition with OpenRouter subcalls.
 */

import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { here, parseDriverJson, runProc, swiplBin, truncate } from "./prolog.ts";

export interface RlmResult {
	ok: boolean;
	kind: string;
	result?: unknown;
	error?: unknown;
	version?: string;
	ready?: boolean;
}

export function findBridge(): string | undefined {
	const candidates = [
		process.env.PI_RLM_BRIDGE,
		join(here, "..", "..", "..", "..", "lisp", "llm", "prolog-rlm-bridge.pl"),
		join(homedir(), "dotfiles", "lisp", "llm", "prolog-rlm-bridge.pl"),
	];
	for (const c of candidates) {
		if (c && existsSync(c)) return c;
	}
	return undefined;
}

export function findRoot(): string | undefined {
	const candidates = [
		process.env.PI_RLM_ROOT,
		join(homedir(), "Documents", "Projects", "prolog-rlm"),
		join(homedir(), "dev", "lost-rob0t", "prolog-rlm"),
	];
	for (const c of candidates) {
		if (c && existsSync(join(c, "prolog", "rlm.pl"))) return c;
	}
	return undefined;
}

function apiKey(): string | undefined {
	if (process.env.OPENROUTER_API_KEY) return process.env.OPENROUTER_API_KEY;
	for (const p of [
		join(homedir(), ".config", "openrouter", "api-key"),
		join(homedir(), ".config", "openrouter", "api_key"),
	]) {
		if (existsSync(p)) return readFileSync(p, "utf8").trim();
	}
	return undefined;
}

async function bridgeRequest(request: Record<string, unknown>, timeoutMs: number): Promise<RlmResult> {
	const bridge = findBridge();
	if (!bridge) throw new Error("prolog-rlm bridge not found; set PI_RLM_BRIDGE");
	const key = apiKey();
	if (!key) throw new Error("OPENROUTER_API_KEY (or ~/.config/openrouter/api-key) required for the RLM runtime");

	const res = await runProc(swiplBin(), ["-q", "-s", bridge], {
		input: JSON.stringify(request),
		env: { ...process.env, OPENROUTER_API_KEY: key },
		timeoutMs,
	});
	const parsed = parseDriverJson(res.stdout);
	if (!parsed) {
		throw new Error(
			`bridge produced no JSON (exit=${res.exitCode} timedOut=${res.timedOut}) stderr: ${truncate(res.stderr, 400)}`,
		);
	}
	return parsed as unknown as RlmResult;
}

export async function rlmStatus(): Promise<RlmResult> {
	const root = findRoot();
	if (!root) {
		return {
			ok: false,
			kind: "config_error",
			error: "prolog-rlm checkout not found; set PI_RLM_ROOT to a checkout containing prolog/rlm.pl",
		};
	}
	return bridgeRequest({ operation: "status", root }, 20_000);
}

export async function rlmCompletion(opts: {
	query: string;
	context: string;
	model?: string;
	budget?: Record<string, unknown>;
	timeoutMs?: number;
}): Promise<RlmResult> {
	const root = findRoot();
	if (!root) {
		throw new Error("prolog-rlm checkout not found; set PI_RLM_ROOT to a checkout containing prolog/rlm.pl");
	}
	return bridgeRequest(
		{
			operation: "completion",
			root,
			query: opts.query,
			context: opts.context,
			model: opts.model ?? process.env.PI_RLM_MODEL ?? "openrouter/auto",
			budget: opts.budget ?? {},
		},
		opts.timeoutMs ?? 120_000,
	);
}
