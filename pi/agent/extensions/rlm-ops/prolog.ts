/**
 * Shared process and Prolog plumbing for the rlm-ops extension.
 *
 * Everything that touches swipl goes through runProc with a hard timeout
 * (call_with_time_limit breaks findall backtracking on the arm64-android
 * build, so timeouts are enforced by killing the process). Board and gate
 * drivers receive their arguments through environment variables as JSON,
 * never through positional argv: swipl consults trailing .pl arguments,
 * which would execute directives from untrusted files.
 */

import { spawn } from "node:child_process";
import { mkdirSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const here = dirname(fileURLToPath(import.meta.url));
export const KB_DRIVER = join(here, "kb_driver.pl");
export const GATE_DRIVER = join(here, "gate_driver.pl");

export interface ProcResult {
	ok: boolean;
	stdout: string;
	stderr: string;
	exitCode: number | null;
	timedOut: boolean;
}

export function runProc(
	command: string,
	args: string[],
	opts: {
		env?: Record<string, string>;
		cwd?: string;
		input?: string;
		timeoutMs?: number;
	} = {},
): Promise<ProcResult> {
	const timeoutMs = opts.timeoutMs ?? 30_000;
	return new Promise((resolve) => {
		const child = spawn(command, args, {
			env: opts.env,
			cwd: opts.cwd,
			stdio: ["pipe", "pipe", "pipe"],
		});
		let stdout = "";
		let stderr = "";
		let timedOut = false;
		let settled = false;

		const timer = setTimeout(() => {
			timedOut = true;
			child.kill("SIGKILL");
		}, timeoutMs);

		child.stdout?.on("data", (d: Buffer) => {
			stdout += d.toString();
		});
		child.stderr?.on("data", (d: Buffer) => {
			stderr += d.toString();
		});
		child.on("error", (err) => {
			if (settled) return;
			settled = true;
			clearTimeout(timer);
			resolve({ ok: false, stdout, stderr: `${stderr}${err.message}`, exitCode: null, timedOut });
		});
		child.on("close", (code) => {
			if (settled) return;
			settled = true;
			clearTimeout(timer);
			resolve({ ok: code === 0 && !timedOut, stdout, stderr, exitCode: code, timedOut });
		});

		if (opts.input !== undefined) {
			child.stdin?.write(opts.input);
		}
		child.stdin?.end();
	});
}

/** Parse the last JSON object line a driver printed. */
export function parseDriverJson(stdout: string): Record<string, unknown> | undefined {
	const lines = stdout.trim().split("\n").filter((l) => l.trim().startsWith("{"));
	const last = lines[lines.length - 1];
	if (!last) return undefined;
	try {
		return JSON.parse(last);
	} catch {
		return undefined;
	}
}

export function swiplBin(): string {
	return process.env.PI_SWIPL || "swipl";
}

/**
 * Session home for shared state. Sub-agents inherit the orchestrator's
 * session through PI_RLM_OPS_KB (the shared board path).
 */
export function sharedSessionHome(): string {
	const kb = process.env.PI_RLM_OPS_KB;
	if (kb) return dirname(kb);
	const root = process.env.PI_RLM_OPS_HOME || join(homedir(), ".cache", "pi-rlm-ops");
	return root;
}

export function newSessionHome(): string {
	const root = process.env.PI_RLM_OPS_HOME || join(homedir(), ".cache", "pi-rlm-ops");
	const id = `s-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
	const dir = join(root, id);
	mkdirSync(dir, { recursive: true });
	return dir;
}

/** Validate that a term/clause parses through the kb driver's parse op; throws otherwise. */
export async function validateTerm(term: string): Promise<void> {
	const res = await runProc(swiplBin(), ["-q", KB_DRIVER], {
		env: { ...process.env, KB_ARGS: JSON.stringify(["parse", term]) },
		timeoutMs: 15_000,
	});
	if (res.exitCode !== 0) {
		throw new Error(`invalid Prolog term: ${truncate(term, 200)}`);
	}
}

/** Escape text for use inside a single-quoted Prolog atom. */
export function prologQuote(text: string): string {
	return `'${text.replace(/\\/g, "\\\\").replace(/'/g, "\\'")}'`;
}

export function truncate(text: string, max: number): string {
	if (text.length <= max) return text;
	return `${text.slice(0, max)}\n... (truncated ${text.length - max} chars; full output in task logs)`;
}
