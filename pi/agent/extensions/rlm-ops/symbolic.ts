/**
 * Symbolic mode: machine-recorded observations, agent-declared
 * verification requirements and rules, and the fail-closed gate that
 * evaluates every requirement against recorded observations only.
 *
 * Trust model (mirrors the prolog-verify contract): observations are
 * appended exclusively by this extension from real tool executions; the
 * agent authors requirements and derivation rules, which are evaluated
 * by swipl but can never fabricate an observation.
 */

import { appendFileSync, existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { GATE_DRIVER, parseDriverJson, prologQuote, runProc, swiplBin } from "./prolog.ts";

export interface GateReport {
	ok: boolean;
	requirements: number;
	passed: number;
	observations: number;
	failed: Array<{ name: string; goal: string; outcome: string; reason?: string }>;
}

interface LoadAttempt {
	loaded: boolean;
	report?: GateReport;
	reason?: string;
}

export class SymbolicStore {
	public readonly obsPath: string;
	public readonly verifyPath: string;
	private obsSeq = 0;

	constructor(sessionHome: string) {
		mkdirSync(sessionHome, { recursive: true });
		this.obsPath = `${sessionHome}/observations.pl`;
		this.verifyPath = `${sessionHome}/verify.pl`;
		if (!existsSync(this.obsPath)) writeFileSync(this.obsPath, "");
		if (!existsSync(this.verifyPath)) writeFileSync(this.verifyPath, "");
	}

	public recordObservation(tool: string, summary: string, ok: boolean): void {
		this.obsSeq += 1;
		const fact =
			`obs(${this.obsSeq}, tool(${prologQuote(tool)}), summary(${prologQuote(summary)}), ok(${ok}), stamp(${Date.now()})).`;
		appendFileSync(this.obsPath, `${fact}\n`);
	}

	/** Declare or replace a named requirement. Goal text must parse as a term. */
	public setRequirement(name: string, goal: string): void {
		if (!/^[a-z][A-Za-z0-9_]*$/.test(name)) {
			throw new Error(`requirement name must be a lowercase Prolog atom, got: ${name}`);
		}
		const lines = readFileSync(this.verifyPath, "utf8").split("\n");
		const kept = lines.filter((l) => l.length > 0 && !l.startsWith(`requirement(${name},`));
		kept.push(`requirement(${name}, ${goal}).`);
		writeFileSync(this.verifyPath, `${kept.join("\n")}\n`);
	}

	/** Remove a named requirement; returns false when it did not exist. */
	public removeRequirement(name: string): boolean {
		const lines = readFileSync(this.verifyPath, "utf8").split("\n");
		const kept = lines.filter((l) => l.length > 0 && !l.startsWith(`requirement(${name},`));
		if (kept.length === lines.filter((l) => l.length > 0).length) return false;
		writeFileSync(this.verifyPath, `${kept.join("\n")}\n`);
		return true;
	}

	/**
	 * Append derivation clauses (facts or Head :- Body rules). Validated by
	 * loading them through the gate driver before accepting: directives,
	 * DCG rules, module terms and syntax errors are rejected fail-closed.
	 */
	public async addRules(clauses: string): Promise<void> {
		const probe = `${this.verifyPath}.probe`;
		writeFileSync(probe, `${readFileSync(this.verifyPath, "utf8")}${clauses}\n`);
		const attempt = await this.loadReport(this.obsPath, probe);
		unlinkSync(probe);
		if (!attempt.loaded) {
			throw new Error(
				`rules rejected by loader: ${attempt.reason ?? "gate driver did not load the clauses"}; keep to plain clauses (facts or Head :- Body), no directives`,
			);
		}
		writeFileSync(this.verifyPath, `${readFileSync(this.verifyPath, "utf8")}${clauses}\n`);
	}

	public async gate(): Promise<GateReport> {
		const attempt = await this.loadReport(this.obsPath, this.verifyPath);
		if (!attempt.loaded) {
			throw new Error(`gate failed closed: ${attempt.reason ?? "verify.pl does not load"}`);
		}
		return attempt.report!;
	}

	/** Loaded=false means the pair did not load at all (fail closed). */
	private async loadReport(obsPath: string, verifyPath: string): Promise<LoadAttempt> {
		const res = await runProc(swiplBin(), ["-q", GATE_DRIVER], {
			env: { ...process.env, GATE_ARGS: JSON.stringify([obsPath, verifyPath]) },
			timeoutMs: 60_000,
		});
		const parsed = parseDriverJson(res.stdout) as Record<string, unknown> | undefined;
		if (!parsed) return { loaded: false, reason: "gate driver produced no JSON (timeout or crash)" };
		if (parsed.kind === "gate_error") {
			return { loaded: false, reason: String(parsed.error) };
		}
		const report: GateReport = {
			ok: parsed.ok === true,
			requirements: Number(parsed.requirements),
			passed: Number(parsed.passed),
			observations: Number(parsed.observations),
			failed: (parsed.failed as GateReport["failed"]) ?? [],
		};
		return { loaded: true, report };
	}
}
