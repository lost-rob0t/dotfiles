// Functional smoke test for rlm-ops TS modules (run with node, no model needed).
import { BgManager } from "../pi/agent/extensions/rlm-ops/bg.ts";
import { KbStore } from "../pi/agent/extensions/rlm-ops/kb.ts";
import { SymbolicStore } from "../pi/agent/extensions/rlm-ops/symbolic.ts";
import { validateTerm } from "../pi/agent/extensions/rlm-ops/prolog.ts";
import { mkdirSync, rmSync } from "node:fs";
import { join } from "node:path";

const home = join(process.env.TEMPDIR_OVERRIDE ?? process.env.HOME, ".cache", "rlm-ops-modtest-data");
rmSync(home, { recursive: true, force: true });
mkdirSync(home, { recursive: true });

let failures = 0;
function check(name, cond, extra = "") {
	if (cond) console.log(`ok   ${name}`);
	else {
		failures++;
		console.log(`FAIL ${name} ${extra}`);
	}
}

// --- BgManager
const exits = [];
const bg = new BgManager((t) => exits.push(t));
const t1 = bg.start({ command: "sleep 0.3; echo hi-from-bg" });
check("bg start id", t1.id === "bg-1" && t1.status === "running");
const done1 = await bg.wait(t1.id, 10_000);
check("bg done", done1.status === "done" && done1.exitCode === 0, JSON.stringify(done1.status));
check("bg output", done1.output.includes("hi-from-bg"));
await new Promise((r) => setTimeout(r, 100));
check("bg exit event", exits.length === 1 && exits[0].id === t1.id);
const t2 = bg.start({ command: "sleep 30", timeoutMs: 300 });
const done2 = await bg.wait(t2.id, 10_000);
check("bg timeout kill", done2.status === "timeout", done2.status);
const t3 = bg.start({ command: "exit 7" });
const done3 = await bg.wait(t3.id, 10_000);
check("bg failure exit", done3.status === "failed" && done3.exitCode === 7);

// --- KbStore
const kb = new KbStore(join(home, "shared.pl"));
const seq = await kb.post("main", "agent-1", "task", "compute(1+2)");
check("kb post", seq === 1);
await kb.post("agent-1", "main", "result", "result(3)");
await kb.assert("analysis(agent-1, done)");
const q1 = await kb.query("msg(F, T, K, P, S, _)", 10);
check("kb query", q1.count === 2, `count=${q1.count}`);
const got = await kb.take("agent-1");
check("kb take", got.length === 1 && got[0].includes("msg(main,'agent-1',task,compute(1+2)"), got[0]);
const got2 = await kb.take("agent-1");
check("kb take empty", got2.length === 0);
const n = await kb.retract("analysis(_, _)");
check("kb retract", n === 1);

// --- validateTerm
try {
	await validateTerm("foo((");
	check("validateTerm rejects", false);
} catch {
	check("validateTerm rejects", true);
}
try {
	await validateTerm("ok(shell, 3)");
	check("validateTerm accepts", true);
} catch (e) {
	check("validateTerm accepts", false, String(e));
}

// --- SymbolicStore + gate
const sym = new SymbolicStore(home);
sym.recordObservation("bash", "bash pytest -q", true);
sym.recordObservation("bash", "bash make install", false);
sym.setRequirement("tests", "tests_green");
try {
	await sym.addRules(":- shell('touch rules_pwned').");
	check("rules reject directives", false);
} catch {
	check("rules reject directives", true);
}
await sym.addRules(
	"tests_green :- obs(_, tool(bash), summary(S), ok(true), _), sub_atom(S, _, _, _, pytest).\n" +
		"install_ok :- obs(_, tool(bash), summary(S), ok(true), _), sub_atom(S, _, _, _, make_install_marker_never).\n",
);
sym.setRequirement("install", "install_ok");
const g1 = await sym.gate();
check("gate fail install", !g1.ok && g1.failed.some((f) => f.name === "install"), JSON.stringify(g1.failed));
check("gate pass tests present", g1.failed.some((f) => f.name === "tests") === false);
sym.removeRequirement("install");
const g2 = await sym.gate();
check("gate pass after removal", g2.ok && g2.requirements === 1 && g2.observations === 2, JSON.stringify(g2));

console.log(failures === 0 ? "ALL MODULE TESTS PASSED" : `${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);
