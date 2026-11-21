/**
 * Background process manager: spawn detached-from-the-turn processes,
 * capture output into a ring buffer, and report completion through a
 * callback so the extension can deliver finish events into the session.
 */

import { spawn, type ChildProcess } from "node:child_process";

export interface BgTask {
	id: string;
	kind: "process" | "agent";
	label: string;
	command: string;
	startedAt: number;
	endedAt?: number;
	status: "running" | "done" | "failed" | "killed" | "timeout";
	exitCode: number | null;
	timedOut: boolean;
	notify: boolean;
	output: string;
}

export interface BgStartOpts {
	command: string;
	argv?: string[];
	cwd?: string;
	env?: Record<string, string>;
	label?: string;
	kind?: "process" | "agent";
	notify?: boolean;
	timeoutMs?: number;
}

const MAX_OUTPUT = 128 * 1024;

export class BgManager {
	private tasks = new Map<string, BgTask>();
	private seq = 0;
	private readonly onExit: (task: BgTask) => void;

	constructor(onExit: (task: BgTask) => void) {
		this.onExit = onExit;
	}

	public start(opts: BgStartOpts): BgTask {
		const id = `bg-${++this.seq}`;
		const task: BgTask = {
			id,
			kind: opts.kind ?? "process",
			label: opts.label ?? opts.command.slice(0, 60),
			command: opts.command,
			startedAt: Date.now(),
			status: "running",
			exitCode: null,
			timedOut: false,
			notify: opts.notify ?? true,
			output: "",
		};
		this.tasks.set(id, task);

		const child: ChildProcess = opts.argv
			? spawn(opts.argv[0], opts.argv.slice(1), {
					cwd: opts.cwd,
					env: { ...process.env, ...opts.env },
					stdio: ["ignore", "pipe", "pipe"],
				})
			: spawn("/bin/sh", ["-c", opts.command], {
					cwd: opts.cwd,
					env: { ...process.env, ...opts.env },
					stdio: ["ignore", "pipe", "pipe"],
				});
		this.procs.set(id, child);

		const append = (d: Buffer) => {
			task.output += d.toString();
			if (task.output.length > MAX_OUTPUT) {
				task.output = task.output.slice(task.output.length - MAX_OUTPUT);
			}
		};
		child.stdout?.on("data", append);
		child.stderr?.on("data", append);

		if (opts.timeoutMs) {
			const timer = setTimeout(() => {
				if (task.status === "running") {
					task.timedOut = true;
					child.kill("SIGKILL");
				}
			}, opts.timeoutMs);
			child.on("close", () => clearTimeout(timer));
		}

		child.on("close", (code, signal) => {
			task.endedAt = Date.now();
			if (task.timedOut) {
				task.status = "timeout";
			} else if (signal) {
				task.status = "killed";
			} else if (code === 0) {
				task.status = "done";
			} else {
				task.status = "failed";
			}
			task.exitCode = code;
			this.onExit(task);
		});

		child.on("error", (err) => {
			task.endedAt = Date.now();
			task.status = "failed";
			task.output += `\nspawn error: ${err.message}`;
			this.onExit(task);
		});

		return task;
	}

	public list(): BgTask[] {
		return [...this.tasks.values()];
	}

	public runningCount(): number {
		return this.list().filter((t) => t.status === "running").length;
	}

	public get(id: string): BgTask | undefined {
		return this.tasks.get(id);
	}

	public logs(id: string, tailBytes = 4096): string {
		const task = this.tasks.get(id);
		if (!task) throw new Error(`unknown task ${id}`);
		return task.output.slice(Math.max(0, task.output.length - tailBytes));
	}

	public kill(id: string): boolean {
		const task = this.tasks.get(id);
		if (!task || task.status !== "running") return false;
		this.killRunning(task);
		return true;
	}

	private killRunning(task: BgTask) {
		const proc = this.procs.get(task.id);
		proc?.kill("SIGTERM");
		setTimeout(() => {
			if (task.status === "running") proc?.kill("SIGKILL");
		}, 3000);
	}

	public killAll(): void {
		for (const task of this.tasks.values()) {
			if (task.status === "running") this.killRunning(task);
		}
	}

	public async wait(id: string, timeoutMs = 60_000): Promise<BgTask> {
		const task = this.tasks.get(id);
		if (!task) throw new Error(`unknown task ${id}`);
		if (task.status !== "running") return task;
		return new Promise((resolve, reject) => {
			const timer = setTimeout(() => {
				clearInterval(poll);
				reject(new Error(`bg_wait timed out after ${timeoutMs}ms`));
			}, timeoutMs);
			const poll = setInterval(() => {
				if (task.status !== "running") {
					clearInterval(poll);
					clearTimeout(timer);
					resolve(task);
				}
			}, 100);
		});
	}

	private procs = new Map<string, ChildProcess>();
}
