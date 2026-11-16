// rmguard classifier harness: drives the plugin's execute.before hook with a
// duck-typed ctx and asserts destructive/non-destructive classification.
// Runs from a directory containing plugin.mjs (the plugin copied from
// opencode/plugins/rmguard/index.ts, which is valid plain JS).

import plugin from "./plugin.mjs";

async function runHook(command) {
  let error = null;
  const ctx = {
    tool: {
      hook(_name, fn) {
        this._fn = fn;
      },
    },
  };
  await plugin.setup(ctx);
  try {
    await ctx.tool._fn({ tool: "bash", input: { command } });
  } catch (e) {
    error = String(e.message);
  }
  return error;
}

const mustBlock = [
  "rm -rf /tmp/opencode/scratch",
  "rm -fr build",
  "rm -r docs/",
  "rm --recursive dist",
  "rm -R /tmp/x",
  "sudo rm -rf /etc/nginx",
  "nohup rm -rf logs &",
  "xargs rm -f",
  "echo names | xargs rm -rf",
  "find . -delete",
  "find . -name '*.tmp' -delete",
  "find . -exec rm {} +",
  "dd if=img.iso of=/dev/sda",
  "dd if=x of=/dev/nvme0n1",
  "mkfs.ext4 /dev/sdb",
  "shred secret.key",
  "cd /home/unseen/x && rm -rf build; echo done",
];

const mustAllow = [
  // 2026-11-16 false positive: not file deletion
  "tea labels delete --repo nsaspy/hackmode --id 1603",
  "cd /home/unseen/starintel/hackmode && tea labels delete --repo nsaspy/hackmode 1603 2>&1; echo ---; tea pr ls --output csv",
  "gh label delete stale --repo nsaspy/hackmode",
  "docker image rm nginx",
  "git rm --cached file.txt",
  "rm single-file.txt",
  "echo 'run rm -rf / now'",
  "ls -rf /home/unseen",
  "grep -r pattern .",
  "tea pr delete 205",
  "vim rm-notes.txt",
  "systemctl restart rmguard",
];

let failed = 0;
for (const command of mustBlock) {
  const error = await runHook(command);
  if (error === null) {
    console.error(`FAIL should block: ${command}`);
    failed += 1;
  } else if (!error.includes("rmguard")) {
    console.error(`FAIL wrong error for: ${command} -> ${error}`);
    failed += 1;
  }
}
for (const command of mustAllow) {
  const error = await runHook(command);
  if (error !== null) {
    console.error(`FAIL should allow: ${command} -> ${error}`);
    failed += 1;
  }
}

// home-root targets must hard-block, not merely hold
const blocked = await runHook("rm -rf ~/starintel");
if (blocked === null || !blocked.includes("BLOCKED")) {
  console.error(`FAIL home-root deletion not BLOCKED: ${blocked}`);
  failed += 1;
}

if (failed > 0) {
  console.error(`${failed} rmguard classifier failures`);
  process.exit(1);
}
console.log("rmguard classifier: all cases pass");
