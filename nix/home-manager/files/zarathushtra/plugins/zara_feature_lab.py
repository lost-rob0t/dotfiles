from __future__ import annotations

import json
import subprocess

from langchain_core.tools import StructuredTool
from zara.plugins import PluginMetadata, ServicePlugin


LAB_EXECUTABLE = "@labExecutable@"
APPROVAL_METADATA = {"zara_requires_approval": True}


class ZaraFeatureLabPlugin(ServicePlugin):
    metadata = PluginMetadata(
        name="zara-feature-lab",
        version="0.1.0",
        api_version="1",
        description="Five-worker Dotfiles Zara plugin incubation lab with Prolog-RLM symbolic admission",
    )

    def start(self, runtime) -> None:
        return None

    def stop(self) -> None:
        return None

    @staticmethod
    def _run(*argv: str, timeout: int = 120) -> str:
        proc = subprocess.run(
            [LAB_EXECUTABLE, *argv],
            text=True,
            capture_output=True,
            timeout=timeout,
            check=False,
        )
        payload = proc.stdout.strip()
        if not payload:
            raise RuntimeError(proc.stderr.strip() or f"zara-feature-lab exited {proc.returncode}")
        try:
            parsed = json.loads(payload)
        except json.JSONDecodeError as exc:
            raise RuntimeError("zara-feature-lab returned invalid JSON") from exc
        if proc.returncode != 0 or parsed.get("ok") is not True:
            raise RuntimeError(parsed.get("error") or f"zara-feature-lab exited {proc.returncode}")
        return json.dumps(parsed, ensure_ascii=False, sort_keys=True)

    def status(self) -> str:
        return self._run("status", timeout=30)

    def launch_all(self) -> str:
        return self._run("start", timeout=180)

    def stop_workers(self) -> str:
        return self._run("stop", timeout=30)

    def promote(self, feature_id: str) -> str:
        if not feature_id or "/" in feature_id or ".." in feature_id:
            raise ValueError("feature_id must be a simple feature name")
        return self._run("promote", feature_id, timeout=600)

    def tools(self):
        return (
            StructuredTool.from_function(
                func=self.status,
                name="feature_lab.status",
                description="Show the five configured Zara feature workers, branches, worktrees, symbolic admission receipts, and live process state.",
            ),
            StructuredTool.from_function(
                func=self.launch_all,
                name="feature_lab.start",
                description="Admit and launch exactly five isolated Zara plugin workers after Prolog-RLM plus zero-model expert checks.",
                metadata=APPROVAL_METADATA,
            ),
            StructuredTool.from_function(
                func=self.stop_workers,
                name="feature_lab.stop",
                description="Stop running Zara feature-lab worker process groups without deleting branches or worktrees.",
                metadata=APPROVAL_METADATA,
            ),
            StructuredTool.from_function(
                func=self.promote,
                name="feature_lab.promote",
                description="Promote one verified Dotfiles feature overlay into a new isolated zara-plugins worktree/branch and run focused validation. Never merges master.",
                metadata=APPROVAL_METADATA,
            ),
        )


def create_plugin():
    return ZaraFeatureLabPlugin()
