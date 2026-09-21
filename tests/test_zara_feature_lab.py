from __future__ import annotations

import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/zara-feature-lab.py"
SPEC = importlib.util.spec_from_file_location("zara_feature_lab", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
LAB = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = LAB
SPEC.loader.exec_module(LAB)


class ZaraFeatureLabTests(unittest.TestCase):
    def test_exact_five_symbolic_workers(self):
        workers = LAB.load_specs(ROOT)
        self.assertEqual(
            [worker["id"] for worker in workers],
            ["symbolic-runtime", "music-mpris", "bash", "proxmox", "nix"],
        )
        self.assertTrue(all(worker["symbolic_policy"] == "zero-model-expert-first" for worker in workers))
        self.assertTrue(all(worker["expert_file"].startswith(".zara/experts/") for worker in workers))

    def test_promotion_reuses_existing_plugins(self):
        workers = {worker["id"]: worker for worker in LAB.load_specs(ROOT)}
        self.assertEqual(workers["music-mpris"]["target_plugin"], "zara-media")
        self.assertEqual(workers["bash"]["target_plugin"], "zara-shell")
        self.assertEqual(workers["proxmox"]["target_plugin"], "zara-sysadmin")
        self.assertEqual(workers["nix"]["target_plugin"], "zara-sysadmin")
        self.assertEqual(workers["symbolic-runtime"]["target_plugin"], "zara-expert")

    def test_prompt_requires_prolog_rlm_and_secret_boundary(self):
        worker = LAB.load_specs(ROOT)[1]
        with tempfile.TemporaryDirectory() as temp:
            paths = LAB.Paths(
                dotfiles=ROOT,
                prolog_rlm=Path(temp) / "prolog-rlm",
                zara_plugins=Path(temp) / "zara-plugins",
                state=Path(temp) / "state",
            )
            prompt = LAB.task_prompt(
                paths,
                Path(temp) / "worktree",
                worker,
                {
                    "runtime": "prolog-rlm",
                    "runtime_ready": True,
                    "provider_policy": "disabled",
                    "max_model_calls": 0,
                    "model_calls": 0,
                },
            )
        self.assertIn("Prolog-RLM admission already passed", prompt)
        self.assertIn("environment variables, OS wallet/keyring, or Emacs auth-source", prompt)
        self.assertIn("--mode isolated-mutate", (ROOT / ".zara/labs/zara-feature-lab/README.md").read_text())

    def test_copy_overlay_rejects_symlinks(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            source = root / "source"
            destination = root / "destination"
            source.mkdir()
            (source / "real.txt").write_text("ok")
            (source / "link").symlink_to(source / "real.txt")
            with self.assertRaises(LAB.LabError):
                LAB.copy_overlay(source, destination)

    def test_feature_config_contains_no_secret_values(self):
        data = json.loads((ROOT / ".zara/labs/zara-feature-lab/features.json").read_text())
        encoded = json.dumps(data).lower()
        for forbidden in ("api_key", "token_value", "password", "private_key"):
            self.assertNotIn(forbidden, encoded)

    def test_promotion_dirty_filter_ignores_only_verifier_runtime(self):
        with tempfile.TemporaryDirectory() as temp:
            repo = Path(temp)
            subprocess.run(["git", "init", "-q", str(repo)], check=True)
            subprocess.run(["git", "-C", str(repo), "config", "user.email", "lab@example.invalid"], check=True)
            subprocess.run(["git", "-C", str(repo), "config", "user.name", "Feature Lab"], check=True)
            (repo / "README").write_text("base\\n")
            subprocess.run(["git", "-C", str(repo), "add", "README"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "base"], check=True)
            (repo / ".prolog").mkdir()
            (repo / ".prolog/facts.kb").write_text("runtime evidence\\n")
            (repo / "feature.txt").write_text("uncommitted feature\\n")
            self.assertEqual(LAB.candidate_dirty_paths(repo), ["feature.txt"])

    def test_runtime_gate_adopts_only_upstream_reasoning_selector(self):
        gate = (ROOT / "scripts/zara-feature-lab-runtime.pl").read_text()
        self.assertIn("current_predicate(rlm:rlm_reasoning_mode_ready/0)", gate)
        self.assertIn("rlm:reasoning_mode_select(", gate)
        self.assertIn("State.effective == symbolic", gate)
        self.assertIn('"pending-upstream"', gate)

    def test_home_manager_autostarts_the_five_worker_lab(self):
        module = (ROOT / "nix/home-manager/mods/zara-feature-lab.nix").read_text()
        desktop = (ROOT / "nix/home-manager/systems/desktop/home.nix").read_text()
        self.assertIn("systemd.user.services.zara-feature-lab", module)
        self.assertIn('ExecStart = "${featureLab}/bin/zara-feature-lab start";', module)
        self.assertIn("featureLab.autoStart = true;", desktop)


if __name__ == "__main__":
    unittest.main()
