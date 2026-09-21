from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts/zara-feature-lab.py"
SPEC = importlib.util.spec_from_file_location("zara_feature_lab", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
LAB = importlib.util.module_from_spec(SPEC)
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


if __name__ == "__main__":
    unittest.main()
