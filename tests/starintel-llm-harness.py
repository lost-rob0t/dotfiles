from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[1]
MODULE = ROOT / "nix/home-manager/mods/starintel-llm-harness.nix"
ORG = ROOT / "nix/home-manager/mods/starintel-llm-harness.org"
LLM = ROOT / "nix/home-manager/mods/llm.nix"


class StarIntelHarnessConfigTests(unittest.TestCase):
    def test_literate_source_matches_generated_module(self):
        org = ORG.read_text(encoding="utf-8")
        match = re.search(r"#\+begin_src nix\n(.*)\n#\+end_src", org, re.S)
        self.assertIsNotNone(match)
        self.assertEqual(match.group(1).strip(), MODULE.read_text(encoding="utf-8").strip())

    def test_llm_profile_imports_and_enables_harness(self):
        source = LLM.read_text(encoding="utf-8")
        self.assertIn("./starintel-llm-harness.nix", source)
        self.assertIn("llm.starintelHarness.enable = true;", source)

    def test_proxy_and_quota_contract_is_explicit(self):
        source = MODULE.read_text(encoding="utf-8")
        self.assertIn('services.llm-log.upstreams.zai = "https://api.z.ai";', source)
        self.assertIn('"${cfg.proxyBaseUrl}/zai/api/coding/paas/v4"', source)
        self.assertIn('quota_provider = "gpt";', source)
        self.assertIn('quota_provider = "zai";', source)
        self.assertIn('unknown_policy = "deny";', source)
        self.assertIn('reserve_percent = cfg.gptReservePercent;', source)
        self.assertIn('reserve_percent = cfg.zaiReservePercent;', source)

    def test_main_profile_uses_prolog_rlm_and_independent_reviewers(self):
        source = MODULE.read_text(encoding="utf-8")
        self.assertIn('planner = "codex-plan";', source)
        self.assertIn('allowed_providers = [ "prolog-rlm-glm" ];', source)
        self.assertIn('reviewers = [ "review-codex" "review-glm" ];', source)
        self.assertIn('review_policy.min_approvals = 2;', source)
        self.assertIn('max_plan_steps = 15;', source)
        self.assertIn('default = 15;', source)
        self.assertIn('default = 7200;', source)
        self.assertIn('worker_profiles = specialistNames;', source)
        self.assertEqual(source.count('role = "worker";'), 17)
        for name in (
            "worker-implementation",
            "worker-tests",
            "worker-prolog-kb",
            "worker-actors",
            "worker-plugins",
            "worker-starlang-spec",
            "worker-shared-config",
            "worker-docs-book",
            "worker-aradr",
            "worker-release",
            "worker-ci",
            "worker-sync-history",
            "worker-server-api",
            "worker-biz",
            "worker-integration-e2e",
        ):
            self.assertIn(f'"{name}"', source)
        self.assertIn('default = "zai-coding-plan/glm-5.3";', source)
        self.assertIn('default = "glm-5.3";', source)
        self.assertGreaterEqual(source.count('"max"'), 2)
        self.assertIn('default = 60.0;', source)

    def test_credentials_are_not_embedded(self):
        source = MODULE.read_text(encoding="utf-8")
        self.assertNotRegex(source, r"sk-[A-Za-z0-9]")
        self.assertIn("--credential-env ZAI_API_KEY", source)
        self.assertNotIn("ZAI_API_KEY =", source)

    def test_prolog_rlm_reuses_private_quota_credential_source(self):
        source = MODULE.read_text(encoding="utf-8")
        self.assertIn("pkgs.python3", source)
        self.assertIn("config.llm.quotaTelemetry.environmentFile", source)
        self.assertIn('mode & 0o077', source)
        self.assertIn('LLM_LOG_ZAI_KEY_FILE', source)
        self.assertIn("refusing non-private credential file", source)
        self.assertIn("z.AI credential unavailable", source)
        self.assertNotIn('source "$quota_env"', source)
        self.assertNotIn('. "$quota_env"', source)


if __name__ == "__main__":
    unittest.main()
