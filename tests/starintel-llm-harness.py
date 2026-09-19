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
        self.assertIn('default = "zai-coding-plan/glm-5.3";', source)
        self.assertIn('default = "glm-5.3";', source)
        self.assertGreaterEqual(source.count('"max"'), 2)
        self.assertIn('default = 30.0;', source)

    def test_credentials_are_not_embedded(self):
        source = MODULE.read_text(encoding="utf-8")
        self.assertNotRegex(source, r"sk-[A-Za-z0-9]")
        self.assertIn("--credential-env ZAI_API_KEY", source)
        self.assertNotIn("ZAI_API_KEY =", source)


if __name__ == "__main__":
    unittest.main()
