{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = config.llm.starintelHarness;

  prologRlmGlm = pkgs.writeShellApplication {
    name = "starintel-prolog-rlm-glm";
    runtimeInputs = [ pkgs.coreutils pkgs.python3 pkgs.swi-prolog ];
    text = ''
      set -euo pipefail
      quota_env=${lib.escapeShellArg config.llm.quotaTelemetry.environmentFile}
      if [ -z "''${ZAI_API_KEY:-}" ] && [ -r "$quota_env" ]; then
        ZAI_API_KEY="$(python3 - "$quota_env" <<'PY'
import os
import shlex
import stat
import sys

path = sys.argv[1]

def require_private(filename):
    mode = stat.S_IMODE(os.stat(filename).st_mode)
    if mode & 0o077:
        raise SystemExit(f"refusing non-private credential file: {filename}")

require_private(path)
values = {}
with open(path, encoding="utf-8") as stream:
    for raw in stream:
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("export "):
            line = line[7:].lstrip()
        key, sep, value = line.partition("=")
        if not sep or key not in {"ZAI_API_KEY", "LLM_LOG_ZAI_KEY_FILE"}:
            continue
        parsed = shlex.split(value, comments=False, posix=True)
        values[key] = parsed[0] if len(parsed) == 1 else value

key = values.get("ZAI_API_KEY", "")
if key:
    print(key, end="")
    raise SystemExit(0)

key_file = values.get("LLM_LOG_ZAI_KEY_FILE", "")
if key_file:
    key_file = os.path.expanduser(key_file)
    require_private(key_file)
    with open(key_file, encoding="utf-8") as stream:
        print(stream.read().strip(), end="")
PY
        )"
        export ZAI_API_KEY
      fi
      if [ -z "''${ZAI_API_KEY:-}" ]; then
        echo "starintel-prolog-rlm-glm: z.AI credential unavailable; configure the private llm quota environment file" >&2
        exit 78
      fi
      prompt="$(cat)"
      bytes="$(printf '%s' "$prompt" | wc -c)"
      if [ "$bytes" -gt ${toString cfg.maxPromptBytes} ]; then
        echo "starintel-prolog-rlm-glm: prompt exceeds ${toString cfg.maxPromptBytes} bytes" >&2
        exit 2
      fi
      exec swipl -q \
        -s ${lib.escapeShellArg "${cfg.prologRlmCheckout}/bin/prolog-rlm.pl"} \
        -- rlm "$prompt" \
        --endpoint ${lib.escapeShellArg "${cfg.proxyBaseUrl}/zai/api/coding/paas/v4/chat/completions"} \
        --model ${lib.escapeShellArg cfg.glmRlmModel} \
        --credential-env ZAI_API_KEY \
        --max-tokens ${toString cfg.maxTokens} \
        --max-cost ${toString cfg.maxCost} \
        --time-limit ${toString cfg.workerTimeLimitSeconds}
    '';
  };

  specialistNames = [
    "worker-implementation"
    "worker-tests"
    "worker-prolog-kb"
    "worker-actors"
    "worker-plugins"
    "worker-starlang-spec"
    "worker-shared-config"
    "worker-docs-book"
    "worker-aradr"
    "worker-release"
    "worker-ci"
    "worker-sync-history"
    "worker-server-api"
    "worker-biz"
    "worker-integration-e2e"
  ];

  specialistProfiles = {
    worker-implementation = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Implement production code for the assigned StarIntel slice. Add or strengthen tests and update affected docs. Do not stop at analysis or a report.";
    };
    worker-tests = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own adversarial, regression, integration, and E2E testing for the assigned slice. Fix product code when tests expose defects; do not only write a test report.";
    };
    worker-prolog-kb = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own Prolog experts, reusable Star KB rules, formal verification, provenance, symbolic plans, and Prolog-RLM integration. Produce executable rules/tests/code.";
    };
    worker-actors = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own Pro Actors, event sourcing, actor lifecycle, A2A protocol, manifests, refresh, database-service actors, and actor tests. Implement code, not architecture prose only.";
    };
    worker-plugins = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own Lisp plugin infrastructure: webhooks, local/remote plugins, capabilities, tenant root overlays, lifecycle and isolation. Implement code plus tests and docs.";
    };
    worker-starlang-spec = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Treat StarLang/spec as canonical source. Implement verification/plan grammar and downstream bindings/ports consistently; add parity and migration tests.";
    };
    worker-shared-config = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own shared configuration libraries and dotfiles-facing contracts. Keep secrets out of Git, preserve XDG/Nix conventions, and implement validation/tests.";
    };
    worker-docs-book = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own StarIntel server wiki/book/tutorial material together with runnable examples, doctests, fixtures, or code improvements. Documentation work must still ship executable evidence.";
    };
    worker-aradr = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own ARADR symbolic planning/research integrated with Prolog-RLM and Star KB. Auto-Dig may be used when it supports the slice. Turn accepted research into code/tests rather than report-only output.";
    };
    worker-release = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own release automation, release branches, version locks, changelogs, migration gates and reproducible artifacts. Lock a chosen release scope without blocking next-version work.";
    };
    worker-ci = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own CI, formal validation, KB statistics, graph/image artifacts, improvement metrics and false-green prevention. Implement gates and generators, not dashboard prose only.";
    };
    worker-sync-history = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own /_changes, RSS/Atom projections, /_sync master-to-master replication, tombstones, content-addressed revisions, internal Git history, conflicts and replay tests.";
    };
    worker-server-api = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own starintel-server APIs, general DB-service boundaries, CouchDB policy now and PostgreSQL-compatible contracts later. Implement server code and integration tests.";
    };
    worker-biz = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own explicitly assigned private StarIntel Biz work end-to-end, including child dependency repos when required. Keep private policy/config private and ship code/tests/evidence.";
    };
    worker-integration-e2e = {
      role = "worker";
      provider = "prolog-rlm-glm";
      instructions = "Own cross-repository and monorepo integration, packaging, migrations and full-stack E2E. Reconcile child repos and fix integration defects until the slice is genuinely green.";
    };
  };

  harnessConfig = {
    schema = "starintel.llm.config.v1";
    proxy = {
      base_url = cfg.proxyBaseUrl;
      quota_base_url = cfg.proxyBaseUrl;
      timeout_seconds = 3;
    };
    providers = {
      codex-plan = {
        argv = [
          "codex"
          "exec"
          "--json"
          "--sandbox"
          "read-only"
          "--ephemeral"
          "-"
        ];
        stdin_prompt = true;
        parser = "codex_jsonl";
        quota_provider = "gpt";
        timeout_seconds = cfg.plannerTimeLimitSeconds;
      };
      prolog-rlm-glm = {
        argv = [ "${prologRlmGlm}/bin/starintel-prolog-rlm-glm" ];
        stdin_prompt = true;
        parser = "text";
        quota_provider = "zai";
        timeout_seconds = cfg.workerTimeLimitSeconds;
      };
      glm-worker = {
        argv = [
          "opencode-worker"
          "--model"
          cfg.glmOpenCodeModel
          "--role"
          "worker"
          "--variant"
          "max"
          "--format"
          "text"
          "--max-retries"
          "0"
        ];
        stdin_prompt = true;
        parser = "text";
        quota_provider = "zai";
        timeout_seconds = cfg.workerTimeLimitSeconds;
      };
      glm-reviewer = {
        argv = [
          "opencode-worker"
          "--model"
          cfg.glmOpenCodeModel
          "--role"
          "reviewer"
          "--variant"
          "max"
          "--format"
          "text"
          "--max-retries"
          "0"
          "--mode"
          "readonly"
        ];
        stdin_prompt = true;
        parser = "text";
        quota_provider = "zai";
        timeout_seconds = cfg.reviewerTimeLimitSeconds;
      };
    };
    profiles = ({
      main = {
        role = "main";
        planner = "codex-plan";
        allowed_providers = [ "prolog-rlm-glm" ];
        worker_profiles = specialistNames;
        reviewers = [ "review-codex" "review-glm" ];
        max_plan_steps = 15;
        max_parallel = cfg.maxParallelWorkers;
        queue_wait_seconds = cfg.queueWaitSeconds;
        review_parallel = 2;
        review_context_bytes = 200000;
        review_policy.min_approvals = 2;
      };
      main-direct-glm = {
        role = "main";
        planner = "codex-plan";
        allowed_providers = [ "glm-worker" ];
        reviewers = [ "review-codex" "review-glm" ];
        max_parallel = cfg.maxParallelWorkers;
        review_parallel = 2;
        review_context_bytes = 200000;
        review_policy.min_approvals = 2;
      };
      review-codex = {
        role = "reviewer";
        provider = "codex-plan";
      };
      review-glm = {
        role = "reviewer";
        provider = "glm-reviewer";
      };
      worker-prolog-rlm = {
        role = "worker";
        provider = "prolog-rlm-glm";
      };
      worker-glm = {
        role = "worker";
        provider = "glm-worker";
      };
    } // specialistProfiles);
    rate_limits = {
      gpt = {
        reserve_percent = cfg.gptReservePercent;
        burst_percent = cfg.burstPercent;
        unknown_policy = "deny";
        unknown_retry_seconds = 60;
        max_concurrency = cfg.gptMaxConcurrency;
        min_interval_seconds = cfg.gptMinIntervalSeconds;
        lease_seconds = cfg.workerTimeLimitSeconds + 60;
      };
      zai = {
        reserve_percent = cfg.zaiReservePercent;
        burst_percent = cfg.burstPercent;
        unknown_policy = "deny";
        unknown_retry_seconds = 60;
        max_concurrency = cfg.zaiMaxConcurrency;
        min_interval_seconds = cfg.zaiMinIntervalSeconds;
        lease_seconds = cfg.workerTimeLimitSeconds + 60;
      };
    };
  };
in
{
  options.llm.starintelHarness = {
    enable = mkEnableOption "StarIntel profile-driven LLM worker harness";

    proxyBaseUrl = mkOption {
      type = types.str;
      default = "http://127.0.0.1:8787";
      description = "Loopback llm-log base URL used for model transport and quota telemetry.";
    };

    prologRlmCheckout = mkOption {
      type = types.str;
      default = "${config.home.homeDirectory}/Documents/Projects/prolog-rlm";
      description = "Checkout containing bin/prolog-rlm.pl.";
    };

    glmOpenCodeModel = mkOption {
      type = types.str;
      default = "zai-coding-plan/glm-5.3";
      description = "OpenCode model reference for the z.AI Coding Plan worker.";
    };

    glmRlmModel = mkOption {
      type = types.str;
      default = "glm-5.3";
      description = "Model ID passed to the z.AI Coding Plan OpenAI-compatible endpoint.";
    };

    gptReservePercent = mkOption {
      type = types.float;
      default = 10.0;
      description = "GPT reported quota held in reserve.";
    };

    zaiReservePercent = mkOption {
      type = types.float;
      default = 10.0;
      description = "z.AI reported quota held in reserve.";
    };

    burstPercent = mkOption {
      type = types.float;
      default = 5.0;
      description = "Early-window burst allowed above the time-proportional budget curve.";
    };

    gptMaxConcurrency = mkOption {
      type = types.int;
      default = 2;
      description = "Cross-process GPT worker concurrency.";
    };

    zaiMaxConcurrency = mkOption {
      type = types.int;
      default = 4;
      description = "Cross-process z.AI worker concurrency.";
    };

    gptMinIntervalSeconds = mkOption {
      type = types.float;
      default = 60.0;
      description = "Minimum launch interval for GPT-backed harness calls.";
    };

    zaiMinIntervalSeconds = mkOption {
      type = types.float;
      default = 60.0;
      description = "Minimum launch interval for z.AI-backed harness calls.";
    };

    maxParallelWorkers = mkOption {
      type = types.int;
      default = 15;
      description = "Maximum logical ready plan steps the main harness may queue in parallel; provider rate limits remain authoritative.";
    };

    queueWaitSeconds = mkOption {
      type = types.int;
      default = 7200;
      description = "Maximum time a main-worker call may wait for subscription/local rate admission.";
    };

    maxPromptBytes = mkOption {
      type = types.int;
      default = 120000;
      description = "Maximum prompt bytes accepted by the Prolog-RLM GLM wrapper.";
    };

    maxTokens = mkOption {
      type = types.int;
      default = 8192;
      description = "Per-call Prolog-RLM token ceiling.";
    };

    maxCost = mkOption {
      type = types.float;
      default = 10.0;
      description = "Per-call Prolog-RLM provider-reported cost ceiling.";
    };

    plannerTimeLimitSeconds = mkOption {
      type = types.int;
      default = 900;
      description = "Codex planning timeout.";
    };

    workerTimeLimitSeconds = mkOption {
      type = types.int;
      default = 1800;
      description = "Worker timeout and local lease duration basis.";
    };

    reviewerTimeLimitSeconds = mkOption {
      type = types.int;
      default = 900;
      description = "Reviewer timeout.";
    };
  };

  config = mkIf (config.llm.enable && cfg.enable) {
    services.llm-log.upstreams.zai = "https://api.z.ai";

    programs.opencode.settings.provider."zai-coding-plan".options.baseURL =
      "${cfg.proxyBaseUrl}/zai/api/coding/paas/v4";

    xdg.configFile."starintel/llm-harness.json".text =
      builtins.toJSON harnessConfig;

    home.sessionVariables = {
      STARINTEL_LLM_CONFIG = "${config.xdg.configHome}/starintel/llm-harness.json";
      STARINTEL_LLM_PROXY = cfg.proxyBaseUrl;
    };

    home.packages = [ prologRlmGlm ];
  };
}
