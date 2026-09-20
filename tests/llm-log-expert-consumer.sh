#!/usr/bin/env bash
set -euo pipefail

module="nix/home-manager/mods/llm.nix"
old_revision="94b79dccd9010b429eff0472c74391fa50b4db0a"

fail() {
  printf 'RED: %s\n' "$*" >&2
  exit 1
}

grep -Fq 'dataDir = llmLogDataDir;' "$module" \
  || fail 'raw capture path is not routed through the canonical llmLogDataDir binding'
grep -Fq 'llmLogDataDir = "${config.home.homeDirectory}/Documents/AI/proxy";' "$module" \
  || fail 'raw capture path moved from ~/Documents/AI/proxy'

grep -Fq 'openai = "https://api.openai.com";' "$module" \
  || fail 'OpenAI upstream routing changed'
grep -Fq 'openrouter = "https://openrouter.ai";' "$module" \
  || fail 'OpenRouter upstream routing changed'
grep -Fq 'anthropic = "https://api.anthropic.com";' "$module" \
  || fail 'Anthropic upstream routing changed'
grep -Fq 'chatgpt = "https://chatgpt.com";' "$module" \
  || fail 'ChatGPT upstream routing changed'

if grep -Fq "llmLogRevision = \"${old_revision}\";" "$module"; then
  fail 'llm-log pin still targets the pre-expert consumer revision'
fi

grep -Eq 'expert[[:space:]]*=[[:space:]]*\{' "$module" \
  || fail 'services.llm-log.expert consumer configuration is absent'
grep -Eq 'enable[[:space:]]*=[[:space:]]*true;' "$module" \
  || fail 'expert consumer is not enabled'
grep -Eq 'require[[:space:]]*=[[:space:]]*false;' "$module" \
  || fail 'initial expert rollout must remain fail-open'

# Tek9 state is separate from append-only raw capture evidence.
grep -Fq 'dataDir = "${llmLogDataDir}/expert";' "$module" \
  || fail 'expert mutable state must live in the dedicated Documents/AI/proxy/expert subdir'

# The deployed pin already supports checkpointed streaming backfill. Home Manager
# must expose bounded wrappers rather than teaching clients to read events.jsonl.
for command in llm-log-query llm-log-backfill llm-log-export; do
  grep -Fq "name = \"${command}\";" "$module" \
    || fail "missing Home Manager wrapper: ${command}"
done

grep -Fq 'systemd.user.services.llm-log-expert-backfill' "$module" \
  || fail 'incremental expert backfill service is absent'
grep -Fq 'systemd.user.timers.llm-log-expert-backfill' "$module" \
  || fail 'incremental expert backfill timer is absent'
grep -Fq 'OnUnitActiveSec = config.llm.expertBackfill.interval;' "$module" \
  || fail 'backfill timer is not driven by the configured interval'

# Operator/client discovery is explicit and does not embed secrets in Nix.
grep -Fq 'LLM_LOG_CORPUS = mkDefault llmLogCorpus;' "$module" \
  || fail 'corpus path is not exported'
grep -Fq 'LLM_LOG_EXPERT_ADMIN_URL = mkDefault llmLogExpertAdmin;' "$module" \
  || fail 'expert admin URL is not exported'
grep -Fq 'LLM_LOG_LEARN_BASE_URL = mkDefault "https://llm.starintel.actor";' "$module" \
  || fail 'KB learner endpoint is not preconfigured'
grep -Fq 'LLM_LOG_LEARN_MODEL_SELECTOR = mkDefault "auto:27b";' "$module" \
  || fail '27B learner selector is not preconfigured'

# OpenCode uses the same bounded CLI instead of opening the raw corpus.
for command in llm-log-query llm-log-backfill llm-log-export; do
  grep -Fq "${command} = {" "$module" \
    || fail "missing OpenCode command: /${command}"
done
grep -Fq 'Never read events.jsonl directly' "$module" \
  || fail 'OpenCode corpus commands do not preserve the bounded-query invariant'

printf 'llm-log expert consumer contract satisfied\n'
