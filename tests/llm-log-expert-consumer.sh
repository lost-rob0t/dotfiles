#!/usr/bin/env bash
set -euo pipefail

module="nix/home-manager/mods/llm.nix"
old_revision="94b79dccd9010b429eff0472c74391fa50b4db0a"

fail() {
  printf 'RED: %s\n' "$*" >&2
  exit 1
}

grep -Fq 'dataDir = "${config.home.homeDirectory}/Documents/AI/proxy";' "$module" \
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

# The owner co-locates mutable Tek9 state with the capture corpus on this
# machine, contained in a dedicated expert/ subdir. The invariant that must
# hold: the expert never targets the corpus root itself, so the append-only
# evidence files (events.jsonl / frames.jsonl / events.pl) stay untouched.
grep -Fq 'dataDir = "${config.home.homeDirectory}/Documents/AI/proxy/expert";' "$module" \
  || fail 'expert mutable state must live in the dedicated Documents/AI/proxy/expert subdir'


printf 'llm-log expert consumer contract satisfied\n'
